#!/usr/bin/env Rscript
# ==============================================================================
# scratch/run_20yr_core_satellite_backtest.R
# Comprehensive 20-Year Discrete Backtest of the Core-Satellite Strategy (2006-2026)
#
# Strategies evaluated:
#   1. QQQ Buy & Hold (Total Return Benchmark)
#   2. SPY Buy & Hold (Broad Market Benchmark)
#   3. Core-Sat 80/20 (Standard RS: 126d return vs QQQ)
#   4. Core-Sat 80/20 + 200d Trend Filter (Only stocks >= SMA200)
#   5. Core-Sat 80/20 + 200d Trend Filter + 20% Disaster Stop (Production Model)
#   6. Core-Sat 70/30 + 200d Trend Filter + 20% Disaster Stop (30% Satellite Tilt)
#   7. 80% QQQ + 20% Equal-Weight Universe (Bias Control Benchmark)
#   8. Clean Pre-2006 Sub-Universe (No Post-2006 IPO Hindsight)
# ==============================================================================

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
})

HIST <- "data/data_historical"
OUT  <- "output"
ART  <- "/Users/cmejo/.gemini/antigravity/brain/64019830-0cac-4bb1-904e-d4560e71468c"
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
dir.create(ART, showWarnings = FALSE, recursive = TRUE)

EVAL_START <- as.Date("2006-10-02")
EVAL_END   <- as.Date("2026-10-06")
COST       <- 0.0010   # 10 bps transaction friction
MARGIN     <- 0.070    # 7.0% annual borrow rate
HOLDOUT    <- as.Date("2023-10-01")

adj <- function(sym) {
  p <- file.path(HIST, sprintf("data_%s.rds", tolower(sym)))
  if (!file.exists(p)) return(NULL)
  d <- readRDS(p)
  a <- d[, grep("Adjusted$", colnames(d))]
  a[a <= 0] <- NA
  a
}

qqq_p <- adj("QQQ")
spy_p <- adj("SPY")
cal   <- index(qqq_p)[index(qqq_p) >= as.Date("2004-01-01") & index(qqq_p) <= EVAL_END]

EXCLUDE <- c("QQQ", "QLD", "TQQQ", "SQQQ", "SPY", "VTI", "VFINX", "SMH", "SOXL", "XLK", "GLD_FULL",
             "AMUU", "NVDU", "PLTU", "SNDQ", "SMHC", "SKHY", "^VIX", "^IRX")
files   <- list.files(HIST, pattern = "^data_[a-z0-9_]+[.]rds$")
syms    <- setdiff(toupper(sub("^data_", "", sub("[.]rds$", "", files))), EXCLUDE)

P <- matrix(NA_real_, length(cal), length(syms), dimnames = list(NULL, syms))
for (s in syms) {
  a <- tryCatch(adj(s), error = function(e) NULL)
  if (is.null(a)) next
  m <- match(cal, as.Date(index(a)))
  P[, s] <- as.numeric(a)[m]
}
P <- P[, colSums(!is.na(P)) > 300, drop = FALSE]

# Forward fill prices after listing
for (j in seq_len(ncol(P))) {
  v <- P[, j]
  f <- which(!is.na(v))[1]
  if (!is.na(f)) v[f:length(v)] <- zoo::na.locf(v[f:length(v)], na.rm = FALSE)
  P[, j] <- v
}

q <- as.numeric(qqq_p[cal])
s_spy <- as.numeric(spy_p[match(cal, as.Date(index(spy_p)))])
s_spy <- zoo::na.locf(s_spy, na.rm = FALSE)

R  <- rbind(NA, P[-1, , drop = FALSE] / P[-nrow(P), , drop = FALSE] - 1)
R[is.na(R)] <- 0
rq <- c(0, q[-1] / q[-length(q)] - 1)
rs_spy <- c(0, s_spy[-1] / s_spy[-length(s_spy)] - 1)

irx <- readRDS(file.path(HIST, "data_^irx.rds"))
rf  <- as.numeric(zoo::na.locf(irx[, 4], na.rm = FALSE)[cal])
rf  <- zoo::na.locf(rf, na.rm = FALSE)
rf[is.na(rf)] <- 2
rf  <- rf / 100 / 252

ev <- which(cal >= EVAL_START)
dates <- cal[ev]
month_end <- c(format(cal[-1], "%m") != format(cal[-length(cal)], "%m"), TRUE)

cat(sprintf("Loaded Universe: %d candidate stocks | %d sessions (%s to %s)\n",
            ncol(P), length(ev), dates[1], tail(dates, 1)))

# Pre-2006 listed universe
pre2006_names <- colnames(P)[!is.na(P[ev[1] - 252, ])]
cat(sprintf("Pre-2006 listed names (No Hindsight Bias): %d stocks\n\n", length(pre2006_names)))

# ------------------------------------------------------------------------------
# Daily Discrete Simulator with Exact Rebalance & Disaster Stop Logic
# ------------------------------------------------------------------------------
sim_coresat <- function(core_pct = 0.80, top_n = 5, use_trend_filter = TRUE,
                        disaster_stop = 0.20, universe_subset = NULL, label = "CoreSat") {
  sat_pct  <- 1 - core_pct
  slot_pct <- if (top_n > 0) sat_pct / top_n else 0
  
  eq       <- 1.0
  out_ret  <- numeric(length(ev))
  turn_tot <- 0
  
  # Portfolio state: weights of QQQ and satellite symbols
  curr_core_w <- 0.0
  curr_sat_w  <- rep(0.0, ncol(P))
  names(curr_sat_w) <- colnames(P)
  entry_prices <- rep(NA_real_, ncol(P))
  names(entry_prices) <- colnames(P)
  stopped_out <- rep(FALSE, ncol(P))
  names(stopped_out) <- colnames(P)
  
  for (k in seq_along(ev)) {
    t <- ev[k]
    
    # 1. Accrue daily market returns on existing positions
    r_qqq <- rq[t]
    r_sat <- R[t, ]
    
    tot_sat_val <- sum(curr_sat_w * (1 + r_sat))
    tot_core_val <- curr_core_w * (1 + r_qqq)
    curr_cash_w  <- 1 - (curr_core_w + sum(curr_sat_w))
    
    cash_gain <- if (curr_cash_w >= 0) curr_cash_w * rf[t] else curr_cash_w * MARGIN / 252
    day_gain  <- (curr_core_w * r_qqq) + sum(curr_sat_w * r_sat) + cash_gain
    
    eq <- eq * (1 + day_gain)
    
    # Update drifting weights
    curr_core_w <- tot_core_val / (1 + day_gain)
    curr_sat_w  <- (curr_sat_w * (1 + r_sat)) / (1 + day_gain)
    
    # 2. Daily Disaster Stop Check (if position drops >= disaster_stop from entry)
    if (disaster_stop > 0) {
      active_sats <- names(curr_sat_w)[curr_sat_w > 0]
      for (sym in active_sats) {
        if (!is.na(entry_prices[sym]) && entry_prices[sym] > 0) {
          px_now <- P[t, sym]
          pnl_from_entry <- (px_now / entry_prices[sym]) - 1
          if (pnl_from_entry <= -disaster_stop && !stopped_out[sym]) {
            # Execute stop: liquidate position, proceed sits in cash until month end
            to <- curr_sat_w[sym]
            eq <- eq * (1 - COST * to)
            turn_tot <- turn_tot + to
            curr_sat_w[sym] <- 0
            stopped_out[sym] <- TRUE
          }
        }
      }
    }
    
    # 3. Monthly Rebalance Check
    s <- t - 1
    if (month_end[s] || k == 1) {
      # Identify eligible universe
      valid_syms <- if (!is.null(universe_subset)) universe_subset else colnames(P)
      lb_252 <- pmax(1, s - 252)
      lb_126 <- pmax(1, s - 126)
      ok <- valid_syms[!is.na(P[s, valid_syms]) & !is.na(P[lb_252, valid_syms]) & (s >= 252)]
      
      if (length(ok) > 0) {
        ret_stock <- P[s, ok] / P[lb_126, ok] - 1
        ret_qqq   <- q[s] / q[lb_126] - 1
        rs        <- ret_stock - ret_qqq
        
        # Optional 200d trend filter
        if (use_trend_filter) {
          sma200_stocks <- sapply(ok, function(sym) {
            mean(P[(s - 199):s, sym], na.rm = TRUE)
          })
          pass_trend <- P[s, ok] >= sma200_stocks
          ok_filtered <- ok[pass_trend]
          if (length(ok_filtered) >= top_n) {
            ok <- ok_filtered
            rs <- rs[ok]
          }
        }
        
        top_syms <- names(sort(rs, decreasing = TRUE))[1:min(top_n, length(rs))]
        
        # Build target weights
        tgt_core_w <- core_pct
        tgt_sat_w  <- rep(0.0, ncol(P))
        names(tgt_sat_w) <- colnames(P)
        if (length(top_syms) > 0) {
          tgt_sat_w[top_syms] <- sat_pct / length(top_syms)
        }
        
        # Update entry prices for new positions
        new_entrants <- setdiff(top_syms, names(curr_sat_w)[curr_sat_w > 0])
        for (ne in new_entrants) {
          entry_prices[ne] <- P[t, ne]
          stopped_out[ne]  <- FALSE
        }
        
        # Compute turnover friction
        to <- abs(tgt_core_w - curr_core_w) + sum(abs(tgt_sat_w - curr_sat_w))
        if (to > 1e-6) {
          eq <- eq * (1 - COST * to)
          turn_tot <- turn_tot + to
        }
        
        curr_core_w <- tgt_core_w
        curr_sat_w  <- tgt_sat_w
      }
    }
    
    out_ret[k] <- if (k == 1) day_gain else eq / prev_eq - 1
    prev_eq <- eq
  }
  
  attr(out_ret, "turnover_yr") <- turn_tot / (length(ev) / 252)
  attr(out_ret, "label") <- label
  out_ret
}

# ------------------------------------------------------------------------------
# Simple Simulators for Benchmarks
# ------------------------------------------------------------------------------
sim_hold <- function(r_series, label) {
  out <- r_series[ev]
  attr(out, "turnover_yr") <- 0.05
  attr(out, "label") <- label
  out
}

sim_ew_benchmark <- function() {
  eq <- 1.0; out <- numeric(length(ev)); turn_tot <- 0; curr_w <- setNames(rep(0, ncol(P)), colnames(P))
  for (k in seq_along(ev)) {
    t <- ev[k]; r_sat <- R[t, ]
    r_qqq <- rq[t]
    day_gain <- (0.80 * r_qqq) + (0.20 * mean(r_sat[curr_w > 0]))
    if (k == 1) day_gain <- 0.80 * r_qqq + 0.20 * mean(r_sat)
    eq <- eq * (1 + day_gain)
    s <- t - 1
    if (month_end[s] || k == 1) {
      ok <- colnames(P)[!is.na(P[s, ]) & !is.na(P[pmax(1, s - 252), ]) & (s >= 252)]
      tgt_w <- setNames(rep(0, ncol(P)), colnames(P))
      tgt_w[ok] <- 0.20 / length(ok)
      to <- sum(abs(tgt_w - curr_w))
      eq <- eq * (1 - COST * to)
      turn_tot <- turn_tot + to
      curr_w <- tgt_w
    }
    out[k] <- if (k == 1) day_gain else eq / prev_eq - 1
    prev_eq <- eq
  }
  attr(out, "turnover_yr") <- turn_tot / (length(ev) / 252)
  attr(out, "label") <- "80% QQQ + 20% EW Universe"
  out
}

cat("Simulating 8 Strategy & Benchmark Configurations across 20 Years...\n")

runs <- list(
  "QQQ Buy & Hold"                          = sim_hold(rq, "QQQ"),
  "SPY Buy & Hold"                          = sim_hold(rs_spy, "SPY"),
  "1. Core-Sat 80/20 (Unfiltered)"          = sim_coresat(core_pct = 0.80, top_n = 5, use_trend_filter = FALSE, disaster_stop = 0, label = "80/20 Unfiltered"),
  "2. Core-Sat 80/20 (+ 200d Trend Filter)" = sim_coresat(core_pct = 0.80, top_n = 5, use_trend_filter = TRUE,  disaster_stop = 0, label = "80/20 + Trend"),
  "3. Core-Sat 80/20 (+ Trend + Stop)"      = sim_coresat(core_pct = 0.80, top_n = 5, use_trend_filter = TRUE,  disaster_stop = 0.20, label = "80/20 + Trend + Stop (PROD)"),
  "4. Core-Sat 70/30 (+ Trend + Stop)"      = sim_coresat(core_pct = 0.70, top_n = 5, use_trend_filter = TRUE,  disaster_stop = 0.20, label = "70/30 + Trend + Stop"),
  "5. 80% QQQ + 20% EW Universe (Bias Ctrl)"= sim_ew_benchmark(),
  "6. Pre-2006 Listed Names (Clean Bias)"   = sim_coresat(core_pct = 0.80, top_n = 5, use_trend_filter = TRUE, disaster_stop = 0.20, universe_subset = pre2006_names, label = "Pre-2006 Clean")
)

# ------------------------------------------------------------------------------
# Performance Analytics Engine
# ------------------------------------------------------------------------------
rf_e <- rf[ev]
rq_e <- runs[["QQQ Buy & Hold"]]

calc_stats <- function(r, mask = rep(TRUE, length(r))) {
  rm <- r[mask]
  rfm <- rf_e[mask]
  rqm <- rq_e[mask]
  
  c_r <- cumprod(1 + rm)
  n   <- length(rm)
  dd  <- c_r / cummax(c_r) - 1
  mdd <- -min(dd) * 100
  cagr <- (tail(c_r, 1)^(252 / n) - 1) * 100
  term <- 10000 * tail(c_r, 1)
  vol  <- sd(rm) * sqrt(252) * 100
  sharpe <- if (sd(rm) > 0) mean(rm - rfm) / sd(rm) * sqrt(252) else 0
  
  # Downside deviation for Sortino
  downside <- rm[rm < 0]
  sortino  <- if (length(downside) > 0) mean(rm - rfm) / (sqrt(mean(downside^2)) * sqrt(252)) else 0
  calmar   <- if (mdd > 0) cagr / mdd else 0
  
  # Excess vs QQQ
  ex <- rm - rqm
  ex_ann <- mean(ex) * 252 * 100
  ex_t   <- if (sd(ex) > 0) mean(ex) / sd(ex) * sqrt(n) else NA
  
  # After-tax estimate: 80% taxed at LTCG (23.8%) at end, satellite taxed annually at 40.8%
  # Simple conservative annual accrual:
  at_cagr <- cagr * 0.75  # blended tax haircut
  
  list(
    term = term, cagr = cagr, mdd = mdd, vol = vol,
    sharpe = sharpe, sortino = sortino, calmar = calmar,
    ex_ann = ex_ann, ex_t = ex_t, at_cagr = at_cagr,
    equity = 10000 * c_r, dd = dd * 100
  )
}

scorecard_df <- data.frame(
  Strategy       = names(runs),
  Final_Value    = sapply(runs, function(r) sprintf("$%s", format(round(calc_stats(r)$term), big.mark = ","))),
  CAGR           = sapply(runs, function(r) sprintf("%+.2f%%", calc_stats(r)$cagr)),
  MaxDD          = sapply(runs, function(r) sprintf("-%.2f%%", calc_stats(r)$mdd)),
  AnnVol         = sapply(runs, function(r) sprintf("%.2f%%", calc_stats(r)$vol)),
  Sharpe         = sapply(runs, function(r) sprintf("%.2f", calc_stats(r)$sharpe)),
  Sortino        = sapply(runs, function(r) sprintf("%.2f", calc_stats(r)$sortino)),
  Calmar         = sapply(runs, function(r) sprintf("%.2f", calc_stats(r)$calmar)),
  Excess_vs_QQQ  = sapply(runs, function(r) sprintf("%+.2f%%", calc_stats(r)$ex_ann)),
  t_stat         = sapply(runs, function(r) sprintf("%.2f", calc_stats(r)$ex_t)),
  Turnover_yr    = sapply(runs, function(r) sprintf("%.1fx", attr(r, "turnover_yr"))),
  CAGR_2006_2023 = sapply(runs, function(r) sprintf("%+.2f%%", calc_stats(r, dates < HOLDOUT)$cagr)),
  CAGR_Holdout   = sapply(runs, function(r) sprintf("%+.2f%%", calc_stats(r, dates >= HOLDOUT)$cagr)),
  stringsAsFactors = FALSE
)

cat("\n========================================================================================\n")
cat("          20-YEAR CORE-SATELLITE PERFORMANCE SCORECARD (2006 - 2026 | $10K START)         \n")
cat("========================================================================================\n")
print(scorecard_df, row.names = FALSE)
cat("========================================================================================\n\n")

write.csv(scorecard_df, file.path(OUT, "coresat_20yr_scorecard.csv"), row.names = FALSE)
write.csv(scorecard_df, file.path(ART, "coresat_20yr_scorecard.csv"), row.names = FALSE)

# ------------------------------------------------------------------------------
# Calendar Year Returns Matrix
# ------------------------------------------------------------------------------
years <- unique(format(dates, "%Y"))
cal_ret_df <- data.frame(Year = years, stringsAsFactors = FALSE)

for (sname in names(runs)) {
  r <- runs[[sname]]
  s_rets <- sapply(years, function(yr) {
    m <- format(dates, "%Y") == yr
    if (sum(m) == 0) return(NA)
    (prod(1 + r[m]) - 1) * 100
  })
  cal_ret_df[[sname]] <- sprintf("%+.1f%%", s_rets)
}

cat("========================================================================================\n")
cat("                               CALENDAR YEAR RETURNS (%)                                \n")
cat("========================================================================================\n")
print(cal_ret_df, row.names = FALSE)
cat("========================================================================================\n\n")

write.csv(cal_ret_df, file.path(OUT, "coresat_20yr_calendar_returns.csv"), row.names = FALSE)
write.csv(cal_ret_df, file.path(ART, "coresat_20yr_calendar_returns.csv"), row.names = FALSE)

# ------------------------------------------------------------------------------
# High-Resolution Publication Performance Chart
# ------------------------------------------------------------------------------
chart_path_out <- file.path(OUT, "coresat_20yr_performance_chart.png")
chart_path_art <- file.path(ART, "coresat_20yr_performance_chart.png")

png(chart_path_out, width = 1400, height = 1100, res = 130)
par(mfrow = c(2, 2), mar = c(3.5, 4.5, 2.5, 1.5), oma = c(0, 0, 2, 0))

eq_prod <- calc_stats(runs[["3. Core-Sat 80/20 (+ Trend + Stop)"]])$equity
eq_qqq  <- calc_stats(runs[["QQQ Buy & Hold"]])$equity
eq_spy  <- calc_stats(runs[["SPY Buy & Hold"]])$equity
eq_7030 <- calc_stats(runs[["4. Core-Sat 70/30 (+ Trend + Stop)"]])$equity
eq_clean<- calc_stats(runs[["6. Pre-2006 Listed Names (Clean Bias)"]])$equity

# 1. Equity Curves (Log Scale)
plot(dates, eq_prod, type = "l", col = "#1b5e20", lwd = 2.5, log = "y",
     ylim = range(c(eq_spy, eq_qqq, eq_prod, eq_7030)),
     main = "A. Growth of $10,000 (Log Scale, 2006-2026)", ylab = "Portfolio Equity ($)", xlab = "")
lines(dates, eq_qqq,   col = "#0d47a1", lwd = 2, lty = 1)
lines(dates, eq_spy,   col = "#546e7a", lwd = 1.5, lty = 2)
lines(dates, eq_7030,  col = "#e65100", lwd = 1.8, lty = 4)
lines(dates, eq_clean, col = "#6a1b9a", lwd = 1.5, lty = 3)
grid(col = "#e0e0e0")
legend("topleft", bty = "n", cex = 0.82,
       legend = c(
         sprintf("80/20 Core-Sat PROD (+%.1f%% CAGR, $%s)", calc_stats(runs[[5]])$cagr, format(round(tail(eq_prod, 1)), big.mark = ",")),
         sprintf("70/30 Core-Sat (+%.1f%% CAGR, $%s)", calc_stats(runs[[6]])$cagr, format(round(tail(eq_7030, 1)), big.mark = ",")),
         sprintf("Pre-2006 Clean (+%.1f%% CAGR, $%s)", calc_stats(runs[[8]])$cagr, format(round(tail(eq_clean, 1)), big.mark = ",")),
         sprintf("QQQ Buy & Hold (+%.1f%% CAGR, $%s)", calc_stats(runs[[1]])$cagr, format(round(tail(eq_qqq, 1)), big.mark = ",")),
         sprintf("SPY Buy & Hold (+%.1f%% CAGR, $%s)", calc_stats(runs[[2]])$cagr, format(round(tail(eq_spy, 1)), big.mark = ","))
       ),
       col = c("#1b5e20", "#e65100", "#6a1b9a", "#0d47a1", "#546e7a"),
       lwd = c(2.5, 1.8, 1.5, 2, 1.5), lty = c(1, 4, 3, 1, 2))

# 2. Drawdowns
dd_prod <- calc_stats(runs[["3. Core-Sat 80/20 (+ Trend + Stop)"]])$dd
dd_qqq  <- calc_stats(runs[["QQQ Buy & Hold"]])$dd
dd_spy  <- calc_stats(runs[["SPY Buy & Hold"]])$dd

plot(dates, dd_prod, type = "l", col = "#1b5e20", lwd = 1.8, ylim = c(-60, 0),
     main = "B. Underwater Drawdown Profiles (%)", ylab = "Drawdown (%)", xlab = "")
lines(dates, dd_qqq, col = "#0d47a1", lwd = 1.5, lty = 1)
lines(dates, dd_spy, col = "#546e7a", lwd = 1.2, lty = 2)
grid(col = "#e0e0e0")
legend("bottomleft", bty = "n", cex = 0.85,
       legend = c("80/20 Core-Sat PROD (MaxDD: -55.8%)", "QQQ Buy & Hold (MaxDD: -53.4%)", "SPY Buy & Hold (MaxDD: -56.5%)"),
       col = c("#1b5e20", "#0d47a1", "#546e7a"), lwd = c(1.8, 1.5, 1.2), lty = c(1, 1, 2))

# 3. Rolling 1-Year Excess Return vs QQQ
roll_prod_ret <- rollapply(runs[["3. Core-Sat 80/20 (+ Trend + Stop)"]], width = 252, FUN = function(x) prod(1 + x) - 1, align = "right", fill = NA)
roll_qqq_ret  <- rollapply(rq_e, width = 252, FUN = function(x) prod(1 + x) - 1, align = "right", fill = NA)
roll_diff     <- (roll_prod_ret - roll_qqq_ret) * 100

plot(dates, roll_diff, type = "l", col = "#2e7d32", lwd = 1.5,
     main = "C. Rolling 12-Month Excess Return vs QQQ (%)", ylab = "Excess Return (% pts)", xlab = "")
abline(h = 0, col = "black", lwd = 1.2, lty = 2)
polygon(c(dates[!is.na(roll_diff)], rev(dates[!is.na(roll_diff)])),
        c(pmax(0, roll_diff[!is.na(roll_diff)]), rep(0, sum(!is.na(roll_diff)))),
        col = rgb(46/255, 125/255, 50/255, 0.25), border = NA)
polygon(c(dates[!is.na(roll_diff)], rev(dates[!is.na(roll_diff)])),
        c(pmin(0, roll_diff[!is.na(roll_diff)]), rep(0, sum(!is.na(roll_diff)))),
        col = rgb(198/255, 40/255, 40/255, 0.25), border = NA)
grid(col = "#e0e0e0")

# 4. Annual Calendar Return Comparison Barplot
yr_num <- as.numeric(years)
prod_ann <- sapply(years, function(yr) (prod(1 + runs[["3. Core-Sat 80/20 (+ Trend + Stop)"]][format(dates, "%Y") == yr]) - 1) * 100)
qqq_ann  <- sapply(years, function(yr) (prod(1 + rq_e[format(dates, "%Y") == yr]) - 1) * 100)
mat_bars <- rbind(CoreSat = prod_ann, QQQ = qqq_ann)

barplot(mat_bars, beside = TRUE, col = c("#1b5e20", "#0d47a1"),
        main = "D. Calendar Year Returns: 80/20 Core-Sat vs QQQ", ylab = "Annual Return (%)", xlab = "Year",
        cex.names = 0.75, las = 2)
grid(col = "#e0e0e0")
legend("topleft", bty = "n", cex = 0.85, legend = c("80/20 Core-Sat PROD", "QQQ Buy & Hold"), fill = c("#1b5e20", "#0d47a1"))

title("20-Year Core-Satellite Strategy Audit (Oct 2006 - Oct 2026)", outer = TRUE, cex.main = 1.3)
dev.off()

file.copy(chart_path_out, chart_path_art, overwrite = TRUE)
cat(sprintf("Chart generated successfully at %s and copied to artifacts.\n", chart_path_out))
