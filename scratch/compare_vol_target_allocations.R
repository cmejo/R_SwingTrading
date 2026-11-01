#!/usr/bin/env Rscript
# ==============================================================================
# scratch/compare_vol_target_allocations.R
# Head-to-Head 20-Year Comparison of 50/50, 40/60, and 30/70 Core-Satellite
# Each with 16% Volatility Targeting to SGOV
# ==============================================================================

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
})

HIST <- "data/data_historical"
OUT  <- "output"
ART  <- "/Users/cmejo/.gemini/antigravity/brain/64019830-0cac-4bb1-904e-d4560e71468c"

EVAL_START <- as.Date("2006-10-02")
EVAL_END   <- as.Date("2026-10-06")
COST       <- 0.0010
MARGIN     <- 0.070

adj <- function(sym) {
  p <- file.path(HIST, sprintf("data_%s.rds", tolower(sym)))
  if (!file.exists(p)) return(NULL)
  d <- readRDS(p)
  a <- d[, grep("Adjusted$", colnames(d))]
  a[a <= 0] <- NA
  a
}

qqq_p <- adj("QQQ")
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

for (j in seq_len(ncol(P))) {
  v <- P[, j]
  f <- which(!is.na(v))[1]
  if (!is.na(f)) v[f:length(v)] <- zoo::na.locf(v[f:length(v)], na.rm = FALSE)
  P[, j] <- v
}

q  <- as.numeric(qqq_p[cal])
R  <- rbind(NA, P[-1, , drop = FALSE] / P[-nrow(P), , drop = FALSE] - 1); R[is.na(R)] <- 0
rq <- c(0, q[-1] / q[-length(q)] - 1)

irx <- readRDS(file.path(HIST, "data_^irx.rds"))
rf  <- as.numeric(zoo::na.locf(irx[, 4], na.rm = FALSE)[cal])
rf  <- zoo::na.locf(rf, na.rm = FALSE)
rf[is.na(rf)] <- 2
rf  <- rf / 100 / 252

roll_vol_20 <- rollapply(rq, width = 20, FUN = function(x) sd(x) * sqrt(252), align = "right", fill = 0.20)

ev <- which(cal >= EVAL_START)
dates <- cal[ev]
month_end <- c(format(cal[-1], "%m") != format(cal[-length(cal)], "%m"), TRUE)
pre2006_names <- colnames(P)[!is.na(P[ev[1] - 252, ])]

# Generalized Simulator with Volatility Targeting into SGOV
sim_vt_sgov <- function(core_pct = 0.50, top_n = 5, vol_target = 0.16,
                        universe_subset = NULL, label = "VT_SGOV") {
  sat_pct <- 1 - core_pct
  
  eq <- 1.0; peak_eq <- 1.0
  out_ret <- numeric(length(ev))
  turn_tot <- 0
  
  curr_core_w  <- 0.0
  curr_sat_w   <- rep(0.0, ncol(P)); names(curr_sat_w) <- colnames(P)
  entry_prices <- rep(NA_real_, ncol(P)); names(entry_prices) <- colnames(P)
  stopped_out  <- rep(FALSE, ncol(P)); names(stopped_out) <- colnames(P)
  
  for (k in seq_along(ev)) {
    t <- ev[k]
    r_qqq <- rq[t]
    r_sat <- R[t, ]
    
    tot_sat_val  <- sum(curr_sat_w * (1 + r_sat))
    tot_core_val <- curr_core_w * (1 + r_qqq)
    curr_cash_w  <- 1 - (curr_core_w + sum(curr_sat_w)) # Earns SGOV / T-bill yield
    
    cash_gain <- if (curr_cash_w >= 0) curr_cash_w * rf[t] else curr_cash_w * MARGIN / 252
    day_gain  <- (curr_core_w * r_qqq) + sum(curr_sat_w * r_sat) + cash_gain
    
    eq <- eq * (1 + day_gain)
    if (eq > peak_eq) peak_eq <- eq
    
    curr_core_w  <- tot_core_val / (1 + day_gain)
    curr_sat_w   <- (curr_sat_w * (1 + r_sat)) / (1 + day_gain)
    
    # 1. Satellite -20% Disaster Stop Watchdog
    active_sats <- names(curr_sat_w)[curr_sat_w > 0]
    for (sym in active_sats) {
      if (!is.na(entry_prices[sym]) && entry_prices[sym] > 0) {
        if ((P[t, sym] / entry_prices[sym] - 1) <= -0.20 && !stopped_out[sym]) {
          to <- curr_sat_w[sym]
          eq <- eq * (1 - COST * to)
          turn_tot <- turn_tot + to
          curr_sat_w[sym] <- 0
          stopped_out[sym] <- TRUE
        }
      }
    }
    
    # 2. Monthly Rebalance Check
    s <- t - 1
    if (month_end[s] || k == 1) {
      realized_v <- roll_vol_20[s]
      scale_mult <- min(1.0, vol_target / max(0.05, realized_v))
      
      tgt_core_w <- core_pct * scale_mult
      
      # Satellite Selection
      valid_syms <- if (!is.null(universe_subset)) universe_subset else colnames(P)
      lb_252 <- pmax(1, s - 252); lb_126 <- pmax(1, s - 126)
      ok <- valid_syms[!is.na(P[s, valid_syms]) & !is.na(P[lb_252, valid_syms]) & (s >= 252)]
      
      tgt_sat_w <- rep(0.0, ncol(P)); names(tgt_sat_w) <- colnames(P)
      if (length(ok) > 0) {
        ret_stock <- P[s, ok] / P[lb_126, ok] - 1
        ret_qqq   <- q[s] / q[lb_126] - 1
        rs        <- ret_stock - ret_qqq
        
        sma200_stocks <- sapply(ok, function(sym) mean(P[(s - 199):s, sym], na.rm = TRUE))
        pass_trend <- P[s, ok] >= sma200_stocks
        ok_filtered <- ok[pass_trend]
        if (length(ok_filtered) >= top_n) {
          ok <- ok_filtered; rs <- rs[ok]
        }
        
        top_syms <- names(sort(rs, decreasing = TRUE))[1:min(top_n, length(rs))]
        if (length(top_syms) > 0) {
          tgt_sat_w[top_syms] <- (sat_pct * scale_mult) / length(top_syms)
        }
        
        new_entrants <- setdiff(top_syms, names(curr_sat_w)[curr_sat_w > 0])
        for (ne in new_entrants) { entry_prices[ne] <- P[t, ne]; stopped_out[ne] <- FALSE }
      }
      
      to <- abs(tgt_core_w - curr_core_w) + sum(abs(tgt_sat_w - curr_sat_w))
      if (to > 1e-6) {
        eq <- eq * (1 - COST * to)
        turn_tot <- turn_tot + to
      }
      
      curr_core_w  <- tgt_core_w
      curr_sat_w   <- tgt_sat_w
    }
    
    out_ret[k] <- if (k == 1) day_gain else eq / prev_eq - 1
    prev_eq <- eq
  }
  
  attr(out_ret, "turnover_yr") <- turn_tot / (length(ev) / 252)
  attr(out_ret, "label") <- label
  out_ret
}

cat("Simulating 50/50, 40/60, and 30/70 with 16% Vol Targeting across 2006-2026...\n")

runs_comp <- list(
  "QQQ Buy & Hold (Benchmark)"     = rq[ev],
  "60/40 + 16% Vol Target (SGOV)"  = sim_vt_sgov(core_pct = 0.60, vol_target = 0.16, label = "60/40 VT"),
  "50/50 + 16% Vol Target (SGOV)"  = sim_vt_sgov(core_pct = 0.50, vol_target = 0.16, label = "50/50 VT"),
  "40/60 + 16% Vol Target (SGOV)"  = sim_vt_sgov(core_pct = 0.40, vol_target = 0.16, label = "40/60 VT"),
  "30/70 + 16% Vol Target (SGOV)"  = sim_vt_sgov(core_pct = 0.30, vol_target = 0.16, label = "30/70 VT")
)

# Run on Clean Pre-2006 Universe
runs_clean <- list(
  "QQQ Buy & Hold (Benchmark)"     = rq[ev],
  "60/40 + 16% Vol Target (Clean)" = sim_vt_sgov(core_pct = 0.60, vol_target = 0.16, universe_subset = pre2006_names, label = "60/40 Clean"),
  "50/50 + 16% Vol Target (Clean)" = sim_vt_sgov(core_pct = 0.50, vol_target = 0.16, universe_subset = pre2006_names, label = "50/50 Clean"),
  "40/60 + 16% Vol Target (Clean)" = sim_vt_sgov(core_pct = 0.40, vol_target = 0.16, universe_subset = pre2006_names, label = "40/60 Clean"),
  "30/70 + 16% Vol Target (Clean)" = sim_vt_sgov(core_pct = 0.30, vol_target = 0.16, universe_subset = pre2006_names, label = "30/70 Clean")
)

calc_full_metrics <- function(r, core_pct, sat_pct, is_clean = FALSE) {
  c_r <- cumprod(1 + r); n <- length(r); dd <- c_r / cummax(c_r) - 1
  mdd <- -min(dd) * 100
  cagr <- (tail(c_r, 1)^(252 / n) - 1) * 100
  term <- 10000 * tail(c_r, 1)
  vol <- sd(r) * sqrt(252) * 100
  rfm <- rf[ev]
  sharpe <- if (sd(r) > 0) mean(r - rfm) / sd(r) * sqrt(252) else 0
  downside <- r[r < 0]
  sortino <- if (length(downside) > 0) mean(r - rfm) / (sqrt(mean(downside^2)) * sqrt(252)) else 0
  calmar <- if (mdd > 0) cagr / mdd else 0
  
  # After-tax exact calculation
  yrs <- unique(format(dates, "%Y"))
  if (core_pct == 1.0) {
    g <- term - 10000
    term_at <- 10000 + g * (1 - 0.238)
  } else {
    core_g <- (10000 * core_pct * prod(1 + rq[ev])) - (10000 * core_pct)
    core_at <- (10000 * core_pct) + core_g * (1 - 0.238)
    
    # Satellite component taxed annually at 40.8%
    sat_cap <- 10000 * sat_pct
    sat_cf <- 0
    # approximate satellite stream
    for (y in yrs) {
      m <- format(dates, "%Y") == y
      rg <- prod(1 + r[m]) - 1 # conservative proxy
      g <- sat_cap * rg
      tx <- g - sat_cf
      if (tx > 0) { sat_cap <- sat_cap + g - tx * 0.408; sat_cf <- 0 }
      else { sat_cap <- sat_cap + g; sat_cf <- -tx }
    }
    term_at <- core_at + sat_cap
  }
  cagr_at <- ((term_at / 10000)^(252 / n) - 1) * 100
  
  m2008 <- format(dates, "%Y") == "2008"; r2008 <- (prod(1 + r[m2008]) - 1) * 100
  m2020 <- format(dates, "%Y") == "2020"; r2020 <- (prod(1 + r[m2020]) - 1) * 100
  m2022 <- format(dates, "%Y") == "2022"; r2022 <- (prod(1 + r[m2022]) - 1) * 100
  
  data.frame(
    PreTax_Final   = sprintf("$%s", format(round(term), big.mark = ",")),
    PreTax_CAGR    = sprintf("%+.2f%%", cagr),
    AfterTax_Final = sprintf("$%s", format(round(term_at), big.mark = ",")),
    AfterTax_CAGR  = sprintf("%+.2f%%", cagr_at),
    MaxDD          = sprintf("-%.1f%%", mdd),
    Vol            = sprintf("%.1f%%", vol),
    Sharpe         = sprintf("%.2f", sharpe),
    Sortino        = sprintf("%.2f", sortino),
    Calmar         = sprintf("%.2f", calmar),
    Ret_2008       = sprintf("%+.1f%%", r2008),
    Ret_2020       = sprintf("%+.1f%%", r2020),
    Ret_2022       = sprintf("%+.1f%%", r2022),
    Turnover_yr    = sprintf("%.1fx", if (!is.null(attr(r, "turnover_yr"))) attr(r, "turnover_yr") else 0.1),
    stringsAsFactors = FALSE
  )
}

splits_test <- list(
  "QQQ Buy & Hold (Benchmark)"     = c(1.0, 0.0),
  "60/40 + 16% Vol Target (SGOV)"  = c(0.6, 0.4),
  "50/50 + 16% Vol Target (SGOV)"  = c(0.5, 0.5),
  "40/60 + 16% Vol Target (SGOV)"  = c(0.4, 0.6),
  "30/70 + 16% Vol Target (SGOV)"  = c(0.3, 0.7)
)

tab_full <- do.call(rbind, mapply(function(r, sp) calc_full_metrics(r, sp[1], sp[2], FALSE),
                                  runs_comp, splits_test, SIMPLIFY = FALSE))
tab_full$Configuration <- names(splits_test)
tab_full <- tab_full[, c("Configuration", "PreTax_Final", "PreTax_CAGR", "AfterTax_Final", "AfterTax_CAGR", "MaxDD", "Vol", "Sharpe", "Sortino", "Calmar", "Ret_2008", "Ret_2020", "Ret_2022", "Turnover_yr")]

tab_clean <- do.call(rbind, mapply(function(r, sp) calc_full_metrics(r, sp[1], sp[2], TRUE),
                                   runs_clean, splits_test, SIMPLIFY = FALSE))
tab_clean$Configuration <- names(splits_test)
tab_clean <- tab_clean[, c("Configuration", "PreTax_Final", "PreTax_CAGR", "AfterTax_Final", "AfterTax_CAGR", "MaxDD", "Vol", "Sharpe", "Sortino", "Calmar", "Ret_2008", "Ret_2020", "Ret_2022", "Turnover_yr")]

cat("\n================ FULL UNIVERSE: 50/50 VS 40/60 VS 30/70 (16% VOL TARGET SGOV) ================\n")
print(tab_full, row.names = FALSE)

cat("\n========= CLEAN PRE-2006 UNIVERSE (NO HINDSIGHT BIAS): 16% VOL TARGET SGOV =========\n")
print(tab_clean, row.names = FALSE)

# Generate Visual Comparison Chart
chart_path <- file.path(ART, "compare_50_40_30_vol_target_chart.png")
png(chart_path, width = 1400, height = 1100, res = 130)
par(mfrow = c(2, 2), mar = c(4, 4.5, 2.5, 1.5), oma = c(0, 0, 2, 0))

eq_curves <- lapply(runs_comp, function(r) 10000 * cumprod(1 + r))
dd_curves <- lapply(runs_comp, function(r) { c_r <- cumprod(1 + r); (c_r / cummax(c_r) - 1) * 100 })

# 1. Equity Curves (Log Scale)
cols <- c("#78909c", "#1565c0", "#2e7d32", "#e65100", "#c2185b")
plot(dates, eq_curves[["50/50 + 16% Vol Target (SGOV)"]], type = "l", col = cols[3], lwd = 2.2, log = "y",
     ylim = range(unlist(eq_curves)), main = "A. Growth of $10,000 (Log Scale, 2006-2026)",
     ylab = "Portfolio Equity ($)", xlab = "")
lines(dates, eq_curves[["40/60 + 16% Vol Target (SGOV)"]], col = cols[4], lwd = 2.5)
lines(dates, eq_curves[["30/70 + 16% Vol Target (SGOV)"]], col = cols[5], lwd = 2.5)
lines(dates, eq_curves[["60/40 + 16% Vol Target (SGOV)"]], col = cols[2], lwd = 1.8)
lines(dates, eq_curves[["QQQ Buy & Hold (Benchmark)"]],   col = cols[1], lwd = 1.5, lty = 2)
grid(col = "#e0e0e0")
legend("topleft", bty = "n", cex = 0.82,
       legend = c(
         sprintf("30/70 + 16%% VT SGOV ($%s | +%.1f%% CAGR)", format(round(tail(eq_curves[[5]], 1)), big.mark = ","), as.numeric(sub("%", "", tab_full$PreTax_CAGR[5]))),
         sprintf("40/60 + 16%% VT SGOV ($%s | +%.1f%% CAGR)", format(round(tail(eq_curves[[4]], 1)), big.mark = ","), as.numeric(sub("%", "", tab_full$PreTax_CAGR[4]))),
         sprintf("50/50 + 16%% VT SGOV ($%s | +%.1f%% CAGR)", format(round(tail(eq_curves[[3]], 1)), big.mark = ","), as.numeric(sub("%", "", tab_full$PreTax_CAGR[3]))),
         sprintf("60/40 + 16%% VT SGOV ($%s | +%.1f%% CAGR)", format(round(tail(eq_curves[[2]], 1)), big.mark = ","), as.numeric(sub("%", "", tab_full$PreTax_CAGR[2]))),
         sprintf("QQQ Buy & Hold ($%s | +%.1f%% CAGR)", format(round(tail(eq_curves[[1]], 1)), big.mark = ","), as.numeric(sub("%", "", tab_full$PreTax_CAGR[1])))
       ),
       col = c(cols[5], cols[4], cols[3], cols[2], cols[1]), lwd = c(2.5, 2.5, 2.2, 1.8, 1.5), lty = c(1, 1, 1, 1, 2))

# 2. Drawdowns Comparison
plot(dates, dd_curves[["50/50 + 16% Vol Target (SGOV)"]], type = "l", col = cols[3], lwd = 2,
     ylim = c(-55, 0), main = "B. Underwater Drawdown Comparison (%)", ylab = "Drawdown (%)", xlab = "")
lines(dates, dd_curves[["40/60 + 16% Vol Target (SGOV)"]], col = cols[4], lwd = 2.2)
lines(dates, dd_curves[["30/70 + 16% Vol Target (SGOV)"]], col = cols[5], lwd = 2.5)
lines(dates, dd_curves[["60/40 + 16% Vol Target (SGOV)"]], col = cols[2], lwd = 1.8)
lines(dates, dd_curves[["QQQ Buy & Hold (Benchmark)"]],   col = cols[1], lwd = 1.2, lty = 2)
grid(col = "#e0e0e0")
legend("bottomleft", bty = "n", cex = 0.82,
       legend = c(
         sprintf("60/40 VT (MaxDD: %s)", tab_full$MaxDD[2]),
         sprintf("50/50 VT (MaxDD: %s)", tab_full$MaxDD[3]),
         sprintf("40/60 VT (MaxDD: %s)", tab_full$MaxDD[4]),
         sprintf("30/70 VT (MaxDD: %s)", tab_full$MaxDD[5]),
         sprintf("QQQ Buy & Hold (MaxDD: %s)", tab_full$MaxDD[1])
       ),
       col = c(cols[2], cols[3], cols[4], cols[5], cols[1]), lwd = 2, lty = c(1, 1, 1, 1, 2))

# 3. Sharpe & Calmar Ratios
sharpes_v <- as.numeric(tab_full$Sharpe)
calmars_v <- as.numeric(tab_full$Calmar)
mat_v <- rbind("Sharpe Ratio" = sharpes_v[2:5], "Calmar Ratio" = calmars_v[2:5])
colnames(mat_v) <- c("60/40 VT", "50/50 VT", "40/60 VT", "30/70 VT")

barplot(mat_v, beside = TRUE, col = c("#1565c0", "#2e7d32"),
        main = "C. Risk-Adjusted Efficiency (Sharpe & Calmar)", ylab = "Ratio Value",
        las = 1, cex.names = 0.85)
grid(col = "#e0e0e0")
legend("topleft", bty = "n", cex = 0.85, legend = c("Sharpe Ratio", "Calmar Ratio"), fill = c("#1565c0", "#2e7d32"))

# 4. Bear Markets Performance (2008 & 2022)
r08_v <- as.numeric(sub("%", "", tab_full$Ret_2008[2:5]))
r22_v <- as.numeric(sub("%", "", tab_full$Ret_2022[2:5]))
mat_bear <- rbind("2008 Crash" = r08_v, "2022 Bear" = r22_v)
colnames(mat_bear) <- c("60/40 VT", "50/50 VT", "40/60 VT", "30/70 VT")

barplot(mat_bear, beside = TRUE, col = c("#c62828", "#1565c0"),
        main = "D. Bear Market Performance: 2008 vs 2022 (%)", ylab = "Annual Return (%)",
        las = 1, cex.names = 0.85)
grid(col = "#e0e0e0")
legend("topleft", bty = "n", cex = 0.85, legend = c("2008 Crash", "2022 Bear"), fill = c("#c62828", "#1565c0"))

title("Head-to-Head Comparison: 16% Volatility Targeting to SGOV (2006-2026)", outer = TRUE, cex.main = 1.3)
dev.off()
cat("Chart saved to", chart_path, "\n")
