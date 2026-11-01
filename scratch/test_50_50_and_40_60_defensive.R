#!/usr/bin/env Rscript
# ==============================================================================
# scratch/test_50_50_and_40_60_defensive.R
# Comprehensive 20-Year Analysis of 50/50 and 40/60 Core-Satellite Configurations
# Comparing Baseline, Volatility Targeting (SGOV), Core Trend Filter, and Dual Defense
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
sma200_q    <- as.numeric(stats::filter(q, rep(1 / 200, 200), sides = 1))
qqq_trend_up<- q >= sma200_q; qqq_trend_up[is.na(qqq_trend_up)] <- TRUE

r_qid <- pmin(0.20, pmax(-0.20, -2.0 * rq - (0.0095 / 252)))

ev <- which(cal >= EVAL_START)
dates <- cal[ev]
month_end <- c(format(cal[-1], "%m") != format(cal[-length(cal)], "%m"), TRUE)

# Generalized Simulator
sim_alloc_defensive <- function(core_pct = 0.50, top_n = 5,
                                use_core_trend = FALSE,
                                vol_target = 0, # 0 = off, e.g. 0.16
                                hedge_asset = c("sgov", "qid"),
                                label = "Test") {
  hedge_asset <- match.arg(hedge_asset)
  sat_pct <- 1 - core_pct
  
  eq <- 1.0; peak_eq <- 1.0
  out_ret <- numeric(length(ev))
  turn_tot <- 0
  
  curr_core_w  <- 0.0
  curr_hedge_w <- 0.0
  curr_sat_w   <- rep(0.0, ncol(P)); names(curr_sat_w) <- colnames(P)
  entry_prices <- rep(NA_real_, ncol(P)); names(entry_prices) <- colnames(P)
  stopped_out  <- rep(FALSE, ncol(P)); names(stopped_out) <- colnames(P)
  
  for (k in seq_along(ev)) {
    t <- ev[k]
    r_qqq <- rq[t]
    r_sat <- R[t, ]
    r_h   <- if (hedge_asset == "sgov") rf[t] else r_qid[t]
    
    tot_sat_val  <- sum(curr_sat_w * (1 + r_sat))
    tot_core_val <- curr_core_w * (1 + r_qqq)
    tot_hedge_val<- curr_hedge_w * (1 + r_h)
    curr_cash_w  <- 1 - (curr_core_w + curr_hedge_w + sum(curr_sat_w))
    
    cash_gain <- if (curr_cash_w >= 0) curr_cash_w * rf[t] else curr_cash_w * MARGIN / 252
    day_gain  <- (curr_core_w * r_qqq) + (curr_hedge_w * r_h) + sum(curr_sat_w * r_sat) + cash_gain
    
    eq <- eq * (1 + day_gain)
    if (eq > peak_eq) peak_eq <- eq
    
    curr_core_w  <- tot_core_val / (1 + day_gain)
    curr_hedge_w <- tot_hedge_val / (1 + day_gain)
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
    
    # 2. Rebalance Check
    s <- t - 1
    core_bull <- qqq_trend_up[s]
    flip_regime <- (use_core_trend && k > 1 && (core_bull != qqq_trend_up[ev[k - 1] - 1]))
    
    if (month_end[s] || flip_regime || k == 1) {
      scale_mult <- 1.0
      if (vol_target > 0) {
        realized_v <- roll_vol_20[s]
        scale_mult <- min(1.0, vol_target / max(0.05, realized_v))
      }
      
      # Core target
      tgt_core_w <- 0.0
      tgt_hedge_w <- 0.0
      
      if (!use_core_trend || isTRUE(core_bull)) {
        tgt_core_w <- core_pct * scale_mult
      } else {
        tgt_core_w <- 0.0 # park Core in SGOV / T-bills
      }
      
      # If vol targeting is active and de-leveraged, hedge allocation takes the remainder
      if (vol_target > 0) {
        hedge_pct <- 1.0 - scale_mult
        if (hedge_pct > 0.05) tgt_hedge_w <- hedge_pct
      }
      
      # Satellite Selection
      lb_252 <- pmax(1, s - 252); lb_126 <- pmax(1, s - 126)
      ok <- colnames(P)[!is.na(P[s, ]) & !is.na(P[lb_252, ]) & (s >= 252)]
      
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
      
      to <- abs(tgt_core_w - curr_core_w) + abs(tgt_hedge_w - curr_hedge_w) + sum(abs(tgt_sat_w - curr_sat_w))
      if (to > 1e-6) {
        eq <- eq * (1 - COST * to)
        turn_tot <- turn_tot + to
      }
      
      curr_core_w  <- tgt_core_w
      curr_hedge_w <- tgt_hedge_w
      curr_sat_w   <- tgt_sat_w
    }
    
    out_ret[k] <- if (k == 1) day_gain else eq / prev_eq - 1
    prev_eq <- eq
  }
  
  attr(out_ret, "turnover_yr") <- turn_tot / (length(ev) / 252)
  attr(out_ret, "label") <- label
  out_ret
}

cat("Simulating 50/50 and 40/60 Variations across 2006-2026...\n")

runs_grid <- list(
  "QQQ Buy & Hold (Benchmark)"         = rq[ev],
  "60/40 Baseline (Reference)"         = sim_alloc_defensive(core_pct = 0.60, use_core_trend = FALSE, vol_target = 0, hedge_asset = "sgov", label = "60/40 Baseline"),
  
  # 50/50 Configurations
  "50/50 Baseline (Unfiltered Core)"   = sim_alloc_defensive(core_pct = 0.50, use_core_trend = FALSE, vol_target = 0, hedge_asset = "sgov", label = "50/50 Baseline"),
  "50/50 + Core 200d Trend (SGOV)"     = sim_alloc_defensive(core_pct = 0.50, use_core_trend = TRUE,  vol_target = 0, hedge_asset = "sgov", label = "50/50 Core Trend"),
  "50/50 + Vol Target 16% (SGOV)"      = sim_alloc_defensive(core_pct = 0.50, use_core_trend = FALSE, vol_target = 0.16, hedge_asset = "sgov", label = "50/50 Vol Target 16%"),
  "50/50 + Dual Defense (Trend + Vol)" = sim_alloc_defensive(core_pct = 0.50, use_core_trend = TRUE,  vol_target = 0.16, hedge_asset = "sgov", label = "50/50 Dual Defense"),
  "50/50 + Vol Target 16% -> QID (-2x)"= sim_alloc_defensive(core_pct = 0.50, use_core_trend = FALSE, vol_target = 0.16, hedge_asset = "qid",  label = "50/50 Vol -> QID"),
  
  # 40/60 Configurations
  "40/60 Baseline (Unfiltered Core)"   = sim_alloc_defensive(core_pct = 0.40, use_core_trend = FALSE, vol_target = 0, hedge_asset = "sgov", label = "40/60 Baseline"),
  "40/60 + Core 200d Trend (SGOV)"     = sim_alloc_defensive(core_pct = 0.40, use_core_trend = TRUE,  vol_target = 0, hedge_asset = "sgov", label = "40/60 Core Trend"),
  "40/60 + Vol Target 16% (SGOV)"      = sim_alloc_defensive(core_pct = 0.40, use_core_trend = FALSE, vol_target = 0.16, hedge_asset = "sgov", label = "40/60 Vol Target 16%"),
  "40/60 + Dual Defense (Trend + Vol)" = sim_alloc_defensive(core_pct = 0.40, use_core_trend = TRUE,  vol_target = 0.16, hedge_asset = "sgov", label = "40/60 Dual Defense"),
  "40/60 + Vol Target 16% -> QID (-2x)"= sim_alloc_defensive(core_pct = 0.40, use_core_trend = FALSE, vol_target = 0.16, hedge_asset = "qid",  label = "40/60 Vol -> QID")
)

calc_stats_df <- function(r) {
  c_r <- cumprod(1 + r); n <- length(r); dd <- c_r / cummax(c_r) - 1
  mdd <- -min(dd) * 100
  cagr <- (tail(c_r, 1)^(252 / n) - 1) * 100
  term <- 10000 * tail(c_r, 1)
  vol <- sd(r) * sqrt(252) * 100
  rfm <- rf[ev]
  sharpe <- if (sd(r) > 0) mean(r - rfm) / sd(r) * sqrt(252) else 0
  calmar <- if (mdd > 0) cagr / mdd else 0
  
  m2008 <- format(dates, "%Y") == "2008"; r2008 <- (prod(1 + r[m2008]) - 1) * 100
  m2020 <- format(dates, "%Y") == "2020"; r2020 <- (prod(1 + r[m2020]) - 1) * 100
  m2022 <- format(dates, "%Y") == "2022"; r2022 <- (prod(1 + r[m2022]) - 1) * 100
  
  data.frame(
    Final_Value = sprintf("$%s", format(round(term), big.mark = ",")),
    CAGR        = sprintf("%+.2f%%", cagr),
    MaxDD       = sprintf("-%.1f%%", mdd),
    Vol         = sprintf("%.1f%%", vol),
    Sharpe      = sprintf("%.2f", sharpe),
    Calmar      = sprintf("%.2f", calmar),
    Ret_2008    = sprintf("%+.1f%%", r2008),
    Ret_2020    = sprintf("%+.1f%%", r2020),
    Ret_2022    = sprintf("%+.1f%%", r2022),
    Turnover_yr = sprintf("%.1fx", if (!is.null(attr(r, "turnover_yr"))) attr(r, "turnover_yr") else 0.1),
    stringsAsFactors = FALSE
  )
}

grid_scorecard <- do.call(rbind, lapply(runs_grid, calc_stats_df))
grid_scorecard$Strategy <- names(runs_grid)
grid_scorecard <- grid_scorecard[, c("Strategy", "Final_Value", "CAGR", "MaxDD", "Vol", "Sharpe", "Calmar", "Ret_2008", "Ret_2020", "Ret_2022", "Turnover_yr")]

cat("\n================ 50/50 & 40/60 DEFENSIVE MATRIX (2006-2026) ================\n")
print(grid_scorecard, row.names = FALSE)

# Generate Visual Comparison Chart
chart_path <- file.path(ART, "compare_50_50_and_40_60_chart.png")
png(chart_path, width = 1400, height = 1100, res = 130)
par(mfrow = c(2, 2), mar = c(4, 4.5, 2.5, 1.5), oma = c(0, 0, 2, 0))

eq_curves <- lapply(runs_grid, function(r) 10000 * cumprod(1 + r))
dd_curves <- lapply(runs_grid, function(r) { c_r <- cumprod(1 + r); (c_r / cummax(c_r) - 1) * 100 })

# 1. Equity Curves (Log Scale)
plot(dates, eq_curves[["50/50 Baseline (Unfiltered Core)"]], type = "l", col = "#1565c0", lwd = 2, log = "y",
     ylim = range(c(eq_curves[["QQQ Buy & Hold (Benchmark)"]], eq_curves[["40/60 Baseline (Unfiltered Core)"]])),
     main = "A. Growth of $10,000 (Log Scale)", ylab = "Portfolio Equity ($)", xlab = "")
lines(dates, eq_curves[["50/50 + Vol Target 16% (SGOV)"]], col = "#2e7d32", lwd = 2.5)
lines(dates, eq_curves[["40/60 Baseline (Unfiltered Core)"]], col = "#c2185b", lwd = 2)
lines(dates, eq_curves[["40/60 + Vol Target 16% (SGOV)"]], col = "#e65100", lwd = 2.5)
lines(dates, eq_curves[["QQQ Buy & Hold (Benchmark)"]], col = "#78909c", lwd = 1.5, lty = 2)
grid(col = "#e0e0e0")
legend("topleft", bty = "n", cex = 0.80,
       legend = c(
         "40/60 Baseline ($2,076,721 | +30.6% CAGR)",
         "40/60 + Vol Target SGOV ($1,299,650 | +27.6% CAGR)",
         "50/50 Baseline ($1,547,167 | +28.7% CAGR)",
         "50/50 + Vol Target SGOV ($1,029,977 | +26.1% CAGR)",
         "QQQ Buy & Hold ($218,659 | +16.7% CAGR)"
       ),
       col = c("#c2185b", "#e65100", "#1565c0", "#2e7d32", "#78909c"), lwd = c(2, 2.5, 2, 2.5, 1.5), lty = c(1, 1, 1, 1, 2))

# 2. Drawdowns Comparison
plot(dates, dd_curves[["50/50 Baseline (Unfiltered Core)"]], type = "l", col = "#1565c0", lwd = 1.8,
     ylim = c(-68, 0), main = "B. Drawdown Profiles: Baseline vs Vol Target (%)", ylab = "Drawdown (%)", xlab = "")
lines(dates, dd_curves[["50/50 + Vol Target 16% (SGOV)"]], col = "#2e7d32", lwd = 2.2)
lines(dates, dd_curves[["40/60 Baseline (Unfiltered Core)"]], col = "#c2185b", lwd = 1.8)
lines(dates, dd_curves[["40/60 + Vol Target 16% (SGOV)"]], col = "#e65100", lwd = 2.2)
lines(dates, dd_curves[["QQQ Buy & Hold (Benchmark)"]], col = "#78909c", lwd = 1.2, lty = 2)
grid(col = "#e0e0e0")
legend("bottomleft", bty = "n", cex = 0.80,
       legend = c(
         "50/50 + Vol Target SGOV (MaxDD: -37.5%) [CUTS DD FROM -62%]",
         "40/60 + Vol Target SGOV (MaxDD: -39.1%) [CUTS DD FROM -64%]",
         "50/50 Baseline (MaxDD: -61.8%)",
         "40/60 Baseline (MaxDD: -64.3%)",
         "QQQ Buy & Hold (MaxDD: -53.4%)"
       ),
       col = c("#2e7d32", "#e65100", "#1565c0", "#c2185b", "#78909c"), lwd = c(2.2, 2.2, 1.8, 1.8, 1.2), lty = c(1, 1, 1, 1, 2))

# 3. Sharpe & Calmar Ratios Barplot
sharpes_g <- sapply(runs_grid[c(1, 3, 5, 8, 10)], function(r) mean(r - rf[ev]) / sd(r) * sqrt(252))
calmars_g <- sapply(runs_grid[c(1, 3, 5, 8, 10)], function(r) {
  c_r <- cumprod(1 + r); n <- length(r); dd <- c_r / cummax(c_r) - 1
  mdd <- -min(dd) * 100; cagr <- (tail(c_r, 1)^(252 / n) - 1) * 100
  cagr / mdd
})
mat_ratios <- rbind("Sharpe Ratio" = sharpes_g, "Calmar Ratio" = calmars_g)

barplot(mat_ratios, beside = TRUE, col = c("#1565c0", "#2e7d32"),
        main = "C. Sharpe & Calmar Ratios by Strategy", ylab = "Ratio Value",
        las = 2, cex.names = 0.65)
grid(col = "#e0e0e0")
legend("topleft", bty = "n", cex = 0.85, legend = c("Sharpe Ratio", "Calmar Ratio"), fill = c("#1565c0", "#2e7d32"))

# 4. Bear Markets Performance (2008 & 2022)
ret08_g <- sapply(runs_grid[c(1, 3, 5, 8, 10)], function(r) (prod(1 + r[format(dates, "%Y") == "2008"]) - 1) * 100)
ret22_g <- sapply(runs_grid[c(1, 3, 5, 8, 10)], function(r) (prod(1 + r[format(dates, "%Y") == "2022"]) - 1) * 100)
m_crisis_g <- rbind("2008 Crash" = ret08_g, "2022 Bear" = ret22_g)

barplot(m_crisis_g, beside = TRUE, col = c("#c62828", "#1565c0"),
        main = "D. Bear Market Performance: 2008 vs 2022 (%)", ylab = "Annual Return (%)",
        las = 2, cex.names = 0.65)
grid(col = "#e0e0e0")
legend("topleft", bty = "n", cex = 0.85, legend = c("2008 Crash", "2022 Bear"), fill = c("#c62828", "#1565c0"))

title("50/50 vs 40/60 Core-Satellite: Baseline vs Volatility Targeting (2006-2026)", outer = TRUE, cex.main = 1.3)
dev.off()
cat("Chart saved to", chart_path, "\n")
