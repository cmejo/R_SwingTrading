#!/usr/bin/env Rscript
# ==============================================================================
# scratch/test_lag_reduction_deep_dive.R
# Comprehensive Empirical Evaluation of Model Lag Reduction Mechanisms:
#   1. Current Baseline: Monthly Rebalance (21d), 200 SMA on stocks, 126d/63d RS
#   2. Rebalance Frequency: Bi-Weekly Rebalance (every 10 trading days)
#   3. Stock-Level Trend Gate: Replace 200 SMA on stocks with 50d lmMA Slope > 0
#   4. Asymmetric Fast-Exit: Monthly entry, but exit mid-month if RS Rank > 10
#   5. High-Velocity Lookback: 63d + 30d RS blend (cutting lookback lag in half)
#   6. Chandelier / ATR Trailing Stop on Winners (Lock in multi-bagger profits)
# ==============================================================================

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(jsonlite)
})

src <- readLines("scratch/compare_symbol_lists.R")
idx <- grep("^lists <- c", src)
eval(parse(text = paste(src[1:(idx - 1)], collapse = "\n")))
set_universe(read_list("symbols_broad.txt"))

source("scratch/test_crisis_min_variance.R")
source("R/02_trend_lmMA.R")

S20 <- as.Date("2007-01-02")
E20 <- as.Date("2026-10-06")
S10 <- as.Date("2016-10-03")
E10 <- as.Date("2026-10-06")

U <- syms
ev <- which(cal >= S20 & cal <= E20)

# Precalculate 50d lmMA slope for benchmark QQQ
qqq_ts <- xts(q, order.by = cal)
lm50_q <- lmMA(qqq_ts, 50)
slope50_q <- as.numeric(lm50_q$slope)
rsq50_q   <- as.numeric(lm50_q$r.squared)
benchmark_bull <- !(slope50_q < 0 & rsq50_q > 0.15)

# Precalculate 50d SMA on stocks
SMA50_stocks <- matrix(NA_real_, nrow = nrow(P), ncol = ncol(P), dimnames = dimnames(P))
for (j in 1:ncol(P)) {
  SMA50_stocks[, j] <- as.numeric(stats::filter(P[, j], rep(1/50, 50), sides = 1))
}

sim_lag_variant <- function(rebal_mode = c("monthly", "biweekly"),
                            stock_gate = c("sma200", "sma50", "none"),
                            rs_lookback = c("dual_126_63", "dual_63_30", "triple_90_45_20"),
                            fast_exit_rank = 0, # 0 = disabled, e.g. 10 = exit if rank > 10
                            trailing_stop_pct = 0.0, # 0 = disabled, e.g. 0.15 = 15% trail from peak
                            margin = 1.5,
                            top_n = 5,
                            vt = 0.22,
                            start = S20,
                            end = E20) {
  
  rebal_mode  <- match.arg(rebal_mode)
  stock_gate  <- match.arg(stock_gate)
  rs_lookback <- match.arg(rs_lookback)
  
  ev_sub <- which(cal >= start & cal <= end)
  sat_pct <- 1.0
  
  cw <- 0
  sw <- setNames(rep(0, length(U)), U)
  dw <- 0
  ep <- setNames(rep(NA_real_, length(U)), U)
  peak_p <- setNames(rep(NA_real_, length(U)), U)
  
  eq <- 1
  peak <- 1
  turn_tot <- 0
  out <- numeric(length(ev_sub))
  
  rebal_counter <- 0
  
  for (k in seq_along(ev_sub)) {
    t <- ev_sub[k]
    prev <- eq
    gross <- cw + sum(sw) + dw
    
    g <- cw * rq[t] + sum(sw * R[t, ]) + dw * r_gld[t] - 
         max(0, gross - 1) * borrow_daily[t] + max(0, 1 - gross) * rf_daily[t]
    
    eq <- eq * (1 + g)
    peak <- max(peak, eq)
    
    cw <- cw * (1 + rq[t]) / (1 + g)
    sw <- sw * (1 + R[t, ]) / (1 + g)
    dw <- dw * (1 + r_gld[t]) / (1 + g)
    
    # Update position peak prices
    for (s in names(sw)[sw > 0]) {
      if (!is.na(P[t, s])) {
        peak_p[s] <- max(peak_p[s], P[t, s], na.rm = TRUE)
      }
    }
    
    # 1. Disaster Stop (-20% from entry)
    for (s in names(sw)[sw > 0]) {
      hit_disaster <- (!is.na(ep[s]) && P[t, s] / ep[s] - 1 <= -0.20)
      if (isTRUE(hit_disaster)) {
        w <- sw[s]
        eq <- eq * (1 - COST * w)
        turn_tot <- turn_tot + w
        sw[s] <- 0
        ep[s] <- NA
        peak_p[s] <- NA
      }
    }
    
    # 2. Trailing Stop from Peak (if enabled)
    if (trailing_stop_pct > 0) {
      for (s in names(sw)[sw > 0]) {
        # Only activate trailing stop after +20% unrealized gain
        unrealized <- if (!is.na(ep[s])) P[t, s] / ep[s] - 1 else 0
        if (unrealized >= 0.20 && !is.na(peak_p[s])) {
          dd_from_peak <- P[t, s] / peak_p[s] - 1
          if (dd_from_peak <= -trailing_stop_pct) {
            w <- sw[s]
            eq <- eq * (1 - COST * w)
            turn_tot <- turn_tot + w
            sw[s] <- 0
            ep[s] <- NA
            peak_p[s] <- NA
          }
        }
      }
    }
    
    s0 <- t - 1
    rebal_counter <- rebal_counter + 1
    
    # Check Rebalance Trigger
    is_rebal <- FALSE
    if (rebal_mode == "monthly") {
      if (month_end[s0] || k == 1) is_rebal <- TRUE
    } else if (rebal_mode == "biweekly") {
      if (rebal_counter >= 10 || k == 1) {
        is_rebal <- TRUE
        rebal_counter <- 0
      }
    }
    
    # 3. Fast-Exit Rank Degradation (Daily Check if enabled and not already rebalancing)
    if (fast_exit_rank > 0 && !is_rebal && any(sw > 0)) {
      ok <- U[MEM[s0, ] & !is.na(P[s0, ]) & !is.na(P[max(1, s0 - 126), ])]
      if (length(ok) >= 15) {
        r126 <- P[s0, ok] / P[s0 - 126, ok] - 1 - (q[s0] / q[s0 - 126] - 1)
        r63  <- P[s0, ok] / P[s0 - 63, ok]  - 1 - (q[s0] / q[s0 - 63] - 1)
        rs_daily <- 0.50 * r126 + 0.50 * r63
        rk_daily <- names(sort(rs_daily, decreasing = TRUE))
        
        # Check held positions
        for (held_sym in names(sw)[sw > 0]) {
          cur_rank <- match(held_sym, rk_daily)
          if (is.na(cur_rank) || cur_rank > fast_exit_rank) {
            # Drop degraded position immediately
            w <- sw[held_sym]
            eq <- eq * (1 - COST * w)
            turn_tot <- turn_tot + w
            sw[held_sym] <- 0
            ep[held_sym] <- NA
            peak_p[held_sym] <- NA
          }
        }
      }
    }
    
    if (is_rebal) {
      sc <- min(1, vt / max(0.05, vol20[s0]))
      bull <- isTRUE(benchmark_bull[s0])
      am <- if (margin > 1 && !bull) 1 else margin
      
      ts <- setNames(rep(0, length(U)), U)
      ok <- U[MEM[s0, ] & !is.na(P[s0, ]) & !is.na(P[max(1, s0 - 252), ])]
      
      if (bull) {
        if (length(ok) >= top_n) {
          # Calculate Relative Strength
          if (rs_lookback == "dual_126_63") {
            r126 <- P[s0, ok] / P[s0 - 126, ok] - 1 - (q[s0] / q[s0 - 126] - 1)
            r63  <- P[s0, ok] / P[s0 - 63, ok]  - 1 - (q[s0] / q[s0 - 63] - 1)
            rs   <- 0.50 * r126 + 0.50 * r63
          } else if (rs_lookback == "dual_63_30") {
            r63  <- P[s0, ok] / P[s0 - 63, ok] - 1 - (q[s0] / q[s0 - 63] - 1)
            r30  <- P[s0, ok] / P[s0 - 30, ok] - 1 - (q[s0] / q[s0 - 30] - 1)
            rs   <- 0.60 * r63 + 0.40 * r30
          } else if (rs_lookback == "triple_90_45_20") {
            r90  <- P[s0, ok] / P[s0 - 90, ok] - 1 - (q[s0] / q[s0 - 90] - 1)
            r45  <- P[s0, ok] / P[s0 - 45, ok] - 1 - (q[s0] / q[s0 - 45] - 1)
            r20  <- P[s0, ok] / P[s0 - 20, ok] - 1 - (q[s0] / q[s0 - 20] - 1)
            rs   <- 0.50 * r90 + 0.30 * r45 + 0.20 * r20
          }
          
          # Stock-level trend gate
          if (stock_gate == "sma200") {
            okf <- ok[!is.na(SMA200[s0, ok]) & P[s0, ok] >= SMA200[s0, ok]]
            if (length(okf) >= top_n) { ok <- okf; rs <- rs[ok] }
          } else if (stock_gate == "sma50") {
            okf <- ok[!is.na(SMA50_stocks[s0, ok]) & P[s0, ok] >= SMA50_stocks[s0, ok]]
            if (length(okf) >= top_n) { ok <- okf; rs <- rs[ok] }
          }
          
          # 100% Volatility Cap
          k2 <- ok[!is.na(VOL63[s0, ok]) & VOL63[s0, ok] <= 1.00]
          if (length(k2) >= top_n) { ok <- k2; rs <- rs[k2] }
          
          rk <- names(sort(rs, decreasing = TRUE))
          held <- intersect(names(sw)[sw > 0], rk[1:top_n])
          top <- unique(c(held, rk))[1:top_n]
          top <- top[!is.na(top)]
          
          wts <- rep(1 / length(top), length(top))
          ts[top] <- sat_pct * sc * am * wts
          
          for (ne in setdiff(top, names(sw)[sw > 0])) {
            ep[ne] <- P[t, ne]
            peak_p[ne] <- P[t, ne]
          }
        }
      } else {
        # Crisis MVP (Bear Regime)
        if (length(ok) >= top_n) {
          vol_ranks <- sort(VOL63[s0, ok])
          low_vol_syms <- names(head(vol_ranks, 10))
          R_sub <- R[(s0 - 62):s0, low_vol_syms, drop = FALSE]
          valid_cols <- apply(R_sub, 2, function(x) !any(is.na(x)) && sd(x) > 0)
          if (sum(valid_cols) >= top_n) {
            low_vol_syms <- names(valid_cols)[valid_cols][1:top_n]
            cov_mat <- cov(R_sub[, low_vol_syms, drop = FALSE])
            w_mvp <- solve_min_var(cov_mat)
            ts[low_vol_syms] <- sat_pct * sc * am * w_mvp
            for (ne in setdiff(low_vol_syms, names(sw)[sw > 0])) {
              ep[ne] <- P[t, ne]
              peak_p[ne] <- P[t, ne]
            }
          }
        }
      }
      
      to <- sum(abs(ts - sw))
      eq <- eq * (1 - COST * to)
      turn_tot <- turn_tot + to
      sw <- ts
    }
    
    out[k] <- eq / prev - 1
  }
  
  attr(out, "turnover_yr") <- turn_tot / (length(ev_sub) / 252)
  out
}

calc_metrics <- function(r) {
  c_r <- cumprod(1 + r); n <- length(r); dd <- c_r / cummax(c_r) - 1
  mdd <- -min(dd) * 100
  cagr <- (tail(c_r, 1)^(252 / n) - 1) * 100
  term <- 8000 * tail(c_r, 1)
  vol <- sd(r) * sqrt(252) * 100
  rfm <- rf_daily[ev]
  sharpe <- if (sd(r) > 0) mean(r - rfm) / sd(r) * sqrt(252) else 0
  sortino <- {
    d_ret <- pmin(r - rfm, 0)
    d_sd  <- sqrt(mean(d_ret^2)) * sqrt(252)
    if (d_sd > 0) mean(r - rfm) * 252 / d_sd else 0
  }
  calmar <- if (mdd > 0) cagr / mdd else 0
  turn <- if (!is.null(attr(r, "turnover_yr"))) attr(r, "turnover_yr") else 0
  
  c(CAGR = cagr, Terminal_8k = term, MaxDD = mdd, Vol = vol,
    Sharpe = sharpe, Sortino = sortino, Calmar = calmar, Turnover = turn)
}

cat("\nRunning 20-Year Lag Reduction Simulations (2007-2026)...\n")

test_runs <- list(
  "1. Baseline Current Model (Monthly, 200 SMA on stocks, 126d/63d RS)" = 
    sim_lag_variant(rebal_mode = "monthly", stock_gate = "sma200", rs_lookback = "dual_126_63"),
    
  "2. Bi-Weekly Rebalance (Every 10d vs 21d)" = 
    sim_lag_variant(rebal_mode = "biweekly", stock_gate = "sma200", rs_lookback = "dual_126_63"),
    
  "3. Low-Lag Stock Gate (50 SMA on stocks vs 200 SMA)" = 
    sim_lag_variant(rebal_mode = "monthly", stock_gate = "sma50", rs_lookback = "dual_126_63"),
    
  "4. Unrestricted Stock Trend (Pure RS Ranking, No Stock Gate)" = 
    sim_lag_variant(rebal_mode = "monthly", stock_gate = "none", rs_lookback = "dual_126_63"),
    
  "5. Asymmetric Fast-Exit (Exit mid-month if RS Rank > 10)" = 
    sim_lag_variant(rebal_mode = "monthly", stock_gate = "sma200", rs_lookback = "dual_126_63", fast_exit_rank = 10),
    
  "6. High-Velocity Lookback (63d + 30d RS Blend)" = 
    sim_lag_variant(rebal_mode = "monthly", stock_gate = "sma200", rs_lookback = "dual_63_30"),
    
  "7. Triple High-Velocity (90d/45d/20d RS Blend)" = 
    sim_lag_variant(rebal_mode = "monthly", stock_gate = "sma200", rs_lookback = "triple_90_45_20"),
    
  "8. Profit Ratchet Stop (15% Trail after +20% gain)" = 
    sim_lag_variant(rebal_mode = "monthly", stock_gate = "sma200", rs_lookback = "dual_126_63", trailing_stop_pct = 0.15)
)

tbl_20 <- t(sapply(test_runs, calc_metrics))
cat("\n========================================================================================================================\n")
cat("                       20-YEAR LAG REDUCTION PERFORMANCE COMPARISON (2007 - 2026)\n")
cat("========================================================================================================================\n")
print(round(tbl_20, 2))
cat("========================================================================================================================\n")
write.csv(tbl_20, "scratch/scorecard_20yr_lag_reduction.csv")
