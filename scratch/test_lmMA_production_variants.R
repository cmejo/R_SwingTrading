#!/usr/bin/env Rscript
# ==============================================================================
# scratch/test_lmMA_production_variants.R
# Test Production-Grade lmMA Implementations: Dual-crossover, Slope, Hysteresis
# ==============================================================================
suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(TTR)
})

source("R/02_trend_lmMA.R")
src <- readLines("scratch/compare_symbol_lists.R")
idx <- grep("^lists <- c", src)
eval(parse(text = paste(src[1:(idx - 1)], collapse = "\n")))
set_universe(read_list("symbols_broad.txt"))

source("scratch/test_crisis_min_variance.R")

qqq_ts <- xts(q, order.by = cal)

# Features
sma200_q <- as.numeric(SMA(qqq_ts, 200))
lm20 <- lmMA(qqq_ts, 20)
lm50 <- lmMA(qqq_ts, 50)
lm100 <- lmMA(qqq_ts, 100)

fit20 <- as.numeric(lm20$fit)
fit50 <- as.numeric(lm50$fit)
fit100 <- as.numeric(lm100$fit)

slope20 <- as.numeric(lm20$slope)
slope50 <- as.numeric(lm50$slope)
slope100 <- as.numeric(lm100$slope)

rsq50 <- as.numeric(lm50$r.squared)
rsq100 <- as.numeric(lm100$r.squared)

# Signals:
# 1. Baseline SMA200
sig_sma200 <- (q >= sma200_q)

# 2. Dual lmMA crossover (Fast 20d lmMA >= Slow 50d lmMA)
sig_dual_lm <- (fit20 >= fit50)

# 3. Slow lmMA Slope > 0 with R-squared confirmation (Trend is significantly upward)
# If slope is negative and R^2 > 0.15 => confirmed downtrend
sig_slope_rsq <- !(slope50 < 0 & rsq50 > 0.15)

# 4. Hybrid: lmMA50 Slope > 0 OR Price >= SMA200 (Ensemble that avoids premature exit while capturing rapid reversals)
sig_hybrid_or <- (q >= sma200_q | slope50 > 0)

# 5. Hybrid Conservative: Price >= lmMA50 with 2% hysteresis band
sig_hysteresis <- rep(TRUE, length(q))
state <- TRUE
for (i in 51:length(q)) {
  if (state) {
    if (q[i] < fit50[i] * 0.97) state <- FALSE # Exit if 3% below lmMA50
  } else {
    if (q[i] > fit50[i] * 1.01) state <- TRUE  # Re-enter if 1% above lmMA50
  }
  sig_hysteresis[i] <- state
}

# 6. Ensemble Voting: (P >= SMA200) + (fit20 >= fit50) + (slope50 > 0) >= 2 votes
votes <- (q >= sma200_q) + (fit20 >= fit50) + (slope50 > 0)
sig_ensemble <- (votes >= 2)

sim_custom_sig <- function(sig_vec, start = S20, end = E20) {
  U <- syms
  ev <- which(cal >= start & cal <= end)
  sat_pct <- 1.0
  cw <- 0
  sw <- setNames(rep(0, length(U)), U)
  dw <- 0
  ep <- setNames(rep(NA_real_, length(U)), U)
  eq <- 1
  peak <- 1
  out <- numeric(length(ev))
  
  for (k in seq_along(ev)) {
    t <- ev[k]
    prev <- eq
    gross <- cw + sum(sw) + dw
    g <- cw * rq[t] + sum(sw * R[t, ]) + dw * r_gld[t] - max(0, gross - 1) * borrow_daily[t] + max(0, 1 - gross) * rf_daily[t]
    eq <- eq * (1 + g)
    peak <- max(peak, eq)
    
    cw <- cw * (1 + rq[t]) / (1 + g)
    sw <- sw * (1 + R[t, ]) / (1 + g)
    dw <- dw * (1 + r_gld[t]) / (1 + g)
    
    # 20% disaster stop
    for (s in names(sw)[sw > 0]) {
      hit <- (!is.na(ep[s]) && P[t, s] / ep[s] - 1 <= -0.20)
      if (isTRUE(hit)) {
        w <- sw[s]
        eq <- eq * (1 - COST * w)
        sw[s] <- 0
      }
    }
    
    s0 <- t - 1
    if (month_end[s0] || k == 1) {
      bull <- isTRUE(sig_vec[s0])
      tc <- 0
      ts <- setNames(rep(0, length(U)), U)
      td <- 0
      ok <- U[MEM[s0, ] & !is.na(P[s0, ]) & !is.na(P[max(1, s0 - 252), ])]
      
      if (bull) {
        if (length(ok) >= 5) {
          r126 <- P[s0, ok] / P[s0 - 126, ok] - 1 - (q[s0] / q[s0 - 126] - 1)
          r63  <- P[s0, ok] / P[s0 - 63, ok]  - 1 - (q[s0] / q[s0 - 63] - 1)
          rs <- 0.50 * r126 + 0.50 * r63
          okf <- ok[!is.na(SMA200[s0, ok]) & P[s0, ok] >= SMA200[s0, ok]]
          if (length(okf) >= 5) { ok <- okf; rs <- rs[ok] }
          k2 <- ok[!is.na(VOL63[s0, ok]) & VOL63[s0, ok] <= 1.00]
          if (length(k2) >= 5) { ok <- k2; rs <- rs[k2] }
          rk <- names(sort(rs, decreasing = TRUE))
          held <- intersect(names(sw)[sw > 0], rk[1:5])
          top <- unique(c(held, rk))[1:5]
          top <- top[!is.na(top)]
          wts <- rep(1 / length(top), length(top))
          ts[top] <- sat_pct * wts
          for (ne in setdiff(top, names(sw)[sw > 0])) ep[ne] <- P[t, ne]
        }
      } else {
        # Crisis Minimum Variance Portfolio
        if (length(ok) >= 5) {
          vol_ranks <- sort(VOL63[s0, ok])
          low_vol_syms <- names(head(vol_ranks, 10))
          R_sub <- R[(s0 - 62):s0, low_vol_syms, drop = FALSE]
          valid_cols <- apply(R_sub, 2, function(x) !any(is.na(x)) && sd(x) > 0)
          if (sum(valid_cols) >= 5) {
            low_vol_syms <- names(valid_cols)[valid_cols][1:5]
            cov_mat <- cov(R_sub[, low_vol_syms, drop = FALSE])
            w_mvp <- solve_min_var(cov_mat)
            ts[low_vol_syms] <- sat_pct * w_mvp
            for (ne in setdiff(low_vol_syms, names(sw)[sw > 0])) ep[ne] <- P[t, ne]
          }
        }
      }
      
      to <- sum(abs(ts - sw))
      eq <- eq * (1 - COST * to)
      sw <- ts
    }
    out[k] <- eq / prev - 1
  }
  out
}

test_sigs <- list(
  "Baseline: 200 SMA"                     = sig_sma200,
  "Dual lmMA Crossover (lmMA20 >= lmMA50)"= sig_dual_lm,
  "lmMA Slope + R^2 (slope < 0 & R^2>0.15)"= sig_slope_rsq,
  "lmMA50 Hysteresis (+1% / -3% Band)"    = sig_hysteresis,
  "Ensemble Voting (2 of 3 Trend Votes)"  = sig_ensemble,
  "Hybrid (P >= 200 SMA OR slope50 > 0)"   = sig_hybrid_or
)

cat("\n========================================================================================\n")
cat(" 20-YEAR SCORECARD (2007 - 2026): lmMA PRODUCTION VARIANTS\n")
cat("========================================================================================\n")
res_20 <- lapply(test_sigs, function(s) sim_custom_sig(s, start = S20, end = E20))
sc_20 <- t(sapply(res_20, met2, start_d = S20, end_d = E20))
print(round(sc_20, 2))

cat("\n========================================================================================\n")
cat(" 10-YEAR SCORECARD (2016 - 2026): lmMA PRODUCTION VARIANTS\n")
cat("========================================================================================\n")
res_10 <- lapply(test_sigs, function(s) sim_custom_sig(s, start = S10, end = E10))
sc_10 <- t(sapply(res_10, met2, start_d = S10, end_d = E10))
print(round(sc_10, 2))
