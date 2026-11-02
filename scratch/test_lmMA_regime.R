#!/usr/bin/env Rscript
# ==============================================================================
# scratch/test_lmMA_regime.R
# Test lmMA as Macro & Stock Trend Filter vs SMA200 / EMA200
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

# Calculate Trend Filters on QQQ
sma200_q <- SMA(qqq_ts, 200)
ema200_q <- EMA(qqq_ts, 200)
sma50_q  <- SMA(qqq_ts, 50)
ema50_q  <- EMA(qqq_ts, 50)

lm50_q   <- lmMA(qqq_ts, 50)$fit
lm100_q  <- lmMA(qqq_ts, 100)$fit
lm200_q  <- lmMA(qqq_ts, 200)$fit

lm50_slope  <- lmMA(qqq_ts, 50)$slope
lm100_slope <- lmMA(qqq_ts, 100)$slope
lm200_slope <- lmMA(qqq_ts, 200)$slope

# Custom Simulator parameterized by trend rule
sim_trend_variant <- function(trend_rule = "sma200", start = S20, end = E20) {
  U <- syms
  ev <- which(cal >= start & cal <= end)
  sat_pct <- 1.0 # 100% stock
  
  cw <- 0
  sw <- setNames(rep(0, length(U)), U)
  dw <- 0
  ep <- setNames(rep(NA_real_, length(U)), U)
  eq <- 1
  peak <- 1
  out <- numeric(length(ev))
  
  # Precompute trend signal vector for QQQ
  bull_sig <- switch(trend_rule,
    "sma200"     = (q >= as.numeric(sma200_q)),
    "ema200"     = (q >= as.numeric(ema200_q)),
    "sma50"      = (q >= as.numeric(sma50_q)),
    "ema50"      = (q >= as.numeric(ema50_q)),
    "lmMA200_px" = (q >= as.numeric(lm200_q)),
    "lmMA100_px" = (q >= as.numeric(lm100_q)),
    "lmMA50_px"  = (q >= as.numeric(lm50_q)),
    "lmMA100_slope" = (as.numeric(lm100_slope) > 0),
    "lmMA50_slope"  = (as.numeric(lm50_slope) > 0)
  )
  bull_sig[is.na(bull_sig)] <- TRUE
  
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
      bull <- bull_sig[s0]
      tc <- 0
      ts <- setNames(rep(0, length(U)), U)
      td <- 0
      ok <- U[MEM[s0, ] & !is.na(P[s0, ]) & !is.na(P[max(1, s0 - 252), ])]
      
      if (bull) {
        if (length(ok) >= 5) {
          r126 <- P[s0, ok] / P[s0 - 126, ok] - 1 - (q[s0] / q[s0 - 126] - 1)
          r63  <- P[s0, ok] / P[s0 - 63, ok]  - 1 - (q[s0] / q[s0 - 63] - 1)
          rs <- 0.50 * r126 + 0.50 * r63
          
          # Trend filter on individual stocks (matches rule)
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

rules <- c("sma200", "ema200", "sma50", "ema50", "lmMA200_px", "lmMA100_px", "lmMA50_px", "lmMA100_slope", "lmMA50_slope")
cat("\nRunning 20-Year (2007-2026) Simulations across Trend Rules...\n")
res_20 <- lapply(rules, function(r) sim_trend_variant(r, start = S20, end = E20))
names(res_20) <- rules
sc_20 <- t(sapply(res_20, met2, start_d = S20, end_d = E20))
print(round(sc_20, 2))

cat("\nRunning 10-Year (2016-2026) Simulations across Trend Rules...\n")
res_10 <- lapply(rules, function(r) sim_trend_variant(r, start = S10, end = E10))
names(res_10) <- rules
sc_10 <- t(sapply(res_10, met2, start_d = S10, end_d = E10))
print(round(sc_10, 2))
