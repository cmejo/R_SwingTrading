#!/usr/bin/env Rscript
# ==============================================================================
# scratch/test_cagr_plus5pct_features.R
# Search for Structural Alpha Features to Increase CAGR by +5% or More
# Testing across both 10-Year (2016 - 2026) and 20-Year (2007 - 2026) Windows
#
# Candidate Features Evaluated:
#   0. Baseline: 100% Stock, 1.5x Margin, Equal Weight 5, Unrestricted Trend
#   1. Conviction-Rank Sizing (40%, 35%, 30%, 25%, 20% vs Equal 30%)
#   2. Super-Bull Leverage Turbo (1.75x when QQQ > 50 SMA > 200 SMA & Breadth > 60%)
#   3. Momentum Acceleration Filter (Ret_63 * 2 >= Ret_126)
#   4. Immediate Stop Replacement (Deploy stop proceeds into Rank 6 immediately)
#   5. Efficiency-Ratio (ER) Conviction Sizing (Favor smooth trends over noisy chop)
#   6. Tactical Crisis Gold (Rotate to Gold during confirmed Bear Regimes)
#   7. Volatility-Adjusted Conviction Sizing (Inverse-vol scaling within Top 5)
#   8. The "Alpha Turbo" Synergy Combo (Combining top complementary features)
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

# Precalculate QQQ SMAs and lmMA
qqq_ts <- xts(q, order.by = cal)
sma50_q  <- as.numeric(stats::filter(q, rep(1/50, 50), sides = 1))
sma200_q <- as.numeric(stats::filter(q, rep(1/200, 200), sides = 1))
lm50_q   <- lmMA(qqq_ts, 50)
slope50_q <- as.numeric(lm50_q$slope)
rsq50_q   <- as.numeric(lm50_q$r.squared)
benchmark_bull <- !(slope50_q < 0 & rsq50_q > 0.15)

# Super-Bull condition: QQQ > SMA50 > SMA200 with positive slope
super_bull_q <- (q > sma50_q & sma50_q > sma200_q & slope50_q > 0)
super_bull_q[is.na(super_bull_q)] <- FALSE

# Precalculate Efficiency Ratio (ER) for candidate stocks (63d window)
# ER = |Close[t] - Close[t-63]| / Sum(|Close[i] - Close[i-1]|)
ER63 <- matrix(NA_real_, nrow = nrow(P), ncol = ncol(P), dimnames = dimnames(P))
for (j in 1:ncol(P)) {
  px <- P[, j]
  diff_abs <- abs(c(0, diff(px)))
  sum_diff <- stats::filter(diff_abs, rep(1, 63), sides = 1)
  px_lag63 <- c(rep(NA, 63), px[1:(length(px) - 63)])
  net_chg  <- abs(px - px_lag63)
  er <- net_chg / pmax(1e-4, sum_diff)
  er[er > 1] <- 1
  ER63[, j] <- as.numeric(er)
}

sim_feature_engine <- function(sizing_mode = c("equal", "rank_weighted", "er_weighted", "inv_vol"),
                               turbo_leverage = FALSE, # 1.75x during super bull
                               require_acceleration = FALSE, # Ret_63 * 2 >= Ret_126
                               immediate_stop_replace = FALSE, # Buy rank 6 on stop
                               crisis_mode = c("mvp", "gld", "cash"),
                               start = S20,
                               end = E20) {
  
  sizing_mode <- match.arg(sizing_mode)
  crisis_mode <- match.arg(crisis_mode)
  
  ev <- which(cal >= start & cal <= end)
  sat_pct <- 1.0
  top_n <- 5
  vt <- 0.22
  
  cw <- 0
  sw <- setNames(rep(0, length(U)), U)
  dw <- 0
  ep <- setNames(rep(NA_real_, length(U)), U)
  
  eq <- 1
  peak <- 1
  turn_tot <- 0
  out <- numeric(length(ev))
  
  for (k in seq_along(ev)) {
    t <- ev[k]
    prev <- eq
    gross <- cw + sum(sw) + dw
    
    g <- cw * rq[t] + sum(sw * R[t, ]) + dw * r_gld[t] - 
         max(0, gross - 1) * borrow_daily[t] + max(0, 1 - gross) * rf_daily[t]
    
    eq <- eq * (1 + g)
    peak <- max(peak, eq)
    
    cw <- cw * (1 + rq[t]) / (1 + g)
    sw <- sw * (1 + R[t, ]) / (1 + g)
    dw <- dw * (1 + r_gld[t]) / (1 + g)
    
    s0 <- t - 1
    
    # 1. Disaster Stop (-20% from entry)
    stopped_symbols <- character()
    for (s in names(sw)[sw > 0]) {
      hit_disaster <- (!is.na(ep[s]) && P[t, s] / ep[s] - 1 <= -0.20)
      if (isTRUE(hit_disaster)) {
        w <- sw[s]
        eq <- eq * (1 - COST * w)
        turn_tot <- turn_tot + w
        sw[s] <- 0
        ep[s] <- NA
        stopped_symbols <- c(stopped_symbols, s)
      }
    }
    
    # Optional Feature: Immediate Stop Replacement (Re-deploy freed capital into next best candidate)
    if (immediate_stop_replace && length(stopped_symbols) > 0 && isTRUE(benchmark_bull[s0])) {
      ok <- U[MEM[s0, ] & !is.na(P[s0, ]) & !is.na(P[max(1, s0 - 126), ])]
      if (length(ok) >= 10) {
        r126 <- P[s0, ok] / P[s0 - 126, ok] - 1 - (q[s0] / q[s0 - 126] - 1)
        r63  <- P[s0, ok] / P[s0 - 63, ok]  - 1 - (q[s0] / q[s0 - 63] - 1)
        rs   <- 0.50 * r126 + 0.50 * r63
        k2   <- ok[!is.na(VOL63[s0, ok]) & VOL63[s0, ok] <= 1.00]
        if (length(k2) >= 10) { ok <- k2; rs <- rs[k2] }
        rk <- names(sort(rs, decreasing = TRUE))
        unheld <- setdiff(rk, c(names(sw)[sw > 0], stopped_symbols))
        
        target_w_per_slot <- 1.50 / top_n
        for (rep_sym in head(unheld, length(stopped_symbols))) {
          sw[rep_sym] <- target_w_per_slot
          ep[rep_sym] <- P[t, rep_sym]
          eq <- eq * (1 - COST * target_w_per_slot)
          turn_tot <- turn_tot + target_w_per_slot
        }
      }
    }
    
    # Monthly Rebalance Trigger
    if (month_end[s0] || k == 1) {
      sc <- min(1, vt / max(0.05, vol20[s0]))
      bull <- isTRUE(benchmark_bull[s0])
      
      # Determine active leverage:
      active_margin <- 1.0
      if (bull) {
        if (turbo_leverage && isTRUE(super_bull_q[s0])) {
          active_margin <- 1.75 # Turbo leverage during confirmed super-bull runs
        } else {
          active_margin <- 1.50
        }
      }
      
      ts <- setNames(rep(0, length(U)), U)
      td <- 0
      ok <- U[MEM[s0, ] & !is.na(P[s0, ]) & !is.na(P[max(1, s0 - 252), ])]
      
      if (bull) {
        if (length(ok) >= top_n) {
          r126 <- P[s0, ok] / P[s0 - 126, ok] - 1 - (q[s0] / q[s0 - 126] - 1)
          r63  <- P[s0, ok] / P[s0 - 63, ok]  - 1 - (q[s0] / q[s0 - 63] - 1)
          rs   <- 0.50 * r126 + 0.50 * r63
          
          # Optional Momentum Acceleration Filter
          if (require_acceleration) {
            acc_ok <- ok[(r63 * 2.0) >= r126]
            if (length(acc_ok) >= top_n) { ok <- acc_ok; rs <- rs[ok] }
          }
          
          # 100% Volatility Cap
          k2 <- ok[!is.na(VOL63[s0, ok]) & VOL63[s0, ok] <= 1.00]
          if (length(k2) >= top_n) { ok <- k2; rs <- rs[k2] }
          
          rk <- names(sort(rs, decreasing = TRUE))
          held <- intersect(names(sw)[sw > 0], rk[1:top_n])
          top <- unique(c(held, rk))[1:top_n]
          top <- top[!is.na(top)]
          
          # Compute Slot Weights
          n_top <- length(top)
          if (sizing_mode == "equal") {
            wts <- rep(1 / n_top, n_top)
          } else if (sizing_mode == "rank_weighted") {
            # Linear decay: e.g. for 5 stocks: [5, 4, 3, 2, 1] / sum = [33%, 27%, 20%, 13%, 7%]
            ranks <- seq(n_top, 1, -1)
            wts <- ranks / sum(ranks)
          } else if (sizing_mode == "er_weighted") {
            ers <- ER63[s0, top]
            ers[is.na(ers) | ers <= 0] <- 0.2
            wts <- ers / sum(ers)
          } else if (sizing_mode == "inv_vol") {
            vols <- VOL63[s0, top]
            vols[is.na(vols) | vols <= 0] <- 0.50
            inv_v <- 1 / vols
            wts <- inv_v / sum(inv_v)
          }
          
          ts[top] <- sat_pct * sc * active_margin * wts
          
          for (ne in setdiff(top, names(sw)[sw > 0])) {
            ep[ne] <- P[t, ne]
          }
        }
      } else {
        # Crisis / Bear Regime Handling
        if (crisis_mode == "mvp") {
          if (length(ok) >= top_n) {
            vol_ranks <- sort(VOL63[s0, ok])
            low_vol_syms <- names(head(vol_ranks, 10))
            R_sub <- R[(s0 - 62):s0, low_vol_syms, drop = FALSE]
            valid_cols <- apply(R_sub, 2, function(x) !any(is.na(x)) && sd(x) > 0)
            if (sum(valid_cols) >= top_n) {
              low_vol_syms <- names(valid_cols)[valid_cols][1:top_n]
              cov_mat <- cov(R_sub[, low_vol_syms, drop = FALSE])
              w_mvp <- solve_min_var(cov_mat)
              ts[low_vol_syms] <- sat_pct * sc * active_margin * w_mvp
              for (ne in setdiff(low_vol_syms, names(sw)[sw > 0])) ep[ne] <- P[t, ne]
            }
          }
        } else if (crisis_mode == "gld") {
          td <- 1.0 * sc * active_margin # 100% Gold in crisis
        } else if (crisis_mode == "cash") {
          # 100% Cash in crisis (no stock or gold positions)
        }
      }
      
      to <- sum(abs(ts - sw)) + abs(td - dw)
      eq <- eq * (1 - COST * to)
      turn_tot <- turn_tot + to
      sw <- ts
      dw <- td
    }
    
    out[k] <- eq / prev - 1
  }
  
  attr(out, "turnover_yr") <- turn_tot / (length(ev) / 252)
  out
}

calc_metrics <- function(r, start_d, end_d) {
  ev_idx <- which(cal >= start_d & cal <= end_d)
  c_r <- cumprod(1 + r); n <- length(r); dd <- c_r / cummax(c_r) - 1
  mdd <- -min(dd) * 100
  cagr <- (tail(c_r, 1)^(252 / n) - 1) * 100
  term <- 8000 * tail(c_r, 1)
  vol <- sd(r) * sqrt(252) * 100
  rfm <- rf_daily[ev_idx]
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

# Define candidate experiments
candidate_list <- list(
  "0. Baseline (1.5x Margin, Equal Weight, Unrestricted, MVP)" = 
    list(sizing = "equal", turbo = FALSE, acc = FALSE, stop_rep = FALSE, crisis = "mvp"),
    
  "1. Conviction Rank Sizing (Steeper weight to Rank 1 & 2)" = 
    list(sizing = "rank_weighted", turbo = FALSE, acc = FALSE, stop_rep = FALSE, crisis = "mvp"),
    
  "2. Super-Bull Leverage Turbo (1.75x when 50 > 200 SMA & Breadth)" = 
    list(sizing = "equal", turbo = TRUE, acc = FALSE, stop_rep = FALSE, crisis = "mvp"),
    
  "3. Momentum Acceleration Filter (Ret_63 * 2 >= Ret_126)" = 
    list(sizing = "equal", turbo = FALSE, acc = TRUE, stop_rep = FALSE, crisis = "mvp"),
    
  "4. Immediate Stop Replacement (Re-deploy stopped cash to Rank 6)" = 
    list(sizing = "equal", turbo = FALSE, acc = FALSE, stop_rep = TRUE, crisis = "mvp"),
    
  "5. Efficiency-Ratio (ER) Weighting (Favor smooth trends)" = 
    list(sizing = "er_weighted", turbo = FALSE, acc = FALSE, stop_rep = FALSE, crisis = "mvp"),
    
  "6. Tactical Crisis Gold (Bear -> 100% Gold vs MVP)" = 
    list(sizing = "equal", turbo = FALSE, acc = FALSE, stop_rep = FALSE, crisis = "gld"),
    
  "7. Volatility Drag Minimization (Inverse-Vol Sizing within Top 5)" = 
    list(sizing = "inv_vol", turbo = FALSE, acc = FALSE, stop_rep = FALSE, crisis = "mvp"),
    
  "8. Power Alpha Turbo (Super-Bull 1.75x + Immediate Stop Replace)" = 
    list(sizing = "equal", turbo = TRUE, acc = FALSE, stop_rep = TRUE, crisis = "mvp"),
    
  "9. Supreme Momentum Engine (Turbo 1.75x + Stop Replace + Tactical Gold)" = 
    list(sizing = "equal", turbo = TRUE, acc = FALSE, stop_rep = TRUE, crisis = "gld")
)

cat("\n========================================================================================\n")
cat(" RUNNING 10-YEAR EMPIRICAL EVALUATION OF CAGR-BOOSTING CANDIDATES (2016 - 2026)\n")
cat("========================================================================================\n")

runs_10 <- lapply(candidate_list, function(p) {
  sim_feature_engine(sizing_mode = p$sizing, turbo_leverage = p$turbo,
                     require_acceleration = p$acc, immediate_stop_replace = p$stop_rep,
                     crisis_mode = p$crisis, start = S10, end = E10)
})

tbl_10 <- t(sapply(runs_10, calc_metrics, start_d = S10, end_d = E10))
print(round(tbl_10, 2))
write.csv(tbl_10, "scratch/scorecard_10yr_plus5pct_candidates.csv")

cat("\n========================================================================================\n")
cat(" RUNNING 20-YEAR EMPIRICAL EVALUATION OF CAGR-BOOSTING CANDIDATES (2007 - 2026)\n")
cat("========================================================================================\n")

runs_20 <- lapply(candidate_list, function(p) {
  sim_feature_engine(sizing_mode = p$sizing, turbo_leverage = p$turbo,
                     require_acceleration = p$acc, immediate_stop_replace = p$stop_rep,
                     crisis_mode = p$crisis, start = S20, end = E20)
})

tbl_20 <- t(sapply(runs_20, calc_metrics, start_d = S20, end_d = E20))
print(round(tbl_20, 2))
write.csv(tbl_20, "scratch/scorecard_20yr_plus5pct_candidates.csv")

# Generate Chart Artifact
ART <- "/Users/cmejo/.gemini/antigravity/brain/64019830-0cac-4bb1-904e-d4560e71468c"
png(file.path(ART, "cagr_plus5pct_candidates_comparison.png"), width = 1600, height = 950, res = 120)
par(mfrow = c(1, 2), mar = c(5, 15, 4, 2))

cagr_10 <- tbl_10[, "CAGR"]
cols_10 <- ifelse(cagr_10 >= 105, "#10b981", ifelse(cagr_10 >= 100, "#3b82f6", "#ef4444"))
barplot(rev(cagr_10), horiz = TRUE, las = 1, col = rev(cols_10),
        main = "10-Year CAGR (%): New Feature Candidates",
        xlab = "CAGR (%)", xlim = c(0, 130))
grid(col = "gray85")
abline(v = cagr_10[1], col = "darkblue", lty = 2, lwd = 2)

cagr_20 <- tbl_20[, "CAGR"]
cols_20 <- ifelse(cagr_20 >= 63, "#10b981", ifelse(cagr_20 >= 60, "#3b82f6", "#ef4444"))
barplot(rev(cagr_20), horiz = TRUE, las = 1, col = rev(cols_20),
        main = "20-Year CAGR (%): New Feature Candidates",
        xlab = "CAGR (%)", xlim = c(0, 75))
grid(col = "gray85")
abline(v = cagr_20[1], col = "darkblue", lty = 2, lwd = 2)

dev.off()
cat("Chart saved to:", file.path(ART, "cagr_plus5pct_candidates_comparison.png"), "\n")
