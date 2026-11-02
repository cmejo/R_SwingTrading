#!/usr/bin/env Rscript
# ==============================================================================
# scratch/test_next_cagr_plus5pct_breakthroughs.R
# Search for New Structural Alpha Features to Increase CAGR by Another +5%
#
# Baseline Engine: Option D (Ultimate Alpha Master)
#   - 100% Stock Momentum (Top 5 stocks)
#   - Winner Retention Buffer (Top 8 Hysteresis)
#   - Super-Bull 2.0x Leverage Turbo
#   - -25% Disaster Stop Floor + Immediate Stop Replacement (Rank 6)
#   - Crisis MVP in Bear
#   - Baseline 20-Yr CAGR: 84.48% ($1.41B on $8k)
#   - Baseline 10-Yr CAGR: 152.02% ($81.49M on $8k)
#
# Breakthrough Candidates Evaluated:
#   0. Baseline: Ultimate Alpha Master (Equal Weight 5, -25% Stop, 2.0x Turbo)
#   1. Convex Rank Sizing (30%, 25%, 20%, 15%, 10% on Ranks 1 to 5)
#   2. Asymmetric Profit Ratchet (+75% gain -> ratchet stop to Peak - 20%)
#   3. Triple-Lookback Momentum (40% 126d + 40% 63d + 20% 21d)
#   4. Accelerated Lookback Momentum (30% 126d + 50% 63d + 20% 42d)
#   5. Super-Bull 4-Stock Concentration (Top 4 @ 25% in Super Bull)
#   6. Super-Bull Ultra-Turbo 2.25x (Schwab Portfolio Margin Tier)
#   7. Super-Bull Institutional 2.50x Leverage Turbo
#   8. Square-Root Inverse-Vol Sizing within Satellite (Risk Equalization)
#   9. The "Apex Compounder" Synergy Combo (Convex Sizing + Profit Ratchet + 2.25x)
# ==============================================================================

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(jsonlite)
})

cat("Loading price and universe datasets...\n")
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

# 1. Macro Diagnostics
qqq_ts <- xts(q, order.by = cal)
sma50_q  <- as.numeric(stats::filter(q, rep(1/50, 50), sides = 1))
sma200_q <- as.numeric(stats::filter(q, rep(1/200, 200), sides = 1))
lm50_q   <- lmMA(qqq_ts, 50)
slope50_q <- as.numeric(lm50_q$slope)
rsq50_q   <- as.numeric(lm50_q$r.squared)
benchmark_bull <- !(slope50_q < 0 & rsq50_q > 0.15)

super_bull_q <- (q > sma50_q & sma50_q > sma200_q & slope50_q > 0)
super_bull_q[is.na(super_bull_q)] <- FALSE

# Precalculate relative returns for 126d, 63d, 42d, 21d
r126_mat <- matrix(NA_real_, nrow = nrow(P), ncol = ncol(P), dimnames = dimnames(P))
r63_mat  <- matrix(NA_real_, nrow = nrow(P), ncol = ncol(P), dimnames = dimnames(P))
r42_mat  <- matrix(NA_real_, nrow = nrow(P), ncol = ncol(P), dimnames = dimnames(P))
r21_mat  <- matrix(NA_real_, nrow = nrow(P), ncol = ncol(P), dimnames = dimnames(P))

q_lag126 <- c(rep(NA, 126), q[1:(length(q) - 126)])
q_lag63  <- c(rep(NA, 63), q[1:(length(q) - 63)])
q_lag42  <- c(rep(NA, 42), q[1:(length(q) - 42)])
q_lag21  <- c(rep(NA, 21), q[1:(length(q) - 21)])

q_r126   <- q / q_lag126 - 1
q_r63    <- q / q_lag63 - 1
q_r42    <- q / q_lag42 - 1
q_r21    <- q / q_lag21 - 1

for (j in 1:ncol(P)) {
  px <- P[, j]
  px_lag126 <- c(rep(NA, 126), px[1:(length(px) - 126)])
  px_lag63  <- c(rep(NA, 63), px[1:(length(px) - 63)])
  px_lag42  <- c(rep(NA, 42), px[1:(length(px) - 42)])
  px_lag21  <- c(rep(NA, 21), px[1:(length(px) - 21)])
  
  r126_mat[, j] <- (px / px_lag126 - 1) - q_r126
  r63_mat[, j]  <- (px / px_lag63 - 1)  - q_r63
  r42_mat[, j]  <- (px / px_lag42 - 1)  - q_r42
  r21_mat[, j]  <- (px / px_lag21 - 1)  - q_r21
}

sim_breakthrough_engine <- function(
    lookback_mode = c("standard", "triple", "accelerated"),
    sizing_mode = c("equal", "convex_rank", "inv_sqrt_vol"),
    use_profit_ratchet = FALSE,     # Ratchet stop to Peak - 20% once stock gains +75%
    super_bull_top_n = 5,           # 5 baseline, test 4
    super_bull_leverage = 2.00,     # 2.00x baseline, test 2.25x, 2.50x
    buffer_rank = 8,
    disaster_stop_pct = 0.25,
    start = S20,
    end = E20
) {
  lookback_mode <- match.arg(lookback_mode)
  sizing_mode   <- match.arg(sizing_mode)
  
  ev <- which(cal >= start & cal <= end)
  sat_pct <- 1.0
  vt <- 0.22
  
  cw <- 0
  sw <- setNames(rep(0, length(U)), U)
  dw <- 0
  ep <- setNames(rep(NA_real_, length(U)), U) # entry price
  hp <- setNames(rep(NA_real_, length(U)), U) # highest price since entry
  
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
    is_sb <- isTRUE(super_bull_q[s0])
    curr_top_n <- if (is_sb) super_bull_top_n else 5
    
    # 1. Update High-Water Mark and Check Stops
    stopped_symbols <- character()
    for (s in names(sw)[sw > 0]) {
      cur_px <- P[t, s]
      if (is.na(hp[s]) || cur_px > hp[s]) hp[s] <- cur_px
      
      # Base disaster stop: -25% from entry
      hit_stop <- (!is.na(ep[s]) && cur_px / ep[s] - 1 <= -disaster_stop_pct)
      
      # Optional: Asymmetric Profit Ratchet for big winners
      if (use_profit_ratchet && !hit_stop && !is.na(ep[s]) && !is.na(hp[s])) {
        gain_from_entry <- hp[s] / ep[s] - 1
        if (gain_from_entry >= 0.75) {
          # Once stock has gained >= 75%, protect profits with a trailing floor of Peak - 20%
          # or Breakeven (whichever is higher)
          trail_stop_level <- max(ep[s] * 1.02, hp[s] * 0.80)
          if (cur_px <= trail_stop_level) {
            hit_stop <- TRUE
          }
        }
      }
      
      if (isTRUE(hit_stop)) {
        w <- sw[s]
        eq <- eq * (1 - COST * w)
        turn_tot <- turn_tot + w
        sw[s] <- 0
        ep[s] <- NA
        hp[s] <- NA
        stopped_symbols <- c(stopped_symbols, s)
      }
    }
    
    # 2. Immediate Stop Replacement (Re-deploy freed capital into Rank 6 unheld)
    if (length(stopped_symbols) > 0 && isTRUE(benchmark_bull[s0])) {
      ok <- U[MEM[s0, ] & !is.na(P[s0, ]) & !is.na(P[max(1, s0 - 126), ])]
      if (length(ok) >= 10) {
        if (lookback_mode == "standard") {
          rs <- 0.50 * r126_mat[s0, ok] + 0.50 * r63_mat[s0, ok]
        } else if (lookback_mode == "triple") {
          rs <- 0.40 * r126_mat[s0, ok] + 0.40 * r63_mat[s0, ok] + 0.20 * r21_mat[s0, ok]
        } else if (lookback_mode == "accelerated") {
          rs <- 0.30 * r126_mat[s0, ok] + 0.50 * r63_mat[s0, ok] + 0.20 * r42_mat[s0, ok]
        }
        
        k2 <- ok[!is.na(VOL63[s0, ok]) & VOL63[s0, ok] <= 1.00]
        if (length(k2) >= 10) { ok <- k2; rs <- rs[k2] }
        rk <- names(sort(rs, decreasing = TRUE))
        unheld <- setdiff(rk, c(names(sw)[sw > 0], stopped_symbols))
        
        act_lev <- if (is_sb) super_bull_leverage else 1.50
        target_w_per_slot <- act_lev / curr_top_n
        
        for (rep_sym in head(unheld, length(stopped_symbols))) {
          sw[rep_sym] <- target_w_per_slot
          ep[rep_sym] <- P[t, rep_sym]
          hp[rep_sym] <- P[t, rep_sym]
          eq <- eq * (1 - COST * target_w_per_slot)
          turn_tot <- turn_tot + target_w_per_slot
        }
      }
    }
    
    # 3. Monthly Rebalance Trigger
    if (month_end[s0] || k == 1) {
      sc <- min(1, vt / max(0.05, vol20[s0]))
      bull <- isTRUE(benchmark_bull[s0])
      
      active_margin <- 1.0
      if (bull) {
        active_margin <- if (is_sb) super_bull_leverage else 1.50
      }
      
      ts <- setNames(rep(0, length(U)), U)
      td <- 0
      ok <- U[MEM[s0, ] & !is.na(P[s0, ]) & !is.na(P[max(1, s0 - 252), ])]
      
      if (bull) {
        if (length(ok) >= curr_top_n) {
          if (lookback_mode == "standard") {
            rs <- 0.50 * r126_mat[s0, ok] + 0.50 * r63_mat[s0, ok]
          } else if (lookback_mode == "triple") {
            rs <- 0.40 * r126_mat[s0, ok] + 0.40 * r63_mat[s0, ok] + 0.20 * r21_mat[s0, ok]
          } else if (lookback_mode == "accelerated") {
            rs <- 0.30 * r126_mat[s0, ok] + 0.50 * r63_mat[s0, ok] + 0.20 * r42_mat[s0, ok]
          }
          
          # 100% Volatility Cap
          k2 <- ok[!is.na(VOL63[s0, ok]) & VOL63[s0, ok] <= 1.00]
          if (length(k2) >= curr_top_n) { ok <- k2; rs <- rs[k2] }
          
          rk <- names(sort(rs, decreasing = TRUE))
          
          # Hysteresis Winner Retention Buffer (Top buffer_rank)
          cur_held <- names(sw)[sw > 0]
          buffer_qual <- intersect(cur_held, rk[1:min(length(rk), buffer_rank)])
          new_picks <- setdiff(rk, buffer_qual)
          slots_needed <- max(0, curr_top_n - length(buffer_qual))
          top <- c(buffer_qual, head(new_picks, slots_needed))[1:curr_top_n]
          top <- top[!is.na(top)]
          
          n_top <- length(top)
          
          # Sizing Schemes:
          if (sizing_mode == "equal") {
            wts <- rep(1 / n_top, n_top)
          } else if (sizing_mode == "convex_rank") {
            # Convex Weighting: 30%, 25%, 20%, 15%, 10% for 5 stocks
            if (n_top == 5) {
              wts <- c(0.30, 0.25, 0.20, 0.15, 0.10)
            } else if (n_top == 4) {
              wts <- c(0.35, 0.27, 0.21, 0.17)
            } else {
              ranks <- seq(n_top, 1, -1)
              wts <- ranks / sum(ranks)
            }
          } else if (sizing_mode == "inv_sqrt_vol") {
            vols <- VOL63[s0, top]
            vols[is.na(vols) | vols <= 0] <- 0.50
            inv_v <- 1 / sqrt(vols)
            wts <- inv_v / sum(inv_v)
          }
          
          ts[top] <- sat_pct * sc * active_margin * wts
          
          for (ne in setdiff(top, names(sw)[sw > 0])) {
            ep[ne] <- P[t, ne]
            hp[ne] <- P[t, ne]
          }
        }
      } else {
        # MVP in Bear Regime
        if (length(ok) >= 5) {
          vol_ranks <- sort(VOL63[s0, ok])
          low_vol_syms <- names(head(vol_ranks, 10))
          R_sub <- R[(s0 - 62):s0, low_vol_syms, drop = FALSE]
          valid_cols <- apply(R_sub, 2, function(x) !any(is.na(x)) && sd(x) > 0)
          if (sum(valid_cols) >= 5) {
            low_vol_syms <- names(valid_cols)[valid_cols][1:5]
            cov_mat <- cov(R_sub[, low_vol_syms, drop = FALSE])
            w_mvp <- solve_min_var(cov_mat)
            ts[low_vol_syms] <- sat_pct * sc * active_margin * w_mvp
            for (ne in setdiff(low_vol_syms, names(sw)[sw > 0])) {
              ep[ne] <- P[t, ne]
              hp[ne] <- P[t, ne]
            }
          }
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

# Define candidate experiments:
breakthrough_list <- list(
  "0. Baseline: Ultimate Alpha Master (Option D, 2.0x, Equal 5, -25% Stop)" = list(
    lookback = "standard", sizing = "equal", ratchet = FALSE, top_n = 5, lev = 2.00
  ),
  
  "1. Convex Rank Sizing (30%, 25%, 20%, 15%, 10% on Ranks 1 to 5)" = list(
    lookback = "standard", sizing = "convex_rank", ratchet = FALSE, top_n = 5, lev = 2.00
  ),
  
  "2. Asymmetric Profit Ratchet (+75% gain -> Trail Stop @ Peak - 20%)" = list(
    lookback = "standard", sizing = "equal", ratchet = TRUE, top_n = 5, lev = 2.00
  ),
  
  "3. Triple-Lookback Momentum (40% 126d + 40% 63d + 20% 21d)" = list(
    lookback = "triple", sizing = "equal", ratchet = FALSE, top_n = 5, lev = 2.00
  ),
  
  "4. Accelerated Lookback Momentum (30% 126d + 50% 63d + 20% 42d)" = list(
    lookback = "accelerated", sizing = "equal", ratchet = FALSE, top_n = 5, lev = 2.00
  ),
  
  "5. Super-Bull 4-Stock Concentration (Top 4 @ 25% in Super-Bull)" = list(
    lookback = "standard", sizing = "equal", ratchet = FALSE, top_n = 4, lev = 2.00
  ),
  
  "6. Super-Bull Ultra-Turbo 2.25x (Portfolio Margin Level)" = list(
    lookback = "standard", sizing = "equal", ratchet = FALSE, top_n = 5, lev = 2.25
  ),
  
  "7. Super-Bull Institutional 2.50x Leverage Turbo" = list(
    lookback = "standard", sizing = "equal", ratchet = FALSE, top_n = 5, lev = 2.50
  ),
  
  "8. Square-Root Inverse-Vol Sizing within Satellite (Risk Balance)" = list(
    lookback = "standard", sizing = "inv_sqrt_vol", ratchet = FALSE, top_n = 5, lev = 2.00
  ),
  
  "9. Apex Master Combo (Convex Sizing + Profit Ratchet + 2.25x Turbo)" = list(
    lookback = "standard", sizing = "convex_rank", ratchet = TRUE, top_n = 5, lev = 2.25
  )
)

cat("\n========================================================================================\n")
cat(" RUNNING 10-YEAR EVALUATION OF NEXT-GEN BREAKTHROUGH CANDIDATES (2016 - 2026)\n")
cat("========================================================================================\n")

runs_10 <- lapply(names(breakthrough_list), function(nm) {
  cat("Running 10-yr:", nm, "...\n")
  p <- breakthrough_list[[nm]]
  sim_breakthrough_engine(
    lookback_mode = p$lookback, sizing_mode = p$sizing,
    use_profit_ratchet = p$ratchet, super_bull_top_n = p$top_n,
    super_bull_leverage = p$lev, buffer_rank = 8, disaster_stop_pct = 0.25,
    start = S10, end = E10
  )
})
names(runs_10) <- names(breakthrough_list)
tbl_10 <- t(sapply(runs_10, calc_metrics, start_d = S10, end_d = E10))
print(round(tbl_10, 2))
write.csv(tbl_10, "scratch/scorecard_10yr_breakthrough_candidates.csv")

cat("\n========================================================================================\n")
cat(" RUNNING 20-YEAR EVALUATION OF NEXT-GEN BREAKTHROUGH CANDIDATES (2007 - 2026)\n")
cat("========================================================================================\n")

runs_20 <- lapply(names(breakthrough_list), function(nm) {
  cat("Running 20-yr:", nm, "...\n")
  p <- breakthrough_list[[nm]]
  sim_breakthrough_engine(
    lookback_mode = p$lookback, sizing_mode = p$sizing,
    use_profit_ratchet = p$ratchet, super_bull_top_n = p$top_n,
    super_bull_leverage = p$lev, buffer_rank = 8, disaster_stop_pct = 0.25,
    start = S20, end = E20
  )
})
names(runs_20) <- names(breakthrough_list)
tbl_20 <- t(sapply(runs_20, calc_metrics, start_d = S20, end_d = E20))
print(round(tbl_20, 2))
write.csv(tbl_20, "scratch/scorecard_20yr_breakthrough_candidates.csv")

# Generate Chart Artifact
ART <- "/Users/cmejo/.gemini/antigravity/brain/64019830-0cac-4bb1-904e-d4560e71468c"
chart_path <- file.path(ART, "breakthrough_cagr_plus5pct_candidates_comparison.png")
png(chart_path, width = 1650, height = 1000, res = 120)
par(mfrow = c(1, 2), mar = c(5, 18, 4, 2))

cagr_10 <- tbl_10[, "CAGR"]
base_10 <- cagr_10[1]
cols_10 <- ifelse(cagr_10 >= base_10 + 5, "#059669", ifelse(cagr_10 >= base_10, "#3b82f6", "#ef4444"))
barplot(rev(cagr_10), horiz = TRUE, las = 1, col = rev(cols_10),
        main = "10-Year CAGR (%): Breakthrough Candidates",
        xlab = "CAGR (%)", xlim = c(0, max(cagr_10) * 1.15))
grid(col = "gray85")
abline(v = base_10, col = "darkblue", lty = 2, lwd = 2)
abline(v = base_10 + 5, col = "#059669", lty = 3, lwd = 2)

cagr_20 <- tbl_20[, "CAGR"]
base_20 <- cagr_20[1]
cols_20 <- ifelse(cagr_20 >= base_20 + 3, "#059669", ifelse(cagr_20 >= base_20, "#3b82f6", "#ef4444"))
barplot(rev(cagr_20), horiz = TRUE, las = 1, col = rev(cols_20),
        main = "20-Year CAGR (%): Breakthrough Candidates",
        xlab = "CAGR (%)", xlim = c(0, max(cagr_20) * 1.15))
grid(col = "gray85")
abline(v = base_20, col = "darkblue", lty = 2, lwd = 2)
abline(v = base_20 + 3, col = "#059669", lty = 3, lwd = 2)

dev.off()
cat("Comparison chart generated at:", chart_path, "\n")
