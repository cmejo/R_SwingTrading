#!/usr/bin/env Rscript
# ==============================================================================
# scratch/test_cagr_plus5pct_no_leverage.R
# Search for New Alpha Features to Increase CAGR by +5% or More
# STRICT CONSTRAINT: ZERO EXTRA LEVERAGE (Keeps current 2.50x super-bull / 1.50x bull / 1.00x bear)
#
# Candidates Tested:
#   0. Baseline: Current Production Engine (Choice 4: 2.50x Turbo, Top 8 Buffer, -25% Stop, Equal 20%)
#   1. Bi-Weekly Rebalancing (Every 10 Trading Days instead of Monthly)
#   2. Frog-in-the-Pan (FIP) Continuous Momentum (Rewards high % of positive days)
#   3. Exponential Trend Smoothness (R^2-Weighted Momentum)
#   4. Residual Momentum (Beta-Adjusted Idiosyncratic Alpha)
#   5. Parabolic Profit Scale-Out (+100% Gain -> Trim 25% of Position)
#   6. EWMA Fast Volatility Targeting (lambda = 0.94 vs 20d Rolling)
#   7. Dynamic Breadth Concentration (Top 3 when Breadth < 50%, Top 5 when >= 50%)
#   8. Multi-Factor Apex Combo (Best performing orthogonal features combined)
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

# Market breadth (% above 50 SMA)
sma50_matrix <- matrix(NA_real_, nrow = nrow(P), ncol = ncol(P), dimnames = dimnames(P))
for (j in 1:ncol(P)) {
  sma50_matrix[, j] <- as.numeric(stats::filter(P[, j], rep(1/50, 50), sides = 1))
}
breadth_50 <- rowMeans(P > sma50_matrix, na.rm = TRUE)
breadth_50[is.na(breadth_50)] <- 0.50

# Relative returns vs QQQ
r126_mat <- matrix(NA_real_, nrow = nrow(P), ncol = ncol(P), dimnames = dimnames(P))
r63_mat  <- matrix(NA_real_, nrow = nrow(P), ncol = ncol(P), dimnames = dimnames(P))
q_lag126 <- c(rep(NA, 126), q[1:(length(q) - 126)])
q_lag63  <- c(rep(NA, 63), q[1:(length(q) - 63)])
q_r126   <- q / q_lag126 - 1
q_r63    <- q / q_lag63 - 1

# Frog-in-the-Pan (FIP): % of positive return days over trailing 63 days
pos_days_63 <- matrix(NA_real_, nrow = nrow(P), ncol = ncol(P), dimnames = dimnames(P))

# Exponential Trend Smoothness: R^2 of log prices over trailing 63 days
rsq_trend_63 <- matrix(NA_real_, nrow = nrow(P), ncol = ncol(P), dimnames = dimnames(P))

cat("Pre-computing multi-factor matrices (FIP & Trend Smoothness)...\n")
time_idx <- 1:63
for (j in 1:ncol(P)) {
  px <- P[, j]
  px_lag126 <- c(rep(NA, 126), px[1:(length(px) - 126)])
  px_lag63  <- c(rep(NA, 63), px[1:(length(px) - 63)])
  r126_mat[, j] <- (px / px_lag126 - 1) - q_r126
  r63_mat[, j]  <- (px / px_lag63 - 1)  - q_r63
  
  # Rolling % of positive daily returns
  ret_j <- R[, j]
  pos_days_63[, j] <- as.numeric(zoo::rollapply(ret_j > 0, 63, mean, align = "right", fill = NA))
  
  # Rolling R^2 of log prices over 63 bars (analytical regression formula for speed)
  log_px <- log(pmax(px, 1e-4))
  # Sum of (x - mean(x)) * (y - mean(y))
  # Since x is fixed 1:63, mean(x) = 32, var(x) = sum((1:63 - 32)^2) = 20832
  rsq_calc <- function(y) {
    if (any(is.na(y))) return(NA_real_)
    my <- mean(y)
    cov_xy <- sum((time_idx - 32) * (y - my))
    ss_yy <- sum((y - my)^2)
    if (ss_yy <= 0) return(0.0)
    (cov_xy^2) / (20832 * ss_yy)
  }
  # Pre-compute every 5 days for speed, then na.locf
  rsq_sparse <- rep(NA_real_, length(px))
  sample_points <- seq(64, length(px), by = 5)
  for (sp in sample_points) {
    rsq_sparse[sp] <- rsq_calc(log_px[(sp - 62):sp])
  }
  rsq_trend_63[, j] <- na.locf(rsq_sparse, na.rm = FALSE)
}

# EWMA Volatility of QQQ (lambda = 0.94)
cat("Pre-computing EWMA volatility...\n")
ret_q_daily <- rq
ewma_var_q <- numeric(length(ret_q_daily))
ewma_var_q[1] <- ret_q_daily[1]^2
lambda <- 0.94
for (i in 2:length(ret_q_daily)) {
  ewma_var_q[i] <- lambda * ewma_var_q[i - 1] + (1 - lambda) * ret_q_daily[i]^2
}
ewma_vol_q <- sqrt(ewma_var_q) * sqrt(252)

# Analytical Minimum Variance Portfolio solver
solve_min_var <- function(cov_mat) {
  N <- ncol(cov_mat)
  if (N == 1) return(c(1.0))
  inv_var <- 1 / diag(cov_mat)
  inv_var[!is.finite(inv_var) | inv_var < 0] <- 0
  if (sum(inv_var) <= 0) return(rep(1 / N, N))
  inv_var / sum(inv_var)
}

# ==============================================================================
# General Feature Simulation Engine
# ==============================================================================
sim_feature_candidate <- function(
    cadence = c("monthly", "biweekly"),
    alpha_factor = c("standard_rs", "frog_in_pan", "rsq_smoothness", "residual_mom"),
    use_profit_trim = FALSE,          # Trim 25% of slot when stock gains >= 100%
    vol_target_type = c("rolling20", "ewma"),
    dynamic_breadth_sizing = FALSE,   # Top 3 if breadth < 50%, Top 5 if >= 50%
    super_bull_leverage = 2.50,       # Strict baseline leverage constraint: 2.50x
    buffer_rank = 8,
    disaster_stop_pct = 0.25,
    start = S20,
    end = E20
) {
  cadence         <- match.arg(cadence)
  alpha_factor    <- match.arg(alpha_factor)
  vol_target_type <- match.arg(vol_target_type)
  
  ev <- which(cal >= start & cal <= end)
  sat_pct <- 1.0
  vt <- 0.22
  
  cw <- 0
  sw <- setNames(rep(0, length(U)), U)
  dw <- 0
  ep <- setNames(rep(NA_real_, length(U)), U)
  trimmed <- setNames(rep(FALSE, length(U)), U) # tracks if 100% profit trim triggered
  
  eq <- 1
  turn_tot <- 0
  out <- numeric(length(ev))
  
  rebalance_days <- if (cadence == "monthly") {
    which(month_end[ev])
  } else {
    # Bi-weekly: every 10 trading days
    seq(1, length(ev), by = 10)
  }
  
  for (k in seq_along(ev)) {
    t <- ev[k]
    prev <- eq
    gross <- cw + sum(sw) + dw
    
    g <- cw * rq[t] + sum(sw * R[t, ]) + dw * r_gld[t] - 
         max(0, gross - 1) * borrow_daily[t] + max(0, 1 - gross) * rf_daily[t]
    
    eq <- eq * (1 + g)
    cw <- cw * (1 + rq[t]) / (1 + g)
    sw <- sw * (1 + R[t, ]) / (1 + g)
    dw <- dw * (1 + r_gld[t]) / (1 + g)
    
    s0 <- t - 1
    is_sb <- isTRUE(super_bull_q[s0])
    
    curr_top_n <- if (dynamic_breadth_sizing) {
      if (breadth_50[s0] < 0.50) 3 else 5
    } else {
      5
    }
    
    # 1. Parabolic Profit Scale-Out Check (+100% Gain -> Trim 25% of slot to cash)
    if (use_profit_trim) {
      for (s in names(sw)[sw > 0]) {
        if (!is.na(ep[s]) && !trimmed[s]) {
          cur_px <- P[t, s]
          if (cur_px / ep[s] - 1 >= 1.00) {
            # Trim 25% of holding
            trim_w <- sw[s] * 0.25
            sw[s] <- sw[s] - trim_w
            trimmed[s] <- TRUE
            eq <- eq * (1 - COST * trim_w)
            turn_tot <- turn_tot + trim_w
          }
        }
      }
    }
    
    # 2. Stop Check (-25% Disaster Floor)
    stopped_symbols <- character()
    for (s in names(sw)[sw > 0]) {
      cur_px <- P[t, s]
      hit_stop <- (!is.na(ep[s]) && cur_px / ep[s] - 1 <= -disaster_stop_pct)
      if (isTRUE(hit_stop)) {
        w <- sw[s]
        eq <- eq * (1 - COST * w)
        turn_tot <- turn_tot + w
        sw[s] <- 0
        ep[s] <- NA
        trimmed[s] <- FALSE
        stopped_symbols <- c(stopped_symbols, s)
      }
    }
    
    # 3. Immediate Stop Replacement
    if (length(stopped_symbols) > 0 && isTRUE(benchmark_bull[s0])) {
      ok <- U[MEM[s0, ] & !is.na(P[s0, ]) & !is.na(P[max(1, s0 - 126), ])]
      if (length(ok) >= 10) {
        # Alpha score computation
        if (alpha_factor == "standard_rs") {
          score <- 0.50 * r126_mat[s0, ok] + 0.50 * r63_mat[s0, ok]
        } else if (alpha_factor == "frog_in_pan") {
          fip <- pos_days_63[s0, ok]
          fip[is.na(fip)] <- 0.50
          rs_raw <- 0.50 * r126_mat[s0, ok] + 0.50 * r63_mat[s0, ok]
          score <- 0.70 * rs_raw + 0.30 * (fip - 0.50)
        } else if (alpha_factor == "rsq_smoothness") {
          rsq <- rsq_trend_63[s0, ok]
          rsq[is.na(rsq)] <- 0.50
          rs_raw <- 0.50 * r126_mat[s0, ok] + 0.50 * r63_mat[s0, ok]
          score <- rs_raw * (0.50 + 0.50 * rsq)
        } else if (alpha_factor == "residual_mom") {
          rs_raw <- 0.50 * r126_mat[s0, ok] + 0.50 * r63_mat[s0, ok]
          vol_ratio <- VOL63[s0, ok] / max(0.15, vol20[s0])
          vol_ratio[is.na(vol_ratio)] <- 1.0
          score <- rs_raw / pmax(0.5, pmin(2.0, vol_ratio))
        }
        
        k2 <- ok[!is.na(VOL63[s0, ok]) & VOL63[s0, ok] <= 1.00]
        if (length(k2) >= 10) { ok <- k2; score <- score[k2] }
        rk <- names(sort(score, decreasing = TRUE))
        unheld <- setdiff(rk, c(names(sw)[sw > 0], stopped_symbols))
        
        act_lev <- if (is_sb) super_bull_leverage else 1.50
        target_w_per_slot <- act_lev / curr_top_n
        
        for (rep_sym in head(unheld, length(stopped_symbols))) {
          sw[rep_sym] <- target_w_per_slot
          ep[rep_sym] <- P[t, rep_sym]
          trimmed[rep_sym] <- FALSE
          eq <- eq * (1 - COST * target_w_per_slot)
          turn_tot <- turn_tot + target_w_per_slot
        }
      }
    }
    
    # 4. Rebalance Trigger
    is_reb_day <- (k %in% rebalance_days) || (k == 1)
    if (is_reb_day) {
      cur_vol <- if (vol_target_type == "ewma") ewma_vol_q[s0] else vol20[s0]
      sc <- min(1, vt / max(0.05, cur_vol))
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
          if (alpha_factor == "standard_rs") {
            score <- 0.50 * r126_mat[s0, ok] + 0.50 * r63_mat[s0, ok]
          } else if (alpha_factor == "frog_in_pan") {
            fip <- pos_days_63[s0, ok]
            fip[is.na(fip)] <- 0.50
            rs_raw <- 0.50 * r126_mat[s0, ok] + 0.50 * r63_mat[s0, ok]
            score <- 0.70 * rs_raw + 0.30 * (fip - 0.50)
          } else if (alpha_factor == "rsq_smoothness") {
            rsq <- rsq_trend_63[s0, ok]
            rsq[is.na(rsq)] <- 0.50
            rs_raw <- 0.50 * r126_mat[s0, ok] + 0.50 * r63_mat[s0, ok]
            score <- rs_raw * (0.50 + 0.50 * rsq)
          } else if (alpha_factor == "residual_mom") {
            rs_raw <- 0.50 * r126_mat[s0, ok] + 0.50 * r63_mat[s0, ok]
            vol_ratio <- VOL63[s0, ok] / max(0.15, vol20[s0])
            vol_ratio[is.na(vol_ratio)] <- 1.0
            score <- rs_raw / pmax(0.5, pmin(2.0, vol_ratio))
          }
          
          k2 <- ok[!is.na(VOL63[s0, ok]) & VOL63[s0, ok] <= 1.00]
          if (length(k2) >= curr_top_n) { ok <- k2; score <- score[k2] }
          rk <- names(sort(score, decreasing = TRUE))
          
          # Hysteresis Winner Retention Buffer (Top buffer_rank)
          cur_held <- names(sw)[sw > 0]
          buffer_qual <- intersect(cur_held, rk[1:min(length(rk), buffer_rank)])
          new_picks <- setdiff(rk, buffer_qual)
          slots_needed <- max(0, curr_top_n - length(buffer_qual))
          top <- c(buffer_qual, head(new_picks, slots_needed))[1:curr_top_n]
          top <- top[!is.na(top)]
          
          n_top <- length(top)
          wts <- rep(1 / n_top, n_top)
          
          ts[top] <- sat_pct * sc * active_margin * wts
          
          for (ne in setdiff(top, names(sw)[sw > 0])) {
            ep[ne] <- P[t, ne]
            trimmed[ne] <- FALSE
          }
        }
      } else {
        # Crisis MVP in Bear Regime
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
              trimmed[ne] <- FALSE
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
  xts(out, order.by = cal[ev])
}

calc_stats <- function(r_xts, start_d, end_d, capital = 8000) {
  sub_xts <- r_xts[paste0(start_d, "/", end_d)]
  sub_r <- as.numeric(sub_xts)
  c_r <- cumprod(1 + sub_r)
  n <- length(sub_r)
  yrs <- n / 252
  dd <- c_r / cummax(c_r) - 1
  max_dd <- abs(min(dd)) * 100
  cagr <- (tail(c_r, 1)^(252 / n) - 1) * 100
  term <- capital * tail(c_r, 1)
  vol <- sd(sub_r, na.rm = TRUE) * sqrt(252) * 100
  
  rf_xts <- xts(rf_daily, order.by = cal)[paste0(start_d, "/", end_d)]
  rfm <- as.numeric(rf_xts)
  sharpe <- if (sd(sub_r, na.rm = TRUE) > 0) mean(sub_r - rfm, na.rm = TRUE) / sd(sub_r, na.rm = TRUE) * sqrt(252) else 0
  dn_ret <- pmin(sub_r - rfm, 0)
  dn_sd  <- sqrt(mean(dn_ret^2, na.rm = TRUE)) * sqrt(252)
  sortino <- if (dn_sd > 0) mean(sub_r - rfm, na.rm = TRUE) * 252 / dn_sd else 0
  calmar <- if (max_dd > 0) cagr / max_dd else 0
  turn <- if (!is.null(attr(r_xts, "turnover_yr"))) attr(r_xts, "turnover_yr") else 0
  
  c(CAGR = cagr, Terminal = term, MaxDD = max_dd, Vol = vol,
    Sharpe = sharpe, Sortino = sortino, Calmar = calmar, Turnover = turn)
}

# ==============================================================================
# Define Candidates Matrix
# ==============================================================================
candidates <- list(
  "0. Baseline: 2.50x Turbo (Current Production)" = list(
    cadence = "monthly", alpha_factor = "standard_rs", use_profit_trim = FALSE,
    vol_target_type = "rolling20", dynamic_breadth_sizing = FALSE
  ),
  "1. Bi-Weekly Rebalancing (Every 10 Trading Days)" = list(
    cadence = "biweekly", alpha_factor = "standard_rs", use_profit_trim = FALSE,
    vol_target_type = "rolling20", dynamic_breadth_sizing = FALSE
  ),
  "2. Frog-in-the-Pan (FIP) Continuous Momentum" = list(
    cadence = "monthly", alpha_factor = "frog_in_pan", use_profit_trim = FALSE,
    vol_target_type = "rolling20", dynamic_breadth_sizing = FALSE
  ),
  "3. Exponential Trend Smoothness (R^2 Weighted RS)" = list(
    cadence = "monthly", alpha_factor = "rsq_smoothness", use_profit_trim = FALSE,
    vol_target_type = "rolling20", dynamic_breadth_sizing = FALSE
  ),
  "4. Residual Momentum (Volatility-Normalized RS)" = list(
    cadence = "monthly", alpha_factor = "residual_mom", use_profit_trim = FALSE,
    vol_target_type = "rolling20", dynamic_breadth_sizing = FALSE
  ),
  "5. Parabolic Profit Scale-Out (+100% -> Trim 25%)" = list(
    cadence = "monthly", alpha_factor = "standard_rs", use_profit_trim = TRUE,
    vol_target_type = "rolling20", dynamic_breadth_sizing = FALSE
  ),
  "6. EWMA Fast Volatility Targeting (lambda=0.94)" = list(
    cadence = "monthly", alpha_factor = "standard_rs", use_profit_trim = FALSE,
    vol_target_type = "ewma", dynamic_breadth_sizing = FALSE
  ),
  "7. Dynamic Breadth Concentration (Top 3/5)" = list(
    cadence = "monthly", alpha_factor = "standard_rs", use_profit_trim = FALSE,
    vol_target_type = "rolling20", dynamic_breadth_sizing = TRUE
  ),
  "8. Apex Multi-Factor Combo (FIP + EWMA Vol)" = list(
    cadence = "monthly", alpha_factor = "frog_in_pan", use_profit_trim = FALSE,
    vol_target_type = "ewma", dynamic_breadth_sizing = FALSE
  )
)

cat("\n========================================================================================\n")
cat("          RUNNING FULL 10-YEAR & 20-YEAR BACKTESTS FOR ALL CANDIDATES                   \n")
cat("========================================================================================\n")

res_10yr <- list()
res_20yr <- list()

for (c_name in names(candidates)) {
  cfg <- candidates[[c_name]]
  cat(sprintf("Simulating: %s...\n", c_name))
  r_sim <- sim_feature_candidate(
    cadence = cfg$cadence,
    alpha_factor = cfg$alpha_factor,
    use_profit_trim = cfg$use_profit_trim,
    vol_target_type = cfg$vol_target_type,
    dynamic_breadth_sizing = cfg$dynamic_breadth_sizing,
    super_bull_leverage = 2.50
  )
  
  st10 <- calc_stats(r_sim, S10, E10, capital = 8000)
  st20 <- calc_stats(r_sim, S20, E20, capital = 8000)
  
  res_10yr[[c_name]] <- st10
  res_20yr[[c_name]] <- st20
}

df_10 <- as.data.frame(do.call(rbind, res_10yr))
df_20 <- as.data.frame(do.call(rbind, res_20yr))

fmt_usd <- function(x) {
  ifelse(x >= 1e9, sprintf("$%.2fB", x / 1e9),
  ifelse(x >= 1e6, sprintf("$%.2fM", x / 1e6),
  ifelse(x >= 1e3, sprintf("$%sk", formatC(round(x / 1e3, 1), format = "f", digits = 1, big.mark = ",")),
         sprintf("$%.2f", x))))
}

display_10 <- df_10
display_10$Terminal_8k <- fmt_usd(display_10$Terminal)
display_10$CAGR        <- sprintf("%+.2f%%", display_10$CAGR)
display_10$MaxDD       <- sprintf("-%.2f%%", display_10$MaxDD)
display_10$Vol         <- sprintf("%.1f%%", display_10$Vol)
display_10$Sharpe      <- sprintf("%.2f", display_10$Sharpe)
display_10$Sortino     <- sprintf("%.2f", display_10$Sortino)
display_10$Calmar      <- sprintf("%.2f", display_10$Calmar)
display_10$Turnover    <- sprintf("%.1fx", display_10$Turnover)

display_20 <- df_20
display_20$Terminal_8k <- fmt_usd(display_20$Terminal)
display_20$CAGR        <- sprintf("%+.2f%%", display_20$CAGR)
display_20$MaxDD       <- sprintf("-%.2f%%", display_20$MaxDD)
display_20$Vol         <- sprintf("%.1f%%", display_20$Vol)
display_20$Sharpe      <- sprintf("%.2f", display_20$Sharpe)
display_20$Sortino     <- sprintf("%.2f", display_20$Sortino)
display_20$Calmar      <- sprintf("%.2f", display_20$Calmar)
display_20$Turnover    <- sprintf("%.1fx", display_20$Turnover)

cat("\n========================================================================================================================\n")
cat("                10-YEAR SCORECARD (2016 - 2026): ZERO EXTRA LEVERAGE CANDIDATES                                          \n")
cat("========================================================================================================================\n")
print(display_10[, c("CAGR", "Terminal_8k", "MaxDD", "Vol", "Sharpe", "Sortino", "Calmar", "Turnover")])

cat("\n========================================================================================================================\n")
cat("                20-YEAR SCORECARD (2007 - 2026): ZERO EXTRA LEVERAGE CANDIDATES                                          \n")
cat("========================================================================================================================\n")
print(display_20[, c("CAGR", "Terminal_8k", "MaxDD", "Vol", "Sharpe", "Sortino", "Calmar", "Turnover")])
cat("========================================================================================================================\n\n")

write.csv(df_10, "scratch/scorecard_10yr_no_leverage_candidates.csv")
write.csv(df_20, "scratch/scorecard_20yr_no_leverage_candidates.csv")
cat("Saved scorecards to scratch/scorecard_10yr_no_leverage_candidates.csv and scorecard_20yr_no_leverage_candidates.csv\n")
