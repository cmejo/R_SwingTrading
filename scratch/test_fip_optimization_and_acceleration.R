#!/usr/bin/env Rscript
# ==============================================================================
# scratch/test_fip_optimization_and_acceleration.R
# In-Depth Non-Leverage Alpha Optimization: FIP, Momentum Acceleration & Volume Flow
# STRICT CONSTRAINT: ZERO EXTRA LEVERAGE (2.50x super-bull / 1.50x bull / 1.00x bear)
# ==============================================================================

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(jsonlite)
})

cat("Loading price, volume, and universe datasets...\n")
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

# Macro Diagnostics
qqq_ts <- xts(q, order.by = cal)
sma50_q  <- as.numeric(stats::filter(q, rep(1/50, 50), sides = 1))
sma200_q <- as.numeric(stats::filter(q, rep(1/200, 200), sides = 1))
lm50_q   <- lmMA(qqq_ts, 50)
slope50_q <- as.numeric(lm50_q$slope)
rsq50_q   <- as.numeric(lm50_q$r.squared)
benchmark_bull <- !(slope50_q < 0 & rsq50_q > 0.15)

super_bull_q <- (q > sma50_q & sma50_q > sma200_q & slope50_q > 0)
super_bull_q[is.na(super_bull_q)] <- FALSE

# Relative returns vs QQQ
r126_mat <- matrix(NA_real_, nrow = nrow(P), ncol = ncol(P), dimnames = dimnames(P))
r63_mat  <- matrix(NA_real_, nrow = nrow(P), ncol = ncol(P), dimnames = dimnames(P))
r21_mat  <- matrix(NA_real_, nrow = nrow(P), ncol = ncol(P), dimnames = dimnames(P))
q_lag126 <- c(rep(NA, 126), q[1:(length(q) - 126)])
q_lag63  <- c(rep(NA, 63), q[1:(length(q) - 63)])
q_lag21  <- c(rep(NA, 21), q[1:(length(q) - 21)])
q_r126   <- q / q_lag126 - 1
q_r63    <- q / q_lag63 - 1
q_r21    <- q / q_lag21 - 1

# Frog-in-the-Pan (FIP) matrices
pos_days_63 <- matrix(NA_real_, nrow = nrow(P), ncol = ncol(P), dimnames = dimnames(P))
pos_days_42 <- matrix(NA_real_, nrow = nrow(P), ncol = ncol(P), dimnames = dimnames(P))
pos_days_126 <- matrix(NA_real_, nrow = nrow(P), ncol = ncol(P), dimnames = dimnames(P))

# Information Discreteness: % neg days - % pos days
id_63 <- matrix(NA_real_, nrow = nrow(P), ncol = ncol(P), dimnames = dimnames(P))

cat("Pre-computing multi-horizon FIP and momentum acceleration matrices...\n")
for (j in 1:ncol(P)) {
  px <- P[, j]
  px_lag126 <- c(rep(NA, 126), px[1:(length(px) - 126)])
  px_lag63  <- c(rep(NA, 63), px[1:(length(px) - 63)])
  px_lag21  <- c(rep(NA, 21), px[1:(length(px) - 21)])
  
  r126_mat[, j] <- (px / px_lag126 - 1) - q_r126
  r63_mat[, j]  <- (px / px_lag63 - 1)  - q_r63
  r21_mat[, j]  <- (px / px_lag21 - 1)  - q_r21
  
  ret_j <- R[, j]
  pos_days_63[, j]  <- as.numeric(zoo::rollapply(ret_j > 0, 63, mean, align = "right", fill = NA))
  pos_days_42[, j]  <- as.numeric(zoo::rollapply(ret_j > 0, 42, mean, align = "right", fill = NA))
  pos_days_126[, j] <- as.numeric(zoo::rollapply(ret_j > 0, 126, mean, align = "right", fill = NA))
  
  neg_days_63 <- as.numeric(zoo::rollapply(ret_j < 0, 63, mean, align = "right", fill = NA))
  # ID = sign(RET) * (%neg - %pos); lower (more negative) is better
  ret_cum_63 <- px / px_lag63 - 1
  id_63[, j] <- sign(ret_cum_63) * (neg_days_63 - pos_days_63[, j])
}

# Analytical Minimum Variance Portfolio solver
solve_min_var <- function(cov_mat) {
  N <- ncol(cov_mat)
  if (N == 1) return(c(1.0))
  inv_var <- 1 / diag(cov_mat)
  inv_var[!is.finite(inv_var) | inv_var < 0] <- 0
  if (sum(inv_var) <= 0) return(rep(1 / N, N))
  inv_var / sum(inv_var)
}

# Score computation engine
calc_alpha_score <- function(mode, s0, ok) {
  rs_raw <- 0.50 * r126_mat[s0, ok] + 0.50 * r63_mat[s0, ok]
  
  if (mode == "standard_rs") {
    return(rs_raw)
  }
  
  if (mode == "fip_standard") {
    # Original candidate: 0.70 RS + 0.30 (FIP - 0.50)
    fip <- pos_days_63[s0, ok]
    fip[is.na(fip)] <- 0.50
    return(0.70 * rs_raw + 0.30 * (fip - 0.50))
  }
  
  if (mode == "fip_rank_blend_25") {
    # 75% RS Rank + 25% FIP Rank (distribution-free)
    fip <- pos_days_63[s0, ok]
    fip[is.na(fip)] <- 0.50
    rk_rs  <- rank(rs_raw, ties.method = "average") / length(ok)
    rk_fip <- rank(fip, ties.method = "average") / length(ok)
    return(0.75 * rk_rs + 0.25 * rk_fip)
  }
  
  if (mode == "fip_rank_blend_40") {
    # 60% RS Rank + 40% FIP Rank
    fip <- pos_days_63[s0, ok]
    fip[is.na(fip)] <- 0.50
    rk_rs  <- rank(rs_raw, ties.method = "average") / length(ok)
    rk_fip <- rank(fip, ties.method = "average") / length(ok)
    return(0.60 * rk_rs + 0.40 * rk_fip)
  }
  
  if (mode == "fip_multi_horizon") {
    # Blends 42-day and 63-day continuous momentum
    fip42 <- pos_days_42[s0, ok]; fip42[is.na(fip42)] <- 0.50
    fip63 <- pos_days_63[s0, ok]; fip63[is.na(fip63)] <- 0.50
    fip_blend <- 0.50 * fip42 + 0.50 * fip63
    return(0.70 * rs_raw + 0.30 * (fip_blend - 0.50))
  }
  
  if (mode == "rs_acceleration") {
    # Momentum Acceleration: r63 vs r126 annualized differential
    # If 3-month RS > 6-month annualized RS, momentum is accelerating
    accel <- r63_mat[s0, ok] * 2 - r126_mat[s0, ok]
    accel[is.na(accel)] <- 0
    return(0.75 * rs_raw + 0.25 * pmax(-0.5, pmin(0.5, accel)))
  }
  
  if (mode == "fip_plus_acceleration") {
    # FIP Continuous Momentum + Momentum Acceleration
    fip <- pos_days_63[s0, ok]; fip[is.na(fip)] <- 0.50
    accel <- r63_mat[s0, ok] * 2 - r126_mat[s0, ok]; accel[is.na(accel)] <- 0
    return(0.60 * rs_raw + 0.25 * (fip - 0.50) + 0.15 * pmax(-0.5, pmin(0.5, accel)))
  }
  
  if (mode == "da_academic_id") {
    # Da et al. (2014) Information Discreteness: Lower ID is better -> score = RS - w * ID
    id_val <- id_63[s0, ok]; id_val[is.na(id_val)] <- 0
    return(rs_raw - 0.30 * id_val)
  }
  
  if (mode == "fip_multi_rank_accel") {
    # Apex Multi-Rank: 60% RS rank + 25% FIP rank + 15% Accel rank
    fip <- pos_days_63[s0, ok]; fip[is.na(fip)] <- 0.50
    accel <- r63_mat[s0, ok] * 2 - r126_mat[s0, ok]; accel[is.na(accel)] <- 0
    rk_rs    <- rank(rs_raw, ties.method = "average") / length(ok)
    rk_fip   <- rank(fip, ties.method = "average") / length(ok)
    rk_accel <- rank(accel, ties.method = "average") / length(ok)
    return(0.60 * rk_rs + 0.25 * rk_fip + 0.15 * rk_accel)
  }
  
  return(rs_raw)
}

sim_fip_candidate <- function(
    alpha_mode = "standard_rs",
    super_bull_leverage = 2.50,
    buffer_rank = 8,
    disaster_stop_pct = 0.25,
    start = S20,
    end = E20
) {
  ev <- which(cal >= start & cal <= end)
  sat_pct <- 1.0
  vt <- 0.22
  
  cw <- 0
  sw <- setNames(rep(0, length(U)), U)
  dw <- 0
  ep <- setNames(rep(NA_real_, length(U)), U)
  
  eq <- 1
  turn_tot <- 0
  out <- numeric(length(ev))
  rebalance_days <- which(month_end[ev])
  
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
    
    # 1. Stop Check (-25% Disaster Floor)
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
        stopped_symbols <- c(stopped_symbols, s)
      }
    }
    
    # 2. Immediate Stop Replacement
    if (length(stopped_symbols) > 0 && isTRUE(benchmark_bull[s0])) {
      ok <- U[MEM[s0, ] & !is.na(P[s0, ]) & !is.na(P[max(1, s0 - 126), ])]
      if (length(ok) >= 10) {
        score <- calc_alpha_score(alpha_mode, s0, ok)
        k2 <- ok[!is.na(VOL63[s0, ok]) & VOL63[s0, ok] <= 1.00]
        if (length(k2) >= 10) { ok <- k2; score <- score[k2] }
        rk <- names(sort(score, decreasing = TRUE))
        unheld <- setdiff(rk, c(names(sw)[sw > 0], stopped_symbols))
        
        act_lev <- if (is_sb) super_bull_leverage else 1.50
        target_w_per_slot <- act_lev / 5
        
        for (rep_sym in head(unheld, length(stopped_symbols))) {
          sw[rep_sym] <- target_w_per_slot
          ep[rep_sym] <- P[t, rep_sym]
          eq <- eq * (1 - COST * target_w_per_slot)
          turn_tot <- turn_tot + target_w_per_slot
        }
      }
    }
    
    # 3. Monthly Rebalance Trigger
    is_reb_day <- (k %in% rebalance_days) || (k == 1)
    if (is_reb_day) {
      cur_vol <- vol20[s0]
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
        if (length(ok) >= 5) {
          score <- calc_alpha_score(alpha_mode, s0, ok)
          k2 <- ok[!is.na(VOL63[s0, ok]) & VOL63[s0, ok] <= 1.00]
          if (length(k2) >= 5) { ok <- k2; score <- score[k2] }
          rk <- names(sort(score, decreasing = TRUE))
          
          # Hysteresis Winner Retention Buffer (Top buffer_rank)
          cur_held <- names(sw)[sw > 0]
          buffer_qual <- intersect(cur_held, rk[1:min(length(rk), buffer_rank)])
          new_picks <- setdiff(rk, buffer_qual)
          slots_needed <- max(0, 5 - length(buffer_qual))
          top <- c(buffer_qual, head(new_picks, slots_needed))[1:5]
          top <- top[!is.na(top)]
          
          ts[top] <- sat_pct * sc * active_margin * 0.20
          
          for (ne in setdiff(top, names(sw)[sw > 0])) {
            ep[ne] <- P[t, ne]
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

modes <- list(
  "0. Baseline (Current Production 2.50x Turbo)"   = "standard_rs",
  "1. FIP Standard (0.70 RS + 0.30 FIP63)"        = "fip_standard",
  "2. FIP Rank Blend (75% RS Rank + 25% FIP Rank)" = "fip_rank_blend_25",
  "3. FIP Rank Blend (60% RS Rank + 40% FIP Rank)" = "fip_rank_blend_40",
  "4. FIP Multi-Horizon (FIP 42d + 63d Blend)"     = "fip_multi_horizon",
  "5. RS Acceleration (RS63 vs RS126 Differential)" = "rs_acceleration",
  "6. FIP + RS Acceleration Combo"                = "fip_plus_acceleration",
  "7. Da et al. Academic Information Discreteness" = "da_academic_id",
  "8. Apex Multi-Rank (60% RS + 25% FIP + 15% Accel)" = "fip_multi_rank_accel"
)

cat("\n========================================================================================\n")
cat("          RUNNING ADVANCED FIP & MOMENTUM ACCELERATION BACKTESTS                         \n")
cat("========================================================================================\n")

res_10yr <- list()
res_20yr <- list()

for (m_name in names(modes)) {
  mode_val <- modes[[m_name]]
  cat(sprintf("Simulating: %s...\n", m_name))
  r_sim <- sim_fip_candidate(alpha_mode = mode_val, super_bull_leverage = 2.50)
  
  st10 <- calc_stats(r_sim, S10, E10, capital = 8000)
  st20 <- calc_stats(r_sim, S20, E20, capital = 8000)
  
  res_10yr[[m_name]] <- st10
  res_20yr[[m_name]] <- st20
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

display_20 <- df_20
display_20$Terminal_8k <- fmt_usd(display_20$Terminal)
display_20$CAGR        <- sprintf("%+.2f%%", display_20$CAGR)
display_20$MaxDD       <- sprintf("-%.2f%%", display_20$MaxDD)
display_20$Vol         <- sprintf("%.1f%%", display_20$Vol)
display_20$Sharpe      <- sprintf("%.2f", display_20$Sharpe)
display_20$Sortino     <- sprintf("%.2f", display_20$Sortino)
display_20$Calmar      <- sprintf("%.2f", display_20$Calmar)

cat("\n========================================================================================================================\n")
cat("                10-YEAR SCORECARD: ADVANCED FIP & MOMENTUM ACCELERATION                                                  \n")
cat("========================================================================================================================\n")
print(display_10[, c("CAGR", "Terminal_8k", "MaxDD", "Vol", "Sharpe", "Sortino", "Calmar")])

cat("\n========================================================================================================================\n")
cat("                20-YEAR SCORECARD: ADVANCED FIP & MOMENTUM ACCELERATION                                                  \n")
cat("========================================================================================================================\n")
print(display_20[, c("CAGR", "Terminal_8k", "MaxDD", "Vol", "Sharpe", "Sortino", "Calmar")])
cat("========================================================================================================================\n\n")

write.csv(df_10, "scratch/scorecard_10yr_fip_optimization.csv")
write.csv(df_20, "scratch/scorecard_20yr_fip_optimization.csv")
cat("Saved scorecards to scratch/scorecard_10yr_fip_optimization.csv and scorecard_20yr_fip_optimization.csv\n")
