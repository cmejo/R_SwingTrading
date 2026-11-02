#!/usr/bin/env Rscript
# ==============================================================================
# scratch/test_fip_fine_tuning.R
# Fine-Tuning Continuous Momentum (FIP) & Alpha Additive Multipliers
# STRICT CONSTRAINT: ZERO EXTRA LEVERAGE (2.50x super-bull / 1.50x bull / 1.00x bear)
# ==============================================================================

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
})

src <- readLines("scratch/compare_symbol_lists.R")
idx <- grep("^lists <- c", src)
eval(parse(text = paste(src[1:(idx - 1)], collapse = "\n")))
set_universe(read_list("symbols_broad.txt"))

source("R/02_trend_lmMA.R")

S20 <- as.Date("2007-01-02")
E20 <- as.Date("2026-10-06")
S10 <- as.Date("2016-10-03")
E10 <- as.Date("2026-10-06")
S3  <- as.Date("2023-10-06")
E3  <- as.Date("2026-10-06")

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
r252_mat <- matrix(NA_real_, nrow = nrow(P), ncol = ncol(P), dimnames = dimnames(P))
r126_mat <- matrix(NA_real_, nrow = nrow(P), ncol = ncol(P), dimnames = dimnames(P))
r63_mat  <- matrix(NA_real_, nrow = nrow(P), ncol = ncol(P), dimnames = dimnames(P))
q_lag252 <- c(rep(NA, 252), q[1:(length(q) - 252)])
q_lag126 <- c(rep(NA, 126), q[1:(length(q) - 126)])
q_lag63  <- c(rep(NA, 63), q[1:(length(q) - 63)])
q_r252   <- q / q_lag252 - 1
q_r126   <- q / q_lag126 - 1
q_r63    <- q / q_lag63 - 1

pos_days_63 <- matrix(NA_real_, nrow = nrow(P), ncol = ncol(P), dimnames = dimnames(P))
pos_days_126 <- matrix(NA_real_, nrow = nrow(P), ncol = ncol(P), dimnames = dimnames(P))

for (j in 1:ncol(P)) {
  px <- P[, j]
  px_lag252 <- c(rep(NA, 252), px[1:(length(px) - 252)])
  px_lag126 <- c(rep(NA, 126), px[1:(length(px) - 126)])
  px_lag63  <- c(rep(NA, 63), px[1:(length(px) - 63)])
  
  r252_mat[, j] <- (px / px_lag252 - 1) - q_r252
  r126_mat[, j] <- (px / px_lag126 - 1) - q_r126
  r63_mat[, j]  <- (px / px_lag63 - 1)  - q_r63
  
  ret_j <- R[, j]
  pos_days_63[, j]  <- as.numeric(zoo::rollapply(ret_j > 0, 63, mean, align = "right", fill = NA))
  pos_days_126[, j] <- as.numeric(zoo::rollapply(ret_j > 0, 126, mean, align = "right", fill = NA))
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

sim_engine <- function(
    alpha_fn,
    start = S20,
    end = E20
) {
  ev <- which(cal >= start & cal <= end)
  sat_pct <- 1.0
  vt <- 0.22
  super_bull_leverage <- 2.50
  buffer_rank <- 8
  disaster_stop_pct <- 0.25
  
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
    
    # 1. Stop Check
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
        score <- alpha_fn(s0, ok)
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
          score <- alpha_fn(s0, ok)
          k2 <- ok[!is.na(VOL63[s0, ok]) & VOL63[s0, ok] <= 1.00]
          if (length(k2) >= 5) { ok <- k2; score <- score[k2] }
          rk <- names(sort(score, decreasing = TRUE))
          
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

test_configs <- list(
  "0. Baseline (Standard RS: 0.50*r126 + 0.50*r63)" = function(s0, ok) {
    0.50 * r126_mat[s0, ok] + 0.50 * r63_mat[s0, ok]
  },
  "1. FIP Standard (0.70 RS + 0.30 FIP63)" = function(s0, ok) {
    rs <- 0.50 * r126_mat[s0, ok] + 0.50 * r63_mat[s0, ok]
    fip <- pos_days_63[s0, ok]; fip[is.na(fip)] <- 0.50
    0.70 * rs + 0.30 * (fip - 0.50)
  },
  "2. FIP Additive +0.25 (RS + 0.25*(FIP-0.50))" = function(s0, ok) {
    rs <- 0.50 * r126_mat[s0, ok] + 0.50 * r63_mat[s0, ok]
    fip <- pos_days_63[s0, ok]; fip[is.na(fip)] <- 0.50
    rs + 0.25 * (fip - 0.50)
  },
  "3. FIP Additive +0.50 (RS + 0.50*(FIP-0.50))" = function(s0, ok) {
    rs <- 0.50 * r126_mat[s0, ok] + 0.50 * r63_mat[s0, ok]
    fip <- pos_days_63[s0, ok]; fip[is.na(fip)] <- 0.50
    rs + 0.50 * (fip - 0.50)
  },
  "4. FIP Additive +1.00 (RS + 1.00*(FIP-0.50))" = function(s0, ok) {
    rs <- 0.50 * r126_mat[s0, ok] + 0.50 * r63_mat[s0, ok]
    fip <- pos_days_63[s0, ok]; fip[is.na(fip)] <- 0.50
    rs + 1.00 * (fip - 0.50)
  },
  "5. FIP Multiplicative (RS * (FIP / 0.50))" = function(s0, ok) {
    rs <- 0.50 * r126_mat[s0, ok] + 0.50 * r63_mat[s0, ok]
    fip <- pos_days_63[s0, ok]; fip[is.na(fip)] <- 0.50
    rs * (fip / 0.50)
  },
  "6. Triple Horizon RS (0.40*r252 + 0.35*r126 + 0.25*r63) + FIP" = function(s0, ok) {
    rs <- 0.40 * r252_mat[s0, ok] + 0.35 * r126_mat[s0, ok] + 0.25 * r63_mat[s0, ok]
    fip <- pos_days_63[s0, ok]; fip[is.na(fip)] <- 0.50
    0.70 * rs + 0.30 * (fip - 0.50)
  },
  "7. Dual Horizon FIP (0.70*RS + 0.15*FIP63 + 0.15*FIP126)" = function(s0, ok) {
    rs <- 0.50 * r126_mat[s0, ok] + 0.50 * r63_mat[s0, ok]
    f63 <- pos_days_63[s0, ok]; f63[is.na(f63)] <- 0.50
    f126 <- pos_days_126[s0, ok]; f126[is.na(f126)] <- 0.50
    0.70 * rs + 0.15 * (f63 - 0.50) + 0.15 * (f126 - 0.50)
  }
)

cat("\n========================================================================================\n")
cat("          RUNNING FIP FINE-TUNING BACKTESTS (3-YR, 10-YR & 20-YR)                        \n")
cat("========================================================================================\n")

res_3yr  <- list()
res_10yr <- list()
res_20yr <- list()

for (c_name in names(test_configs)) {
  fn <- test_configs[[c_name]]
  cat(sprintf("Simulating: %s...\n", c_name))
  r_sim <- sim_engine(alpha_fn = fn)
  
  res_3yr[[c_name]]  <- calc_stats(r_sim, S3, E3, capital = 8000)
  res_10yr[[c_name]] <- calc_stats(r_sim, S10, E10, capital = 8000)
  res_20yr[[c_name]] <- calc_stats(r_sim, S20, E20, capital = 8000)
}

df_3  <- as.data.frame(do.call(rbind, res_3yr))
df_10 <- as.data.frame(do.call(rbind, res_10yr))
df_20 <- as.data.frame(do.call(rbind, res_20yr))

fmt_usd <- function(x) {
  ifelse(x >= 1e9, sprintf("$%.2fB", x / 1e9),
  ifelse(x >= 1e6, sprintf("$%.2fM", x / 1e6),
  ifelse(x >= 1e3, sprintf("$%sk", formatC(round(x / 1e3, 1), format = "f", digits = 1, big.mark = ",")),
         sprintf("$%.2f", x))))
}

format_scorecard <- function(df) {
  d <- df
  d$Terminal_8k <- fmt_usd(d$Terminal)
  d$CAGR        <- sprintf("%+.2f%%", d$CAGR)
  d$MaxDD       <- sprintf("-%.2f%%", d$MaxDD)
  d$Vol         <- sprintf("%.1f%%", d$Vol)
  d$Sharpe      <- sprintf("%.2f", d$Sharpe)
  d$Sortino     <- sprintf("%.2f", d$Sortino)
  d$Calmar      <- sprintf("%.2f", d$Calmar)
  d[, c("CAGR", "Terminal_8k", "MaxDD", "Vol", "Sharpe", "Sortino", "Calmar")]
}

cat("\n========================================================================================================================\n")
cat("                3-YEAR SCORECARD (2023 - 2026): FIP FINE-TUNING                                                          \n")
cat("========================================================================================================================\n")
print(format_scorecard(df_3))

cat("\n========================================================================================================================\n")
cat("                10-YEAR SCORECARD (2016 - 2026): FIP FINE-TUNING                                                         \n")
cat("========================================================================================================================\n")
print(format_scorecard(df_10))

cat("\n========================================================================================================================\n")
cat("                20-YEAR SCORECARD (2007 - 2026): FIP FINE-TUNING                                                         \n")
cat("========================================================================================================================\n")
print(format_scorecard(df_20))
cat("========================================================================================================================\n\n")

write.csv(df_3, "scratch/scorecard_3yr_fip_finetuning.csv")
write.csv(df_10, "scratch/scorecard_10yr_fip_finetuning.csv")
write.csv(df_20, "scratch/scorecard_20yr_fip_finetuning.csv")
cat("Saved scorecards to scratch/scorecard_*_fip_finetuning.csv\n")
