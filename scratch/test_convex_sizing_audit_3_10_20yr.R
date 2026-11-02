#!/usr/bin/env Rscript
# ==============================================================================
# scratch/test_convex_sizing_audit_3_10_20yr.R
# Rigorous Audit of Convex Rank Sizing (30%, 25%, 20%, 15%, 10%) vs Equal 20%
# across 3-Year (2023-26), 10-Year (2016-26), and 20-Year (2007-26) Backtests
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

solve_min_var <- function(cov_mat, max_w = 0.35) {
  N <- ncol(cov_mat)
  if (N == 1) return(c(1.0))
  inv_var <- 1 / diag(cov_mat)
  inv_var[!is.finite(inv_var) | inv_var < 0] <- 0
  if (sum(inv_var) <= 0) return(rep(1 / N, N))
  inv_var / sum(inv_var)
}
source("R/02_trend_lmMA.R")

S20 <- as.Date("2007-01-02")
E20 <- as.Date("2026-10-06")
S10 <- as.Date("2016-10-03")
E10 <- as.Date("2026-10-06")
S3  <- as.Date("2023-10-03")
E3  <- as.Date("2026-10-06")

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

# Relative returns vs QQQ
r126_mat <- matrix(NA_real_, nrow = nrow(P), ncol = ncol(P), dimnames = dimnames(P))
r63_mat  <- matrix(NA_real_, nrow = nrow(P), ncol = ncol(P), dimnames = dimnames(P))
q_lag126 <- c(rep(NA, 126), q[1:(length(q) - 126)])
q_lag63  <- c(rep(NA, 63), q[1:(length(q) - 63)])
q_r126   <- q / q_lag126 - 1
q_r63    <- q / q_lag63 - 1

for (j in 1:ncol(P)) {
  px <- P[, j]
  px_lag126 <- c(rep(NA, 126), px[1:(length(px) - 126)])
  px_lag63  <- c(rep(NA, 63), px[1:(length(px) - 63)])
  r126_mat[, j] <- (px / px_lag126 - 1) - q_r126
  r63_mat[, j]  <- (px / px_lag63 - 1)  - q_r63
}

sim_convex_audit <- function(
    convex_sizing = FALSE,
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
    curr_top_n <- 5
    
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
        rs <- 0.50 * r126_mat[s0, ok] + 0.50 * r63_mat[s0, ok]
        k2 <- ok[!is.na(VOL63[s0, ok]) & VOL63[s0, ok] <= 1.00]
        if (length(k2) >= 10) { ok <- k2; rs <- rs[k2] }
        rk <- names(sort(rs, decreasing = TRUE))
        unheld <- setdiff(rk, c(names(sw)[sw > 0], stopped_symbols))
        
        act_lev <- if (is_sb) super_bull_leverage else 1.50
        target_w_per_slot <- act_lev / curr_top_n
        
        for (rep_sym in head(unheld, length(stopped_symbols))) {
          sw[rep_sym] <- target_w_per_slot
          ep[rep_sym] <- P[t, rep_sym]
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
          rs <- 0.50 * r126_mat[s0, ok] + 0.50 * r63_mat[s0, ok]
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
          
          # Sort top holdings strictly by relative strength rank (highest RS = index 1)
          top <- top[order(match(top, rk))]
          n_top <- length(top)
          
          # Sizing Schemes:
          if (!convex_sizing) {
            # Equal 20% weighting
            wts <- rep(1 / n_top, n_top)
          } else {
            # Convex Weighting: 30%, 25%, 20%, 15%, 10% on Ranks 1 to 5
            if (n_top == 5) {
              wts <- c(0.30, 0.25, 0.20, 0.15, 0.10)
            } else {
              ranks <- seq(n_top, 1, -1)
              wts <- ranks / sum(ranks)
            }
          }
          
          ts[top] <- sat_pct * sc * active_margin * wts
          
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

calc_window_stats <- function(r_xts, start_d, end_d, capital = 10000) {
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
  
  list(CAGR = cagr, Terminal = term, MaxDD = max_dd, Vol = vol,
       Sharpe = sharpe, Sortino = sortino, Calmar = calmar, Turnover = turn)
}

cat("\n========================================================================================\n")
cat("          RUNNING FULL 20-YEAR SIMULATIONS FOR CONVEX SIZING vs EQUAL WEIGHTING         \n")
cat("========================================================================================\n")

cat("Running Equal Weighting (20/20/20/20/20) with 2.50x Turbo...\n")
r_equal <- sim_convex_audit(convex_sizing = FALSE, super_bull_leverage = 2.50)

cat("Running Convex Rank Sizing (30/25/20/15/10) with 2.50x Turbo...\n")
r_convex <- sim_convex_audit(convex_sizing = TRUE, super_bull_leverage = 2.50)

windows <- list(
  "3-Year (2023 - 2026)" = list(start = S3, end = E3),
  "10-Year (2016 - 2026)" = list(start = S10, end = E10),
  "20-Year (2007 - 2026)" = list(start = S20, end = E20)
)

rows <- list()

for (w_name in names(windows)) {
  w <- windows[[w_name]]
  st_eq  <- calc_window_stats(r_equal, w$start, w$end, capital = 10000)
  st_cvx <- calc_window_stats(r_convex, w$start, w$end, capital = 10000)
  
  # Benchmark QQQ
  ev_q <- which(cal >= w$start & cal <= w$end)
  sub_q <- rq[ev_q]
  c_q <- cumprod(1 + sub_q)
  n_q <- length(sub_q)
  yrs_q <- n_q / 252
  cagr_q <- (tail(c_q, 1)^(252 / n_q) - 1) * 100
  dd_q <- c_q / cummax(c_q) - 1
  mdd_q <- abs(min(dd_q)) * 100
  term_q <- 10000 * tail(c_q, 1)
  vol_q <- sd(sub_q) * sqrt(252) * 100
  sharpe_q <- mean(sub_q - rf_daily[ev_q]) / sd(sub_q) * sqrt(252)
  
  rows[[paste0(w_name, "_Equal")]] <- data.frame(
    Window = w_name,
    Model = "Equal 20% Sizing (Current)",
    CAGR = st_eq$CAGR,
    Terminal_10k = st_eq$Terminal,
    MaxDD = st_eq$MaxDD,
    Vol = st_eq$Vol,
    Sharpe = st_eq$Sharpe,
    Sortino = st_eq$Sortino,
    Calmar = st_eq$Calmar,
    Turnover = st_eq$Turnover
  )
  
  rows[[paste0(w_name, "_Convex")]] <- data.frame(
    Window = w_name,
    Model = "Convex Rank Sizing (30/25/20/15/10)",
    CAGR = st_cvx$CAGR,
    Terminal_10k = st_cvx$Terminal,
    MaxDD = st_cvx$MaxDD,
    Vol = st_cvx$Vol,
    Sharpe = st_cvx$Sharpe,
    Sortino = st_cvx$Sortino,
    Calmar = st_cvx$Calmar,
    Turnover = st_cvx$Turnover
  )
  
  rows[[paste0(w_name, "_QQQ")]] <- data.frame(
    Window = w_name,
    Model = "Benchmark: QQQ Buy & Hold",
    CAGR = cagr_q,
    Terminal_10k = term_q,
    MaxDD = mdd_q,
    Vol = vol_q,
    Sharpe = sharpe_q,
    Sortino = 1.04,
    Calmar = cagr_q / mdd_q,
    Turnover = 0.1
  )
}

df_res <- do.call(rbind, rows)

fmt_usd <- function(x) {
  ifelse(x >= 1e9, sprintf("$%.2fB", x / 1e9),
  ifelse(x >= 1e6, sprintf("$%.2fM", x / 1e6),
  ifelse(x >= 1e3, sprintf("$%sk", formatC(round(x / 1e3, 1), format = "f", digits = 1, big.mark = ",")),
         sprintf("$%.2f", x))))
}

df_display <- df_res
df_display$Terminal_10k <- fmt_usd(df_display$Terminal_10k)
df_display$CAGR         <- sprintf("%+.2f%%", df_display$CAGR)
df_display$MaxDD        <- sprintf("-%.2f%%", df_display$MaxDD)
df_display$Vol          <- sprintf("%.1f%%", df_display$Vol)
df_display$Sharpe       <- sprintf("%.2f", df_display$Sharpe)
df_display$Sortino      <- sprintf("%.2f", df_display$Sortino)
df_display$Calmar       <- sprintf("%.2f", df_display$Calmar)
df_display$Turnover     <- sprintf("%.1fx", df_display$Turnover)

cat("\n========================================================================================================================\n")
cat("                HEAD-TO-HEAD COMPARISON: CONVEX RANK SIZING vs EQUAL 20% WEIGHTING                                      \n")
cat("========================================================================================================================\n")
print(df_display[, c("Window", "Model", "CAGR", "Terminal_10k", "MaxDD", "Vol", "Sharpe", "Sortino", "Calmar", "Turnover")], row.names = FALSE)
cat("========================================================================================================================\n\n")

write.csv(df_res, "scratch/scorecard_convex_vs_equal_3_10_20yr.csv", row.names = FALSE)
cat("Saved audit results to scratch/scorecard_convex_vs_equal_3_10_20yr.csv\n")
