#!/usr/bin/env Rscript
# ==============================================================================
# scratch/test_fip_tax_audit_3_10_20yr.R
# Comprehensive Tax Audit (3, 10 & 20-Year) Comparing:
#   Baseline (Standard RS) vs. Candidate Winner (FIP Continuous Momentum)
# Across All 6 Tax Regimes with $10,000 Starting Capital
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

r126_mat <- matrix(NA_real_, nrow = nrow(P), ncol = ncol(P), dimnames = dimnames(P))
r63_mat  <- matrix(NA_real_, nrow = nrow(P), ncol = ncol(P), dimnames = dimnames(P))
q_lag126 <- c(rep(NA, 126), q[1:(length(q) - 126)])
q_lag63  <- c(rep(NA, 63), q[1:(length(q) - 63)])
q_r126   <- q / q_lag126 - 1
q_r63    <- q / q_lag63 - 1

pos_days_63 <- matrix(NA_real_, nrow = nrow(P), ncol = ncol(P), dimnames = dimnames(P))

for (j in 1:ncol(P)) {
  px <- P[, j]
  px_lag126 <- c(rep(NA, 126), px[1:(length(px) - 126)])
  px_lag63  <- c(rep(NA, 63), px[1:(length(px) - 63)])
  r126_mat[, j] <- (px / px_lag126 - 1) - q_r126
  r63_mat[, j]  <- (px / px_lag63 - 1)  - q_r63
  
  ret_j <- R[, j]
  pos_days_63[, j] <- as.numeric(zoo::rollapply(ret_j > 0, 63, mean, align = "right", fill = NA))
}

solve_min_var <- function(cov_mat) {
  N <- ncol(cov_mat)
  if (N == 1) return(c(1.0))
  inv_var <- 1 / diag(cov_mat)
  inv_var[!is.finite(inv_var) | inv_var < 0] <- 0
  if (sum(inv_var) <= 0) return(rep(1 / N, N))
  inv_var / sum(inv_var)
}

sim_strategy <- function(use_fip = FALSE) {
  ev <- which(cal >= S20 & cal <= E20)
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
    
    if (length(stopped_symbols) > 0 && isTRUE(benchmark_bull[s0])) {
      ok <- U[MEM[s0, ] & !is.na(P[s0, ]) & !is.na(P[max(1, s0 - 126), ])]
      if (length(ok) >= 10) {
        rs_raw <- 0.50 * r126_mat[s0, ok] + 0.50 * r63_mat[s0, ok]
        score <- if (use_fip) {
          fip <- pos_days_63[s0, ok]; fip[is.na(fip)] <- 0.50
          0.70 * rs_raw + 0.30 * (fip - 0.50)
        } else {
          rs_raw
        }
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
          rs_raw <- 0.50 * r126_mat[s0, ok] + 0.50 * r63_mat[s0, ok]
          score <- if (use_fip) {
            fip <- pos_days_63[s0, ok]; fip[is.na(fip)] <- 0.50
            0.70 * rs_raw + 0.30 * (fip - 0.50)
          } else {
            rs_raw
          }
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
  
  res <- xts(out, order.by = cal[ev])
  attr(res, "annual_turnover") <- turn_tot / (length(ev) / 252)
  res
}

cat("Simulating Baseline and FIP engines...\n")
r_base <- sim_strategy(use_fip = FALSE)
turn_base <- attr(r_base, "annual_turnover")

r_fip <- sim_strategy(use_fip = TRUE)
turn_fip <- attr(r_fip, "annual_turnover")

cat(sprintf("Baseline Annual Turnover: %.2fx\n", turn_base))
cat(sprintf("FIP Annual Turnover:      %.2fx\n", turn_fip))

# Tax Simulator Engine
sim_after_tax <- function(r_xts, start_d, end_d, regime, capital = 10000) {
  sub_r <- r_xts[paste0(start_d, "/", end_d)]
  dates <- index(sub_r)
  years <- as.numeric(format(dates, "%Y"))
  uyears <- unique(years)
  
  eq <- capital
  daily_eq <- numeric(length(sub_r))
  idx <- 1
  
  for (yr in uyears) {
    yr_mask <- (years == yr)
    yr_rets <- as.numeric(sub_r[yr_mask])
    
    start_yr_eq <- eq
    yr_cum <- 1.0
    for (r_d in yr_rets) {
      yr_cum <- yr_cum * (1 + r_d)
      daily_eq[idx] <- start_yr_eq * yr_cum
      idx <- idx + 1
    }
    
    pre_tax_gain <- start_yr_eq * (yr_cum - 1)
    
    if (pre_tax_gain > 0) {
      tax_rate <- if (regime == "tts") 0.37 else
                  if (regime == "nyc") (0.85 * 0.55576 + 0.15 * 0.38376) else
                  if (regime == "nys") (0.85 * 0.51700 + 0.15 * 0.34500) else
                  0.0 # ira
      tax_owed <- pre_tax_gain * tax_rate
      eq <- (start_yr_eq * yr_cum) - tax_owed
    } else {
      eq <- start_yr_eq * yr_cum
    }
  }
  
  n_days <- length(sub_r)
  yrs_n <- n_days / 252
  cagr <- (eq / capital)^(1 / yrs_n) - 1
  
  c_eq <- daily_eq / capital
  peak <- cummax(c_eq)
  dd <- (c_eq - peak) / peak
  max_dd <- abs(min(dd))
  
  list(Terminal = eq, CAGR = cagr * 100, MaxDD = max_dd * 100)
}

sim_qqq_tax <- function(start_d, end_d, regime, capital = 10000) {
  ev_q <- which(cal >= start_d & cal <= end_d)
  q_sub <- q[ev_q]
  dates_q <- cal[ev_q]
  r_q_sub <- rq[ev_q]
  
  gross_gain <- (tail(q_sub, 1) / q_sub[1]) - 1
  yrs_n <- length(q_sub) / 252
  
  tax_rate <- if (regime == "nyc") 0.38376 else 0.34500
  div_yield <- 0.006
  eff_tax_rate <- tax_rate + (div_yield * yrs_n * 0.38376) / max(0.01, gross_gain)
  
  term_pre <- capital * (1 + gross_gain)
  term_post <- capital + (term_pre - capital) * (1 - min(tax_rate, eff_tax_rate))
  cagr_post <- (term_post / capital)^(1 / yrs_n) - 1
  
  c_r <- cumprod(1 + r_q_sub)
  dd <- c_r / cummax(c_r) - 1
  max_dd <- abs(min(dd)) * 100
  
  list(Terminal = term_post, CAGR = cagr_post * 100, MaxDD = max_dd)
}

horizons <- list(
  "3-Year (2023-2026)"   = list(S = S3, E = E3),
  "10-Year (2016-2026)"  = list(S = S10, E = E10),
  "20-Year (2007-2026)"  = list(S = S20, E = E20)
)

scorecard_rows <- list()

for (h_name in names(horizons)) {
  H <- horizons[[h_name]]
  
  # Baseline
  b_tts <- sim_after_tax(r_base, H$S, H$E, "tts")
  b_nyc <- sim_after_tax(r_base, H$S, H$E, "nyc")
  b_nys <- sim_after_tax(r_base, H$S, H$E, "nys")
  b_ira <- sim_after_tax(r_base, H$S, H$E, "ira")
  
  # FIP Winner
  f_tts <- sim_after_tax(r_fip, H$S, H$E, "tts")
  f_nyc <- sim_after_tax(r_fip, H$S, H$E, "nyc")
  f_nys <- sim_after_tax(r_fip, H$S, H$E, "nys")
  f_ira <- sim_after_tax(r_fip, H$S, H$E, "ira")
  
  # QQQ
  q_nyc <- sim_qqq_tax(H$S, H$E, "nyc")
  q_nys <- sim_qqq_tax(H$S, H$E, "nys")
  
  scorecard_rows[[paste(h_name, "Baseline - TTS Sec. 475(f)")]] <- c(Horizon = h_name, Strategy = "Baseline", Regime = "TTS Sec. 475(f)", CAGR = b_tts$CAGR, Terminal = b_tts$Terminal, MaxDD = b_tts$MaxDD)
  scorecard_rows[[paste(h_name, "FIP Winner - TTS Sec. 475(f)")]] <- c(Horizon = h_name, Strategy = "FIP Winner", Regime = "TTS Sec. 475(f)", CAGR = f_tts$CAGR, Terminal = f_tts$Terminal, MaxDD = f_tts$MaxDD)
  
  scorecard_rows[[paste(h_name, "Baseline - Taxable NYC")]]     <- c(Horizon = h_name, Strategy = "Baseline", Regime = "Taxable NYC", CAGR = b_nyc$CAGR, Terminal = b_nyc$Terminal, MaxDD = b_nyc$MaxDD)
  scorecard_rows[[paste(h_name, "FIP Winner - Taxable NYC")]]     <- c(Horizon = h_name, Strategy = "FIP Winner", Regime = "Taxable NYC", CAGR = f_nyc$CAGR, Terminal = f_nyc$Terminal, MaxDD = f_nyc$MaxDD)
  
  scorecard_rows[[paste(h_name, "Baseline - Taxable NYS")]]     <- c(Horizon = h_name, Strategy = "Baseline", Regime = "Taxable NYS", CAGR = b_nys$CAGR, Terminal = b_nys$Terminal, MaxDD = b_nys$MaxDD)
  scorecard_rows[[paste(h_name, "FIP Winner - Taxable NYS")]]     <- c(Horizon = h_name, Strategy = "FIP Winner", Regime = "Taxable NYS", CAGR = f_nys$CAGR, Terminal = f_nys$Terminal, MaxDD = f_nys$MaxDD)
  
  scorecard_rows[[paste(h_name, "Baseline - Tax-Advantaged")]]  <- c(Horizon = h_name, Strategy = "Baseline", Regime = "Tax-Advantaged (IRA)", CAGR = b_ira$CAGR, Terminal = b_ira$Terminal, MaxDD = b_ira$MaxDD)
  scorecard_rows[[paste(h_name, "FIP Winner - Tax-Advantaged")]]  <- c(Horizon = h_name, Strategy = "FIP Winner", Regime = "Tax-Advantaged (IRA)", CAGR = f_ira$CAGR, Terminal = f_ira$Terminal, MaxDD = f_ira$MaxDD)
  
  scorecard_rows[[paste(h_name, "Benchmark QQQ (NYC)")]]        <- c(Horizon = h_name, Strategy = "QQQ Buy & Hold", Regime = "QQQ NYC Taxable", CAGR = q_nyc$CAGR, Terminal = q_nyc$Terminal, MaxDD = q_nyc$MaxDD)
  scorecard_rows[[paste(h_name, "Benchmark QQQ (NYS)")]]        <- c(Horizon = h_name, Strategy = "QQQ Buy & Hold", Regime = "QQQ NYS Taxable", CAGR = q_nys$CAGR, Terminal = q_nys$Terminal, MaxDD = q_nys$MaxDD)
}

df_tax <- as.data.frame(do.call(rbind, scorecard_rows), stringsAsFactors = FALSE)
df_tax$CAGR <- as.numeric(df_tax$CAGR)
df_tax$Terminal <- as.numeric(df_tax$Terminal)
df_tax$MaxDD <- as.numeric(df_tax$MaxDD)

fmt_usd <- function(x) {
  ifelse(x >= 1e9, sprintf("$%.2fB", x / 1e9),
  ifelse(x >= 1e6, sprintf("$%.2fM", x / 1e6),
  ifelse(x >= 1e3, sprintf("$%sk", formatC(round(x / 1e3, 1), format = "f", digits = 1, big.mark = ",")),
         sprintf("$%.2f", x))))
}

df_tax$Terminal_Fmt <- fmt_usd(df_tax$Terminal)
df_tax$CAGR_Fmt     <- sprintf("%+.2f%%", df_tax$CAGR)
df_tax$MaxDD_Fmt    <- sprintf("-%.2f%%", df_tax$MaxDD)

cat("\n========================================================================================================================\n")
cat("                HEAD-TO-HEAD TAX AUDIT: BASELINE vs FIP CONTINUOUS MOMENTUM ($10,000 START)                            \n")
cat("========================================================================================================================\n")
print(df_tax[, c("Horizon", "Strategy", "Regime", "CAGR_Fmt", "Terminal_Fmt", "MaxDD_Fmt")])
cat("========================================================================================================================\n\n")

write.csv(df_tax, "scratch/scorecard_fip_vs_baseline_tax_audit.csv")
cat("Saved to scratch/scorecard_fip_vs_baseline_tax_audit.csv\n")
