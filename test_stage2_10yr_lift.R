#!/usr/bin/env Rscript
# ==============================================================================
# Test 10-Year Portfolio Lift of Stage-2 Features (CMF_20D, RS_60, EfficiencyRatio_20D)
# ==============================================================================

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(TTR)
  library(parallel)
  library(glmnet)
  library(jsonlite)
})

source("R/02_trend_lmMA.R")

DATA_DIR_10Y   <- "data/data_10yr"
CACHE_DIR_LAT  <- "data/cache_10yr_latest"
CACHE_DIR_S2   <- "data/cache_10yr_stage2"
dir.create(CACHE_DIR_S2, showWarnings = FALSE, recursive = TRUE)

spy_ohlcv <- readRDS(file.path(DATA_DIR_10Y, "data_spy.rds"))

lat_files <- list.files(CACHE_DIR_LAT, pattern = "_10yr_lat_wf[.]rds$", full.names = TRUE)
lat_syms <- character(length(lat_files))
for (i in seq_along(lat_files)) {
  obj <- readRDS(lat_files[i])
  lat_syms[i] <- obj$symbol
}
lat_syms <- setdiff(lat_syms, "UNH")

process_stage2_sym <- function(s) {
  cf_s2  <- file.path(CACHE_DIR_S2, sprintf("%s_10yr_s2_wf.rds", tolower(gsub("[^A-Za-z0-9]", "_", s))))
  cf_lat <- file.path(CACHE_DIR_LAT, sprintf("%s_10yr_lat_wf.rds", tolower(gsub("[^A-Za-z0-9]", "_", s))))
  df_file<- file.path(DATA_DIR_10Y, sprintf("data_%s.rds", tolower(gsub("[^A-Za-z0-9]", "_", s))))
  if (!file.exists(cf_lat) || !file.exists(df_file)) return(NULL)

  l_obj <- readRDS(cf_lat)
  df    <- readRDS(df_file)
  c_idx <- l_obj$dates
  df_sub <- df[c_idx]
  spy_sub<- spy_ohlcv[c_idx]

  cl <- as.numeric(Cl(df_sub))
  hi <- as.numeric(Hi(df_sub))
  lo <- as.numeric(Lo(df_sub))
  op <- as.numeric(Op(df_sub))
  vo <- tryCatch(as.numeric(Vo(df_sub)), error = function(e) rep(1e6, length(cl)))
  vo[is.na(vo) | vo <= 0] <- 1e6
  spy_cl <- as.numeric(Cl(spy_sub))

  # 1. CMF_20D (20-day Chaikin Money Flow: negative values in uptrends mark institutional pullback washouts)
  hl_range <- pmax(hi - lo, 1e-4)
  clv_1d   <- ((cl - lo) - (hi - cl)) / hl_range
  clv_1d[is.na(clv_1d)] <- 0
  cmf_20   <- as.numeric(TTR::runSum(clv_1d * vo, n = 20)) / (as.numeric(TTR::runSum(vo, n = 20)) + 1e-4)
  cmf_20[is.na(cmf_20)] <- 0
  cmf_20   <- pmin(pmax(cmf_20, -1.0), 1.0)

  # 2. EfficiencyRatio_20D (Kaufman 20-Day Efficiency Ratio)
  abs_diff_1d <- c(0, abs(diff(cl)))
  net_move_20 <- abs(cl - as.numeric(lag.xts(Cl(df_sub), k = 20)))
  path_len_20 <- as.numeric(TTR::runSum(abs_diff_1d, n = 20))
  er_20       <- net_move_20 / (path_len_20 + 1e-4)
  er_20[is.na(er_20)] <- 0.30
  er_20       <- pmin(pmax(er_20, 0.0), 1.0)

  # 3. RS_60 (60-Day Quarterly Structural Relative Strength vs SPY)
  ret_60_stk <- (cl / as.numeric(lag.xts(Cl(df_sub), k = 60))) - 1
  ret_60_spy <- (spy_cl / as.numeric(lag.xts(Cl(spy_sub), k = 60))) - 1
  rs_60      <- ret_60_stk - ret_60_spy
  rs_60[is.na(rs_60)] <- 0.0
  rs_60      <- pmin(pmax(rs_60, -1.0), 1.5)

  # 4. ATR_Compression_5v20 (5-Day TR / 20-Day TR Volatility Compression)
  tr_v     <- as.numeric(TTR::ATR(HLC(df_sub), n = 14)$tr)
  tr_v[is.na(tr_v) | tr_v <= 0] <- cl[is.na(tr_v) | tr_v <= 0] * 0.02
  atr_comp <- as.numeric(TTR::runMean(tr_v, n = 5)) / (as.numeric(TTR::runMean(tr_v, n = 20)) + 1e-5)
  atr_comp[is.na(atr_comp)] <- 1.0
  atr_comp <- pmin(pmax(atr_comp, 0.3), 2.5)

  # Stage-2 probability refinement:
  # - Reward high path efficiency (EfficiencyRatio_20D > 0.35)
  # - Reward constructive pullback washout inside uptrends (CMF_20D < 0 when RS_20 > 0 & RS_60 > 0)
  # - Reward positive 60-day structural RS (RS_60 > 0) and tight 5d/20d ATR compression (< 0.90)
  p_lat <- as.numeric(l_obj$pred_prob_boost)
  rs_20 <- as.numeric(l_obj$rs_spy)
  tq_v  <- as.numeric(l_obj$trend_quality)

  s2_tilt <- (
    0.012 * pmax(0, er_20 - 0.30) +
    0.010 * pmin(pmax(rs_60, -0.20), 0.50) -
    0.012 * cmf_20 +
    0.006 * pmax(0, 1.0 - atr_comp)
  )
  p_s2 <- pmin(0.999, pmax(0.001, p_lat + s2_tilt))

  # Re-simulate bracket with EfficiencyRatio_20D + RS_60 runner confirmation
  atr <- as.numeric(TTR::ATR(HLC(df_sub), n = 14)$atr)
  bad_a <- is.na(atr) | atr <= 0
  atr[bad_a] <- cl[bad_a] * 0.02
  nb <- length(c_idx)

  ret_out <- numeric(nb)
  in_pos <- FALSE
  entry_p <- 0; stop_p <- 0; t1_p <- 0; t2_p <- 0; high_p <- 0; days_h <- 0; risk_1r <- 0
  t1_hit <- FALSE; t2_hit <- FALSE; pos_shares_pct <- 0

  for (d in 2:nb) {
    cur_o <- op[d]; cur_h <- hi[d]; cur_l <- lo[d]; cur_c <- cl[d]; prev_c <- cl[d - 1]; cur_atr <- atr[d]
    if (!in_pos && !is.na(p_s2[d - 1]) && p_s2[d - 1] >= 0.58 && !is.na(rs_20[d - 1]) && rs_20[d - 1] > 0) {
      in_pos <- TRUE
      entry_p <- ifelse(!is.na(cur_o) && cur_o > 0, cur_o, cur_c)
      high_p <- entry_p; days_h <- 0; t1_hit <- FALSE; t2_hit <- FALSE; pos_shares_pct <- 1.0
      risk_1r <- 2.0 * cur_atr
      stop_p <- entry_p - risk_1r; t1_p <- entry_p + 1.5 * risk_1r; t2_p <- entry_p + 3.0 * risk_1r
      ret_out[d] <- (cur_c - entry_p) / entry_p - (10 / 10000)
      next
    }
    if (in_pos) {
      days_h <- days_h + 1
      high_p <- max(high_p, cur_h)
      chand_stop <- high_p - 2.5 * cur_atr
      if (cur_l <= stop_p) {
        exit_p <- min(cur_o, stop_p)
        ret_out[d] <- pos_shares_pct * ((exit_p - prev_c) / prev_c) - (10 / 10000)
        in_pos <- FALSE; pos_shares_pct <- 0; next
      }
      if (!t1_hit && cur_h >= t1_p) {
        t1_hit <- TRUE; pos_shares_pct <- 0.50; stop_p <- max(entry_p, chand_stop)
      }
      if (t1_hit && !t2_hit && cur_h >= t2_p) {
        t2_hit <- TRUE; pos_shares_pct <- 0.25; stop_p <- max(stop_p, chand_stop)
      }
      if (t1_hit) {
        stop_p <- max(stop_p, chand_stop)
        if (high_p >= (entry_p + 2.2 * risk_1r)) stop_p <- max(stop_p, entry_p + 0.85 * risk_1r)
      }
      cur_tq <- ifelse(!is.na(tq_v[d]), tq_v[d], 0.50)
      cur_er <- ifelse(!is.na(er_20[d]), er_20[d], 0.30)
      cur_rs <- ifelse(!is.na(rs_20[d]), rs_20[d], 0.0)
      is_adaptive_runner <- t1_hit && ((cur_tq >= 0.65) || (cur_er >= 0.45)) && (cur_rs >= 0.05) && (cur_c >= entry_p * 1.02)
      eff_max_hold <- if (is_adaptive_runner) 12L else 7L
      if (!t2_hit && days_h >= eff_max_hold) {
        ret_out[d] <- pos_shares_pct * ((cur_c - prev_c) / prev_c) - (10 / 10000)
        in_pos <- FALSE; pos_shares_pct <- 0; next
      }
      ret_out[d] <- pos_shares_pct * ((cur_c - prev_c) / prev_c)
    }
  }

  l_obj$pred_prob_boost <- xts(p_s2, order.by = c_idx)
  l_obj$bracket_ratchet <- xts(ret_out, order.by = c_idx)
  l_obj$rs_60           <- xts(rs_60, order.by = c_idx)
  l_obj$eff_ratio       <- xts(er_20, order.by = c_idx)
  l_obj$cmf_20          <- xts(cmf_20, order.by = c_idx)
  saveRDS(l_obj, cf_s2)
  return(s)
}

invisible(mclapply(lat_syms, process_stage2_sym, mc.cores = min(8, detectCores())))

# Evaluate 10-Year Portfolio Lift using compare_prior_vs_latest.R framework
source("compare_prior_vs_latest.R")

b6_stage2 <- build_matrix_bundle(CACHE_DIR_S2, "_10yr_s2_wf[.]rds$", exclude_syms = c("UNH"), use_boost_prob = TRUE, use_ratchet_ret = TRUE)
# Add rs_60 and eff_ratio to b6_stage2
c_files_s2 <- list.files(CACHE_DIR_S2, pattern = "_10yr_s2_wf[.]rds$", full.names = TRUE)
s2_map <- list()
for (cf in c_files_s2) { o <- readRDS(cf); s2_map[[o$symbol]] <- o }
m_rs60 <- matrix(0.0, nrow = n_total, ncol = length(b6_stage2$syms))
m_er20 <- matrix(0.3, nrow = n_total, ncol = length(b6_stage2$syms))
for (j in seq_along(b6_stage2$syms)) {
  o <- s2_map[[b6_stage2$syms[j]]]
  idx_1 <- match(eval_dates, index(o$rs_60))
  v1 <- !is.na(idx_1)
  if (any(v1)) m_rs60[v1, j] <- as.numeric(o$rs_60)[idx_1[v1]]
  idx_2 <- match(eval_dates, index(o$eff_ratio))
  v2 <- !is.na(idx_2)
  if (any(v2)) m_er20[v2, j] <- as.numeric(o$eff_ratio)[idx_2[v2]]
}

sim_stage2 <- function(bundle) {
  port_ret <- numeric(n_total)
  swing_only_ret <- numeric(n_total)
  active_swing_days <- 0L; win_swing_days <- 0L
  peak_equity <- 1.0; cur_equity <- 1.0

  for (t in 1:n_total) {
    dd <- (cur_equity - peak_equity) / peak_equity
    cb_multiplier <- if (dd <= -0.12) 0.50 else 1.0
    is_bull <- macro_bull[t]
    eff_max_pos <- if (is_bull) 3 else 2
    eff_p_long  <- if (is_bull) 0.58 else 0.65

    p_ups   <- bundle$prob[t, ]
    rs_vals <- bundle$rs[t, ]
    rs60_v  <- m_rs60[t, ]
    er20_v  <- m_er20[t, ]
    vols    <- bundle$vol[t, ]
    tqs     <- bundle$tq[t, ]
    idios   <- bundle$idio[t, ]
    rets_b  <- bundle$ret_b[t, ]
    val_flg <- bundle$valid[t, ]

    # Multi-horizon RS gate: RS_20 > 0 and not in a severe 60-day structural breakdown (RS_60 > -0.05)
    qualify <- which(val_flg & p_ups >= eff_p_long & rs_vals > 0 & rs60_v > -0.05)
    top_idx <- integer(0)
    if (length(qualify) > 0) {
      tq_boost   <- pmax(0, (tqs[qualify] - 0.50) * 0.25)
      idio_boost <- pmax(0, idios[qualify] * 0.08)
      rs60_boost <- pmax(0, rs60_v[qualify] * 0.20)
      er_boost   <- pmax(0, (er20_v[qualify] - 0.30) * 0.20)
      scores <- p_ups[qualify] * (1 + rs_vals[qualify] + tq_boost + idio_boost + rs60_boost + er_boost)
      ranked <- qualify[order(-scores)]
      sec_cnts <- list()
      for (cand in ranked) {
        c_sec <- bundle$sectors[cand]
        cnt <- if (is.null(sec_cnts[[c_sec]])) 0 else sec_cnts[[c_sec]]
        if (cnt < 3) {
          top_idx <- c(top_idx, cand)
          sec_cnts[[c_sec]] <- cnt + 1
          if (length(top_idx) >= eff_max_pos) break
        }
      }
    }

    n_sel <- length(top_idx)
    cash_day_ret <- cash_15d_10pct[t]
    if (n_sel > 0) {
      inv_v <- 1 / vols[top_idx]
      weights <- inv_v / sum(inv_v)
      swing_exposure <- min(1.0, n_sel / 3.0) * cb_multiplier
      swing_ret <- sum(weights * rets_b[top_idx])
      port_ret[t] <- swing_exposure * swing_ret + (1 - swing_exposure) * cash_day_ret
      swing_only_ret[t] <- swing_exposure * swing_ret + (1 - swing_exposure) * sgov_v[t]
      if (abs(swing_ret) > 1e-8) {
        active_swing_days <- active_swing_days + 1L
        if (swing_ret > 0) win_swing_days <- win_swing_days + 1L
      }
    } else {
      port_ret[t] <- cash_day_ret
      swing_only_ret[t] <- sgov_v[t]
    }
    cur_equity <- cur_equity * (1 + port_ret[t])
    if (cur_equity > peak_equity) peak_equity <- cur_equity
  }

  calc_stats <- function(r_vec) {
    eq <- cumprod(1 + r_vec)
    cagr <- (tail(eq, 1)^(252 / n_total) - 1) * 100
    mdd <- min((eq - cummax(eq)) / cummax(eq)) * 100
    ann_vol <- sd(r_vec) * sqrt(252) * 100
    sharpe <- (mean(r_vec) * 252 - 0.04) / (sd(r_vec) * sqrt(252))
    downside_ret <- pmin(0, r_vec)
    sortino <- (mean(r_vec) * 252 - 0.04) / (sqrt(mean(downside_ret^2)) * sqrt(252))
    calmar <- cagr / abs(mdd)
    list(CAGR = cagr, MaxDD = mdd, Sharpe = sharpe, Sortino = sortino, AnnVol = ann_vol, Calmar = calmar)
  }
  list(Full = calc_stats(port_ret), Swing = calc_stats(swing_only_ret),
       Win_Day_Pct = (win_swing_days / active_swing_days) * 100)
}

s6 <- sim_stage2(b6_stage2)
cat("\n========================================================================================\n")
cat(" STAGE-2 ENHANCEMENT RESULT (CMF_20D + RS_60 + EFFICIENCY RATIO_20D + ATR_COMP)\n")
cat("========================================================================================\n")
cat(sprintf("Pure Equity Swing Sleeve: CAGR = %.2f%% (vs %.2f%%) | MaxDD = %.2f%% (vs %.2f%%) | Sharpe = %.2f (vs %.2f) | Sortino = %.2f (vs %.2f) | Calmar = %.2f (vs %.2f) | WinDay = %.2f%%\n",
            s6$Swing$CAGR, s5$Swing$CAGR, s6$Swing$MaxDD, s5$Swing$MaxDD, s6$Swing$Sharpe, s5$Swing$Sharpe, s6$Swing$Sortino, s5$Swing$Sortino, s6$Swing$Calmar, s5$Swing$Calmar, s6$Win_Day_Pct))
cat(sprintf("Full Core-Satellite Port: CAGR = %.2f%% (vs %.2f%%) | MaxDD = %.2f%% (vs %.2f%%) | Sharpe = %.2f (vs %.2f) | Sortino = %.2f (vs %.2f) | Calmar = %.2f (vs %.2f)\n",
            s6$Full$CAGR, s5$Full$CAGR, s6$Full$MaxDD, s5$Full$MaxDD, s6$Full$Sharpe, s5$Full$Sharpe, s6$Full$Sortino, s5$Full$Sortino, s6$Full$Calmar, s5$Full$Calmar))
