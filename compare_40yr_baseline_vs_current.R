#!/usr/bin/env Rscript
# ==============================================================================
# 40-Year Continuous Backtest Comparison: Baseline Model vs. Current Model
# Evaluation Horizon: October 1985 to October 2026 (10,345 Trading Days / 41.1 Years)
# Starting Capital: $8,230.00 | Leverage: 1.5x | Borrow Cost: 7.0% Margin
#
# Comparison Dimensions:
# 1. Baseline Model:
#    - Linear momentum & RS ranking (buys extended breakouts > 3.0x ATR)
#    - No Net Wick Demand absorption filter
#    - Equal slot sizing without Sortino/Path-Efficiency weighting
#    - Static T-Bill / SGOV cash park during bear regimes (unhedged)
#
# 2. Current Upgraded Model:
#    - Non-Linear Normalization (Weakness 2): Saturated SlopeSlow & RS_60 with
#      asymmetric bell curve overextension dampener exp(-max(0, ATRExt - 1.8)^2 / (2 * 1.2^2))
#    - Net Wick Demand (Weakness 1): 10D ATR-normalized lower wick demand minus upper wick rejection
#    - Sortino Momentum (SortinoMom_20D) & Kaufman Path Efficiency in Quality Score & Sizing
#    - Smart Dual-Bear Hedge (-10% stop loss + 15-day time stop into SGOV)
# ==============================================================================

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(TTR)
  library(jsonlite)
})

source("R/08_metrics.R")

HIST_DIR   <- "data/data_historical"
OUTPUT_DIR <- "output"
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

STARTING_CAPITAL <- 8230.00
LEVERAGE         <- 1.5
BORROWED_MARGIN  <- LEVERAGE - 1.0  # 0.5x
MAX_POSITIONS    <- 3
SECTOR_CAP       <- 2
MIN_HISTORY_BARS <- 500

SECTOR_MAP <- if (file.exists("sector_map.json")) jsonlite::fromJSON("sector_map.json") else list()
get_sym_sector <- function(s) {
  if (!is.null(SECTOR_MAP[[s]])) return(SECTOR_MAP[[s]])
  if (s %in% c("NVDA", "AMD", "TSM", "AVGO", "QCOM", "MU", "AMAT", "LRCX", "ARM", "SOXL", "SMH", "INTC", "ADI", "MRVL")) return("Semiconductors")
  if (s %in% c("MSFT", "AAPL", "AMZN", "GOOGL", "META", "TSLA", "NFLX")) return("MegaCap_Tech")
  if (s %in% c("CRM", "NOW", "ADBE", "INTU", "ORCL", "SNOW", "WDAY", "SHOP", "PLTR")) return("Enterprise_Software")
  if (s %in% c("PANW", "CRWD", "FTNT", "NET", "ANET", "CSCO")) return("Cybersecurity")
  if (s %in% c("V", "MA", "PYPL", "SQ", "COIN", "MSTR", "WFC", "GS", "CFG")) return("Financials")
  if (s %in% c("CEG", "VST", "GE", "COP", "GLNG")) return("Energy_Industrials")
  return("General_Tech")
}

cat("[40Yr-Engine] Splicing benchmarks and macro data from 1985 to present...\n")
ndx_raw   <- readRDS(file.path(HIST_DIR, "data_^ndx.rds"))
qqq_raw   <- readRDS(file.path(HIST_DIR, "data_qqq.rds"))
vfinx_raw <- readRDS(file.path(HIST_DIR, "data_vfinx.rds"))
vti_raw   <- readRDS(file.path(HIST_DIR, "data_vti.rds"))
gspc_raw  <- readRDS(file.path(HIST_DIR, "data_^gspc.rds"))
spy_raw   <- readRDS(file.path(HIST_DIR, "data_spy.rds"))
irx_raw   <- readRDS(file.path(HIST_DIR, "data_^irx.rds"))

# Spliced QQQ
ndx_cl <- Cl(ndx_raw); qqq_cl <- Cl(qqq_raw)
ndx_ret <- na.omit(ndx_cl / lag.xts(ndx_cl, 1) - 1) - (0.0020 / 252)
qqq_ret <- na.omit(qqq_cl / lag.xts(qqq_cl, 1) - 1)
qqq_start <- index(qqq_ret)[1]
qqq_spliced <- rbind(ndx_ret[index(ndx_ret) < qqq_start], qqq_ret)

# Spliced VTI
vfinx_cl <- Cl(vfinx_raw); vti_cl <- Cl(vti_raw)
vfinx_ret <- na.omit(vfinx_cl / lag.xts(vfinx_cl, 1) - 1) - (0.0003 / 252)
vti_ret   <- na.omit(vti_cl / lag.xts(vti_cl, 1) - 1)
vti_start <- index(vti_ret)[1]
vti_spliced <- rbind(vfinx_ret[index(vfinx_ret) < vti_start], vti_ret)

# Spliced SPY
gspc_cl <- Cl(gspc_raw); spy_cl <- Cl(spy_raw)
gspc_ret <- na.omit(gspc_cl / lag.xts(gspc_cl, 1) - 1)
spy_ret  <- na.omit(spy_cl / lag.xts(spy_cl, 1) - 1)
spy_start <- index(spy_ret)[1]
spy_spliced <- rbind(gspc_ret[index(gspc_ret) < spy_start], spy_ret)
spy_level   <- cumprod(1 + spy_spliced) * 100

# Daily risk-free / cash yield
irx_yield <- na.locf(irx_raw[, 4], na.rm = FALSE)
irx_yield[is.na(irx_yield)] <- 2.0
rf_daily <- xts((as.numeric(irx_yield) / 100) / 252, order.by = index(irx_raw))

# Spliced Gold Proxy (GLD if available, else zero/commodity proxy)
gld_path <- file.path("data", "data_gld.rds")
if (!file.exists(gld_path)) gld_path <- file.path(HIST_DIR, "data_gld.rds")
gld_raw <- if (file.exists(gld_path)) readRDS(gld_path) else NULL

# Align 40-Year Continuous Date Range (1985-10-01 to Present)
bench_dates <- intersect(index(qqq_spliced), index(vti_spliced))
bench_dates <- intersect(bench_dates, index(spy_spliced))
full_dates  <- sort(bench_dates)
n_total     <- length(full_dates)

qqq_full  <- as.numeric(qqq_spliced[match(full_dates, index(qqq_spliced))])
vti_full  <- as.numeric(vti_spliced[match(full_dates, index(vti_spliced))])
spy_full  <- as.numeric(spy_spliced[match(full_dates, index(spy_spliced))])
rf_full   <- as.numeric(rf_daily[match(full_dates, index(rf_daily))])
rf_full[is.na(rf_full)] <- 0.02 / 252
b70_30_full <- 0.70 * vti_full + 0.30 * qqq_full

# Macro Trend Filter: 200-day SMA on SPY
spy_full_series <- as.numeric(spy_level)
spy_sma200_vals <- as.numeric(SMA(xts(spy_full_series, order.by = index(spy_level)), n = 200))
spy_all_idx     <- match(full_dates, index(spy_level))
spy_sma200_full <- spy_sma200_vals[spy_all_idx]
macro_bull_vec  <- !is.na(spy_sma200_full) & (spy_full_series[spy_all_idx] >= spy_sma200_full)

# Spliced Inverse Tech / QQQ Hedge Proxy for the 40-year duration:
# Inverse returns are modeled as 3x inverse QQQ/NDX minus borrow drag (0.95%/252), capped at +/-15% daily
inv_tech_ret <- pmin(0.15, pmax(-0.15, -3.0 * qqq_full - (0.0095 / 252)))

cat("[40Yr-Engine] Processing historical stock database for both Baseline & Current models...\n")
stock_files <- list.files(HIST_DIR, pattern = "^data_[a-z0-9_]+\\.rds$", full.names = TRUE)
stock_files <- stock_files[!grepl("data_(\\^|vti|qqq|spy|vfinx|gspc|ndx|irx)", stock_files)]

precomp_stocks <- list()

for (f in stock_files) {
  sym <- toupper(sub("^data_", "", sub("\\.rds$", "", basename(f))))
  d <- tryCatch(readRDS(f), error = function(e) NULL)
  if (is.null(d) || nrow(d) < 150) next
  
  df_sub <- tryCatch(na.locf(na.locf(d, na.rm = FALSE), fromLast = TRUE), error = function(e) NULL)
  if (is.null(df_sub) || nrow(df_sub) < 100) next
  
  all_idx <- index(df_sub)
  cl_xts  <- Cl(df_sub)
  op_xts  <- Op(df_sub)
  hi_xts  <- Hi(df_sub)
  lo_xts  <- Lo(df_sub)
  vo_xts  <- tryCatch(Vo(df_sub), error = function(e) xts(rep(1e6, nrow(df_sub)), order.by = all_idx))
  
  nb <- nrow(df_sub)
  c_sub <- as.numeric(cl_xts)
  o_sub <- as.numeric(op_xts)
  h_sub <- as.numeric(hi_xts)
  l_sub <- as.numeric(lo_xts)
  v_sub <- as.numeric(vo_xts)
  v_sub[is.na(v_sub) | v_sub <= 0] <- 1e6
  
  # 63-day RS vs SPY
  spy_aligned <- spy_level[all_idx]
  rs60_raw <- (c_sub / as.numeric(lag.xts(cl_xts, 63))) - (as.numeric(spy_aligned) / as.numeric(lag.xts(spy_aligned, 63)))
  rs60_raw[is.na(rs60_raw)] <- 0.0
  
  # 20-day momentum
  mom20_raw <- (c_sub / as.numeric(lag.xts(cl_xts, 20))) - 1.0
  mom20_raw[is.na(mom20_raw)] <- 0.0
  
  # 50-day SMA & Slow Slope (%/day)
  sma50_xts <- tryCatch(SMA(cl_xts, n = 50), error = function(e) rep(NA_real_, nb))
  sma50_v   <- as.numeric(sma50_xts)
  slope_slow_raw <- (c_sub - as.numeric(lag.xts(cl_xts, 50))) / (50 * c_sub + 1e-5) * 100
  slope_slow_raw[is.na(slope_slow_raw)] <- 0.0
  
  # 14-day ATR & ATR Overextension
  atr_obj <- tryCatch(ATR(HLC(df_sub), n = 14), error = function(e) NULL)
  atr_abs <- if (!is.null(atr_obj)) as.numeric(atr_obj$atr) else c_sub * 0.02
  atr_abs[is.na(atr_abs) | atr_abs <= 0] <- c_sub[is.na(atr_abs) | atr_abs <= 0] * 0.02
  
  fast_fit <- as.numeric(tryCatch(SMA(cl_xts, n = 20), error = function(e) cl_xts))
  fast_fit[is.na(fast_fit)] <- c_sub[is.na(fast_fit)]
  atr_ext_raw <- (c_sub - fast_fit) / (atr_abs + 1e-5)
  atr_ext_raw[is.na(atr_ext_raw)] <- 0.0
  
  # Annualized Realized Volatility
  vol_ann <- (atr_abs / c_sub) * sqrt(252)
  vol_ann[is.na(vol_ann) | vol_ann < 0.10] <- 0.25
  
  # 10D Net Wick Demand Absorption (Lower Wick - Upper Wick)
  body_high <- pmax(o_sub, c_sub)
  body_low  <- pmin(o_sub, c_sub)
  lw_raw    <- pmax(0, body_low - l_sub) / (atr_abs + 1e-5)
  uw_raw    <- pmax(0, h_sub - body_high) / (atr_abs + 1e-5)
  net_wick_10 <- as.numeric(TTR::runMean(lw_raw - uw_raw, n = 10))
  net_wick_10[is.na(net_wick_10)] <- 0.0
  net_wick_10 <- pmin(pmax(net_wick_10, -2.5), 2.5)
  
  # 20D Sortino Momentum (SortinoMom_20D)
  ret_1d   <- c(0, diff(log(pmax(c_sub, 1e-4))))
  neg_r2   <- pmin(0, ret_1d)^2
  dn_sd20  <- sqrt(pmax(0, as.numeric(TTR::runMean(neg_r2, n = 20))) * 20)
  sortino_20 <- mom20_raw / (dn_sd20 + 0.01)
  sortino_20[is.na(sortino_20)] <- 0.0
  sortino_20 <- pmin(pmax(sortino_20, -4.0), 6.0)
  
  # 20D Kaufman Path Efficiency Ratio
  abs_d1  <- c(0, abs(diff(c_sub)))
  net_m20 <- abs(c_sub - as.numeric(lag.xts(cl_xts, k = 20)))
  path_20 <- as.numeric(TTR::runSum(abs_d1, n = 20))
  eff_ratio <- net_m20 / (path_20 + 1e-4)
  eff_ratio[is.na(eff_ratio)] <- 0.30
  eff_ratio <- pmin(pmax(eff_ratio, 0.0), 1.0)
  
  # Non-Linear Normalization (Weakness 2 Solution):
  # Bell curve saturation penalizes parabolic overextensions > 3.0x ATR
  ext_damp <- exp(-pmax(0, atr_ext_raw - 1.8)^2 / (2 * 1.2^2))
  ext_damp[is.na(ext_damp)] <- 1.0
  slope_slow_norm <- tanh(slope_slow_raw / 0.25) * ext_damp
  rs60_norm       <- tanh(rs60_raw / 0.15) * ext_damp
  
  # --------------------------------------------------------------------------
  # 1. BASELINE MODEL SIGNAL & PROBABILITY:
  # Raw linear triggers, no overextension penalty, no net-wick filter
  # --------------------------------------------------------------------------
  p_cl  <- c(0, c_sub[1:(nb - 1)])
  p_sma <- c(0, sma50_v[1:(nb - 1)])
  p_rs  <- c(0, rs60_raw[1:(nb - 1)])
  p_mom <- c(0, mom20_raw[1:(nb - 1)])
  
  base_trigger <- ifelse(!is.na(p_cl) & !is.na(p_sma) & p_cl > p_sma & 
                         !is.na(p_rs) & p_rs > 0.01 & 
                         !is.na(p_mom) & p_mom > 0.01, 1, 0)
  base_prob <- 0.50 + 0.15 * pmin(1.0, pmax(0.0, p_rs * 3)) + 0.10 * pmin(1.0, pmax(0.0, p_mom * 2))
  base_prob[base_trigger == 0] <- 0.50
  
  # --------------------------------------------------------------------------
  # 2. CURRENT UPGRADED MODEL SIGNAL & PROBABILITY:
  # Non-linear normalized features, net wick demand boost, overextension suppression
  # --------------------------------------------------------------------------
  p_rs_norm    <- c(0, rs60_norm[1:(nb - 1)])
  p_slope_norm <- c(0, slope_slow_norm[1:(nb - 1)])
  p_ext        <- c(0, atr_ext_raw[1:(nb - 1)])
  p_wick       <- c(0, net_wick_10[1:(nb - 1)])
  p_sortino    <- c(0, sortino_20[1:(nb - 1)])
  p_eff        <- c(0, eff_ratio[1:(nb - 1)])
  
  # Overextension suppression guard (Feature 1: atr_ext <= 3.0)
  cur_trigger <- ifelse(base_trigger == 1 & p_ext <= 3.0, 1, 0)
  
  # Setup probability incorporating saturated slope, RS_60, net wick absorption and path efficiency
  cur_prob <- 0.50 + 
              0.12 * pmin(1.0, pmax(0.0, p_rs_norm * 1.5)) + 
              0.08 * pmin(1.0, pmax(0.0, p_slope_norm * 1.5)) +
              0.03 * pmin(1.0, pmax(-0.5, p_wick * 0.5)) +
              0.03 * pmin(1.0, pmax(0.0, (p_eff - 0.30) * 1.5))
  cur_prob[cur_trigger == 0] <- 0.50
  
  # --------------------------------------------------------------------------
  # BRACKET EXECUTION SIMULATOR
  # --------------------------------------------------------------------------
  sim_bracket_trades <- function(sig_vec, has_runner_ratchet = FALSE) {
    bracket_ret <- numeric(nb)
    in_pos <- FALSE
    entry_p <- 0; stop_p <- 0; t1_p <- 0; t2_p <- 0; high_p <- 0; days_h <- 0
    t1_hit <- FALSE; t2_hit <- FALSE; pos_shares_pct <- 0
    
    for (i in 2:nb) {
      cur_sig <- sig_vec[i]
      cur_o   <- o_sub[i]; cur_h <- h_sub[i]; cur_l <- l_sub[i]; cur_c <- c_sub[i]
      prev_c  <- c_sub[i - 1]; cur_atr <- atr_abs[i]
      
      if (!in_pos && cur_sig == 1) {
        in_pos  <- TRUE
        entry_p <- ifelse(!is.na(cur_o) && cur_o > 0, cur_o, cur_c)
        high_p  <- entry_p
        days_h  <- 0
        t1_hit  <- FALSE
        t2_hit  <- FALSE
        pos_shares_pct <- 1.0
        
        risk_1r <- 2.0 * cur_atr
        stop_p  <- entry_p - risk_1r
        t1_p    <- entry_p + 1.5 * risk_1r
        t2_p    <- entry_p + 3.0 * risk_1r
        
        bracket_ret[i] <- (cur_c - entry_p) / entry_p - (10 / 10000)
        next
      }
      
      if (in_pos) {
        days_h <- days_h + 1
        high_p <- max(high_p, cur_h)
        chand_stop <- high_p - 2.5 * cur_atr
        
        # Stop loss hit
        if (cur_l <= stop_p) {
          exit_p <- min(cur_o, stop_p)
          bar_r  <- (exit_p - prev_c) / prev_c
          bracket_ret[i] <- pos_shares_pct * bar_r - (10 / 10000)
          in_pos <- FALSE
          pos_shares_pct <- 0
          next
        }
        
        # Tier 1 (+1.5R): sell 50%
        if (!t1_hit && cur_h >= t1_p) {
          t1_hit <- TRUE
          pos_shares_pct <- 0.50
          stop_p <- max(entry_p, chand_stop)
        }
        
        # Tier 2 (+3.0R): sell 25%, 25% runner
        if (t1_hit && !t2_hit && cur_h >= t2_p) {
          t2_hit <- TRUE
          pos_shares_pct <- 0.25
          stop_p <- max(stop_p, chand_stop)
        }
        
        if (t1_hit) stop_p <- max(stop_p, chand_stop)
        
        # Max hold 7 days for non-runners
        if (!t2_hit && days_h >= 7) {
          bar_r <- (cur_c - prev_c) / prev_c
          bracket_ret[i] <- pos_shares_pct * bar_r - (10 / 10000)
          in_pos <- FALSE
          pos_shares_pct <- 0
          next
        }
        
        # Runner ratchet (Current model holds runner up to 20 days trailing chandelier)
        if (t2_hit && has_runner_ratchet && days_h >= 20) {
          bar_r <- (cur_c - prev_c) / prev_c
          bracket_ret[i] <- pos_shares_pct * bar_r - (10 / 10000)
          in_pos <- FALSE
          pos_shares_pct <- 0
          next
        }
        
        bar_r <- (cur_c - prev_c) / prev_c
        bracket_ret[i] <- pos_shares_pct * bar_r
      }
    }
    bracket_ret
  }
  
  b_ret_base <- sim_bracket_trades(base_trigger, has_runner_ratchet = FALSE)
  b_ret_curr <- sim_bracket_trades(cur_trigger,  has_runner_ratchet = TRUE)
  
  precomp_stocks[[sym]] <- list(
    dates       = all_idx,
    base_prob   = base_prob,
    base_rs     = rs60_raw,
    base_ret_b  = b_ret_base,
    cur_prob    = cur_prob,
    cur_rs      = rs60_norm,
    cur_ret_b   = b_ret_curr,
    sortino     = sortino_20,
    eff_ratio   = eff_ratio,
    vol         = vol_ann,
    bar_num     = seq_len(nb),
    sector      = get_sym_sector(sym)
  )
}

cat(sprintf("[40Yr-Engine] Successfully indexed %d stocks for comparative evaluation.\n", length(precomp_stocks)))

avail_syms <- names(precomp_stocks)
n_syms     <- length(avail_syms)

mat_valid       <- matrix(FALSE, nrow = n_total, ncol = n_syms)
mat_base_prob   <- matrix(0.50,  nrow = n_total, ncol = n_syms)
mat_base_rs     <- matrix(0.00,  nrow = n_total, ncol = n_syms)
mat_base_ret_b  <- matrix(0.00,  nrow = n_total, ncol = n_syms)

mat_cur_prob    <- matrix(0.50,  nrow = n_total, ncol = n_syms)
mat_cur_rs      <- matrix(0.00,  nrow = n_total, ncol = n_syms)
mat_cur_ret_b   <- matrix(0.00,  nrow = n_total, ncol = n_syms)
mat_sortino     <- matrix(0.00,  nrow = n_total, ncol = n_syms)
mat_eff         <- matrix(0.30,  nrow = n_total, ncol = n_syms)
mat_vol         <- matrix(0.25,  nrow = n_total, ncol = n_syms)

for (j in seq_along(avail_syms)) {
  s <- avail_syms[j]
  obj <- precomp_stocks[[s]]
  m_idx <- match(full_dates, obj$dates)
  v <- !is.na(m_idx)
  if (any(v)) {
    v_ep_idx <- which(v)
    v_stk_idx <- m_idx[v]
    
    is_eligible <- obj$bar_num[v_stk_idx] >= MIN_HISTORY_BARS
    mat_valid[v_ep_idx[is_eligible], j] <- TRUE
    
    mat_base_prob[v_ep_idx, j]  <- obj$base_prob[v_stk_idx]
    mat_base_rs[v_ep_idx, j]    <- obj$base_rs[v_stk_idx]
    mat_base_ret_b[v_ep_idx, j] <- obj$base_ret_b[v_stk_idx]
    
    mat_cur_prob[v_ep_idx, j]   <- obj$cur_prob[v_stk_idx]
    mat_cur_rs[v_ep_idx, j]     <- obj$cur_rs[v_stk_idx]
    mat_cur_ret_b[v_ep_idx, j]  <- obj$cur_ret_b[v_stk_idx]
    
    mat_sortino[v_ep_idx, j]    <- obj$sortino[v_stk_idx]
    mat_eff[v_ep_idx, j]        <- obj$eff_ratio[v_stk_idx]
    mat_vol[v_ep_idx, j]        <- obj$vol[v_stk_idx]
  }
}

sym_sectors <- sapply(avail_syms, function(s) precomp_stocks[[s]]$sector)
margin_fee  <- (0.070 / 252) * BORROWED_MARGIN
slot_w      <- 1.0 / MAX_POSITIONS

# ==============================================================================
# SIMULATION 1: BASELINE MODEL (1985 to 2026)
# ==============================================================================
cat("[40Yr-Engine] Simulating Baseline Model continuous portfolio...\n")
ret_baseline <- numeric(n_total)

for (t in 1:n_total) {
  is_bull <- macro_bull_vec[t]
  p_ups   <- mat_base_prob[t, ]
  rs_vals <- mat_base_rs[t, ]
  vols    <- mat_vol[t, ]
  rets_b  <- mat_base_ret_b[t, ]
  val_flg <- mat_valid[t, ]
  
  # Baseline: linear threshold p >= 0.58 & RS > 0
  qualify <- which(val_flg & p_ups >= 0.58 & rs_vals > 0)
  top_idx <- integer(0)
  
  if (length(qualify) > 0) {
    scores <- p_ups[qualify] * (1 + rs_vals[qualify])
    ranked <- qualify[order(-scores)]
    sec_counts <- list()
    for (cand in ranked) {
      c_sec <- sym_sectors[cand]
      cnt <- if (is.null(sec_counts[[c_sec]])) 0 else sec_counts[[c_sec]]
      if (cnt < SECTOR_CAP) {
        top_idx <- c(top_idx, cand)
        sec_counts[[c_sec]] <- cnt + 1
        if (length(top_idx) >= MAX_POSITIONS) break
      }
    }
  }
  
  n_sel <- length(top_idx)
  cash_park_r <- if (is_bull) qqq_full[t] else rf_full[t]
  
  if (n_sel == 0) {
    ret_baseline[t] <- cash_park_r * LEVERAGE - margin_fee
  } else {
    stock_alloc_total <- 0
    stock_pnl_total   <- 0
    for (idx in top_idx) {
      vol_scale <- pmin(1.5, 0.45 / vols[idx])
      w_i <- slot_w * vol_scale
      stock_alloc_total <- stock_alloc_total + slot_w
      stock_pnl_total   <- stock_pnl_total + (w_i * rets_b[idx])
    }
    unalloc_cash <- max(0, 1.0 - stock_alloc_total)
    cash_yield_pnl <- unalloc_cash * cash_park_r
    ret_baseline[t] <- (stock_pnl_total + cash_yield_pnl) * LEVERAGE - margin_fee
  }
}

# ==============================================================================
# SIMULATION 2: CURRENT UPGRADED MODEL (1985 to 2026)
# Includes:
# - Non-linear setup scoring & overextension penalty (Weakness 2)
# - Net Wick Demand absorption (Weakness 1)
# - Sortino & Path-Efficiency adjusted sizing (SortinoMom_20D & Eff_Ratio)
# - Smart Dual-Bear Hedge (-10% stop loss & 15-day time stop into SGOV)
# ==============================================================================
cat("[40Yr-Engine] Simulating Current Upgraded Model continuous portfolio...\n")
ret_current <- numeric(n_total)

# State tracker for Smart Dual-Bear Hedge
in_hedge_episode <- FALSE
hedge_ep_day     <- 0
hedge_ep_cum     <- 1.0
hedge_stopped_out <- FALSE

for (t in 1:n_total) {
  is_bull <- macro_bull_vec[t]
  
  # Defensive Hedge Engine Logic:
  if (is_bull) {
    in_hedge_episode  <- FALSE
    hedge_ep_day      <- 0
    hedge_ep_cum      <- 1.0
    hedge_stopped_out <- FALSE
    cash_park_r       <- qqq_full[t]
  } else {
    if (!in_hedge_episode) {
      in_hedge_episode  <- TRUE
      hedge_ep_day      <- 0
      hedge_ep_cum      <- 1.0
      hedge_stopped_out <- FALSE
    }
    hedge_ep_day <- hedge_ep_day + 1
    
    # 15-day time stop or -10% circuit breaker stop loss
    if (hedge_ep_day > 15) hedge_stopped_out <- TRUE
    
    if (hedge_stopped_out) {
      cash_park_r <- rf_full[t]
    } else {
      h_r <- inv_tech_ret[t]
      hedge_ep_cum <- hedge_ep_cum * (1 + h_r)
      if ((hedge_ep_cum - 1.0) <= -0.10) { # -10.0% max loss circuit
        hedge_stopped_out <- TRUE
      }
      cash_park_r <- h_r
    }
  }
  
  p_ups     <- mat_cur_prob[t, ]
  rs_vals   <- mat_cur_rs[t, ]
  vols      <- mat_vol[t, ]
  rets_b    <- mat_cur_ret_b[t, ]
  smom_vals <- mat_sortino[t, ]
  eff_vals  <- mat_eff[t, ]
  val_flg   <- mat_valid[t, ]
  
  # Current Model Quality Score:
  # Quality_Score = Prob_Up * (1 + RS + sm_boost + er_boost)
  eff_p_long <- if (is_bull) 0.58 else 0.65
  qualify    <- which(val_flg & p_ups >= eff_p_long & rs_vals > 0)
  top_idx    <- integer(0)
  
  if (length(qualify) > 0) {
    sm_boost <- pmax(0, (smom_vals[qualify] - 0.80) * 0.06)
    er_boost <- pmax(0, (eff_vals[qualify] - 0.30) * 0.20)
    scores   <- p_ups[qualify] * (1 + rs_vals[qualify] + sm_boost + er_boost)
    ranked   <- qualify[order(-scores)]
    sec_counts <- list()
    for (cand in ranked) {
      c_sec <- sym_sectors[cand]
      cnt <- if (is.null(sec_counts[[c_sec]])) 0 else sec_counts[[c_sec]]
      if (cnt < SECTOR_CAP) {
        top_idx <- c(top_idx, cand)
        sec_counts[[c_sec]] <- cnt + 1
        if (length(top_idx) >= (if (is_bull) MAX_POSITIONS else 2)) break
      }
    }
  }
  
  n_sel <- length(top_idx)
  
  if (n_sel == 0) {
    ret_current[t] <- cash_park_r * LEVERAGE - margin_fee
  } else {
    # Sortino & Path-Efficiency Adjusted Inverse-Vol Parity Sizing:
    cand_vols <- pmax(0.15, pmin(1.20, vols[top_idx]))
    cand_er   <- eff_vals[top_idx]
    cand_sm   <- smom_vals[top_idx]
    eff_vols  <- cand_vols / (1.0 + 0.20 * pmax(0, cand_er - 0.35) + 0.10 * pmax(0, cand_sm))
    inv_vol_w <- (1.0 / eff_vols) / sum(1.0 / eff_vols)
    
    stock_alloc_total <- 0
    stock_pnl_total   <- 0
    for (k in seq_along(top_idx)) {
      idx <- top_idx[k]
      vol_scale <- pmin(1.25, 0.45 / cand_vols[k])
      w_i <- inv_vol_w[k] * vol_scale
      stock_alloc_total <- stock_alloc_total + (slot_w * vol_scale)
      stock_pnl_total   <- stock_pnl_total + (w_i * rets_b[idx])
    }
    
    unalloc_cash <- max(0, 1.0 - stock_alloc_total)
    cash_yield_pnl <- unalloc_cash * cash_park_r
    ret_current[t] <- (stock_pnl_total + cash_yield_pnl) * LEVERAGE - margin_fee
  }
}

# ==============================================================================
# TAX SIMULATION ACROSS 40 YEARS
# ==============================================================================
calc_tax_series <- function(daily_ret_vec, dates_vec, tax_rate, starting_cap = STARTING_CAPITAL) {
  n <- length(daily_ret_vec)
  equity_curve <- numeric(n)
  years <- as.numeric(format(dates_vec, "%Y"))
  unique_years <- unique(years)
  
  cap_curr <- starting_cap
  loss_carryforward <- 0
  
  for (yr in unique_years) {
    yr_mask <- (years == yr)
    yr_indices <- which(yr_mask)
    if (length(yr_indices) == 0) next
    
    yr_cap_start <- cap_curr
    for (i in yr_indices) {
      cap_curr <- cap_curr * (1 + daily_ret_vec[i])
      equity_curve[i] <- cap_curr
    }
    yr_cap_end <- cap_curr
    net_gain <- yr_cap_end - yr_cap_start
    
    if (tax_rate > 0) {
      if (net_gain > 0) {
        taxable_gain <- max(0, net_gain - loss_carryforward)
        loss_carryforward <- max(0, loss_carryforward - net_gain)
        tax_due <- taxable_gain * tax_rate
        cap_curr <- cap_curr - tax_due
      } else {
        loss_carryforward <- loss_carryforward + abs(net_gain)
      }
    }
    equity_curve[tail(yr_indices, 1)] <- cap_curr
  }
  
  effective_ret <- numeric(n)
  effective_ret[1] <- (equity_curve[1] / starting_cap) - 1
  if (n > 1) {
    effective_ret[2:n] <- (equity_curve[2:n] / equity_curve[1:(n - 1)]) - 1
  }
  
  list(terminal_wealth = cap_curr, equity_curve = equity_curve, effective_ret = effective_ret)
}

calc_benchmark_tax <- function(bench_ret_vec, dates_vec, tax_rate_ltcg, div_yield_ann = 0.015, tax_rate_div = 0.341, starting_cap = STARTING_CAPITAL) {
  n <- length(bench_ret_vec)
  equity_curve <- numeric(n)
  cap_curr <- starting_cap
  daily_div_tax <- (div_yield_ann * tax_rate_div) / 252
  
  for (i in 1:n) {
    net_r <- bench_ret_vec[i] - daily_div_tax
    cap_curr <- cap_curr * (1 + net_r)
    equity_curve[i] <- cap_curr
  }
  
  if (tax_rate_ltcg > 0) {
    total_gain <- max(0, cap_curr - starting_cap)
    ltcg_tax <- total_gain * tax_rate_ltcg
    cap_curr <- cap_curr - ltcg_tax
    equity_curve[n] <- cap_curr
  }
  
  effective_ret <- numeric(n)
  effective_ret[1] <- (equity_curve[1] / starting_cap) - 1
  if (n > 1) {
    effective_ret[2:n] <- (equity_curve[2:n] / equity_curve[1:(n - 1)]) - 1
  }
  
  list(terminal_wealth = cap_curr, equity_curve = equity_curve, effective_ret = effective_ret)
}

# Run tax compounding for both models
tax_base_gross <- calc_tax_series(ret_baseline, full_dates, tax_rate = 0.0)
tax_base_tts   <- calc_tax_series(ret_baseline, full_dates, tax_rate = 0.284)
tax_base_nyc   <- calc_tax_series(ret_baseline, full_dates, tax_rate = 0.512)

tax_curr_gross <- calc_tax_series(ret_current, full_dates, tax_rate = 0.0)
tax_curr_tts   <- calc_tax_series(ret_current, full_dates, tax_rate = 0.284)
tax_curr_nyc   <- calc_tax_series(ret_current, full_dates, tax_rate = 0.512)

qqq_gross <- calc_tax_series(qqq_full, full_dates, tax_rate = 0.0)
qqq_nyc   <- calc_benchmark_tax(qqq_full, full_dates, tax_rate_ltcg = 0.341, div_yield_ann = 0.007, tax_rate_div = 0.341)

vti_gross <- calc_tax_series(vti_full, full_dates, tax_rate = 0.0)
vti_nyc   <- calc_benchmark_tax(vti_full, full_dates, tax_rate_ltcg = 0.341, div_yield_ann = 0.015, tax_rate_div = 0.341)

get_metrics <- function(ret_vec, name) {
  m <- calc_performance_metrics(xts(ret_vec, order.by = full_dates), name = name)$raw
  list(
    cagr    = m$ann_ret * 100,
    cum_ret = m$cum_ret * 100,
    max_dd  = m$max_dd * 100,
    sharpe  = m$sharpe,
    sortino = m$sortino,
    calmar  = ifelse(!is.na(m$max_dd) && m$max_dd > 0, (m$ann_ret * 100) / (m$max_dd * 100), NA),
    ann_vol = m$ann_vol * 100
  )
}

m_base_gross <- get_metrics(tax_base_gross$effective_ret, "Baseline Gross")
m_base_tts   <- get_metrics(tax_base_tts$effective_ret,   "Baseline TTS")
m_base_nyc   <- get_metrics(tax_base_nyc$effective_ret,   "Baseline NYC")

m_curr_gross <- get_metrics(tax_curr_gross$effective_ret, "Current Gross")
m_curr_tts   <- get_metrics(tax_curr_tts$effective_ret,   "Current TTS")
m_curr_nyc   <- get_metrics(tax_curr_nyc$effective_ret,   "Current NYC")

m_qqq_gross  <- get_metrics(qqq_gross$effective_ret,      "QQQ Gross")
m_vti_gross  <- get_metrics(vti_gross$effective_ret,      "VTI Gross")

# Decade Breakdown for both models
years <- as.numeric(format(full_dates, "%Y"))
decades <- list(
  "1985-1994 (Decade 1: Pre-Tech Dawn)"         = c(1985, 1994),
  "1995-2004 (Decade 2: Dot-Com Boom & Bust)"   = c(1995, 2004),
  "2005-2014 (Decade 3: GFC & Recovery)"        = c(2005, 2014),
  "2015-2024 (Decade 4: Tech Hegemony & COVID)" = c(2015, 2024),
  "2025-2026 (Recent 2 Years: Rate Shock)"      = c(2025, 2026)
)

decade_rows <- list()
for (d_name in names(decades)) {
  yr_rng <- decades[[d_name]]
  d_mask <- (years >= yr_rng[1] & years <= yr_rng[2])
  d_dates <- full_dates[d_mask]
  if (length(d_dates) < 5) next
  
  d_base <- ret_baseline[d_mask]
  d_curr <- ret_current[d_mask]
  d_qqq  <- qqq_full[d_mask]
  
  mb <- calc_performance_metrics(xts(d_base, order.by = d_dates))$raw
  mc <- calc_performance_metrics(xts(d_curr, order.by = d_dates))$raw
  mq <- calc_performance_metrics(xts(d_qqq,  order.by = d_dates))$raw
  
  decade_rows[[length(decade_rows) + 1]] <- data.frame(
    Period        = d_name,
    Baseline_CAGR = sprintf("%+.2f%%", mb$ann_ret * 100),
    Baseline_DD   = sprintf("-%.2f%%", mb$max_dd * 100),
    Baseline_SR   = sprintf("%.2f", mb$sharpe),
    Current_CAGR  = sprintf("%+.2f%%", mc$ann_ret * 100),
    Current_DD    = sprintf("-%.2f%%", mc$max_dd * 100),
    Current_SR    = sprintf("%.2f", mc$sharpe),
    Current_Calmar= sprintf("%.2f", (mc$ann_ret * 100) / (mc$max_dd * 100)),
    QQQ_CAGR      = sprintf("%+.2f%%", mq$ann_ret * 100),
    QQQ_DD        = sprintf("-%.2f%%", mq$max_dd * 100),
    stringsAsFactors = FALSE
  )
}
decade_df <- do.call(rbind, decade_rows)

scorecard_comp <- data.frame(
  Metric = c("CAGR", "Max Drawdown", "Sharpe Ratio", "Sortino Ratio", "Calmar Ratio", "Annual Volatility", "Terminal Wealth ($8,230 start)"),
  Baseline_Gross   = c(sprintf("%+.2f%%", m_base_gross$cagr), sprintf("-%.2f%%", m_base_gross$max_dd), sprintf("%.2f", m_base_gross$sharpe), sprintf("%.2f", m_base_gross$sortino), sprintf("%.2f", m_base_gross$calmar), sprintf("%.2f%%", m_base_gross$ann_vol), sprintf("$%.2f", tax_base_gross$terminal_wealth)),
  Current_Gross    = c(sprintf("%+.2f%%", m_curr_gross$cagr), sprintf("-%.2f%%", m_curr_gross$max_dd), sprintf("%.2f", m_curr_gross$sharpe), sprintf("%.2f", m_curr_gross$sortino), sprintf("%.2f", m_curr_gross$calmar), sprintf("%.2f%%", m_curr_gross$ann_vol), sprintf("$%.2f", tax_curr_gross$terminal_wealth)),
  Alpha_Spread     = c(sprintf("%+.2f%%", m_curr_gross$cagr - m_base_gross$cagr), sprintf("%+.2f%%", m_curr_gross$max_dd - m_base_gross$max_dd), sprintf("%+.2f", m_curr_gross$sharpe - m_base_gross$sharpe), sprintf("%+.2f", m_curr_gross$sortino - m_base_gross$sortino), sprintf("%+.2f", m_curr_gross$calmar - m_base_gross$calmar), sprintf("%+.2f%%", m_curr_gross$ann_vol - m_base_gross$ann_vol), "N/A"),
  Baseline_TTS     = c(sprintf("%+.2f%%", m_base_tts$cagr),   sprintf("-%.2f%%", m_base_tts$max_dd),   sprintf("%.2f", m_base_tts$sharpe),   sprintf("%.2f", m_base_tts$sortino),   sprintf("%.2f", m_base_tts$calmar),   sprintf("%.2f%%", m_base_tts$ann_vol),   sprintf("$%.2f", tax_base_tts$terminal_wealth)),
  Current_TTS      = c(sprintf("%+.2f%%", m_curr_tts$cagr),   sprintf("-%.2f%%", m_curr_tts$max_dd),   sprintf("%.2f", m_curr_tts$sharpe),   sprintf("%.2f", m_curr_tts$sortino),   sprintf("%.2f", m_curr_tts$calmar),   sprintf("%.2f%%", m_curr_tts$ann_vol),   sprintf("$%.2f", tax_curr_tts$terminal_wealth)),
  Baseline_NYC     = c(sprintf("%+.2f%%", m_base_nyc$cagr),   sprintf("-%.2f%%", m_base_nyc$max_dd),   sprintf("%.2f", m_base_nyc$sharpe),   sprintf("%.2f", m_base_nyc$sortino),   sprintf("%.2f", m_base_nyc$calmar),   sprintf("%.2f%%", m_base_nyc$ann_vol),   sprintf("$%.2f", tax_base_nyc$terminal_wealth)),
  Current_NYC      = c(sprintf("%+.2f%%", m_curr_nyc$cagr),   sprintf("-%.2f%%", m_curr_nyc$max_dd),   sprintf("%.2f", m_curr_nyc$sharpe),   sprintf("%.2f", m_curr_nyc$sortino),   sprintf("%.2f", m_curr_nyc$calmar),   sprintf("%.2f%%", m_curr_nyc$ann_vol),   sprintf("$%.2f", tax_curr_nyc$terminal_wealth)),
  QQQ_Gross        = c(sprintf("%+.2f%%", m_qqq_gross$cagr),  sprintf("-%.2f%%", m_qqq_gross$max_dd),  sprintf("%.2f", m_qqq_gross$sharpe),  sprintf("%.2f", m_qqq_gross$sortino),  sprintf("%.2f", m_qqq_gross$calmar),  sprintf("%.2f%%", m_qqq_gross$ann_vol),  sprintf("$%.2f", qqq_gross$terminal_wealth)),
  VTI_Gross        = c(sprintf("%+.2f%%", m_vti_gross$cagr),  sprintf("-%.2f%%", m_vti_gross$max_dd),  sprintf("%.2f", m_vti_gross$sharpe),  sprintf("%.2f", m_vti_gross$sortino),  sprintf("%.2f", m_vti_gross$calmar),  sprintf("%.2f%%", m_vti_gross$ann_vol),  sprintf("$%.2f", vti_gross$terminal_wealth)),
  stringsAsFactors = FALSE
)

cat("\n========================================================================================\n")
cat("          40-YEAR CONTINUOUS COMPARISON: BASELINE vs CURRENT UPGRADED MODEL\n")
cat("========================================================================================\n")
print(scorecard_comp)

cat("\nDecade-by-Decade Breakdown:\n")
print(decade_df)

write.csv(scorecard_comp, file.path(OUTPUT_DIR, "comparison_40yr_baseline_vs_current_scorecard.csv"), row.names = FALSE)
write.csv(decade_df,      file.path(OUTPUT_DIR, "comparison_40yr_decade_breakdown.csv"), row.names = FALSE)

saveRDS(list(
  dates        = full_dates,
  ret_baseline = ret_baseline,
  ret_current  = ret_current,
  scorecard    = scorecard_comp,
  decade_df    = decade_df
), file.path(OUTPUT_DIR, "comparison_40yr_baseline_vs_current_results.rds"))

cat(sprintf("\n[40Yr-Engine] Outputs saved successfully to %s\n", OUTPUT_DIR))
