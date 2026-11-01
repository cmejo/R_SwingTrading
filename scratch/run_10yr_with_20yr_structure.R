#!/usr/bin/env Rscript
# ==============================================================================
# scratch/run_10yr_with_20yr_structure.R
# 10-Year Historical Backtest (2016-2026, 2,520 Trading Sessions)
# Exactly Mirroring the 20-Year Simulation Architecture
# ==============================================================================

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(TTR)
  library(jsonlite)
})

source("R/08_metrics.R")

cat("========================================================================================\n")
cat(" 10-YEAR HISTORICAL BACKTEST WITH 20-YEAR SIMULATION ARCHITECTURE (2016-2026)\n")
cat(" Period: Exactly 2,520 Trading Sessions | Starting Capital: $10,000.00\n")
cat(" Comparing: 1.5x Dynamic Leverage vs. 1.0x Cash Baseline vs. Benchmarks\n")
cat("========================================================================================\n\n")

HIST_DIR   <- "data/data_historical"
STARTING_CAPITAL <- 10000.00

# 1. Load spliced macro indices
spy_raw  <- readRDS(file.path(HIST_DIR, "data_spy.rds"))
qqq_raw  <- readRDS(file.path(HIST_DIR, "data_qqq.rds"))
vti_raw  <- readRDS(file.path(HIST_DIR, "data_vti.rds"))
irx_raw  <- readRDS(file.path(HIST_DIR, "data_^irx.rds"))
gld_path <- file.path(HIST_DIR, "data_gld_full.rds")
gld_raw  <- if (file.exists(gld_path)) readRDS(gld_path) else readRDS("data/data_gld.rds")

# Align common trading dates for the full historical period
common_dates <- sort(intersect(index(spy_raw), intersect(index(qqq_raw), index(vti_raw))))
# Select exactly the last 2,520 trading days (10 full trading years: October 2016 to October 2026)
eval_dates_10yr <- tail(common_dates, 2520)
n_days <- length(eval_dates_10yr)

cat(sprintf("[1. Data Splicing] Horizon: %s to %s (%d trading sessions)\n",
            as.character(eval_dates_10yr[1]), as.character(eval_dates_10yr[n_days]), n_days))

spy_cl <- Cl(spy_raw)[eval_dates_10yr]
qqq_cl <- Cl(qqq_raw)[eval_dates_10yr]
vti_cl <- Cl(vti_raw)[eval_dates_10yr]
gld_cl <- Cl(gld_raw)[eval_dates_10yr]

spy_ret <- (spy_cl / lag.xts(spy_cl, 1)) - 1; spy_ret[is.na(spy_ret)] <- 0
qqq_ret <- (qqq_cl / lag.xts(qqq_cl, 1)) - 1; qqq_ret[is.na(qqq_ret)] <- 0
vti_ret <- (vti_cl / lag.xts(vti_cl, 1)) - 1; vti_ret[is.na(vti_ret)] <- 0
gld_ret <- (gld_cl / lag.xts(gld_cl, 1)) - 1; gld_ret[is.na(gld_ret)] <- 0

spy_sub <- as.numeric(spy_ret)
qqq_sub <- as.numeric(qqq_ret)
vti_sub <- as.numeric(vti_ret)
gld_sub <- as.numeric(gld_ret)
b70_30  <- 0.70 * qqq_sub + 0.30 * vti_sub

# Risk-free rate
irx_yield <- na.locf(irx_raw[, 4], na.rm = FALSE)
irx_yield[is.na(irx_yield)] <- 2.0
rf_daily <- xts((as.numeric(irx_yield) / 100) / 252, order.by = index(irx_raw))
rf_sub   <- as.numeric(rf_daily[eval_dates_10yr])
rf_sub[is.na(rf_sub)] <- 0.02 / 252

# Macro Regimes:
# Macro Bull: SPY >= SMA(SPY, 200)
spy_sma200 <- SMA(Cl(spy_raw), n = 200)[eval_dates_10yr]
macro_bull <- as.numeric(spy_cl) >= as.numeric(spy_sma200)
macro_bull[is.na(macro_bull)] <- TRUE

# QQQ Bear: QQQ < SMA(QQQ, 50)
qqq_sma50 <- SMA(Cl(qqq_raw), n = 50)[eval_dates_10yr]
qqq_bear  <- as.numeric(qqq_cl) < as.numeric(qqq_sma50)
qqq_bear[is.na(qqq_bear)] <- FALSE

# GLD Bear: GLD < SMA(GLD, 50)
gld_sma50 <- SMA(Cl(gld_raw), n = 50)[eval_dates_10yr]
gld_bear  <- as.numeric(gld_cl) < as.numeric(gld_sma50)
gld_bear[is.na(gld_bear)] <- TRUE

dual_bear <- (qqq_bear & gld_bear)

# Inverse hedge return proxy: -3x QQQ minus borrow drag, capped +/-15%
inv_hedge_daily <- pmin(0.15, pmax(-0.15, -3.0 * qqq_sub - (0.0095 / 252)))

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

# 2. Precompute stock indicators across universe (same as 20-year backtest)
cat("[2. Indexing stock data profiles...]\n")
stock_files <- list.files(HIST_DIR, pattern = "^data_[a-z0-9_]+\\.rds$", full.names = TRUE)
bench_patterns <- c("data_\\^", "data_vti", "data_qqq", "data_spy", "data_vfinx", "data_gspc", "data_ndx", "data_irx", "data_gld")
for (bp in bench_patterns) stock_files <- stock_files[!grepl(bp, stock_files)]

stocks_list <- list()
for (f in stock_files) {
  sym <- toupper(sub("^data_", "", sub("\\.rds$", "", basename(f))))
  d <- tryCatch(readRDS(f), error = function(e) NULL)
  if (is.null(d) || nrow(d) < 150) next
  
  df_sub <- tryCatch(na.locf(na.locf(d, na.rm = FALSE), fromLast = TRUE), error = function(e) NULL)
  if (is.null(df_sub) || nrow(df_sub) < 100) next
  
  all_dates_stk <- as.Date(index(df_sub))
  cl_xts <- Cl(df_sub); op_xts <- Op(df_sub); hi_xts <- Hi(df_sub); lo_xts <- Lo(df_sub)
  c_sub <- as.numeric(cl_xts); o_sub <- as.numeric(op_xts); h_sub <- as.numeric(hi_xts); l_sub <- as.numeric(lo_xts)
  nb <- length(c_sub)
  
  # 63-day RS vs SPY
  m_idx <- match(all_dates_stk, index(Cl(spy_raw)))
  valid_m <- !is.na(m_idx)
  spy_match <- numeric(length(all_dates_stk))
  spy_match[valid_m] <- as.numeric(Cl(spy_raw))[m_idx[valid_m]]
  spy_match[!valid_m] <- NA
  spy_match <- na.locf(na.locf(spy_match, na.rm = FALSE), fromLast = TRUE)
  
  spy_lag63 <- c(rep(spy_match[1], 63), head(spy_match, -63))
  stk_lag63 <- c(rep(c_sub[1], 63), head(c_sub, -63))
  rs_raw <- (c_sub / stk_lag63) - (spy_match / spy_lag63)
  rs_raw[is.na(rs_raw)] <- 0.0
  
  sma50_xts <- tryCatch(SMA(cl_xts, n = 50), error = function(e) cl_xts)
  sma50_v   <- as.numeric(sma50_xts)
  slope_slow <- (c_sub - as.numeric(lag.xts(cl_xts, 50))) / (50 * c_sub + 1e-5) * 100
  slope_slow[is.na(slope_slow)] <- 0.0
  
  atr_obj <- tryCatch(ATR(HLC(df_sub), n = 14)$atr, error = function(e) cl_xts * 0.02)
  atr_abs <- as.numeric(atr_obj)
  atr_abs[is.na(atr_abs) | atr_abs <= 0] <- c_sub[is.na(atr_abs) | atr_abs <= 0] * 0.02
  
  fast_fit <- as.numeric(tryCatch(SMA(cl_xts, n = 20), error = function(e) cl_xts))
  fast_fit[is.na(fast_fit)] <- c_sub[is.na(fast_fit)]
  atr_ext <- (c_sub - fast_fit) / (atr_abs + 1e-5)
  atr_ext[is.na(atr_ext)] <- 0.0
  
  vol_ann <- (atr_abs / c_sub) * sqrt(252)
  vol_ann[is.na(vol_ann) | vol_ann < 0.10] <- 0.25
  
  b_high <- pmax(o_sub, c_sub); b_low <- pmin(o_sub, c_sub)
  lw_r <- pmax(0, b_low - l_sub) / (atr_abs + 1e-5)
  uw_r <- pmax(0, h_sub - b_high) / (atr_abs + 1e-5)
  n_wick <- as.numeric(TTR::runMean(lw_r - uw_r, n = 10))
  n_wick[is.na(n_wick)] <- 0.0
  
  ret_1d <- c(0, diff(log(pmax(c_sub, 1e-4))))
  dn_sd  <- sqrt(pmax(0, as.numeric(TTR::runMean(pmin(0, ret_1d)^2, n = 20))) * 20)
  smom20 <- ((c_sub / as.numeric(lag.xts(cl_xts, 20))) - 1.0) / (dn_sd + 0.01)
  smom20[is.na(smom20)] <- 0.0
  
  net_m <- abs(c_sub - as.numeric(lag.xts(cl_xts, 20)))
  path  <- as.numeric(TTR::runSum(c(0, abs(diff(c_sub))), n = 20))
  eff_r <- net_m / (path + 1e-4)
  eff_r[is.na(eff_r)] <- 0.30
  
  ext_damp <- exp(-pmax(0, atr_ext - 1.8)^2 / (2 * 1.2^2))
  slope_norm <- tanh(slope_slow / 0.25) * ext_damp
  rs_norm    <- tanh(rs_raw / 0.15) * ext_damp
  
  p_cl    <- c(0, c_sub[1:(nb - 1)])
  p_sma50 <- c(0, sma50_v[1:(nb - 1)])
  p_rs    <- c(0, rs_norm[1:(nb - 1)])
  p_slp   <- c(0, slope_norm[1:(nb - 1)])
  p_ext   <- c(0, atr_ext[1:(nb - 1)])
  p_nw    <- c(0, n_wick[1:(nb - 1)])
  p_eff   <- c(0, eff_r[1:(nb - 1)])
  
  trigger <- ifelse(!is.na(p_cl) & !is.na(p_sma50) & p_cl > p_sma50 &
                    !is.na(p_rs) & p_rs > 0.01 & p_ext <= 3.0, 1, 0)
  trigger[is.na(trigger)] <- 0
  
  prob_est <- 0.50 + 0.12 * pmin(1.0, pmax(0.0, p_rs * 1.5)) +
                     0.08 * pmin(1.0, pmax(0.0, p_slp * 1.5)) +
                     0.03 * pmin(1.0, pmax(-0.5, p_nw * 0.5)) +
                     0.03 * pmin(1.0, pmax(0.0, (p_eff - 0.30) * 1.5))
  prob_est[trigger == 0] <- 0.50
  
  stocks_list[[sym]] <- list(
    dates     = all_dates_stk,
    open      = o_sub,
    high      = h_sub,
    low       = l_sub,
    close     = c_sub,
    atr       = atr_abs,
    vol       = vol_ann,
    rs        = rs_norm,
    prob      = prob_est,
    atr_ext   = p_ext,
    sortino   = smom20,
    eff_ratio = eff_r,
    net_wick  = n_wick,
    sector    = get_sym_sector(sym)
  )
}
cat(sprintf("Indexed %d stocks for the 10-year test.\n\n", length(stocks_list)))

# 3. Exact Simulation Engine from 20-Year Backtest
run_exact_sim <- function(stocks_list,
                          eval_dates,
                          max_positions = 5,
                          max_per_sector = 2,
                          t1_r = 2.0,
                          t2_r = 4.0,
                          t1_scale_pct = 0.35,
                          t2_scale_pct = 0.35,
                          stop_atr_mult = 2.0,
                          chand_mult = 2.5,
                          leverage = 1.5,
                          use_bear_hedge = TRUE
) {
  n_sub <- length(eval_dates)
  
  portfolio_equity   <- numeric(n_sub)
  portfolio_cash     <- numeric(n_sub)
  daily_return_vec   <- numeric(n_sub)
  margin_interest    <- numeric(n_sub)
  
  current_equity <- STARTING_CAPITAL
  current_cash   <- STARTING_CAPITAL
  peak_equity    <- STARTING_CAPITAL
  
  open_positions <- list()
  trade_log      <- list()
  
  in_hedge_ep    <- FALSE
  hedge_day      <- 0
  hedge_cum      <- 1.0
  hedge_stopped  <- FALSE
  
  slot_budget <- 1.0 / max_positions
  friction_rate <- 0.0010
  
  portfolio_equity[1] <- current_equity
  portfolio_cash[1]   <- current_cash
  
  pending_buy_orders <- list()
  
  for (t in 2:n_sub) {
    curr_d <- eval_dates[t]
    is_macro_bull <- macro_bull[t - 1]
    is_dual_bear  <- dual_bear[t - 1]
    
    # 1. Morning executions at Open
    if (length(pending_buy_orders) > 0 && length(open_positions) < max_positions) {
      for (ord in pending_buy_orders) {
        if (length(open_positions) >= max_positions) break
        stk <- stocks_list[[ord$symbol]]
        idx_t <- match(curr_d, stk$dates)
        if (is.na(idx_t)) next
        
        open_px <- stk$open[idx_t]
        atr_val <- stk$atr[idx_t]
        if (is.na(open_px) || open_px <= 0 || is.na(atr_val) || atr_val <= 0) next
        
        exec_px <- open_px * (1 + friction_rate)
        target_dollar <- current_equity * slot_budget * ord$size_mult * leverage
        max_single_stock_margin <- current_equity * 0.50 * leverage
        target_dollar <- min(target_dollar, max_single_stock_margin)
        
        n_shares <- floor(target_dollar / exec_px)
        if (n_shares <= 0) next
        
        actual_cost <- n_shares * exec_px
        current_cash <- current_cash - actual_cost
        
        stop_loss_dist <- stop_atr_mult * atr_val
        stop_loss_px   <- exec_px - stop_loss_dist
        t1_px          <- exec_px + t1_r * stop_loss_dist
        t2_px          <- exec_px + t2_r * stop_loss_dist
        
        open_positions[[length(open_positions) + 1]] <- list(
          symbol          = ord$symbol,
          sector          = ord$sector,
          entry_date      = curr_d,
          entry_price     = exec_px,
          shares          = n_shares,
          original_shares = n_shares,
          shares_pct      = 1.0,
          stop_price      = stop_loss_px,
          initial_stop_dist = stop_loss_dist,
          t1_price        = t1_px,
          t2_price        = t2_px,
          atr             = atr_val,
          days_held       = 1,
          t1_hit          = FALSE,
          t2_hit          = FALSE,
          is_runner       = FALSE,
          high_water_mark = exec_px
        )
      }
      pending_buy_orders <- list()
    }
    
    # 2. Intraday monitoring
    surviving_positions <- list()
    for (pos in open_positions) {
      stk <- stocks_list[[pos$symbol]]
      idx_t <- match(curr_d, stk$dates)
      if (is.na(idx_t)) {
        surviving_positions[[length(surviving_positions) + 1]] <- pos
        next
      }
      
      high_px <- stk$high[idx_t]; low_px <- stk$low[idx_t]; close_px <- stk$close[idx_t]
      open_px <- stk$open[idx_t]; atr_val <- stk$atr[idx_t]
      
      if (pos$entry_date != curr_d) pos$days_held <- pos$days_held + 1
      
      high_wm <- max(pos$high_water_mark, high_px)
      chand_stop <- high_wm - chand_mult * atr_val
      
      # Stop loss check
      if (low_px <= pos$stop_price) {
        exit_px <- min(open_px, pos$stop_price)
        proceeds <- pos$shares * exit_px * (1 - friction_rate)
        trade_pnl <- (exit_px * (1 - friction_rate) - pos$entry_price) * pos$shares
        current_cash <- current_cash + proceeds
        trade_log[[length(trade_log) + 1]] <- list(
          symbol = pos$symbol, entry_date = pos$entry_date, exit_date = curr_d,
          entry_price = pos$entry_price, exit_price = exit_px,
          shares = pos$shares, pnl = trade_pnl, reason = "STOP_LOSS",
          days_held = pos$days_held, win = (trade_pnl > 0)
        )
        next
      }
      
      # Tier 1 target
      if (!pos$t1_hit && high_px >= pos$t1_price) {
        pos$t1_hit <- TRUE
        shares_to_sell <- pos$original_shares * t1_scale_pct
        shares_to_sell <- min(shares_to_sell, pos$shares)
        exit_px <- pos$t1_price
        proceeds <- shares_to_sell * exit_px * (1 - friction_rate)
        trade_pnl <- (exit_px * (1 - friction_rate) - pos$entry_price) * shares_to_sell
        current_cash <- current_cash + proceeds
        pos$shares <- pos$shares - shares_to_sell
        pos$shares_pct <- pos$shares / pos$original_shares
        pos$stop_price <- max(pos$entry_price, chand_stop)
        trade_log[[length(trade_log) + 1]] <- list(
          symbol = pos$symbol, entry_date = pos$entry_date, exit_date = curr_d,
          entry_price = pos$entry_price, exit_price = exit_px,
          shares = shares_to_sell, pnl = trade_pnl, reason = "TARGET_1_SCALE",
          days_held = pos$days_held, win = TRUE
        )
      }
      
      # Tier 2 target
      if (pos$t1_hit && !pos$t2_hit && high_px >= pos$t2_price) {
        pos$t2_hit <- TRUE
        pos$is_runner <- TRUE
        shares_to_sell <- min(pos$original_shares * t2_scale_pct, pos$shares)
        exit_px <- pos$t2_price
        proceeds <- shares_to_sell * exit_px * (1 - friction_rate)
        trade_pnl <- (exit_px * (1 - friction_rate) - pos$entry_price) * shares_to_sell
        current_cash <- current_cash + proceeds
        pos$shares <- pos$shares - shares_to_sell
        pos$shares_pct <- pos$shares / pos$original_shares
        pos$stop_price <- max(pos$stop_price, chand_stop)
        trade_log[[length(trade_log) + 1]] <- list(
          symbol = pos$symbol, entry_date = pos$entry_date, exit_date = curr_d,
          entry_price = pos$entry_price, exit_price = exit_px,
          shares = shares_to_sell, pnl = trade_pnl, reason = "TARGET_2_SCALE",
          days_held = pos$days_held, win = TRUE
        )
      }
      
      if (pos$t1_hit) pos$stop_price <- max(pos$stop_price, chand_stop)
      
      # Max hold: 7 days for base, 20 days for runner
      max_hold <- if (pos$is_runner) 20 else 7
      if (pos$days_held >= max_hold) {
        exit_px <- close_px * (1 - friction_rate)
        proceeds <- pos$shares * exit_px
        trade_pnl <- (exit_px - pos$entry_price) * pos$shares
        current_cash <- current_cash + proceeds
        trade_log[[length(trade_log) + 1]] <- list(
          symbol = pos$symbol, entry_date = pos$entry_date, exit_date = curr_d,
          entry_price = pos$entry_price, exit_price = exit_px,
          shares = pos$shares, pnl = trade_pnl, reason = "TIME_STOP",
          days_held = pos$days_held, win = (trade_pnl > 0)
        )
        next
      }
      
      pos$high_water_mark <- high_wm
      surviving_positions[[length(surviving_positions) + 1]] <- pos
    }
    open_positions <- surviving_positions
    
    # 3. Mark to market
    total_stock_value <- 0.0
    for (pos in open_positions) {
      stk <- stocks_list[[pos$symbol]]
      idx_t <- match(curr_d, stk$dates)
      if (!is.na(idx_t)) {
        total_stock_value <- total_stock_value + pos$shares * stk$close[idx_t]
      } else {
        total_stock_value <- total_stock_value + pos$shares * pos$entry_price
      }
    }
    
    borrowed_debt <- max(0, -current_cash)
    daily_margin_interest <- borrowed_debt * (0.070 / 252)
    idle_cash <- max(0, current_cash)
    daily_cash_yield <- idle_cash * rf_sub[t]
    
    # Defensive hedge sleeve
    hedge_pnl <- 0.0
    if (use_bear_hedge && is_dual_bear && idle_cash > 0) {
      if (!in_hedge_ep) {
        in_hedge_ep   <- TRUE
        hedge_day     <- 0
        hedge_cum     <- 1.0
        hedge_stopped <- FALSE
      }
      hedge_day <- hedge_day + 1
      if (hedge_day > 15) hedge_stopped <- TRUE
      
      if (!hedge_stopped) {
        h_ret <- inv_hedge_daily[t]
        hedge_pnl <- idle_cash * h_ret
        hedge_cum <- hedge_cum * (1 + h_ret)
        if ((hedge_cum - 1.0) <= -0.10) {
          hedge_stopped <- TRUE
        }
      }
    } else {
      in_hedge_ep <- FALSE
    }
    
    current_equity <- total_stock_value + current_cash - daily_margin_interest + daily_cash_yield + hedge_pnl
    if (current_equity > peak_equity) peak_equity <- current_equity
    
    portfolio_equity[t] <- current_equity
    portfolio_cash[t]   <- current_cash
    margin_interest[t]  <- daily_margin_interest
    
    prev_eq <- portfolio_equity[t - 1]
    daily_return_vec[t] <- if (prev_eq > 0) (current_equity - prev_eq) / prev_eq else 0
    
    # 4. Post-close signal generation
    empty_slots <- max_positions - length(open_positions)
    if (empty_slots > 0 && t < n_sub) {
      cands_syms   <- character(0)
      cands_scores <- numeric(0)
      cands_sector <- character(0)
      cands_sizes  <- numeric(0)
      held_syms    <- sapply(open_positions, function(p) p$symbol)
      eff_p_hurdle <- if (macro_bull[t]) 0.58 else 0.65
      
      for (sym in names(stocks_list)) {
        if (sym %in% held_syms) next
        stk <- stocks_list[[sym]]
        idx_t <- match(curr_d, stk$dates)
        if (is.na(idx_t) || idx_t < 150) next
        
        p_up    <- stk$prob[idx_t]
        rs_val  <- stk$rs[idx_t]
        vol_ann <- stk$vol[idx_t]
        atr_ext <- stk$atr_ext[idx_t]
        smom    <- stk$sortino[idx_t]
        eff_r   <- stk$eff_ratio[idx_t]
        nwick   <- stk$net_wick[idx_t]
        
        if (is.na(p_up) || is.na(rs_val) || is.na(vol_ann)) next
        if (!is.na(atr_ext) && atr_ext > 3.0) next
        
        if (p_up >= eff_p_hurdle && rs_val > 0) {
          sm_boost <- if (!is.na(smom)) pmax(0, (smom - 0.80) * 0.06) else 0
          er_boost <- if (!is.na(eff_r)) pmax(0, (eff_r - 0.30) * 0.20) else 0
          nw_boost <- if (!is.na(nwick)) pmax(0, nwick * 0.08) else 0
          
          score <- p_up * (1.0 + rs_val + sm_boost + er_boost + nw_boost)
          eff_vol <- pmax(0.15, vol_ann) / (1.0 + 0.20 * er_boost + 0.10 * sm_boost)
          base_mult <- pmin(1.25, 0.35 / eff_vol)
          
          cands_syms   <- c(cands_syms, sym)
          cands_scores <- c(cands_scores, score)
          cands_sector <- c(cands_sector, stk$sector)
          cands_sizes  <- c(cands_sizes, base_mult)
        }
      }
      
      if (length(cands_syms) > 0) {
        ranked_idx <- order(-cands_scores)
        top_syms   <- cands_syms[ranked_idx]
        top_secs   <- cands_sector[ranked_idx]
        top_sizes  <- cands_sizes[ranked_idx]
        
        orders_count <- 0
        held_sectors <- if (length(open_positions) > 0) sapply(open_positions, function(p) p$sector) else character(0)
        sec_counts   <- if (length(held_sectors) > 0) table(held_sectors) else integer(0)
        
        for (k in seq_along(top_syms)) {
          if (orders_count >= empty_slots) break
          sym_k <- top_syms[k]
          sec_k <- top_secs[k]
          cur_s_cnt <- if (!is.na(sec_counts[sec_k])) as.integer(sec_counts[sec_k]) else 0L
          if (cur_s_cnt >= max_per_sector) next
          
          pending_buy_orders[[length(pending_buy_orders) + 1]] <- list(
            symbol    = sym_k,
            sector    = sec_k,
            size_mult = top_sizes[k]
          )
          sec_counts[sec_k] <- cur_s_cnt + 1L
          orders_count <- orders_count + 1
        }
      }
    }
  }
  
  term_w <- tail(portfolio_equity, 1)
  n_yr   <- n_sub / 252
  cagr   <- ((term_w / STARTING_CAPITAL)^(1 / n_yr) - 1) * 100
  cum_m  <- cummax(portfolio_equity)
  dd     <- (portfolio_equity - cum_m) / cum_m
  max_dd <- abs(min(dd)) * 100
  ann_v  <- sd(daily_return_vec) * sqrt(252) * 100
  rf_d   <- rf_sub
  ex_ret <- daily_return_vec - rf_d
  sharpe <- (mean(ex_ret) / sd(daily_return_vec)) * sqrt(252)
  dn_ret <- daily_return_vec[daily_return_vec < 0]
  sortino<- if (length(dn_ret) > 0) (mean(ex_ret) / sd(dn_ret)) * sqrt(252) else NA
  calmar <- cagr / max_dd
  win_r  <- if (length(trade_log) > 0) sum(sapply(trade_log, function(x) x$win)) / length(trade_log) * 100 else NA
  
  return(list(
    terminal      = term_w,
    cagr          = cagr,
    max_dd        = max_dd,
    annual_vol    = ann_v,
    sharpe        = sharpe,
    sortino       = sortino,
    calmar        = calmar,
    win_rate      = win_r,
    trades_count  = length(trade_log),
    equity_curve  = portfolio_equity,
    daily_returns = daily_return_vec
  ))
}

cat("[3. Simulating 10-Year Backtests with 20-Year Architecture...]\n\n")

# Model 1: 1.5x Dynamic Leverage + Smart Dual-Bear Hedge (Exact 20-Year Structure)
cat("Running Model 1: 1.5x Dynamic Leverage + Smart Dual-Bear Hedge...\n")
res_10yr_15x <- run_exact_sim(stocks_list, eval_dates_10yr, leverage = 1.5, use_bear_hedge = TRUE)

# Model 2: 1.0x Cash / Unleveraged + Smart Dual-Bear Hedge
cat("Running Model 2: 1.0x Cash (Unleveraged) + Smart Dual-Bear Hedge...\n")
res_10yr_10x <- run_exact_sim(stocks_list, eval_dates_10yr, leverage = 1.0, use_bear_hedge = TRUE)

# Model 3: 1.0x Cash / Unleveraged (Unhedged)
cat("Running Model 3: 1.0x Cash (Unleveraged, No Bear Hedge)...\n")
res_10yr_nohedge <- run_exact_sim(stocks_list, eval_dates_10yr, leverage = 1.0, use_bear_hedge = FALSE)

# Benchmarks
calc_bench <- function(ret_vec) {
  cum_ret <- cumprod(1 + ret_vec)
  cagr <- ((tail(cum_ret, 1))^(252 / n_days) - 1) * 100
  cum_max <- cummax(cum_ret)
  dd <- (cum_ret - cum_max) / cum_max
  max_dd <- abs(min(dd)) * 100
  ann_vol <- sd(ret_vec) * sqrt(252) * 100
  ex_ret <- ret_vec - rf_sub
  sharpe <- (mean(ex_ret) / sd(ret_vec)) * sqrt(252)
  dn_ret <- ret_vec[ret_vec < 0]
  sortino <- if (length(dn_ret) > 0) (mean(ex_ret) / sd(dn_ret)) * sqrt(252) else NA
  calmar <- cagr / max_dd
  terminal <- STARTING_CAPITAL * tail(cum_ret, 1)
  list(terminal = terminal, cagr = cagr, max_dd = max_dd, annual_vol = ann_vol, sharpe = sharpe, sortino = sortino, calmar = calmar)
}

bench_qqq <- calc_bench(qqq_sub)
bench_b70 <- calc_bench(b70_30)
bench_spy <- calc_bench(spy_sub)

# Master 10-Year Scorecard
master_10yr_df <- data.frame(
  Strategy = c(
    "1. Current Strategy (1.5x Leverage + Dual-Bear Hedge)",
    "2. Unleveraged Model (1.0x Cash + Dual-Bear Hedge)",
    "3. Unleveraged & Unhedged (1.0x Cash, No Hedge)",
    "4. Buy & Hold QQQ (100%)",
    "5. Buy & Hold 70/30 QQQ/VTI",
    "6. Buy & Hold SPY (100%)"
  ),
  Terminal_Gross = c(
    sprintf("$%s", format(round(res_10yr_15x$terminal, 2), big.mark = ",")),
    sprintf("$%s", format(round(res_10yr_10x$terminal, 2), big.mark = ",")),
    sprintf("$%s", format(round(res_10yr_nohedge$terminal, 2), big.mark = ",")),
    sprintf("$%s", format(round(bench_qqq$terminal, 2), big.mark = ",")),
    sprintf("$%s", format(round(bench_b70$terminal, 2), big.mark = ",")),
    sprintf("$%s", format(round(bench_spy$terminal, 2), big.mark = ","))
  ),
  CAGR = c(
    sprintf("%+.2f%%", res_10yr_15x$cagr),
    sprintf("%+.2f%%", res_10yr_10x$cagr),
    sprintf("%+.2f%%", res_10yr_nohedge$cagr),
    sprintf("%+.2f%%", bench_qqq$cagr),
    sprintf("%+.2f%%", bench_b70$cagr),
    sprintf("%+.2f%%", bench_spy$cagr)
  ),
  Max_Drawdown = c(
    sprintf("-%.2f%%", res_10yr_15x$max_dd),
    sprintf("-%.2f%%", res_10yr_10x$max_dd),
    sprintf("-%.2f%%", res_10yr_nohedge$max_dd),
    sprintf("-%.2f%%", bench_qqq$max_dd),
    sprintf("-%.2f%%", bench_b70$max_dd),
    sprintf("-%.2f%%", bench_spy$max_dd)
  ),
  Sharpe = c(
    sprintf("%.2f", res_10yr_15x$sharpe),
    sprintf("%.2f", res_10yr_10x$sharpe),
    sprintf("%.2f", res_10yr_nohedge$sharpe),
    sprintf("%.2f", bench_qqq$sharpe),
    sprintf("%.2f", bench_b70$sharpe),
    sprintf("%.2f", bench_spy$sharpe)
  ),
  Sortino = c(
    sprintf("%.2f", res_10yr_15x$sortino),
    sprintf("%.2f", res_10yr_10x$sortino),
    sprintf("%.2f", res_10yr_nohedge$sortino),
    sprintf("%.2f", bench_qqq$sortino),
    sprintf("%.2f", bench_b70$sortino),
    sprintf("%.2f", bench_spy$sortino)
  ),
  Calmar = c(
    sprintf("%.2f", res_10yr_15x$calmar),
    sprintf("%.2f", res_10yr_10x$calmar),
    sprintf("%.2f", res_10yr_nohedge$calmar),
    sprintf("%.2f", bench_qqq$calmar),
    sprintf("%.2f", bench_b70$calmar),
    sprintf("%.2f", bench_spy$calmar)
  ),
  Annual_Vol = c(
    sprintf("%.2f%%", res_10yr_15x$annual_vol),
    sprintf("%.2f%%", res_10yr_10x$annual_vol),
    sprintf("%.2f%%", res_10yr_nohedge$annual_vol),
    sprintf("%.2f%%", bench_qqq$annual_vol),
    sprintf("%.2f%%", bench_b70$annual_vol),
    sprintf("%.2f%%", bench_spy$annual_vol)
  )
)

cat("========================================================================================\n")
cat(" 10-YEAR MASTER PERFORMANCE COMPARISON (2016-2026):\n")
cat("========================================================================================\n")
print(master_10yr_df)

# Calendar Returns Breakdown across the 10 years
years <- sort(unique(format(eval_dates_10yr, "%Y")))
yearly_mat <- matrix(NA_real_, nrow = length(years), ncol = 6)
rownames(yearly_mat) <- years
colnames(yearly_mat) <- c("Strategy_1.5x", "Strategy_1.0x", "Unhedged_1.0x", "QQQ_100", "Blend_70_30", "SPY_100")

for (i in seq_along(years)) {
  yr <- years[i]
  idx_yr <- which(format(eval_dates_10yr, "%Y") == yr)
  calc_yr <- function(r) (prod(1 + r[idx_yr]) - 1) * 100
  yearly_mat[i, 1] <- calc_yr(res_10yr_15x$daily_returns)
  yearly_mat[i, 2] <- calc_yr(res_10yr_10x$daily_returns)
  yearly_mat[i, 3] <- calc_yr(res_10yr_nohedge$daily_returns)
  yearly_mat[i, 4] <- calc_yr(qqq_sub)
  yearly_mat[i, 5] <- calc_yr(b70_30)
  yearly_mat[i, 6] <- calc_yr(spy_sub)
}

df_yearly <- as.data.frame(yearly_mat)
df_yearly_formatted <- as.data.frame(lapply(df_yearly, function(col) sprintf("%+.2f%%", col)))
rownames(df_yearly_formatted) <- years

cat("\n========================================================================================\n")
cat(" 10-YEAR CALENDAR YEAR RETURNS (2016-2026):\n")
cat("========================================================================================\n")
print(df_yearly_formatted)

write.csv(master_10yr_df, "output/ten_year_with_20yr_structure_scorecard.csv", row.names = FALSE)
write.csv(df_yearly, "output/ten_year_with_20yr_structure_calendar_returns.csv", row.names = TRUE)

cat("\nArtifacts saved to:\n - output/ten_year_with_20yr_structure_scorecard.csv\n - output/ten_year_with_20yr_structure_calendar_returns.csv\n")
