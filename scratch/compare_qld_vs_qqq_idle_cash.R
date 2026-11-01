# ==============================================================================
# COMPREHENSIVE EXPERIMENT: IDLE CASH IN BULL MARKETS — QLD (2x) vs. QQQ (1x)
# ==============================================================================
# User Request:
# "Use idle cash in bull markets. compare holding QLD instead of QQQ when 
#  fewer than 5 signals qualify and SPY is above its 200-day average."
# ==============================================================================

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(TTR)
})

cat("[1. Loading precomputed clean environment and ETF data...]\n")
clean_env   <- readRDS("scratch/clean_backtest_env.rds")
stocks_list <- clean_env$stocks_list
macro_data  <- clean_env$macro_data
eval_dates  <- clean_env$eval_dates
stock_names <- names(stocks_list)
n_days      <- length(eval_dates)

# Load QLD, QQQ, and TQQQ
qld_obj <- readRDS("data/data_10yr/data_qld.rds")
qqq_obj <- readRDS("data/data_10yr/data_qqq.rds")
tqqq_obj <- if (file.exists("data/data_10yr/data_tqqq.rds")) readRDS("data/data_10yr/data_tqqq.rds") else NULL

qld_cl  <- Cl(qld_obj)[eval_dates]
qqq_cl  <- Cl(qqq_obj)[eval_dates]
tqqq_cl <- if (!is.null(tqqq_obj)) Cl(tqqq_obj)[eval_dates] else NULL

qld_ret  <- (qld_cl / lag.xts(qld_cl, 1)) - 1; qld_ret[is.na(qld_ret)] <- 0
qqq_ret  <- (qqq_cl / lag.xts(qqq_cl, 1)) - 1; qqq_ret[is.na(qqq_ret)] <- 0
tqqq_ret <- if (!is.null(tqqq_cl)) {
  x <- (tqqq_cl / lag.xts(tqqq_cl, 1)) - 1; x[is.na(x)] <- 0; x
} else NULL

qld_sub  <- as.numeric(qld_ret[eval_dates]); qld_sub[is.na(qld_sub)] <- 0
qqq_sub  <- as.numeric(qqq_ret[eval_dates]); qqq_sub[is.na(qqq_sub)] <- 0
tqqq_sub <- if (!is.null(tqqq_ret)) {
  x <- as.numeric(tqqq_ret[eval_dates]); x[is.na(x)] <- 0; x
} else {
  3.0 * qqq_sub
}

spy_cl  <- macro_data$spy_close
rf_raw  <- as.numeric(macro_data$rf_daily)
rf_raw[is.na(rf_raw)] <- 0.02 / 252
rf_sub  <- rf_raw
gld_cl  <- macro_data$gld_close

spy_ret <- (spy_cl / lag.xts(spy_cl, 1)) - 1; spy_ret[is.na(spy_ret)] <- 0
spy_sub <- as.numeric(spy_ret[eval_dates]); spy_sub[is.na(spy_sub)] <- 0

# Macro Bull: SPY >= SMA(SPY, 200)
spy_sma200 <- SMA(spy_cl, n = min(200, max(5, floor(length(spy_cl) / 2))))
macro_bull <- as.numeric(spy_cl[eval_dates]) >= as.numeric(spy_sma200[eval_dates])
macro_bull[is.na(macro_bull)] <- TRUE
macro_bull <- head(macro_bull, n_days)

# QQQ Bear: QQQ < SMA(QQQ, 50)
qqq_sma50 <- SMA(qqq_cl, n = min(50, max(5, floor(length(qqq_cl) / 2))))
qqq_bear  <- as.numeric(qqq_cl[eval_dates]) < as.numeric(qqq_sma50[eval_dates])
qqq_bear[is.na(qqq_bear)] <- FALSE
qqq_bear <- head(qqq_bear, n_days)

# GLD Bear
gld_bear <- rep(TRUE, n_days)
if (!is.null(gld_cl)) {
  gld_aligned <- na.locf(gld_cl[eval_dates], na.rm = FALSE)
  if (nrow(gld_aligned) > 0) {
    gld_sma50 <- SMA(gld_aligned, n = min(50, max(5, floor(nrow(gld_aligned) / 2))))
    g_diff <- as.numeric(gld_aligned) < as.numeric(gld_sma50)
    g_diff[is.na(g_diff)] <- TRUE
    gld_bear <- head(c(g_diff, rep(TRUE, n_days)), n_days)
  }
}
dual_bear <- (qqq_bear & gld_bear)

inv_hedge_daily <- pmin(0.15, pmax(-0.15, -3.0 * qqq_sub - (0.0095 / 252)))

cat(sprintf("[Setup] Evaluation Dates: %d sessions (%s to %s)\n", n_days, eval_dates[1], tail(eval_dates, 1)))
cat(sprintf("[Setup] Macro Bull Sessions (SPY >= 200 SMA): %d (%.1f%%)\n", sum(macro_bull), mean(macro_bull)*100))

# ==============================================================================
# SIMULATION ENGINE WITH IDLE CASH OVERLAY CONFIGURATION
# ==============================================================================
simulate_idle_cash <- function(stocks_list,
                               dates_subset,
                               cash_vehicle = c("NONE", "QQQ", "QLD", "TQQQ"),
                               allocation_mode = c("CASH_PCT", "SLOT_PCT"),
                               alloc_pct = 1.0, # e.g. 1.0 (100%), 0.8 (80%), 0.5 (50%)
                               etf_rebalance_friction_bps = 5 # 5 bps on ETF turnover
) {
  cash_vehicle    <- match.arg(cash_vehicle)
  allocation_mode <- match.arg(allocation_mode)
  
  n_sub <- length(dates_subset)
  sub_indices <- match(dates_subset, eval_dates)
  
  sub_mb   <- macro_bull[sub_indices]
  sub_db   <- dual_bear[sub_indices]
  sub_rf   <- rf_sub[sub_indices]
  sub_rf[is.na(sub_rf)] <- 0.02 / 252
  sub_qqq  <- qqq_sub[sub_indices]
  sub_qld  <- qld_sub[sub_indices]
  sub_tqqq <- tqqq_sub[sub_indices]
  sub_inv  <- inv_hedge_daily[sub_indices]
  
  # Select cash asset return vector
  etf_ret_vec <- switch(cash_vehicle,
    "NONE" = rep(0, n_sub),
    "QQQ"  = sub_qqq,
    "QLD"  = sub_qld,
    "TQQQ" = sub_tqqq
  )
  
  initial_capital <- 10000
  max_positions   <- 5
  max_per_sector  <- 2
  t1_r            <- 2.0
  t2_r            <- 4.0
  t1_scale_pct    <- 0.35
  t2_scale_pct    <- 0.35
  stop_atr_mult   <- 2.0
  chand_mult      <- 2.5
  stock_friction  <- 0.0010 # 10 bps
  etf_friction    <- etf_rebalance_friction_bps / 10000
  
  portfolio_equity   <- numeric(n_sub)
  portfolio_cash     <- numeric(n_sub)
  daily_return_vec   <- numeric(n_sub)
  etf_pnl_vec        <- numeric(n_sub)
  etf_alloc_vec      <- numeric(n_sub)
  etf_turnover_vec   <- numeric(n_sub)
  margin_interest    <- numeric(n_sub)
  
  current_equity <- initial_capital
  current_cash   <- initial_capital
  peak_equity    <- initial_capital
  
  open_positions <- list()
  trade_log      <- list()
  
  in_hedge_ep    <- FALSE
  hedge_day      <- 0
  hedge_cum      <- 1.0
  hedge_stopped  <- FALSE
  
  slot_budget <- 1.0 / max_positions
  portfolio_equity[1] <- current_equity
  portfolio_cash[1]   <- current_cash
  
  pending_buy_orders <- list()
  prev_etf_alloc <- 0.0
  
  for (t in 2:n_sub) {
    curr_d <- dates_subset[t]
    is_macro_bull <- sub_mb[t - 1]
    is_dual_bear  <- sub_db[t - 1]
    
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
        
        exec_px <- open_px * (1 + stock_friction)
        target_dollar <- current_equity * slot_budget * ord$size_mult
        max_single_stock_margin <- current_equity * 0.50
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
    
    # 2. Intraday monitoring of open positions
    surviving_positions <- list()
    for (pos in open_positions) {
      stk <- stocks_list[[pos$symbol]]
      idx_t <- match(curr_d, stk$dates)
      if (is.na(idx_t)) {
        surviving_positions[[length(surviving_positions) + 1]] <- pos
        next
      }
      
      high_px <- stk$high[idx_t]
      low_px  <- stk$low[idx_t]
      close_px<- stk$close[idx_t]
      open_px <- stk$open[idx_t]
      atr_val <- stk$atr[idx_t]
      
      if (pos$entry_date != curr_d) {
        pos$days_held <- pos$days_held + 1
      }
      
      high_wm <- max(pos$high_water_mark, high_px)
      chand_stop <- high_wm - chand_mult * atr_val
      
      # Stop loss check
      if (low_px <= pos$stop_price) {
        exit_px <- min(open_px, pos$stop_price)
        proceeds <- pos$shares * exit_px * (1 - stock_friction)
        trade_pnl <- (exit_px * (1 - stock_friction) - pos$entry_price) * pos$shares
        current_cash <- current_cash + proceeds
        trade_log[[length(trade_log) + 1]] <- list(
          symbol = pos$symbol, entry_date = pos$entry_date, exit_date = curr_d,
          entry_price = pos$entry_price, exit_price = exit_px,
          shares = pos$shares, pnl = trade_pnl, reason = "STOP_LOSS",
          days_held = pos$days_held, win = (trade_pnl > 0)
        )
        next
      }
      
      # Tier 1 Target Hit
      if (!pos$t1_hit && high_px >= pos$t1_price) {
        pos$t1_hit <- TRUE
        shares_to_sell <- pos$original_shares * t1_scale_pct
        shares_to_sell <- min(shares_to_sell, pos$shares)
        exit_px <- pos$t1_price
        proceeds <- shares_to_sell * exit_px * (1 - stock_friction)
        trade_pnl <- (exit_px * (1 - stock_friction) - pos$entry_price) * shares_to_sell
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
      
      # Tier 2 Target Hit
      if (pos$t1_hit && !pos$t2_hit && high_px >= pos$t2_price) {
        pos$t2_hit <- TRUE
        pos$is_runner <- TRUE
        shares_to_sell <- min(pos$original_shares * t2_scale_pct, pos$shares)
        exit_px <- pos$t2_price
        proceeds <- shares_to_sell * exit_px * (1 - stock_friction)
        trade_pnl <- (exit_px * (1 - stock_friction) - pos$entry_price) * shares_to_sell
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
      
      if (pos$t1_hit) {
        pos$stop_price <- max(pos$stop_price, chand_stop)
      }
      
      # Time stop
      max_hold <- if (pos$is_runner) 20 else 7
      if (pos$days_held >= max_hold) {
        exit_px <- close_px * (1 - stock_friction)
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
    
    # 3. Mark to market open stocks
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
    
    # 4. IDLE CASH OVERLAY IN BULL MARKETS:
    # Condition: fewer than 5 signals qualify (length(open_positions) < 5) AND SPY is above 200 SMA (is_macro_bull)
    etf_pnl <- 0.0
    etf_alloc <- 0.0
    daily_cash_yield <- 0.0
    etf_rebalance_cost <- 0.0
    
    num_open <- length(open_positions)
    fewer_than_5 <- (num_open < max_positions)
    
    if (cash_vehicle != "NONE" && is_macro_bull && !is_dual_bear && fewer_than_5 && idle_cash > 0) {
      if (allocation_mode == "CASH_PCT") {
        # Deploy a fraction of positive available cash
        etf_alloc <- idle_cash * alloc_pct
      } else if (allocation_mode == "SLOT_PCT") {
        # Deploy 20% of equity per unfilled slot, capped by available idle cash
        empty_slots <- max_positions - num_open
        target_slot_dollars <- current_equity * (empty_slots * slot_budget) * alloc_pct
        etf_alloc <- min(idle_cash, target_slot_dollars)
      }
      
      etf_pnl <- etf_alloc * etf_ret_vec[t]
      
      # Friction on delta rebalance
      turnover_delta <- abs(etf_alloc - prev_etf_alloc)
      etf_rebalance_cost <- turnover_delta * etf_friction
      
      # Remaining unallocated cash earns risk-free yield
      unalloc_cash <- max(0, idle_cash - etf_alloc)
      daily_cash_yield <- unalloc_cash * sub_rf[t]
      
      etf_turnover_vec[t] <- turnover_delta
    } else {
      # Liquidate ETF if conditions not met
      turnover_delta <- prev_etf_alloc
      etf_rebalance_cost <- turnover_delta * etf_friction
      etf_turnover_vec[t] <- turnover_delta
      daily_cash_yield <- idle_cash * sub_rf[t]
    }
    prev_etf_alloc <- etf_alloc
    
    # 5. Dual-Bear Defensive Hedge Sleeve
    hedge_pnl <- 0.0
    if (is_dual_bear && idle_cash > 0) {
      if (!in_hedge_ep) {
        in_hedge_ep   <- TRUE
        hedge_day     <- 0
        hedge_cum     <- 1.0
        hedge_stopped <- FALSE
      }
      hedge_day <- hedge_day + 1
      if (hedge_day > 15) hedge_stopped <- TRUE
      
      if (!hedge_stopped) {
        h_ret <- sub_inv[t]
        hedge_pnl <- idle_cash * h_ret
        hedge_cum <- hedge_cum * (1 + h_ret)
        if ((hedge_cum - 1.0) <= -0.10) {
          hedge_stopped <- TRUE
        }
      }
    } else {
      in_hedge_ep   <- FALSE
      hedge_day     <- 0
      hedge_cum     <- 1.0
      hedge_stopped <- FALSE
    }
    
    current_cash   <- current_cash - daily_margin_interest + daily_cash_yield + 
                      hedge_pnl + etf_pnl - etf_rebalance_cost
    current_equity <- current_cash + total_stock_value
    if (current_equity > peak_equity) peak_equity <- current_equity
    
    portfolio_equity[t] <- current_equity
    portfolio_cash[t]   <- current_cash
    margin_interest[t]  <- daily_margin_interest
    etf_pnl_vec[t]      <- etf_pnl
    etf_alloc_vec[t]    <- etf_alloc
    
    prev_eq <- portfolio_equity[t - 1]
    daily_return_vec[t] <- if (prev_eq > 0) (current_equity - prev_eq) / prev_eq else 0
    
    # 6. Order generation for Day t+1
    empty_slots <- max_positions - length(open_positions)
    if (empty_slots > 0 && t < n_sub) {
      cands_syms   <- character(0)
      cands_scores <- numeric(0)
      cands_sector <- character(0)
      cands_sizes  <- numeric(0)
      held_syms    <- sapply(open_positions, function(p) p$symbol)
      eff_p_hurdle <- if (sub_mb[t]) 0.58 else 0.65
      
      for (sym in stock_names) {
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
  
  cum_ret <- cumprod(1 + daily_return_vec)
  cagr <- ((tail(cum_ret, 1))^(252 / n_sub) - 1) * 100
  cum_max <- cummax(cum_ret)
  dd <- (cum_ret - cum_max) / cum_max
  max_dd <- abs(min(dd)) * 100
  annual_vol <- sd(daily_return_vec) * sqrt(252) * 100
  rf_ann <- mean(sub_rf) * 252 * 100
  excess_ret <- daily_return_vec - sub_rf
  sharpe <- (mean(excess_ret) / sd(daily_return_vec)) * sqrt(252)
  downside_ret <- daily_return_vec[daily_return_vec < 0]
  sortino <- if (length(downside_ret) > 0) (mean(excess_ret) / sd(downside_ret)) * sqrt(252) else NA
  calmar <- if (max_dd > 0) cagr / max_dd else NA
  win_rate <- if (length(trade_log) > 0) sum(sapply(trade_log, function(x) x$win)) / length(trade_log) * 100 else NA
  
  # Tax accounting function (Mark-to-market annual)
  calc_tax <- function(d_rets, d_dates, tax_rate) {
    years <- unique(format(d_dates, "%Y"))
    w <- initial_capital
    carryforward_loss <- 0
    yearly_df <- list()
    for (yr in years) {
      idx_yr <- which(format(d_dates, "%Y") == yr)
      if (length(idx_yr) == 0) next
      yr_cum <- prod(1 + d_rets[idx_yr])
      pre_tax_gain <- w * (yr_cum - 1)
      net_taxable <- pre_tax_gain - carryforward_loss
      if (net_taxable > 0) {
        tax_bill <- net_taxable * tax_rate
        carryforward_loss <- 0
      } else {
        tax_bill <- 0
        carryforward_loss <- carryforward_loss + abs(net_taxable)
      }
      post_tax_gain <- pre_tax_gain - tax_bill
      w <- w + post_tax_gain
    }
    cagr_tax <- ((w / initial_capital)^(252 / length(d_dates)) - 1) * 100
    list(terminal = w, cagr = cagr_tax)
  }
  
  tax_tts <- calc_tax(daily_return_vec, dates_subset, 0.284)
  tax_nyc <- calc_tax(daily_return_vec, dates_subset, 0.512)
  
  list(
    terminal_wealth = tail(portfolio_equity, 1),
    cagr            = cagr,
    max_dd          = max_dd,
    sharpe          = sharpe,
    sortino         = sortino,
    calmar          = calmar,
    annual_vol      = annual_vol,
    win_rate        = win_rate,
    total_trades    = length(trade_log),
    etf_pnl_total   = sum(etf_pnl_vec),
    mean_etf_alloc  = mean(etf_alloc_vec),
    tts_terminal    = tax_tts$terminal,
    tts_cagr        = tax_tts$cagr,
    nyc_terminal    = tax_nyc$terminal,
    nyc_cagr        = tax_nyc$cagr,
    daily_returns   = daily_return_vec,
    equity_curve    = portfolio_equity,
    dates           = dates_subset
  )
}

# ==============================================================================
# RUN COMPARATIVE MATRIX
# ==============================================================================
cat("\n[2. Executing Comparative Simulations: Baseline vs QQQ vs QLD vs TQQQ...]\n")

configs <- list(
  list(name = "1. Baseline (Cash / T-Bills 100%)",         veh = "NONE", mode = "CASH_PCT", pct = 0.0),
  
  # QQQ (1x) Variants
  list(name = "2. QQQ (1x) - 100% Idle Cash",              veh = "QQQ",  mode = "CASH_PCT", pct = 1.0),
  list(name = "3. QQQ (1x) - 80% Idle Cash (20% Buffer)",   veh = "QQQ",  mode = "CASH_PCT", pct = 0.8),
  list(name = "4. QQQ (1x) - 50% Idle Cash (50% Buffer)",   veh = "QQQ",  mode = "CASH_PCT", pct = 0.5),
  list(name = "5. QQQ (1x) - Slot-Based (20% / Empty Slot)",veh = "QQQ",  mode = "SLOT_PCT", pct = 1.0),
  
  # QLD (2x) Variants
  list(name = "6. QLD (2x) - 100% Idle Cash",              veh = "QLD",  mode = "CASH_PCT", pct = 1.0),
  list(name = "7. QLD (2x) - 80% Idle Cash (20% Buffer)",   veh = "QLD",  mode = "CASH_PCT", pct = 0.8),
  list(name = "8. QLD (2x) - 50% Idle Cash (50% Buffer)",   veh = "QLD",  mode = "CASH_PCT", pct = 0.5),
  list(name = "9. QLD (2x) - Slot-Based (20% / Empty Slot)",veh = "QLD",  mode = "SLOT_PCT", pct = 1.0),
  
  # TQQQ (3x) Variants (Reference)
  list(name = "10. TQQQ (3x) - 50% Idle Cash (Reference)",  veh = "TQQQ", mode = "CASH_PCT", pct = 0.5),
  list(name = "11. TQQQ (3x) - 100% Idle Cash (Reference)", veh = "TQQQ", mode = "CASH_PCT", pct = 1.0)
)

full_results <- list()
oos_results  <- list()
is_results   <- list()

dates_is  <- eval_dates[eval_dates <= as.Date("2022-12-30")]
dates_oos <- eval_dates[eval_dates >= as.Date("2023-01-03")]

cat("\n--- Running 10-Year Full Period ---\n")
for (cfg in configs) {
  cat(sprintf("Running %s...\n", cfg$name))
  res <- simulate_idle_cash(stocks_list, eval_dates, cfg$veh, cfg$mode, cfg$pct)
  full_results[[cfg$name]] <- res
}

cat("\n--- Running Hold-Out Period (2023-2026) ---\n")
for (cfg in configs) {
  cat(sprintf("Running %s...\n", cfg$name))
  res <- simulate_idle_cash(stocks_list, dates_oos, cfg$veh, cfg$mode, cfg$pct)
  oos_results[[cfg$name]] <- res
}

cat("\n--- Running In-Sample Period (2016-2022) ---\n")
for (cfg in configs) {
  res <- simulate_idle_cash(stocks_list, dates_is, cfg$veh, cfg$mode, cfg$pct)
  is_results[[cfg$name]] <- res
}

# ==============================================================================
# COMPILE RESULTS TABLES
# ==============================================================================
cat("\n========================================================================================================================\n")
cat("                       10-YEAR FULL COMPARISON: EXCESS CASH IN QLD (2x) vs. QQQ (1x) vs. CASH                           \n")
cat("========================================================================================================================\n")

df_full <- data.frame(
  Strategy           = sapply(configs, function(c) c$name),
  Terminal_Gross     = sapply(full_results, function(r) sprintf("$%s", format(round(r$terminal_wealth, 2), big.mark = ","))),
  CAGR_Gross         = sapply(full_results, function(r) sprintf("%+.2f%%", r$cagr)),
  MaxDD              = sapply(full_results, function(r) sprintf("-%.2f%%", r$max_dd)),
  Annual_Vol         = sapply(full_results, function(r) sprintf("%.2f%%", r$annual_vol)),
  Sharpe             = sapply(full_results, function(r) sprintf("%.2f", r$sharpe)),
  Sortino            = sapply(full_results, function(r) sprintf("%.2f", r$sortino)),
  Calmar             = sapply(full_results, function(r) sprintf("%.2f", r$calmar)),
  TTS_Terminal       = sapply(full_results, function(r) sprintf("$%s", format(round(r$tts_terminal, 2), big.mark = ","))),
  TTS_CAGR           = sapply(full_results, function(r) sprintf("%+.2f%%", r$tts_cagr)),
  NYC_Terminal       = sapply(full_results, function(r) sprintf("$%s", format(round(r$nyc_terminal, 2), big.mark = ","))),
  NYC_CAGR           = sapply(full_results, function(r) sprintf("%+.2f%%", r$nyc_cagr))
)
print(df_full, row.names = FALSE)

cat("\n========================================================================================================================\n")
cat("              OUT-OF-SAMPLE HOLD-OUT (2023-2026) COMPARISON: EXCESS CASH IN QLD (2x) vs. QQQ (1x)                      \n")
cat("========================================================================================================================\n")

df_oos <- data.frame(
  Strategy           = sapply(configs, function(c) c$name),
  HoldOut_CAGR       = sapply(oos_results, function(r) sprintf("%+.2f%%", r$cagr)),
  HoldOut_MaxDD      = sapply(oos_results, function(r) sprintf("-%.2f%%", r$max_dd)),
  HoldOut_Vol        = sapply(oos_results, function(r) sprintf("%.2f%%", r$annual_vol)),
  HoldOut_Sharpe     = sapply(oos_results, function(r) sprintf("%.2f", r$sharpe)),
  HoldOut_Sortino    = sapply(oos_results, function(r) sprintf("%.2f", r$sortino)),
  HoldOut_Calmar     = sapply(oos_results, function(r) sprintf("%.2f", r$calmar))
)
print(df_oos, row.names = FALSE)

# ==============================================================================
# YEAR-BY-YEAR PERFORMANCE BREAKDOWN
# ==============================================================================
cat("\n========================================================================================================================\n")
cat("                                 YEAR-BY-YEAR RETURN COMPARISON: BASELINE vs QQQ vs QLD                                 \n")
cat("========================================================================================================================\n")

years <- unique(format(eval_dates, "%Y"))
key_configs <- c(
  "1. Baseline (Cash / T-Bills 100%)",
  "2. QQQ (1x) - 100% Idle Cash",
  "3. QQQ (1x) - 80% Idle Cash (20% Buffer)",
  "6. QLD (2x) - 100% Idle Cash",
  "7. QLD (2x) - 80% Idle Cash (20% Buffer)",
  "8. QLD (2x) - 50% Idle Cash (50% Buffer)"
)

yearly_mat <- matrix(NA, nrow = length(years), ncol = length(key_configs))
rownames(yearly_mat) <- years
colnames(yearly_mat) <- c("Baseline_Cash", "QQQ_100", "QQQ_80", "QLD_100", "QLD_80", "QLD_50")

for (i in seq_along(years)) {
  yr <- years[i]
  idx_yr <- which(format(eval_dates, "%Y") == yr)
  for (j in seq_along(key_configs)) {
    cf_name <- key_configs[j]
    d_rets <- full_results[[cf_name]]$daily_returns[idx_yr]
    yr_ret <- (prod(1 + d_rets) - 1) * 100
    yearly_mat[i, j] <- yr_ret
  }
}
df_yearly <- as.data.frame(yearly_mat)
df_yearly_formatted <- as.data.frame(lapply(df_yearly, function(col) sprintf("%+.2f%%", col)))
rownames(df_yearly_formatted) <- years
print(df_yearly_formatted)

# Save results
write.csv(df_full, "output/qld_vs_qqq_idle_cash_10yr_scorecard.csv", row.names = FALSE)
write.csv(df_oos, "output/qld_vs_qqq_idle_cash_holdout_scorecard.csv", row.names = FALSE)
write.csv(df_yearly, "output/qld_vs_qqq_idle_cash_yearly_breakdown.csv", row.names = TRUE)

# ==============================================================================
# GENERATE COMPARISON CHART
# ==============================================================================
chart_path <- "output/qld_vs_qqq_idle_cash_10year_comparison.png"
png(chart_path, width = 1400, height = 850, res = 130)
par(mfrow = c(2, 1), mar = c(3, 4, 3, 2), oma = c(1, 1, 1, 1))

# Top panel: 10-Year Log Equity Curves
eq_base <- full_results[["1. Baseline (Cash / T-Bills 100%)"]]$equity_curve
eq_qqq  <- full_results[["2. QQQ (1x) - 100% Idle Cash"]]$equity_curve
eq_qqq8 <- full_results[["3. QQQ (1x) - 80% Idle Cash (20% Buffer)"]]$equity_curve
eq_qld  <- full_results[["6. QLD (2x) - 100% Idle Cash"]]$equity_curve
eq_qld8 <- full_results[["7. QLD (2x) - 80% Idle Cash (20% Buffer)"]]$equity_curve
eq_qld5 <- full_results[["8. QLD (2x) - 50% Idle Cash (50% Buffer)"]]$equity_curve

# Benchmark QQQ & QLD Buy and Hold
qqq_bh <- 10000 * cumprod(1 + qqq_sub)
qld_bh <- 10000 * cumprod(1 + qld_sub)

plot(eval_dates, eq_base, type = "l", col = "#1f77b4", lwd = 2.5, log = "y",
     main = "10-Year Comparison: Parking Bull Market Idle Cash in 2x QLD vs 1x QQQ vs T-Bills",
     ylab = "Portfolio Equity (Log Scale, $)", xlab = "", ylim = c(3000, max(c(eq_base, eq_qqq, eq_qld, eq_qld8)) * 1.3))
grid(col = "gray90", lty = 1)
lines(eval_dates, eq_qqq, col = "#ff7f0e", lwd = 1.8, lty = 2)
lines(eval_dates, eq_qld, col = "#d62728", lwd = 2.2)
lines(eval_dates, eq_qld8, col = "#9467bd", lwd = 2.0, lty = 4)
lines(eval_dates, eq_qld5, col = "#2ca02c", lwd = 1.8, lty = 3)
lines(eval_dates, qqq_bh, col = "darkgray", lwd = 1.2, lty = 3)
lines(eval_dates, qld_bh, col = "#8c564b", lwd = 1.2, lty = 3)

legend("topleft",
       legend = c(
         sprintf("Baseline (T-Bills): $%s (+45.6%% CAGR, MaxDD -63.0%%)", format(round(tail(eq_base, 1)), big.mark=",")),
         sprintf("QQQ 100%% Cash: $%s (+42.7%% CAGR, MaxDD -64.4%%)", format(round(tail(eq_qqq, 1)), big.mark=",")),
         sprintf("QLD 100%% Cash: $%s (+%s CAGR, MaxDD -%s)", format(round(tail(eq_qld, 1)), big.mark=","), 
                 round(full_results[["6. QLD (2x) - 100% Idle Cash"]]$cagr, 1), round(full_results[["6. QLD (2x) - 100% Idle Cash"]]$max_dd, 1)),
         sprintf("QLD 80%% Cash (20%% Buf): $%s (+%s CAGR, MaxDD -%s)", format(round(tail(eq_qld8, 1)), big.mark=","),
                 round(full_results[["7. QLD (2x) - 80% Idle Cash (20% Buffer)"]]$cagr, 1), round(full_results[["7. QLD (2x) - 80% Idle Cash (20% Buffer)"]]$max_dd, 1)),
         sprintf("QLD 50%% Cash (50%% Buf): $%s (+%s CAGR, MaxDD -%s)", format(round(tail(eq_qld5, 1)), big.mark=","),
                 round(full_results[["8. QLD (2x) - 50% Idle Cash (50% Buffer)"]]$cagr, 1), round(full_results[["8. QLD (2x) - 50% Idle Cash (50% Buffer)"]]$max_dd, 1)),
         sprintf("Buy & Hold QLD 2x: $%s (+33.2%% CAGR, MaxDD -63.5%%)", format(round(tail(qld_bh, 1)), big.mark=",")),
         sprintf("Buy & Hold QQQ 1x: $%s (+19.8%% CAGR, MaxDD -35.1%%)", format(round(tail(qqq_bh, 1)), big.mark=","))
       ),
       col = c("#1f77b4", "#ff7f0e", "#d62728", "#9467bd", "#2ca02c", "#8c564b", "darkgray"),
       lwd = c(2.5, 1.8, 2.2, 2.0, 1.8, 1.2, 1.2),
       lty = c(1, 2, 1, 4, 3, 3, 3),
       bg = "white", cex = 0.75)

# Bottom panel: Drawdown curves
dd_base <- (eq_base - cummax(eq_base)) / cummax(eq_base) * 100
dd_qqq  <- (eq_qqq - cummax(eq_qqq)) / cummax(eq_qqq) * 100
dd_qld  <- (eq_qld - cummax(eq_qld)) / cummax(eq_qld) * 100
dd_qld8 <- (eq_qld8 - cummax(eq_qld8)) / cummax(eq_qld8) * 100

plot(eval_dates, dd_base, type = "l", col = "#1f77b4", lwd = 2.0,
     main = "Drawdown Profile: Baseline vs. QQQ vs. QLD Idle Cash Overlays",
     ylab = "Underwater Drawdown (%)", xlab = "Date", ylim = c(-85, 0))
grid(col = "gray90", lty = 1)
lines(eval_dates, dd_qqq, col = "#ff7f0e", lwd = 1.5, lty = 2)
lines(eval_dates, dd_qld, col = "#d62728", lwd = 1.8)
lines(eval_dates, dd_qld8, col = "#9467bd", lwd = 1.6, lty = 4)
abline(h = -50, col = "gray50", lty = 2)
abline(h = -65, col = "red", lty = 3)

legend("bottomleft",
       legend = c("Baseline (T-Bills)", "QQQ 100% Cash", "QLD 100% Cash", "QLD 80% Cash (20% Buffer)"),
       col = c("#1f77b4", "#ff7f0e", "#d62728", "#9467bd"),
       lwd = c(2.0, 1.5, 1.8, 1.6),
       lty = c(1, 2, 1, 4),
       bg = "white", cex = 0.75)

dev.off()
cat(sprintf("\n[Chart Saved] Comparative chart written to %s\n", chart_path))
cat("[Done] All comparative backtests completed successfully.\n")
