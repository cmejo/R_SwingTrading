#!/usr/bin/env Rscript
# ==============================================================================
# scratch/test_breadth_implementation_options.R
# Test Breadth as:
# Option 1: Dynamic Execution Hurdle Modulation (Oversold Dip Accumulator + Exhaustion Guard)
# Option 2: Feature Matrix Probability Shift
# Option 3: Baseline Comparison
# ==============================================================================

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(TTR)
})

source("R/08_metrics.R")

cat("[Loading clean backtest environment...]\n")
env_data <- readRDS("scratch/clean_backtest_env.rds")
stocks_list <- env_data$stocks_list
macro_data  <- env_data$macro_data
eval_dates  <- env_data$eval_dates

# 1. Compute 10-day smoothed Intraday Breadth Ratio across all stocks in stocks_list
all_dates <- eval_dates
n_dates <- length(all_dates)
n_stocks <- length(stocks_list)

intraday_mat <- matrix(NA_real_, nrow = n_dates, ncol = n_stocks)
rownames(intraday_mat) <- as.character(all_dates)
colnames(intraday_mat) <- names(stocks_list)

for (s in names(stocks_list)) {
  stk <- stocks_list[[s]]
  m_idx <- match(all_dates, stk$dates)
  valid <- !is.na(m_idx)
  if (sum(valid) > 0) {
    op <- stk$open[m_idx[valid]]
    cl <- stk$close[m_idx[valid]]
    diff_oc <- cl - op
    sign_oc <- ifelse(diff_oc > 0, 1, ifelse(diff_oc < 0, -1, 0))
    intraday_mat[valid, s] <- sign_oc
  }
}

n_adv <- rowSums(intraday_mat == 1, na.rm = TRUE)
n_dec <- rowSums(intraday_mat == -1, na.rm = TRUE)
n_tot <- rowSums(!is.na(intraday_mat), na.rm = TRUE)
n_tot[n_tot == 0] <- 1

net_adv_dec <- (n_adv - n_dec) / n_tot
breadth_xts <- xts(net_adv_dec, order.by = all_dates)
breadth_10d <- as.numeric(TTR::SMA(breadth_xts, n = 10))
breadth_10d[is.na(breadth_10d)] <- 0.0

macro_data$intraday_breadth_10d <- breadth_10d

# 2. Simulator supporting Dynamic Breadth Hurdle Modulation
run_sim_breadth <- function(stocks_list,
                            macro_data,
                            eval_dates,
                            breadth_mode = c("NONE", "DYNAMIC_HURDLE", "CONVICTION_BOOST", "HYBRID"),
                            base_p_long = 0.58,
                            max_positions = 5,
                            leverage = 1.0,
                            use_bear_hedge = TRUE) {
  
  breadth_mode <- match.arg(breadth_mode)
  n_days <- length(eval_dates)
  
  spy_cl <- macro_data$spy_close
  qqq_cl <- macro_data$qqq_close
  rf_vec <- macro_data$rf_daily
  rf_vec[is.na(rf_vec)] <- 0.02 / 252
  gld_cl <- macro_data$gld_close
  breadth_vec <- macro_data$intraday_breadth_10d
  
  spy_sma50 <- SMA(spy_cl, n = 50)
  qqq_sma50 <- SMA(qqq_cl, n = 50)
  gld_sma50 <- if (!is.null(gld_cl)) SMA(gld_cl, n = 50) else NULL
  
  macro_bull <- as.numeric(qqq_cl > qqq_sma50)
  macro_bull[is.na(macro_bull)] <- 1
  
  dual_bear <- if (!is.null(gld_sma50)) {
    as.numeric(qqq_cl < qqq_sma50 & gld_cl < gld_sma50)
  } else {
    as.numeric(qqq_cl < qqq_sma50 & spy_cl < spy_sma50)
  }
  dual_bear[is.na(dual_bear)] <- 0
  
  qqq_ret <- (qqq_cl / lag.xts(qqq_cl, 1)) - 1
  qqq_ret[is.na(qqq_ret)] <- 0
  inv_hedge_daily <- -1.0 * as.numeric(qqq_ret)
  
  initial_capital <- 10000
  slot_budget <- 1.0 / max_positions
  friction_rate <- 0.0010
  
  portfolio_equity <- numeric(n_days)
  daily_return_vec <- numeric(n_days)
  
  current_equity <- initial_capital
  current_cash   <- initial_capital
  peak_equity    <- initial_capital
  
  open_positions <- list()
  trade_log      <- list()
  pending_buys   <- list()
  
  in_hedge_ep    <- FALSE
  hedge_day      <- 0
  hedge_cum      <- 1.0
  hedge_stopped  <- FALSE
  
  portfolio_equity[1] <- current_equity
  
  for (t in 2:n_days) {
    curr_d <- eval_dates[t]
    is_bull <- !is.na(macro_bull[t - 1]) && macro_bull[t - 1] == 1
    is_db   <- !is.na(dual_bear[t - 1]) && dual_bear[t - 1] == 1
    br_val  <- if (!is.na(breadth_vec[t - 1])) breadth_vec[t - 1] else 0.0
    
    # 1. Open executions
    if (length(pending_buys) > 0 && length(open_positions) < max_positions) {
      for (ord in pending_buys) {
        if (length(open_positions) >= max_positions) break
        stk <- stocks_list[[ord$symbol]]
        idx_t <- match(curr_d, stk$dates)
        if (is.na(idx_t)) next
        
        open_px <- stk$open[idx_t]
        atr_val <- stk$atr[idx_t]
        if (is.na(open_px) || open_px <= 0 || is.na(atr_val) || atr_val <= 0) next
        
        exec_px <- open_px * (1 + friction_rate)
        target_dollar <- current_equity * slot_budget * ord$size_mult * leverage
        max_single <- current_equity * 0.50 * leverage
        target_dollar <- min(target_dollar, max_single)
        
        n_shares <- floor(target_dollar / exec_px)
        if (n_shares <= 0) next
        
        current_cash <- current_cash - n_shares * exec_px
        stop_dist <- 2.0 * atr_val
        stop_px   <- exec_px - stop_dist
        t1_px     <- exec_px + 2.0 * stop_dist
        t2_px     <- exec_px + 4.0 * stop_dist
        
        open_positions[[length(open_positions) + 1]] <- list(
          symbol      = ord$symbol,
          sector      = ord$sector,
          entry_date  = curr_d,
          entry_price = exec_px,
          shares      = n_shares,
          orig_shares = n_shares,
          stop_price  = stop_px,
          t1_price    = t1_px,
          t2_price    = t2_px,
          atr         = atr_val,
          days_held   = 1,
          t1_hit      = FALSE,
          t2_hit      = FALSE,
          is_runner   = FALSE,
          high_wm     = exec_px
        )
      }
      pending_buys <- list()
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
      high_wm <- max(pos$high_wm, high_px)
      chand_stop <- high_wm - 2.5 * atr_val
      
      # Stop loss
      if (low_px <= pos$stop_price) {
        exit_px <- min(open_px, pos$stop_price)
        proceeds <- pos$shares * exit_px * (1 - friction_rate)
        pnl <- (exit_px * (1 - friction_rate) - pos$entry_price) * pos$shares
        current_cash <- current_cash + proceeds
        trade_log[[length(trade_log) + 1]] <- list(win = (pnl > 0), pnl = pnl)
        next
      }
      
      # T1 Scale
      if (!pos$t1_hit && high_px >= pos$t1_price) {
        pos$t1_hit <- TRUE
        to_sell <- min(pos$orig_shares * 0.35, pos$shares)
        exit_px <- pos$t1_price
        proceeds <- to_sell * exit_px * (1 - friction_rate)
        pnl <- (exit_px * (1 - friction_rate) - pos$entry_price) * to_sell
        current_cash <- current_cash + proceeds
        pos$shares <- pos$shares - to_sell
        pos$stop_price <- max(pos$entry_price, chand_stop)
        trade_log[[length(trade_log) + 1]] <- list(win = TRUE, pnl = pnl)
      }
      
      # T2 Scale
      if (pos$t1_hit && !pos$t2_hit && high_px >= pos$t2_price) {
        pos$t2_hit <- TRUE
        pos$is_runner <- TRUE
        to_sell <- min(pos$orig_shares * 0.35, pos$shares)
        exit_px <- pos$t2_price
        proceeds <- to_sell * exit_px * (1 - friction_rate)
        pnl <- (exit_px * (1 - friction_rate) - pos$entry_price) * to_sell
        current_cash <- current_cash + proceeds
        pos$shares <- pos$shares - to_sell
        pos$stop_price <- max(pos$stop_price, chand_stop)
        trade_log[[length(trade_log) + 1]] <- list(win = TRUE, pnl = pnl)
      }
      
      if (pos$t1_hit) pos$stop_price <- max(pos$stop_price, chand_stop)
      
      # Time stop: 7 days base / 20 days runner
      max_hold <- if (pos$is_runner) 20 else 7
      if (pos$days_held >= max_hold) {
        exit_px <- close_px * (1 - friction_rate)
        proceeds <- pos$shares * exit_px
        pnl <- (exit_px - pos$entry_price) * pos$shares
        current_cash <- current_cash + proceeds
        trade_log[[length(trade_log) + 1]] <- list(win = (pnl > 0), pnl = pnl)
        next
      }
      
      pos$high_wm <- high_wm
      surviving_positions[[length(surviving_positions) + 1]] <- pos
    }
    open_positions <- surviving_positions
    
    # 3. Mark to market
    stock_val <- 0.0
    for (pos in open_positions) {
      stk <- stocks_list[[pos$symbol]]
      idx_t <- match(curr_d, stk$dates)
      px <- if (!is.na(idx_t)) stk$close[idx_t] else pos$entry_price
      stock_val <- stock_val + pos$shares * px
    }
    
    debt <- max(0, -current_cash)
    margin_cost <- debt * (0.070 / 252)
    idle_cash <- max(0, current_cash)
    cash_yield <- idle_cash * as.numeric(rf_vec[t])
    
    # Dual bear hedge
    hedge_pnl <- 0.0
    if (use_bear_hedge && is_db && idle_cash > 0) {
      if (!in_hedge_ep) {
        in_hedge_ep <- TRUE; hedge_day <- 0; hedge_cum <- 1.0; hedge_stopped <- FALSE
      }
      hedge_day <- hedge_day + 1
      if (hedge_day > 15) hedge_stopped <- TRUE
      if (!hedge_stopped) {
        h_ret <- inv_hedge_daily[t]
        hedge_pnl <- idle_cash * h_ret
        hedge_cum <- hedge_cum * (1 + h_ret)
        if ((hedge_cum - 1.0) <= -0.10) hedge_stopped <- TRUE
      }
    } else {
      in_hedge_ep <- FALSE
    }
    
    current_equity <- stock_val + current_cash - margin_cost + cash_yield + hedge_pnl
    if (current_equity > peak_equity) peak_equity <- current_equity
    portfolio_equity[t] <- current_equity
    prev_eq <- portfolio_equity[t - 1]
    daily_return_vec[t] <- if (prev_eq > 0) (current_equity - prev_eq) / prev_eq else 0
    
    # 4. Signal generation for tomorrow
    empty_slots <- max_positions - length(open_positions)
    if (empty_slots > 0 && t < n_days) {
      cands_syms   <- character(0)
      cands_scores <- numeric(0)
      cands_sector <- character(0)
      cands_sizes  <- numeric(0)
      held_syms    <- sapply(open_positions, function(p) p$symbol)
      
      # Compute Dynamic Hurdle based on Breadth Mode
      eff_hurdle <- if (is_bull) base_p_long else 0.65
      size_mult_breadth <- 1.0
      
      if (breadth_mode == "DYNAMIC_HURDLE") {
        # Negative IC means oversold breadth = high forward returns -> lower hurdle!
        # Overextended breadth = low forward returns -> raise hurdle!
        if (br_val <= -0.15) {
          eff_hurdle <- eff_hurdle - 0.03 # Lower hurdle to 0.55 to accumulate washouts
        } else if (br_val >= 0.30) {
          eff_hurdle <- eff_hurdle + 0.05 # Raise hurdle to 0.63 to prevent chasing exhaustion
        }
      } else if (breadth_mode == "CONVICTION_BOOST") {
        # Boost sizing on oversold washouts
        if (br_val <= -0.15) {
          size_mult_breadth <- 1.20
        } else if (br_val >= 0.30) {
          size_mult_breadth <- 0.80
        }
      } else if (breadth_mode == "HYBRID") {
        # Both hurdle adjustment + conviction boost
        if (br_val <= -0.15) {
          eff_hurdle <- eff_hurdle - 0.03
          size_mult_breadth <- 1.15
        } else if (br_val >= 0.30) {
          eff_hurdle <- eff_hurdle + 0.05
          size_mult_breadth <- 0.85
        }
      }
      
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
        
        if (p_up >= eff_hurdle && rs_val > 0) {
          sm_b <- if (!is.na(smom)) pmax(0, (smom - 0.80) * 0.06) else 0
          er_b <- if (!is.na(eff_r)) pmax(0, (eff_r - 0.30) * 0.20) else 0
          nw_b <- if (!is.na(nwick)) pmax(0, nwick * 0.08) else 0
          
          score <- p_up * (1.0 + rs_val + sm_b + er_b + nw_b)
          eff_vol <- pmax(0.15, vol_ann) / (1.0 + 0.20 * er_b + 0.10 * sm_b)
          base_mult <- pmin(1.25, 0.35 / eff_vol) * size_mult_breadth
          
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
          if (cur_s_cnt >= 2) next
          
          pending_buys[[length(pending_buys) + 1]] <- list(
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
  
  # Calculate summary metrics
  term_w <- tail(portfolio_equity, 1)
  n_yr <- n_days / 252
  cagr <- ((term_w / initial_capital)^(1 / n_yr) - 1) * 100
  cum_max <- cummax(portfolio_equity)
  dd <- (portfolio_equity - cum_max) / cum_max
  max_dd <- abs(min(dd)) * 100
  
  rf_d <- as.numeric(rf_vec)
  rf_d[is.na(rf_d)] <- 0.02 / 252
  ann_vol <- sd(daily_return_vec) * sqrt(252) * 100
  sharpe <- (mean(daily_return_vec - rf_d) / sd(daily_return_vec)) * sqrt(252)
  
  down_rets <- pmin(0, daily_return_vec)
  sortino <- if (sd(down_rets) > 0) (mean(daily_return_vec - rf_d) / sd(down_rets)) * sqrt(252) else NA
  calmar <- cagr / max_dd
  
  win_rate <- if (length(trade_log) > 0) sum(sapply(trade_log, function(x) x$win)) / length(trade_log) * 100 else NA
  
  return(list(
    mode           = breadth_mode,
    terminal       = term_w,
    cagr           = cagr,
    max_dd         = max_dd,
    sharpe         = sharpe,
    sortino        = sortino,
    calmar         = calmar,
    annual_vol     = ann_vol,
    win_rate       = win_rate,
    total_trades   = length(trade_log),
    equity_curve   = portfolio_equity,
    daily_returns  = daily_return_vec
  ))
}

cat("\nRunning comparisons across 10-year clean discrete engine...\n")
res_none     <- run_sim_breadth(stocks_list, macro_data, eval_dates, breadth_mode = "NONE")
res_hurdle   <- run_sim_breadth(stocks_list, macro_data, eval_dates, breadth_mode = "DYNAMIC_HURDLE")
res_convict  <- run_sim_breadth(stocks_list, macro_data, eval_dates, breadth_mode = "CONVICTION_BOOST")
res_hybrid   <- run_sim_breadth(stocks_list, macro_data, eval_dates, breadth_mode = "HYBRID")

cat("========================================================================================\n")
cat(" BREADTH MECHANISM COMPARISON ON 10-YEAR CLEAN ENGINE (2016-2026):\n")
cat("========================================================================================\n")

df_comp <- data.frame(
  Configuration      = c("1. Baseline (No Breadth)", "2. Dynamic Hurdle Modulation", "3. Conviction Sizing Boost", "4. Hybrid (Hurdle + Conviction)"),
  Terminal_Wealth    = c(sprintf("$%s", format(round(res_none$terminal, 2), big.mark = ",")),
                         sprintf("$%s", format(round(res_hurdle$terminal, 2), big.mark = ",")),
                         sprintf("$%s", format(round(res_convict$terminal, 2), big.mark = ",")),
                         sprintf("$%s", format(round(res_hybrid$terminal, 2), big.mark = ","))),
  CAGR               = c(sprintf("%+.2f%%", res_none$cagr),
                         sprintf("%+.2f%%", res_hurdle$cagr),
                         sprintf("%+.2f%%", res_convict$cagr),
                         sprintf("%+.2f%%", res_hybrid$cagr)),
  Max_Drawdown       = c(sprintf("-%.2f%%", res_none$max_dd),
                         sprintf("-%.2f%%", res_hurdle$max_dd),
                         sprintf("-%.2f%%", res_convict$max_dd),
                         sprintf("-%.2f%%", res_hybrid$max_dd)),
  Sharpe_Ratio       = c(sprintf("%.2f", res_none$sharpe),
                         sprintf("%.2f", res_hurdle$sharpe),
                         sprintf("%.2f", res_convict$sharpe),
                         sprintf("%.2f", res_hybrid$sharpe)),
  Sortino_Ratio      = c(sprintf("%.2f", res_none$sortino),
                         sprintf("%.2f", res_hurdle$sortino),
                         sprintf("%.2f", res_convict$sortino),
                         sprintf("%.2f", res_hybrid$sortino)),
  Calmar_Ratio       = c(sprintf("%.2f", res_none$calmar),
                         sprintf("%.2f", res_hurdle$calmar),
                         sprintf("%.2f", res_convict$calmar),
                         sprintf("%.2f", res_hybrid$calmar)),
  Win_Rate           = c(sprintf("%.1f%%", res_none$win_rate),
                         sprintf("%.1f%%", res_hurdle$win_rate),
                         sprintf("%.1f%%", res_convict$win_rate),
                         sprintf("%.1f%%", res_hybrid$win_rate)),
  Total_Trades       = c(res_none$total_trades, res_hurdle$total_trades, res_convict$total_trades, res_hybrid$total_trades)
)

print(df_comp)

write.csv(df_comp, "output/breadth_implementation_comparison.csv", row.names = FALSE)
