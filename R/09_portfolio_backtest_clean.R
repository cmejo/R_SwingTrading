#!/usr/bin/env Rscript
# ==============================================================================
# R/09_portfolio_backtest_clean.R
# Institutional Zero-Lookahead Discrete Event-Driven Portfolio Simulator
#
# Core Design Principles:
# 1. Point-in-Time Signal Generation:
#    Signals and candidate ranking for Day t use ONLY data available through Day t Close.
# 2. Next-Day Open Execution (T+1):
#    All new positions fill at Open(t+1) with 10 bps slippage/friction.
#    Zero unearned overnight gaps. Zero same-day close-to-close lookahead.
# 3. Authentic Intraday Bracket Simulation:
#    - Low(t+1) <= StopPrice: Exits at min(Open(t+1), StopPrice) - 10 bps.
#    - High(t+1) >= T1 (+2.0R): Sells 50% at T1, ratchets stop to max(Entry, Chandelier).
#    - High(t+1) >= T2 (+4.0R): Sells 25% at T2, trails 25% runner via 2.5x ATR Chandelier.
#    - Time stop: 7 days base / 20 days runner exit at Close(t+1) - 10 bps.
# 4. Strict Balance Sheet & Margin Conservation:
#    - Equity = Cash + Long Market Value - Borrowed Margin.
#    - Daily 7.0% margin interest charged on all borrowed capital.
#    - Idle cash earns daily T-Bill / SGOV risk-free rate.
#    - Zero capital double-counting.
# ==============================================================================

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(TTR)
})

source("R/08_metrics.R")

#' Run Clean Institutional Discrete Portfolio Backtest
#'
#' @param stocks_list List of stock data objects containing OHLCV and precomputed features/signals.
#' @param macro_data List containing spliced SPY, QQQ, and IRX (risk-free) data.
#' @param eval_dates Date vector for simulation horizon.
#' @param initial_capital Starting portfolio cash (default: 10000).
#' @param max_positions Maximum concurrent positions (default: 3).
#' @param max_per_sector Maximum positions per sector (default: 2).
#' @param leverage Maximum portfolio leverage multiplier (default: 1.5).
#' @param target_vol Target annualized volatility for position sizing (default: 0.45).
#' @param cost_bps Transaction friction per trade side in bps (default: 10).
#' @param margin_rate Annual margin borrowing interest rate (default: 0.070).
#' @param p_long Minimum calibrated probability to qualify for Long entry (default: 0.58).
#' @param min_history_bars Minimum historical bars required before stock is tradeable (default: 250).
#' @param use_bear_hedge Logical; whether to deploy dynamic inverse hedge when Macro is Dual-Bear (default: TRUE).
#' @param hedge_stop_loss Max loss circuit breaker for hedge sleeve (default: -0.10).
#' @param hedge_time_stop Max hold days for hedge sleeve (default: 15).
#' @param sizing_mode Position sizing engine ("inverse_vol", "equal", or "sortino_er").
#' @param drawdown_throttle Logical; whether to apply two-tier drawdown cushion (-8% / -12%).
#' @return Comprehensive list containing equity curve, daily returns, trade log, and metrics.
#' @export
run_clean_portfolio_backtest <- function(stocks_list,
                                         macro_data,
                                         eval_dates,
                                         initial_capital = 10000,
                                         max_positions = 5,
                                         max_per_sector = 2,
                                         leverage = 1.0,
                                         target_vol = 0.35,
                                         use_vol_targeting = FALSE,
                                         target_portfolio_vol = 0.35,
                                         cost_bps = 10,
                                         margin_rate = 0.070,
                                         p_long = 0.58,
                                         min_history_bars = 250,
                                         use_bear_hedge = TRUE,
                                         hedge_stop_loss = -0.10,
                                         hedge_time_stop = 15,
                                         sizing_mode = "sortino_er",
                                         drawdown_throttle = FALSE,
                                         use_net_wick = TRUE,
                                         t1_r = 2.0,
                                         t2_r = 4.0,
                                         t1_scale_pct = 0.35,
                                         t2_scale_pct = 0.35) {

  n_days <- length(eval_dates)
  if (n_days < 10) stop("eval_dates must contain at least 10 trading sessions.")

  # 1. Align Macro Benchmarks strictly across eval_dates
  spy_cl   <- macro_data$spy_close
  qqq_cl   <- macro_data$qqq_close
  rf_vec   <- macro_data$rf_daily
  gld_cl   <- macro_data$gld_close

  spy_ret <- (spy_cl / lag.xts(spy_cl, 1)) - 1
  spy_ret[is.na(spy_ret)] <- 0
  qqq_ret <- (qqq_cl / lag.xts(qqq_cl, 1)) - 1
  qqq_ret[is.na(qqq_ret)] <- 0

  spy_sub <- as.numeric(spy_ret[eval_dates])
  qqq_sub <- as.numeric(qqq_ret[eval_dates])
  spy_sub[is.na(spy_sub)] <- 0
  qqq_sub[is.na(qqq_sub)] <- 0
  rf_sub  <- as.numeric(rf_vec[eval_dates])
  rf_sub[is.na(rf_sub)] <- 0.02 / 252

  # 200-day SMA on SPY for Macro Trend Filter (safe fallback if short history)
  spy_sma_n <- min(200, max(5, floor(length(spy_cl) / 2)))
  spy_sma200 <- SMA(spy_cl, n = spy_sma_n)
  macro_bull <- as.numeric(spy_cl[eval_dates]) >= as.numeric(spy_sma200[eval_dates])
  macro_bull[is.na(macro_bull)] <- TRUE
  macro_bull <- head(macro_bull, n_days)

  # Dual-Bear Detection: QQQ < 50-day SMA and GLD downtrend (if GLD available)
  qqq_sma_n <- min(50, max(5, floor(length(qqq_cl) / 2)))
  qqq_sma50 <- SMA(qqq_cl, n = qqq_sma_n)
  qqq_bear  <- as.numeric(qqq_cl[eval_dates]) < as.numeric(qqq_sma50[eval_dates])
  qqq_bear[is.na(qqq_bear)] <- FALSE
  qqq_bear <- head(qqq_bear, n_days)

  gld_bear <- rep(TRUE, n_days)
  if (!is.null(gld_cl)) {
    gld_aligned <- na.locf(gld_cl[eval_dates], na.rm = FALSE)
    if (nrow(gld_aligned) > 0) {
      gld_sma_n <- min(50, max(5, floor(nrow(gld_aligned) / 2)))
      gld_sma50 <- SMA(gld_aligned, n = gld_sma_n)
      g_diff <- as.numeric(gld_aligned) < as.numeric(gld_sma50)
      g_diff[is.na(g_diff)] <- TRUE
      gld_bear <- head(c(g_diff, rep(TRUE, n_days)), n_days)
    }
  }
  dual_bear <- (qqq_bear & gld_bear)

  # Intraday Breadth Ratio: 10D smoothed market breadth for conviction sizing
  breadth_vec <- if (!is.null(macro_data$intraday_breadth_10d)) {
    as.numeric(macro_data$intraday_breadth_10d[eval_dates])
  } else if (file.exists("data/intraday_breadth_10d.rds")) {
    b_rds <- readRDS("data/intraday_breadth_10d.rds")
    m_br <- merge(xts(rep(0, n_days), order.by = eval_dates), b_rds)
    m_br <- na.locf(na.locf(m_br, na.rm = FALSE), fromLast = TRUE)
    as.numeric(m_br[eval_dates, 2])
  } else {
    rep(0.0, n_days)
  }
  breadth_vec[is.na(breadth_vec)] <- 0.0

  # Inverse Tech / QQQ Hedge Return proxy (3x inverse QQQ minus borrow drag, capped at +/-15%)
  inv_hedge_daily <- pmin(0.15, pmax(-0.15, -3.0 * qqq_sub - (0.0095 / 252)))

  # 2. Portfolio State Trackers
  portfolio_equity   <- numeric(n_days)
  portfolio_cash     <- numeric(n_days)
  portfolio_invested <- numeric(n_days)
  portfolio_borrowed <- numeric(n_days)
  daily_return_vec   <- numeric(n_days)
  margin_interest    <- numeric(n_days)
  cash_yield_vec     <- numeric(n_days)

  current_equity <- initial_capital
  current_cash   <- initial_capital
  peak_equity    <- initial_capital

  # Active open positions: list of position records
  open_positions <- list()
  trade_log      <- list()

  # Defensive Hedge State
  in_hedge_ep    <- FALSE
  hedge_day      <- 0
  hedge_cum      <- 1.0
  hedge_stopped  <- FALSE

  slot_budget <- 1.0 / max_positions
  friction_rate <- cost_bps / 10000

  cat(sprintf("[CleanBacktest] Starting discrete simulation: %d sessions (%s to %s)\n",
              n_days, as.character(eval_dates[1]), as.character(tail(eval_dates, 1))))
  cat(sprintf("                Capital: $%.2f | Max Positions: %d | Leverage: %.1fx | Sizing: %s\n",
              initial_capital, max_positions, leverage, sizing_mode))

  # Day 1: Initialize
  portfolio_equity[1]   <- current_equity
  portfolio_cash[1]     <- current_cash
  portfolio_invested[1] <- 0
  portfolio_borrowed[1] <- 0

  # Pending orders generated on day t for execution on day t+1
  pending_buy_orders <- list()

  for (t in 2:n_days) {
    prev_d <- eval_dates[t - 1]
    curr_d <- eval_dates[t]

    # Yesterday's macro state sets today's market posture (ZERO lookahead)
    is_macro_bull <- macro_bull[t - 1]
    is_dual_bear  <- dual_bear[t - 1]

    # Two-tier drawdown cushion calculation based on previous day's close
    current_dd <- (current_equity - peak_equity) / peak_equity
    dd_scale <- 1.0
    if (drawdown_throttle) {
      if (current_dd <= -0.12) {
        dd_scale <- 0.45
      } else if (current_dd <= -0.08) {
        dd_scale <- 0.72
      }
    }

    # Dynamic Portfolio Volatility Targeting
    dynamic_lev <- leverage
    if (use_vol_targeting && t >= 22) {
      recent_rets <- daily_return_vec[(t - 20):(t - 1)]
      r_vol <- sd(recent_rets) * sqrt(252)
      if (!is.na(r_vol) && r_vol > 0.05) {
        dynamic_lev <- min(leverage, max(0.25, target_portfolio_vol / r_vol))
      }
    }

    # ==========================================================================
    # STEP 1: MORNING EXECUTION (09:30 ET on Day t)
    # Execute pending buy orders generated post-close on Day t-1
    # ==========================================================================
    if (length(pending_buy_orders) > 0 && length(open_positions) < max_positions) {
      for (ord in pending_buy_orders) {
        if (length(open_positions) >= max_positions) break

        # Check sector cap
        held_sectors <- if (length(open_positions) > 0) sapply(open_positions, function(p) p$sector) else character(0)
        sec_count <- sum(held_sectors == ord$sector)
        if (sec_count >= max_per_sector) next

        stk <- stocks_list[[ord$symbol]]
        idx_t <- match(curr_d, stk$dates)
        if (is.na(idx_t)) next

        open_px <- stk$open[idx_t]
        close_px <- stk$close[idx_t]
        high_px  <- stk$high[idx_t]
        low_px   <- stk$low[idx_t]
        atr_val  <- stk$atr[idx_t]
        if (is.na(atr_val) || atr_val <= 0) atr_val <- open_px * 0.02
        if (is.na(open_px) || open_px <= 0) open_px <- close_px
        if (is.na(open_px) || open_px <= 0) next

        # Entry execution at Open(t) with 10 bps slippage
        fill_price <- open_px * (1 + friction_rate)

        # Allocate capital according to sizing engine, drawdown scale, and dynamic leverage
        target_dollar <- current_equity * slot_budget * ord$size_mult * dd_scale * dynamic_lev
        max_single_stock_margin <- current_equity * 0.50 * dynamic_lev
        target_dollar <- min(target_dollar, max_single_stock_margin)

        shares <- target_dollar / fill_price
        if (shares <= 0) next

        cost_dollars <- shares * fill_price
        friction_dollars <- shares * open_px * friction_rate

        risk_1r <- 2.0 * atr_val
        stop_px <- open_px - risk_1r
        t1_px   <- open_px + t1_r * risk_1r
        t2_px   <- open_px + t2_r * risk_1r

        new_pos <- list(
          symbol          = ord$symbol,
          sector          = ord$sector,
          entry_date      = curr_d,
          entry_price     = open_px,
          shares          = shares,
          original_shares = shares,
          prev_price      = open_px,
          stop_price      = stop_px,
          t1_price        = t1_px,
          t2_price        = t2_px,
          high_water_mark = max(open_px, high_px),
          days_held       = 1,
          t1_hit          = FALSE,
          t2_hit          = FALSE,
          shares_pct      = 1.0,
          is_runner       = FALSE
        )

        open_positions[[length(open_positions) + 1]] <- new_pos
        current_cash <- current_cash - cost_dollars - friction_dollars
      }
      pending_buy_orders <- list()
    }

    # ==========================================================================
    # STEP 2: INTRADAY SESSION (09:30 - 16:00 ET on Day t)
    # Check stop-losses, targets, trailing ratchets, and time stops
    # ==========================================================================
    surviving_positions <- list()
    realized_pnl_today  <- 0.0

    if (length(open_positions) > 0) {
      for (pos in open_positions) {
        stk <- stocks_list[[pos$symbol]]
        idx_t <- match(curr_d, stk$dates)
        if (is.na(idx_t)) {
          surviving_positions[[length(surviving_positions) + 1]] <- pos
          next
        }

        open_px  <- stk$open[idx_t]
        high_px  <- stk$high[idx_t]
        low_px   <- stk$low[idx_t]
        close_px <- stk$close[idx_t]
        atr_val  <- stk$atr[idx_t]
        if (is.na(atr_val) || atr_val <= 0) atr_val <- close_px * 0.02
        if (is.na(open_px) || open_px <= 0) open_px <- close_px
        if (is.na(close_px)) {
          surviving_positions[[length(surviving_positions) + 1]] <- pos
          next
        }

        # If entered today, days_held was set to 1; if held overnight, increment
        if (pos$entry_date != curr_d) {
          pos$days_held <- pos$days_held + 1
        }

        high_wm <- max(pos$high_water_mark, high_px)
        chand_stop <- high_wm - 2.5 * atr_val

        # 1. Stop-Loss Check (Intraday)
        if (low_px <= pos$stop_price) {
          exit_px <- min(open_px, pos$stop_price) # Gap-down protection
          proceeds <- pos$shares * exit_px * (1 - friction_rate)
          trade_pnl <- (exit_px * (1 - friction_rate) - pos$entry_price) * pos$shares

          realized_pnl_today <- realized_pnl_today + trade_pnl
          current_cash <- current_cash + proceeds

          trade_log[[length(trade_log) + 1]] <- list(
            symbol = pos$symbol, entry_date = pos$entry_date, exit_date = curr_d,
            entry_price = pos$entry_price, exit_price = exit_px,
            shares = pos$shares, pnl = trade_pnl, reason = "STOP_LOSS",
            days_held = pos$days_held, win = (trade_pnl > 0)
          )
          next # Position closed
        }

        # 2. Tier 1 Profit Target (+2.0R): Scale out t1_scale_pct (default 35%)
        if (!pos$t1_hit && high_px >= pos$t1_price) {
          pos$t1_hit <- TRUE
          shares_to_sell <- pos$original_shares * t1_scale_pct
          shares_to_sell <- min(shares_to_sell, pos$shares)
          exit_px <- pos$t1_price
          proceeds <- shares_to_sell * exit_px * (1 - friction_rate)
          trade_pnl <- (exit_px * (1 - friction_rate) - pos$entry_price) * shares_to_sell

          realized_pnl_today <- realized_pnl_today + trade_pnl
          current_cash <- current_cash + proceeds
          pos$shares <- pos$shares - shares_to_sell
          pos$shares_pct <- pos$shares / pos$original_shares

          # Ratchet stop to breakeven or Chandelier stop
          pos$stop_price <- max(pos$entry_price, chand_stop)

          trade_log[[length(trade_log) + 1]] <- list(
            symbol = pos$symbol, entry_date = pos$entry_date, exit_date = curr_d,
            entry_price = pos$entry_price, exit_price = exit_px,
            shares = shares_to_sell, pnl = trade_pnl, reason = "TARGET_1_SCALE",
            days_held = pos$days_held, win = TRUE
          )
        }

        # 3. Tier 2 Profit Target (+4.0R): Scale out t2_scale_pct (default 35%), leave remaining Runner (30%)
        if (pos$t1_hit && !pos$t2_hit && high_px >= pos$t2_price) {
          pos$t2_hit <- TRUE
          pos$is_runner <- TRUE
          shares_to_sell <- pos$original_shares * t2_scale_pct
          shares_to_sell <- min(shares_to_sell, pos$shares)
          exit_px <- pos$t2_price
          proceeds <- shares_to_sell * exit_px * (1 - friction_rate)
          trade_pnl <- (exit_px * (1 - friction_rate) - pos$entry_price) * shares_to_sell

          realized_pnl_today <- realized_pnl_today + trade_pnl
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

        # Chandelier trailing update
        if (pos$t1_hit) {
          pos$stop_price <- max(pos$stop_price, chand_stop)
        }

        # 4. Vertical Time Stop
        # Base positions exit at Day 7; Confirmed runners exit at Day 20
        max_hold <- if (pos$is_runner) 20 else 7
        if (pos$days_held >= max_hold) {
          exit_px <- close_px
          proceeds <- pos$shares * exit_px * (1 - friction_rate)
          trade_pnl <- (exit_px * (1 - friction_rate) - pos$entry_price) * pos$shares

          realized_pnl_today <- realized_pnl_today + trade_pnl
          current_cash <- current_cash + proceeds

          trade_log[[length(trade_log) + 1]] <- list(
            symbol = pos$symbol, entry_date = pos$entry_date, exit_date = curr_d,
            entry_price = pos$entry_price, exit_price = exit_px,
            shares = pos$shares, pnl = trade_pnl, reason = "TIME_STOP",
            days_held = pos$days_held, win = (trade_pnl > 0)
          )
          next # Position closed
        }

        # Position remains open for overnight holding
        pos$high_water_mark <- high_wm
        pos$prev_price      <- close_px
        surviving_positions[[length(surviving_positions) + 1]] <- pos
      }
      open_positions <- surviving_positions
    }

    # ==========================================================================
    # STEP 3: POST-CLOSE BALANCE SHEET & MARGIN ACCOUNTING (16:00 ET on Day t)
    # ==========================================================================
    # Calculate market value of open positions at Close(t)
    total_stock_value <- 0.0
    if (length(open_positions) > 0) {
      for (pos in open_positions) {
        stk <- stocks_list[[pos$symbol]]
        idx_t <- match(curr_d, stk$dates)
        cur_cl <- if (!is.na(idx_t)) stk$close[idx_t] else pos$prev_price
        total_stock_value <- total_stock_value + (pos$shares * cur_cl)
      }
    }

    # Balance sheet: Equity = Cash + Stock Value
    total_assets <- current_cash + total_stock_value

    # Margin Interest: If stock value > current equity, margin debt is incurred
    borrowed_debt <- max(0, total_stock_value - current_equity)
    daily_margin_interest <- borrowed_debt * (margin_rate / 252)

    # Cash Yield: Idle positive cash earns T-Bill rate
    idle_cash <- max(0, current_cash)
    daily_cash_yield <- idle_cash * rf_sub[t]

    # Defensive Hedge Sleeve Deployment (Dual-Bear Regime)
    hedge_pnl <- 0.0
    if (use_bear_hedge && is_dual_bear && idle_cash > 0) {
      if (!in_hedge_ep) {
        in_hedge_ep   <- TRUE
        hedge_day     <- 0
        hedge_cum     <- 1.0
        hedge_stopped <- FALSE
      }
      hedge_day <- hedge_day + 1

      if (hedge_day > hedge_time_stop) hedge_stopped <- TRUE

      if (!hedge_stopped) {
        h_ret <- inv_hedge_daily[t]
        hedge_pnl <- idle_cash * h_ret
        hedge_cum <- hedge_cum * (1 + h_ret)
        if ((hedge_cum - 1.0) <= hedge_stop_loss) {
          hedge_stopped <- TRUE
        }
      }
    } else {
      in_hedge_ep   <- FALSE
      hedge_day     <- 0
      hedge_cum     <- 1.0
      hedge_stopped <- FALSE
    }

    # Net equity at close of Day t
    new_equity <- current_equity + (total_assets - (portfolio_cash[t - 1] + portfolio_invested[t - 1])) -
                  daily_margin_interest + daily_cash_yield + hedge_pnl
    if (new_equity <= 0) new_equity <- 1.0 # Protect from negative capital

    daily_ret <- (new_equity / current_equity) - 1.0

    current_equity <- new_equity
    peak_equity    <- max(peak_equity, current_equity)

    portfolio_equity[t]   <- current_equity
    portfolio_cash[t]     <- current_cash
    portfolio_invested[t] <- total_stock_value
    portfolio_borrowed[t] <- borrowed_debt
    daily_return_vec[t]   <- daily_ret
    margin_interest[t]    <- daily_margin_interest
    cash_yield_vec[t]     <- daily_cash_yield

    # ==========================================================================
    # STEP 4: POST-CLOSE SIGNAL GENERATION FOR TOMORROW (16:35 ET on Day t)
    # Ranks universe using information through Day t Close.
    # Orders will execute at Open(t+1).
    # ==========================================================================
    empty_slots <- max_positions - length(open_positions)
    if (empty_slots > 0 && t < n_days) {
      cands_syms   <- character(0)
      cands_scores <- numeric(0)
      cands_sector <- character(0)
      cands_sizes  <- numeric(0)

      held_syms <- sapply(open_positions, function(p) p$symbol)

      # In Defensive regime, enforce stricter probability hurdle
      eff_p_hurdle <- if (macro_bull[t]) p_long else 0.65

      for (sym in names(stocks_list)) {
        if (sym %in% held_syms) next
        stk <- stocks_list[[sym]]
        idx_t <- match(curr_d, stk$dates)
        if (is.na(idx_t) || idx_t < min_history_bars) next

        p_up    <- stk$prob[idx_t]
        rs_val  <- stk$rs[idx_t]
        vol_ann <- stk$vol[idx_t]
        atr_ext <- stk$atr_ext[idx_t]
        smom    <- stk$sortino[idx_t]
        eff_r   <- stk$eff_ratio[idx_t]
        nwick   <- stk$net_wick[idx_t]

        if (is.na(p_up) || is.na(rs_val) || is.na(vol_ann)) next

        # Overextension guard: skip extended breakouts > 3.0x ATR
        if (!is.na(atr_ext) && atr_ext > 3.0) next

        # Left-tail crash guard
        if (!is.null(stk$left_tail) && !is.na(stk$left_tail[idx_t]) && stk$left_tail[idx_t] < -3.6) next

        # Entry hurdle
        if (p_up >= eff_p_hurdle && rs_val > 0) {
          # Stage-3 Quality Score
          sm_boost <- if (!is.na(smom)) pmax(0, (smom - 0.80) * 0.06) else 0
          er_boost <- if (!is.na(eff_r)) pmax(0, (eff_r - 0.30) * 0.20) else 0
          nw_boost <- if (use_net_wick && !is.na(nwick)) pmax(0, nwick * 0.10) else 0

          score <- p_up * (1.0 + rs_val + sm_boost + er_boost + nw_boost)

          # Sizing multiplier
          size_mult <- 1.0
          if (sizing_mode == "inverse_vol") {
            size_mult <- pmin(1.25, target_vol / pmax(0.15, vol_ann))
          } else if (sizing_mode == "sortino_er") {
            eff_vol <- pmax(0.15, vol_ann) / (1.0 + 0.20 * er_boost + 0.10 * sm_boost)
            size_mult <- pmin(1.25, target_vol / eff_vol)
          }

          # Breadth Conviction Sizing Multiplier (1.20x oversold <= -0.15, 0.80x overextended >= +0.30)
          cur_breadth <- breadth_vec[t]
          breadth_mult <- if (!is.na(cur_breadth)) {
            if (cur_breadth <= -0.15) 1.20 else if (cur_breadth >= 0.30) 0.80 else 1.0
          } else {
            1.0
          }
          size_mult <- size_mult * breadth_mult

          cands_syms   <- c(cands_syms, sym)
          cands_scores <- c(cands_scores, score)
          cands_sector <- c(cands_sector, stk$sector)
          cands_sizes  <- c(cands_sizes, size_mult)
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
          cand_s   <- top_syms[k]
          cand_sec <- top_secs[k]
          cur_cnt  <- if (!is.na(sec_counts[cand_sec])) as.integer(sec_counts[cand_sec]) else 0

          if (cur_cnt < max_per_sector) {
            pending_buy_orders[[length(pending_buy_orders) + 1]] <- list(
              symbol    = cand_s,
              sector    = cand_sec,
              size_mult = top_sizes[k]
            )
            orders_count <- orders_count + 1
            sec_counts[cand_sec] <- cur_cnt + 1
          }
        }
      }
    }
  }

  # Compile Comprehensive Performance Metrics
  port_xts <- xts(daily_return_vec, order.by = eval_dates)
  m_strat  <- calc_performance_metrics(port_xts, name = "Clean_Discrete_Portfolio")$raw
  m_spy    <- calc_performance_metrics(xts(spy_sub, order.by = eval_dates), name = "SPY")$raw
  m_qqq    <- calc_performance_metrics(xts(qqq_sub, order.by = eval_dates), name = "QQQ")$raw

  trades_df <- if (length(trade_log) > 0) do.call(rbind, lapply(trade_log, as.data.frame)) else data.frame()
  n_trades  <- nrow(trades_df)
  win_rate  <- if (n_trades > 0) sum(trades_df$win) / n_trades else 0.0

  list(
    equity_curve       = portfolio_equity,
    daily_returns      = daily_return_vec,
    dates              = eval_dates,
    terminal_wealth    = tail(portfolio_equity, 1),
    cagr               = m_strat$ann_ret * 100,
    max_dd             = m_strat$max_dd * 100,
    sharpe             = m_strat$sharpe,
    sortino            = m_strat$sortino,
    calmar             = if (!is.na(m_strat$max_dd) && m_strat$max_dd > 0) (m_strat$ann_ret * 100) / (m_strat$max_dd * 100) else NA,
    annual_vol         = m_strat$ann_vol * 100,
    win_rate           = win_rate * 100,
    total_trades       = n_trades,
    trade_log          = trades_df,
    benchmark_spy      = m_spy,
    benchmark_qqq      = m_qqq,
    total_margin_paid  = sum(margin_interest),
    total_cash_yield   = sum(cash_yield_vec)
  )
}
