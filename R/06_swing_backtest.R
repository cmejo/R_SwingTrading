#' Swing Trading Simulator & Backtest Engine
#'
#' Simulates out-of-sample swing trading on test bars:
#'   - Realistic next-day execution (no lookahead bias)
#'   - Volatility targeting using GARCH(1,1) conditional volatility
#'   - Benchmark comparisons: Buy & Hold and Classic MA Crossover
#'   - Detailed performance metrics: Sharpe, Max DD, Win Rate, Profit Factor

suppressMessages({
  library(xts)
  library(zoo)
  library(quantmod)
})

#' Run Swing Trading Backtest
#'
#' @param ohlcv Full OHLCV xts object.
#' @param model_res Result object returned by `fit_logistic_swing_model`.
#' @param pipeline_out Result object returned by `build_feature_dataset`.
#' @param allow_short Logical; whether short positions (-1) are taken or held in cash (default: FALSE for long-only swing trading).
#' @param target_vol Annualized target volatility for position sizing (default: 0.25).
#' @param max_leverage Maximum position sizing multiplier (default: 1.5).
#' @param cost_bps Transaction friction / slippage in basis points per trade (default: 10 bps).
#' @return A list containing equity curves, trade logs, and performance metrics.
#' @export
run_swing_backtest <- function(ohlcv,
                               model_res,
                               pipeline_out,
                               allow_short = FALSE,
                               target_vol = 0.30,
                               max_leverage = 1.0,
                               cost_bps = 10) {
  
  test_dates <- model_res$test_dates
  n_test <- length(test_dates)
  
  # Extract price series aligned to test dates
  cl <- Cl(ohlcv)[test_dates]
  op <- Op(ohlcv)[test_dates]
  
  # Calculate 1-period forward daily returns
  daily_rets <- na.omit(diff(log(cl)))
  
  # Raw signals from model (+1, 0, -1)
  raw_signals <- model_res$pred_class
  if (!allow_short) {
    # Long-only mode: replace -1 with 0 (cash)
    raw_signals <- ifelse(raw_signals == 1, 1, 0)
  }
  
  # Signal xts aligned to test dates
  sig_xts <- xts(raw_signals, order.by = test_dates)
  
  # Shift signals by 1 day for next-bar execution (t -> t+1)
  pos_xts <- lag.xts(sig_xts, k = 1)
  pos_xts[is.na(pos_xts)] <- 0
  
  # Volatility targeting weights via GARCH conditional volatility
  garch_vol_test <- pipeline_out$model_data$GARCH_Vol[model_res$test_idx]
  garch_vol_xts  <- xts(garch_vol_test, order.by = test_dates)
  
  # Volatility scaling weight: W_t = clamp(TargetVol / Sigma_t, 0, MaxLeverage)
  vol_weights <- pmin(max_leverage, target_vol / pmax(garch_vol_xts, 0.05))
  vol_weights <- lag.xts(vol_weights, k = 1)
  vol_weights[is.na(vol_weights)] <- 1.0
  
  # Effective position weight
  effective_pos <- pos_xts * vol_weights
  
  # Daily returns of the underlying asset
  asset_rets <- na.omit((cl - lag.xts(cl, 1)) / lag.xts(cl, 1))
  common_idx <- index(asset_rets)
  
  # Realistic next-day execution: on entry bar, order fills at Open (eliminates unearned overnight gap)
  is_entry_bar <- (pos_xts[common_idx] > 0 & lag.xts(pos_xts[common_idx], 1) == 0)
  is_entry_bar[is.na(is_entry_bar)] <- FALSE
  open_to_close_ret <- tryCatch({
    (cl[common_idx] - op[common_idx]) / pmax(op[common_idx], 1e-4)
  }, error = function(e) asset_rets)
  
  bar_rets <- ifelse(is_entry_bar, open_to_close_ret, asset_rets)
  strat_gross_ret <- effective_pos[common_idx] * bar_rets
  
  # Transaction costs: applied whenever position changes
  pos_change <- abs(diff(effective_pos[common_idx]))
  pos_change[is.na(pos_change)] <- 0
  trade_costs <- pos_change * (cost_bps / 10000)
  
  strat_net_ret <- strat_gross_ret - trade_costs
  
  # Benchmark 1: Buy & Hold
  bh_ret <- asset_rets
  
  # Benchmark 2: Classic MA Crossover on the same test period
  fast_lm_fit <- pipeline_out$dual_lm$fast_lm$fit[test_dates]
  slow_lm_fit <- pipeline_out$dual_lm$slow_lm$fit[test_dates]
  ma_sig_vec  <- ifelse(as.numeric(fast_lm_fit) > as.numeric(slow_lm_fit), 1, ifelse(allow_short, -1, 0))
  ma_raw_sig  <- xts(ma_sig_vec, order.by = test_dates)
  ma_pos      <- lag.xts(ma_raw_sig, 1)[common_idx]
  ma_pos[is.na(ma_pos)] <- 0
  ma_ret      <- ma_pos * asset_rets
  
  # Cumulative Equity Curves (starting at $10,000)
  init_capital <- 10000
  strat_equity <- init_capital * cumprod(1 + strat_net_ret)
  bh_equity    <- init_capital * cumprod(1 + bh_ret)
  ma_equity    <- init_capital * cumprod(1 + ma_ret)
  
  equity_xts <- merge(strat_equity, bh_equity, ma_equity)
  colnames(equity_xts) <- c("ML_Swing_Strategy", "Buy_and_Hold", "Classic_MA_Cross")
  
  # Source standardized performance metrics
  if (!exists("calc_performance_metrics")) {
    source("R/08_metrics.R")
  }

  perf_table <- rbind(
    calc_performance_metrics(strat_net_ret, "ML Swing Strategy (GARCH Vol-Targeted)")$formatted,
    calc_performance_metrics(ma_ret, "Classic MA Trend Cross")$formatted,
    calc_performance_metrics(bh_ret, paste0("Buy & Hold (", gsub("\\.[A-Za-z]+$", "", colnames(Cl(ohlcv))[1]), ")"))$formatted
  )
  
  # Generate Trade Log using discrete directional position (pos_xts)
  pos_vals <- as.numeric(pos_xts[common_idx])
  trade_dates <- common_idx
  trades_list <- list()
  in_trade <- FALSE
  entry_price <- 0
  entry_date <- NULL
  entry_pos <- 0
  
  for (i in 1:length(pos_vals)) {
    cur_pos <- pos_vals[i]
    cur_price <- as.numeric(cl[trade_dates[i]])
    
    if (!in_trade && cur_pos != 0) {
      in_trade <- TRUE
      entry_price <- cur_price
      entry_date <- trade_dates[i]
      entry_pos <- cur_pos
    } else if (in_trade && (cur_pos != entry_pos || i == length(pos_vals))) {
      pnl_pct <- (cur_price - entry_price) / entry_price * sign(entry_pos)
      trades_list[[length(trades_list) + 1]] <- data.frame(
        EntryDate = as.character(entry_date),
        ExitDate = as.character(trade_dates[i]),
        Position = ifelse(entry_pos > 0, "LONG", "SHORT"),
        EntryPrice = round(entry_price, 2),
        ExitPrice = round(cur_price, 2),
        ReturnPct = sprintf("%.2f%%", pnl_pct * 100),
        HoldingDays = as.numeric(as.Date(trade_dates[i]) - as.Date(entry_date))
      )
      if (cur_pos != 0) {
        entry_price <- cur_price
        entry_date <- trade_dates[i]
        entry_pos <- cur_pos
      } else {
        in_trade <- FALSE
      }
    }
  }
  
  trade_log <- if (length(trades_list) > 0) do.call(rbind, trades_list) else data.frame()
  
  return(list(
    equity_curves = equity_xts,
    performance_table = perf_table,
    trade_log = trade_log,
    strat_net_ret = strat_net_ret,
    bh_ret = bh_ret,
    ma_ret = ma_ret,
    effective_positions = effective_pos[common_idx]
  ))
}

#' Event-Driven Bracket Execution Backtest (F1)
#'
#' Simulates the strategy's exact live bracket execution rules:
#'   1. Signal generated at t-1 triggers market entry at bar t Open.
#'   2. Sizing: Uses available cash, initial 1R = 2 * sigma_daily * Entry.
#'   3. Stop Loss: Entry - 1R (-2 * sigma_daily).
#'   4. Tier 1: Target at +1.5R (+3 * sigma_daily) exits 50% of position.
#'   5. Breakeven Ratchet: Upon Tier 1 fill, stop loss moves to Entry.
#'   6. Chandelier Trailing Stop: Trailing stop anchored at highest_price - 2.5 * ATR14 protects runner.
#'   7. Tier 2: Target at +3.0R (+6 * sigma_daily) exits remaining 50%.
#'   8. Time Exit: Position held >= 5 trading days is liquidated at bar t Close.
#'   9. Regime/Model Exit: Signal flip to cash/bearish liquidates remaining at bar t Close.
#'
#' @param ohlcv Full OHLCV xts object.
#' @param model_res Output object from logistic/elastic net model.
#' @param pipeline_out Output object from feature pipeline.
#' @param initial_capital Starting portfolio cash (default: $10,000).
#' @param cost_bps Slippage/transaction cost in basis points (default: 5 bps).
#' @return List containing trades_df, equity_curve, daily_returns, metrics, and summary_table.
#' @export
simulate_bracket_backtest <- function(ohlcv,
                                      model_res,
                                      pipeline_out,
                                      initial_capital = 10000,
                                      cost_bps = 5) {
  if (!exists("calc_performance_metrics")) {
    source("R/08_metrics.R")
  }
  
  test_dates <- model_res$test_dates
  n_days <- length(test_dates)
  if (n_days == 0) stop("test_dates is empty in model_res.")

  op <- Op(ohlcv)[test_dates]
  hi <- Hi(ohlcv)[test_dates]
  lo <- Lo(ohlcv)[test_dates]
  cl <- Cl(ohlcv)[test_dates]

  atr_xts <- tryCatch({
    TTR::ATR(HLC(ohlcv), n = 14)$atr[test_dates]
  }, error = function(e) cl * 0.02)

  garch_vol_vec <- pipeline_out$model_data$GARCH_Vol[model_res$test_idx]
  garch_vol_xts <- xts(garch_vol_vec, order.by = test_dates)

  raw_signals <- model_res$pred_class
  sig_xts <- xts(raw_signals, order.by = test_dates)

  cash <- initial_capital
  pos <- NULL
  trades <- list()
  equity_vec <- numeric(n_days)

  for (i in 1:n_days) {
    cur_d   <- test_dates[i]
    o_px    <- as.numeric(op[i])
    h_px    <- as.numeric(hi[i])
    l_px    <- as.numeric(lo[i])
    c_px    <- as.numeric(cl[i])
    cur_atr <- as.numeric(atr_xts[i])
    if (is.na(cur_atr) || cur_atr <= 0) cur_atr <- c_px * 0.02

    # Check entry if flat and yesterday's signal was BUY (i > 1)
    if (is.null(pos) && i > 1) {
      prev_sig <- as.numeric(sig_xts[i - 1])
      if (!is.na(prev_sig) && prev_sig == 1 && cash > 100) {
        ent_px <- if (!is.na(o_px) && o_px > 0) o_px else c_px
        ann_vol <- as.numeric(garch_vol_xts[i - 1])
        if (is.na(ann_vol) || ann_vol < 0.05) ann_vol <- 0.25
        daily_vol <- ann_vol / sqrt(252)

        risk_1r <- 2.0 * daily_vol * ent_px
        stop_px <- round(ent_px - risk_1r, 2)
        t1_px   <- round(ent_px + 1.5 * risk_1r, 2)
        t2_px   <- round(ent_px + 3.0 * risk_1r, 2)

        shs <- floor((cash * 0.99) / ent_px)
        if (shs > 0) {
          cost_entry <- shs * ent_px * (cost_bps / 10000)
          cash <- cash - (shs * ent_px + cost_entry)
          pos <- list(
            symbol = colnames(cl)[1],
            entry_date = cur_d,
            entry_price = ent_px,
            shares = shs,
            initial_shares = shs,
            stop_loss = stop_px,
            tier1_target = t1_px,
            tier2_target = t2_px,
            tier1_filled = FALSE,
            highest_price = ent_px,
            risk_1r = risk_1r,
            days_held = 0
          )
        }
      }
    }

    # Evaluate active position on current bar
    if (!is.null(pos)) {
      pos$days_held <- pos$days_held + 1
      pos$highest_price <- max(pos$highest_price, h_px)
      chandelier_stop <- round(pos$highest_price - 2.5 * cur_atr, 2)

      # 1. Stop-Loss Hit
      if (l_px <= pos$stop_loss) {
        exit_px <- min(o_px, pos$stop_loss)
        shs_exit <- pos$shares
        pnl <- (exit_px - pos$entry_price) * shs_exit - (shs_exit * exit_px * (cost_bps / 10000))
        cash <- cash + (shs_exit * exit_px) - (shs_exit * exit_px * (cost_bps / 10000))
        r_mult <- (exit_px - pos$entry_price) / pos$risk_1r
        trades[[length(trades) + 1]] <- data.frame(
          Symbol = pos$symbol, EntryDate = as.character(pos$entry_date),
          ExitDate = as.character(cur_d), EntryPrice = pos$entry_price,
          ExitPrice = exit_px, Shares = shs_exit, PnL = pnl,
          ReturnPct = (exit_px / pos$entry_price - 1) * 100, R_Multiple = r_mult,
          Reason = if (pos$tier1_filled) "BREAKEVEN_OR_TRAILING_STOP" else "STOP_LOSS",
          DaysHeld = pos$days_held, stringsAsFactors = FALSE
        )
        pos <- NULL
      } else {
        # 2. Tier 1 Target Hit
        if (!pos$tier1_filled && h_px >= pos$tier1_target) {
          t1_shs <- max(1, floor(pos$shares / 2))
          exit_px <- max(o_px, pos$tier1_target)
          pnl <- (exit_px - pos$entry_price) * t1_shs - (t1_shs * exit_px * (cost_bps / 10000))
          cash <- cash + (t1_shs * exit_px) - (t1_shs * exit_px * (cost_bps / 10000))
          r_mult <- (exit_px - pos$entry_price) / pos$risk_1r
          trades[[length(trades) + 1]] <- data.frame(
            Symbol = pos$symbol, EntryDate = as.character(pos$entry_date),
            ExitDate = as.character(cur_d), EntryPrice = pos$entry_price,
            ExitPrice = exit_px, Shares = t1_shs, PnL = pnl,
            ReturnPct = (exit_px / pos$entry_price - 1) * 100, R_Multiple = r_mult,
            Reason = "TIER1_TARGET", DaysHeld = pos$days_held, stringsAsFactors = FALSE
          )
          pos$tier1_filled <- TRUE
          pos$shares <- pos$shares - t1_shs
          pos$stop_loss <- max(pos$entry_price, chandelier_stop)

          # Check if Tier 2 target also reached on same bar
          if (pos$shares > 0 && h_px >= pos$tier2_target) {
            t2_shs <- pos$shares
            exit_px2 <- max(o_px, pos$tier2_target)
            pnl2 <- (exit_px2 - pos$entry_price) * t2_shs - (t2_shs * exit_px2 * (cost_bps / 10000))
            cash <- cash + (t2_shs * exit_px2) - (t2_shs * exit_px2 * (cost_bps / 10000))
            r_mult2 <- (exit_px2 - pos$entry_price) / pos$risk_1r
            trades[[length(trades) + 1]] <- data.frame(
              Symbol = pos$symbol, EntryDate = as.character(pos$entry_date),
              ExitDate = as.character(cur_d), EntryPrice = pos$entry_price,
              ExitPrice = exit_px2, Shares = t2_shs, PnL = pnl2,
              ReturnPct = (exit_px2 / pos$entry_price - 1) * 100, R_Multiple = r_mult2,
              Reason = "TIER2_TARGET", DaysHeld = pos$days_held, stringsAsFactors = FALSE
            )
            pos <- NULL
          }
        } else if (pos$tier1_filled) {
          pos$stop_loss <- max(pos$stop_loss, chandelier_stop)

          # 3. Tier 2 Target Hit on subsequent bar
          if (h_px >= pos$tier2_target) {
            t2_shs <- pos$shares
            exit_px <- max(o_px, pos$tier2_target)
            pnl <- (exit_px - pos$entry_price) * t2_shs - (t2_shs * exit_px * (cost_bps / 10000))
            cash <- cash + (t2_shs * exit_px) - (t2_shs * exit_px * (cost_bps / 10000))
            r_mult <- (exit_px - pos$entry_price) / pos$risk_1r
            trades[[length(trades) + 1]] <- data.frame(
              Symbol = pos$symbol, EntryDate = as.character(pos$entry_date),
              ExitDate = as.character(cur_d), EntryPrice = pos$entry_price,
              ExitPrice = exit_px, Shares = t2_shs, PnL = pnl,
              ReturnPct = (exit_px / pos$entry_price - 1) * 100, R_Multiple = r_mult,
              Reason = "TIER2_TARGET", DaysHeld = pos$days_held, stringsAsFactors = FALSE
            )
            pos <- NULL
          }
        }

        # 4. Time Expiration (5 Trading Days)
        if (!is.null(pos) && pos$days_held >= 5) {
          shs_exit <- pos$shares
          exit_px <- c_px
          pnl <- (exit_px - pos$entry_price) * shs_exit - (shs_exit * exit_px * (cost_bps / 10000))
          cash <- cash + (shs_exit * exit_px) - (shs_exit * exit_px * (cost_bps / 10000))
          r_mult <- (exit_px - pos$entry_price) / pos$risk_1r
          trades[[length(trades) + 1]] <- data.frame(
            Symbol = pos$symbol, EntryDate = as.character(pos$entry_date),
            ExitDate = as.character(cur_d), EntryPrice = pos$entry_price,
            ExitPrice = exit_px, Shares = shs_exit, PnL = pnl,
            ReturnPct = (exit_px / pos$entry_price - 1) * 100, R_Multiple = r_mult,
            Reason = "TIME_EXPIRATION", DaysHeld = pos$days_held, stringsAsFactors = FALSE
          )
          pos <- NULL
        }

        # 5. Model Bearish Signal Flip Exit (only if model flips to distinctly bearish < 0 on subsequent bars)
        if (!is.null(pos) && pos$entry_date != cur_d && as.numeric(sig_xts[i]) < 0) {
          shs_exit <- pos$shares
          exit_px <- c_px
          pnl <- (exit_px - pos$entry_price) * shs_exit - (shs_exit * exit_px * (cost_bps / 10000))
          cash <- cash + (shs_exit * exit_px) - (shs_exit * exit_px * (cost_bps / 10000))
          r_mult <- (exit_px - pos$entry_price) / pos$risk_1r
          trades[[length(trades) + 1]] <- data.frame(
            Symbol = pos$symbol, EntryDate = as.character(pos$entry_date),
            ExitDate = as.character(cur_d), EntryPrice = pos$entry_price,
            ExitPrice = exit_px, Shares = shs_exit, PnL = pnl,
            ReturnPct = (exit_px / pos$entry_price - 1) * 100, R_Multiple = r_mult,
            Reason = "MODEL_SIGNAL_EXIT", DaysHeld = pos$days_held, stringsAsFactors = FALSE
          )
          pos <- NULL
        }
      }
    }

    # Record end-of-day equity
    cur_equity <- cash + (if (!is.null(pos)) pos$shares * c_px else 0)
    equity_vec[i] <- cur_equity
  }

  equity_xts <- xts(equity_vec, order.by = test_dates)
  daily_returns <- na.omit(diff(equity_xts) / lag.xts(equity_xts, 1))

  trades_df <- if (length(trades) > 0) do.call(rbind, trades) else data.frame()
  perf <- calc_performance_metrics(daily_returns, name = "EventDriven_Bracket_Strategy")

  return(list(
    trades_df = trades_df,
    equity_curve = equity_xts,
    daily_returns = daily_returns,
    metrics = perf$raw,
    summary_table = perf$formatted
  ))
}

#' Alias for simulate_bracket_backtest
#' @export
run_event_driven_backtest <- simulate_bracket_backtest
