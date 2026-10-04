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
  
  # Strategy Daily Return
  strat_gross_ret <- effective_pos[common_idx] * asset_rets
  
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
  
  # Helper to compute annualized performance metrics
  calc_metrics <- function(r_series, name = "Strategy") {
    r_vec <- as.numeric(r_series)
    cum_ret <- prod(1 + r_vec) - 1
    n_days <- length(r_vec)
    ann_ret <- (1 + cum_ret)^(252 / max(n_days, 1)) - 1
    ann_vol <- sd(r_vec) * sqrt(252)
    sharpe  <- if (ann_vol > 0) ann_ret / ann_vol else 0
    
    # Drawdowns
    eq <- cumprod(1 + r_vec)
    peaks <- cummax(eq)
    dds <- (eq - peaks) / peaks
    max_dd <- abs(min(dds))
    calmar <- if (max_dd > 0) ann_ret / max_dd else NA
    
    # Trade statistics
    pos_trades <- r_vec[r_vec > 0]
    neg_trades <- r_vec[r_vec < 0]
    win_rate <- if (length(r_vec[r_vec != 0]) > 0) length(pos_trades) / length(r_vec[r_vec != 0]) else 0
    profit_factor <- if (sum(abs(neg_trades)) > 0) sum(pos_trades) / sum(abs(neg_trades)) else NA
    
    data.frame(
      Strategy = name,
      Cumulative_Return = sprintf("%.2f%%", cum_ret * 100),
      Annualized_Return = sprintf("%.2f%%", ann_ret * 100),
      Annualized_Vol    = sprintf("%.2f%%", ann_vol * 100),
      Sharpe_Ratio      = sprintf("%.2f", sharpe),
      Max_Drawdown      = sprintf("%.2f%%", max_dd * 100),
      Calmar_Ratio      = sprintf("%.2f", calmar),
      Win_Rate          = sprintf("%.1f%%", win_rate * 100),
      Profit_Factor     = sprintf("%.2f", profit_factor)
    )
  }
  
  perf_table <- rbind(
    calc_metrics(strat_net_ret, "ML Swing Strategy (GARCH Vol-Targeted)"),
    calc_metrics(ma_ret, "Classic MA Trend Cross"),
    calc_metrics(bh_ret, paste0("Buy & Hold (", gsub("\\.[A-Za-z]+$", "", colnames(Cl(ohlcv))[1]), ")"))
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
