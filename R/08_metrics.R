#' Standardized Performance & Risk Metrics Engine
#'
#' Unified calculation of risk-adjusted return metrics across all backtesting,
#' evaluation, and monitoring modules.

suppressMessages({
  library(xts)
  library(zoo)
})

#' Calculate Standard Performance Metrics from Return Series
#'
#' @param r_series Numeric vector or xts of daily arithmetic net returns.
#' @param name Strategy or asset label.
#' @param rf Annualized risk-free rate (default: 0).
#' @return A list containing both raw numeric metrics and a formatted single-row data.frame.
#' @export
calc_performance_metrics <- function(r_series, name = "Strategy", rf = 0) {
  r_vec <- as.numeric(na.omit(r_series))
  n_days <- length(r_vec)
  
  if (n_days == 0) {
    empty_df <- data.frame(
      Strategy          = name,
      Cumulative_Return = "0.00%",
      Annualized_Return = "0.00%",
      Annualized_Vol    = "0.00%",
      Sharpe_Ratio      = "0.00",
      Sortino_Ratio     = "N/A",
      Max_Drawdown      = "0.00%",
      Calmar_Ratio      = "N/A",
      Win_Rate          = "0.0%",
      Profit_Factor     = "N/A",
      stringsAsFactors  = FALSE
    )
    return(list(raw = list(), formatted = empty_df))
  }
  
  # 1. Returns and Compounding
  cum_ret <- prod(1 + r_vec) - 1
  ann_ret <- if (n_days >= 5) (1 + cum_ret)^(252 / n_days) - 1 else cum_ret * (252 / max(1, n_days))
  
  # 2. Volatility and Downside Deviation
  daily_sd <- sd(r_vec)
  ann_vol  <- daily_sd * sqrt(252)
  
  # Standardized Sharpe Ratio: (mean(r) - rf/252) / sd(r) * sqrt(252)
  daily_rf <- rf / 252
  excess_mean <- mean(r_vec) - daily_rf
  sharpe <- if (!is.na(daily_sd) && daily_sd > 1e-8) {
    (excess_mean / daily_sd) * sqrt(252)
  } else {
    0.0
  }
  
  # Sortino Ratio (Downside deviation of negative returns relative to daily_rf)
  neg_excess <- pmin(0, r_vec - daily_rf)
  downside_dev <- sqrt(mean(neg_excess^2)) * sqrt(252)
  sortino <- if (!is.na(downside_dev) && downside_dev > 1e-8) {
    (excess_mean * 252) / downside_dev
  } else {
    NA_real_
  }
  
  # 3. Drawdowns
  eq <- cumprod(1 + r_vec)
  peaks <- cummax(eq)
  dds <- (eq - peaks) / peaks
  max_dd <- abs(min(dds, na.rm = TRUE))
  calmar <- if (!is.na(max_dd) && max_dd > 1e-6) ann_ret / max_dd else NA_real_
  
  # 4. Win Rate and Profit Factor
  active_trades <- r_vec[r_vec != 0]
  pos_trades <- r_vec[r_vec > 0]
  neg_trades <- r_vec[r_vec < 0]
  
  win_rate <- if (length(active_trades) > 0) length(pos_trades) / length(active_trades) else 0.0
  gross_gain <- sum(pos_trades)
  gross_loss <- sum(abs(neg_trades))
  profit_factor <- if (gross_loss > 1e-8) {
    gross_gain / gross_loss
  } else if (gross_gain > 0) {
    Inf
  } else {
    NA_real_
  }
  
  raw_res <- list(
    strategy      = name,
    cum_ret       = cum_ret,
    ann_ret       = ann_ret,
    ann_vol       = ann_vol,
    sharpe        = sharpe,
    sortino       = sortino,
    max_dd        = max_dd,
    calmar        = calmar,
    win_rate      = win_rate,
    profit_factor = profit_factor,
    n_days        = n_days
  )
  
  formatted_df <- data.frame(
    Strategy          = name,
    Cumulative_Return = sprintf("%+.2f%%", cum_ret * 100),
    Annualized_Return = sprintf("%+.2f%%", ann_ret * 100),
    Annualized_Vol    = sprintf("%.2f%%", ann_vol * 100),
    Sharpe_Ratio      = sprintf("%.2f", sharpe),
    Sortino_Ratio     = if (!is.na(sortino)) sprintf("%.2f", sortino) else "N/A",
    Max_Drawdown      = sprintf("-%.2f%%", max_dd * 100),
    Calmar_Ratio      = if (!is.na(calmar)) sprintf("%.2f", calmar) else "N/A",
    Win_Rate          = sprintf("%.1f%%", win_rate * 100),
    Profit_Factor     = if (!is.na(profit_factor) && is.finite(profit_factor)) sprintf("%.2f", profit_factor) else if (identical(profit_factor, Inf)) "Inf" else "N/A",
    stringsAsFactors  = FALSE
  )
  
  return(list(raw = raw_res, formatted = formatted_df))
}
