#' Linear Model Moving Average (lmMA) Implementation
#'
#' Fits a rolling single-factor linear regression over window `n` to extract
#' the local trend level (fit endpoint), instantaneous slope (beta), and
#' trend quality (R-squared).

suppressMessages({
  library(xts)
  library(zoo)
  library(TTR)
})

#' Calculate Linear Model Moving Average (lmMA)
#'
#' @param x Numeric vector or xts time series containing price data.
#' @param n Integer specifying the rolling window size (default: 50).
#' @return A list containing:
#'   - alpha: Rolling regression intercept
#'   - beta: Rolling regression slope (instantaneous trend)
#'   - r.squared: Coefficient of determination (trend quality)
#'   - fit: Fitted value at current observation (endpoint of trend line)
#'   - intercept: Alias for alpha
#'   - slope: Alias for beta
#' @export
lmMA <- function(x, n = 50) {
  if (nrow(x) < n) {
    stop(sprintf("Input length (%d) is shorter than rolling window n (%d)", nrow(x), n))
  }
  
  # Rolling regression of price on time indices
  time_idx <- 1:nrow(x)
  rl <- rollSFM(x, time_idx, n = n)
  
  # Calculate endpoint fit: alpha + beta * current_time
  rl$fit <- rl$alpha + rl$beta * time_idx
  rl$intercept <- rl$alpha
  rl$slope <- rl$beta
  
  # Ensure xts preservation
  if (is.xts(x)) {
    idx <- index(x)
    rl$alpha <- xts(rl$alpha, order.by = idx)
    rl$beta <- xts(rl$beta, order.by = idx)
    rl$r.squared <- xts(rl$r.squared, order.by = idx)
    rl$fit <- xts(rl$fit, order.by = idx)
    rl$intercept <- xts(rl$intercept, order.by = idx)
    rl$slope <- xts(rl$slope, order.by = idx)
  }
  
  return(rl)
}

#' Calculate Dual lmMA Trends (Fast and Slow)
#'
#' @param price Price xts series (typically Close price).
#' @param fast_n Window length for fast trend (e.g., 20 or 50).
#' @param slow_n Window length for slow trend (e.g., 50 or 200).
#' @return A list containing fast_lm, slow_lm, and divergence percentage.
calculate_dual_lmMA <- function(price, fast_n = 20, slow_n = 50) {
  fast_lm <- lmMA(price, n = fast_n)
  slow_lm <- lmMA(price, n = slow_n)
  
  # Divergence %: signed percentage distance between fast and slow trend
  dist_pct <- (fast_lm$fit - slow_lm$fit) / slow_lm$fit * 100
  
  return(list(
    fast_lm = fast_lm,
    slow_lm = slow_lm,
    dist_pct = dist_pct
  ))
}
