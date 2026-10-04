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
  n_obs <- NROW(x)
  if (n_obs < n) {
    stop(sprintf("Input length (%d) is shorter than rolling window n (%d)", n_obs, n))
  }
  
  # Align time index strictly to x's time-series index
  if (is.xts(x)) {
    time_idx <- xts(1:n_obs, order.by = index(x))
  } else {
    time_idx <- 1:n_obs
  }
  
  rl_raw <- rollSFM(x, time_idx, n = n)
  
  # Extract components safely by column name
  alpha_val <- as.numeric(rl_raw[, "alpha"])
  beta_val  <- as.numeric(rl_raw[, "beta"])
  rsq_val   <- as.numeric(rl_raw[, "r.squared"])
  fit_val   <- alpha_val + beta_val * as.numeric(time_idx)
  
  if (is.xts(x)) {
    idx <- index(x)
    rl <- list(
      alpha     = xts(alpha_val, order.by = idx),
      beta      = xts(beta_val, order.by = idx),
      r.squared = xts(rsq_val, order.by = idx),
      fit       = xts(fit_val, order.by = idx),
      intercept = xts(alpha_val, order.by = idx),
      slope     = xts(beta_val, order.by = idx)
    )
  } else {
    rl <- list(
      alpha     = alpha_val,
      beta      = beta_val,
      r.squared = rsq_val,
      fit       = fit_val,
      intercept = alpha_val,
      slope     = beta_val
    )
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
