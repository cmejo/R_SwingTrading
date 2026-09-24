#' GARCH(1,1) Volatility Model Implementation
#'
#' Fits a stationary GARCH(1,1) process to log returns to model time-varying
#' conditional heteroskedasticity, volatility clustering, and standardized shocks.
#'
#' Model Specification:
#'   r_t = mu + eps_t, eps_t = sigma_t * z_t, z_t ~ iid N(0, 1)
#'   sigma_t^2 = a0 + a1 * eps_{t-1}^2 + b1 * sigma_{t-1}^2

suppressMessages({
  library(xts)
  library(zoo)
  library(tseries)
})

#' Fit GARCH(1,1) and extract conditional volatility
#'
#' @param price_xts xts series of prices (Close or Adjusted).
#' @param train_idx Optional vector of integer indices to fit GARCH model on (for no-lookahead out-of-sample testing).
#' @return A list containing:
#'   - model: Fitted garch object from tseries
#'   - sigma: xts series of daily conditional standard deviation sigma_t
#'   - annualized_vol: xts series of annualized volatility (sigma_t * sqrt(252))
#'   - shocks: Standardized return residuals (eps_t / sigma_t)
#'   - vol_percentile: Rolling percentile ranking of volatility (0 to 1)
#' @export
compute_garch_volatility <- function(price_xts, train_idx = NULL) {
  # Calculate continuous log returns
  rets <- na.omit(diff(log(price_xts)))
  n <- nrow(rets)
  dates <- index(rets)
  rets_vec <- as.numeric(rets)
  
  # Determine sample to fit parameters
  fit_sample <- if (!is.null(train_idx)) {
    # Adjust train_idx for the diff offset (-1)
    adj_train <- intersect(train_idx - 1, 1:n)
    rets_vec[adj_train]
  } else {
    rets_vec
  }
  
  cat("[GARCH] Estimating GARCH(1,1) parameters via quasi-maximum likelihood...\n")
  fit <- tryCatch({
    garch(fit_sample, order = c(1, 1), trace = FALSE)
  }, error = function(e) {
    cat(sprintf("[GARCH] Warning: GARCH convergence failed (%s). Falling back to ARCH(1)...\n", e$message))
    tryCatch({
      garch(fit_sample, order = c(0, 1), trace = FALSE)
    }, error = function(e2) {
      cat("[GARCH] Warning: ARCH failed. Using rolling empirical standard deviation.\n")
      NULL
    })
  })
  
  if (!is.null(fit)) {
    # Extract fitted parameters
    coefs <- coef(fit)
    a0 <- coefs["a0"]
    a1 <- coefs["a1"]
    b1 <- if ("b1" %in% names(coefs)) coefs["b1"] else 0
    cat(sprintf("[GARCH] Fitted: a0=%.6f, a1=%.4f, b1=%.4f (persistence=%.4f)\n",
                a0, a1, b1, a1 + b1))
    
    # Compute full conditional variance path recursively
    sigma2 <- numeric(n)
    # Unconditional variance as initialization
    unc_var <- if ((a1 + b1) < 1 && (1 - a1 - b1) > 0) a0 / (1 - a1 - b1) else var(fit_sample)
    sigma2[1] <- unc_var
    
    for (t in 2:n) {
      eps_prev2 <- rets_vec[t - 1]^2
      sigma2[t] <- a0 + a1 * eps_prev2 + b1 * sigma2[t - 1]
    }
    sigma_vec <- sqrt(sigma2)
  } else {
    # Fallback to rolling standard deviation (20-day window)
    sigma_vec <- as.numeric(TTR::runSD(rets, n = 20))
    # Replace initial NAs with sample sd
    sigma_vec[is.na(sigma_vec)] <- sd(rets_vec, na.rm = TRUE)
  }
  
  # Standardized shocks: z_t = r_t / sigma_t
  shocks_vec <- rets_vec / pmax(sigma_vec, 1e-6)
  
  # Rolling volatility percentile (50-day window)
  vol_pct <- rollapply(sigma_vec, width = 50, FUN = function(x) {
    cur <- tail(x, 1)
    mean(x <= cur)
  }, fill = 0.5, align = "right")
  
  # Convert to xts matching the return dates
  sigma_xts <- xts(sigma_vec, order.by = dates)
  ann_vol_xts <- sigma_xts * sqrt(252)
  shocks_xts <- xts(shocks_vec, order.by = dates)
  vol_pct_xts <- xts(vol_pct, order.by = dates)
  
  colnames(sigma_xts) <- "GARCH_Sigma"
  colnames(ann_vol_xts) <- "GARCH_AnnVol"
  colnames(shocks_xts) <- "GARCH_Shock"
  colnames(vol_pct_xts) <- "GARCH_VolPct"
  
  return(list(
    model = fit,
    sigma = sigma_xts,
    annualized_vol = ann_vol_xts,
    shocks = shocks_xts,
    vol_percentile = vol_pct_xts
  ))
}
