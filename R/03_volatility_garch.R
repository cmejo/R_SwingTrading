#' Two-Regime Markov-Switching GARCH (MS-GARCH) Volatility Engine
#'
#' Wholesale replacement of single-regime GARCH(1,1) with a 2-regime
#' Markov-Switching GARCH (MS-GARCH) process.
#'
#' Model Specification:
#'   r_t = eps_t, eps_t = sigma_{k,t} * z_t, z_t ~ iid N(0, 1)
#'   sigma_{k,t}^2 = alpha_{0,k} + alpha_{1,k} * eps_{t-1}^2 + beta_k * sigma_{k,t-1}^2
#'   for regimes k in {1, 2}, governed by a first-order Markov transition matrix:
#'   P = [ P_{1,1}   1 - P_{1,1} ]
#'       [ P_{2,1}   1 - P_{2,1} ]
#'
#' Automatically identifies Low-Vol vs High-Vol regimes, extracts regime-specific
#' GARCH parameters (alpha0, alpha1, beta, persistence, unconditional variance),
#' transition probabilities, conditional volatility, and filtered state probabilities
#' P(S_t = High Vol | F_t).

suppressMessages({
  library(xts)
  library(zoo)
})

# Resilient package loader for MSGARCH
has_msgarch <- requireNamespace("MSGARCH", quietly = TRUE)
if (!has_msgarch) {
  cat("[MS-GARCH] Notice: Package 'MSGARCH' not found in R environment. Attempting auto-installation...\n")
  tryCatch({
    install.packages("MSGARCH", repos = "https://cloud.r-project.org", quiet = TRUE)
    has_msgarch <- requireNamespace("MSGARCH", quietly = TRUE)
  }, error = function(e) {
    cat(sprintf("[MS-GARCH] Auto-installation note: %s\n", e$message))
  })
}

if (has_msgarch) {
  suppressMessages(library(MSGARCH))
}

#' Fit Two-Regime MS-GARCH and extract regime parameters and state probabilities
#'
#' @param price_xts xts series of prices (Close or Adjusted).
#' @param train_idx Optional vector of integer indices to fit parameters on (for no-lookahead out-of-sample testing).
#' @return A list containing:
#'   - model: Fitted MSGARCH ML object
#'   - spec: MSGARCH specification object
#'   - regime_params: List of high_vol, low_vol parameters, transition matrix, and stable probabilities
#'   - high_regime_idx: Regime index (1 or 2) corresponding to the High-Volatility regime
#'   - low_regime_idx: Regime index (1 or 2) corresponding to the Low-Volatility regime
#'   - sigma: xts series of daily conditional standard deviation sigma_t (GARCH_Sigma)
#'   - annualized_vol: xts series of annualized conditional volatility sigma_t * sqrt(252) (GARCH_AnnVol)
#'   - shocks: Standardized return residuals eps_t / sigma_t (GARCH_Shock)
#'   - vol_percentile: Rolling 50-day percentile ranking of volatility (GARCH_VolPct)
#'   - prob_high_vol: Filtered probability P(S_t = High Vol | F_t) (MSGARCH_ProbHighVol)
#'   - prob_low_vol: Filtered probability P(S_t = Low Vol | F_t) (MSGARCH_ProbLowVol)
#'   - regime_state: Binary regime classification (1 = Low Vol, 2 = High Vol) based on P(High) > 0.5
#'   - smooth_prob_high_vol: Smoothed probability P(S_t = High Vol | F_T)
#' @export
compute_msgarch_volatility <- function(price_xts, train_idx = NULL) {
  # Calculate continuous log returns
  rets <- na.omit(diff(log(price_xts)))
  n <- nrow(rets)
  dates <- index(rets)
  rets_vec <- as.numeric(rets)
  
  if (n < 20) {
    stop("[MS-GARCH] Error: Insufficient return observations (minimum 20 required).")
  }
  
  # Determine sample to fit parameters (prevents lookahead bias in backtests)
  fit_sample <- if (!is.null(train_idx)) {
    adj_train <- intersect(train_idx - 1, 1:n)
    if (length(adj_train) < 20) rets_vec else rets_vec[adj_train]
  } else {
    rets_vec
  }
  
  spec <- NULL
  fit  <- NULL
  
  if (isTRUE(has_msgarch)) {
    cat("[MS-GARCH] Fitting 2-regime Markov-Switching GARCH (sGARCH/norm) via ML...\n")
    spec <- tryCatch({
      CreateSpec(
        variance.spec = list(model = c("sGARCH", "sGARCH")),
        distribution.spec = list(distribution = c("norm", "norm"))
      )
    }, error = function(e) NULL)
    
    if (!is.null(spec)) {
      fit <- tryCatch({
        FitML(spec = spec, data = fit_sample)
      }, error = function(e) {
        cat(sprintf("[MS-GARCH] Warning: ML fitting failed (%s). Attempting fallback...\n", e$message))
        NULL
      })
    }
  } else {
    cat("[MS-GARCH] Notice: MSGARCH package not loaded. Using robust empirical volatility.\n")
  }
  
  # Check parameter validity
  is_valid_fit <- !is.null(fit) && !is.null(fit$par) && all(is.finite(fit$par))
  
  if (is_valid_fit) {
    p <- fit$par
    
    # Regime 1 parameters
    a0_1 <- as.numeric(p["alpha0_1"])
    a1_1 <- as.numeric(p["alpha1_1"])
    b1_1 <- as.numeric(p["beta_1"])
    pers_1 <- a1_1 + b1_1
    unc_var_1 <- if (pers_1 < 0.999 && (1 - pers_1) > 0) a0_1 / (1 - pers_1) else a0_1 / 1e-4
    unc_ann_vol_1 <- sqrt(unc_var_1 * 252)
    
    # Regime 2 parameters
    a0_2 <- as.numeric(p["alpha0_2"])
    a1_2 <- as.numeric(p["alpha1_2"])
    b1_2 <- as.numeric(p["beta_2"])
    pers_2 <- a1_2 + b1_2
    unc_var_2 <- if (pers_2 < 0.999 && (1 - pers_2) > 0) a0_2 / (1 - pers_2) else a0_2 / 1e-4
    unc_ann_vol_2 <- sqrt(unc_var_2 * 252)
    
    # Dynamically designate High-Vol vs Low-Vol regime
    high_regime_idx <- if (unc_var_2 >= unc_var_1) 2 else 1
    low_regime_idx  <- if (high_regime_idx == 2) 1 else 2
    
    # Transition probabilities
    p11 <- as.numeric(p["P_1_1"])
    p21 <- as.numeric(p["P_2_1"])
    p12 <- 1 - p11
    p22 <- 1 - p21
    
    trans_mat <- matrix(c(p11, p12, p21, p22), nrow = 2, byrow = TRUE,
                        dimnames = list(c("t|k=1", "t|k=2"), c("t+1|k=1", "t+1|k=2")))
    
    # Unconditional / stationary state distribution: pi * P = pi, pi1 + pi2 = 1
    denom <- (p12 + p21)
    pi1 <- if (denom > 0) p21 / denom else 0.5
    pi2 <- 1 - pi1
    stable_prob <- setNames(c(pi1, pi2), c("State 1", "State 2"))
    
    # Regime parameter bundles
    low_params <- list(
      regime_index = low_regime_idx,
      alpha0 = if (low_regime_idx == 1) a0_1 else a0_2,
      alpha1 = if (low_regime_idx == 1) a1_1 else a1_2,
      beta   = if (low_regime_idx == 1) b1_1 else b1_2,
      persistence = if (low_regime_idx == 1) pers_1 else pers_2,
      unc_variance = if (low_regime_idx == 1) unc_var_1 else unc_var_2,
      unc_ann_vol = if (low_regime_idx == 1) unc_ann_vol_1 else unc_ann_vol_2
    )
    
    high_params <- list(
      regime_index = high_regime_idx,
      alpha0 = if (high_regime_idx == 1) a0_1 else a0_2,
      alpha1 = if (high_regime_idx == 1) a1_1 else a1_2,
      beta   = if (high_regime_idx == 1) b1_1 else b1_2,
      persistence = if (high_regime_idx == 1) pers_1 else pers_2,
      unc_variance = if (high_regime_idx == 1) unc_var_1 else unc_var_2,
      unc_ann_vol = if (high_regime_idx == 1) unc_ann_vol_1 else unc_ann_vol_2
    )
    
    cat(sprintf("[MS-GARCH] Regime %d (LOW-VOL):  a0=%.6f, a1=%.4f, b1=%.4f (pers=%.4f, unc_vol=%.1f%%)\n",
                low_regime_idx, low_params$alpha0, low_params$alpha1, low_params$beta,
                low_params$persistence, low_params$unc_ann_vol * 100))
    cat(sprintf("[MS-GARCH] Regime %d (HIGH-VOL): a0=%.6f, a1=%.4f, b1=%.4f (pers=%.4f, unc_vol=%.1f%%)\n",
                high_regime_idx, high_params$alpha0, high_params$alpha1, high_params$beta,
                high_params$persistence, high_params$unc_ann_vol * 100))
    cat(sprintf("[MS-GARCH] Transition Matrix: P(1->1)=%.4f, P(2->1)=%.4f | Steady-State Low/High: %.1f%% / %.1f%%\n",
                p11, p21, stable_prob[low_regime_idx] * 100, stable_prob[high_regime_idx] * 100))
    
    # Compute conditional volatility and state probabilities on the full series
    vol_calc <- tryCatch({
      if (is.null(train_idx)) {
        as.numeric(Volatility(fit))
      } else {
        as.numeric(Volatility(spec, par = p, data = rets_vec))
      }
    }, error = function(e) {
      cat(sprintf("[MS-GARCH] Volatility calculation error: %s. Using sample sd.\n", e$message))
      rep(sd(rets_vec), n)
    })
    
    states_calc <- tryCatch({
      if (is.null(train_idx)) {
        State(fit)
      } else {
        State(spec, par = p, data = rets_vec)
      }
    }, error = function(e) {
      cat(sprintf("[MS-GARCH] State calculation error: %s.\n", e$message))
      NULL
    })
    
    sigma_vec <- vol_calc
    
    if (!is.null(states_calc) && !is.null(states_calc$FiltProb)) {
      # Filtered probabilities P(S_t = k | F_t)
      p_high_vec <- as.numeric(states_calc$FiltProb[, 1, high_regime_idx])
      p_low_vec  <- as.numeric(states_calc$FiltProb[, 1, low_regime_idx])
      
      # Smoothed probabilities P(S_t = k | F_T)
      smooth_high_vec <- if (!is.null(states_calc$SmoothProb)) {
        as.numeric(states_calc$SmoothProb[1:n, 1, high_regime_idx])
      } else {
        p_high_vec
      }
    } else {
      # Fallback neutral probabilities
      p_high_vec <- rep(0.5, n)
      p_low_vec  <- rep(0.5, n)
      smooth_high_vec <- rep(0.5, n)
    }
    
    regime_params_bundle <- list(
      high_vol = high_params,
      low_vol = low_params,
      trans_mat = trans_mat,
      stable_prob = stable_prob,
      fitted_par = p
    )
    
  } else {
    # ROBUST FALLBACK: Rolling 20-day standard deviation
    cat("[MS-GARCH] Notice: Utilizing rolling empirical standard deviation fallback.\n")
    sd_fallback <- sd(rets_vec, na.rm = TRUE)
    if (is.na(sd_fallback) || sd_fallback <= 0) sd_fallback <- 0.02
    
    roll_sd <- as.numeric(TTR::runSD(rets, n = 20))
    roll_sd[is.na(roll_sd) | roll_sd <= 0] <- sd_fallback
    sigma_vec <- roll_sd
    
    high_regime_idx <- 2
    low_regime_idx  <- 1
    
    p_high_vec <- rep(0.5, n)
    p_low_vec  <- rep(0.5, n)
    smooth_high_vec <- rep(0.5, n)
    
    regime_params_bundle <- list(
      high_vol = list(regime_index = 2, alpha0 = NA, alpha1 = NA, beta = NA, persistence = NA, unc_variance = NA, unc_ann_vol = sd_fallback * sqrt(252)),
      low_vol  = list(regime_index = 1, alpha0 = NA, alpha1 = NA, beta = NA, persistence = NA, unc_variance = NA, unc_ann_vol = sd_fallback * sqrt(252)),
      trans_mat = matrix(c(0.9, 0.1, 0.1, 0.9), nrow = 2),
      stable_prob = c(0.5, 0.5),
      fitted_par = NULL
    )
  }
  
  # Ensure sigma bounds are strictly positive
  sigma_vec <- pmax(sigma_vec, 1e-6)
  
  # Standardized shocks: z_t = r_t / sigma_t
  shocks_vec <- rets_vec / sigma_vec
  
  # Rolling volatility percentile (50-day window)
  vol_pct <- rollapply(sigma_vec, width = min(50, n), FUN = function(x) {
    cur <- tail(x, 1)
    mean(x <= cur)
  }, fill = 0.5, align = "right")
  
  # Binary regime classification: 1 for Low Vol, 2 for High Vol
  regime_state_vec <- ifelse(p_high_vec > 0.5, 2, 1)
  
  # Convert series to xts with exact dates
  sigma_xts          <- xts(sigma_vec, order.by = dates)
  ann_vol_xts        <- sigma_xts * sqrt(252)
  shocks_xts         <- xts(shocks_vec, order.by = dates)
  vol_pct_xts        <- xts(vol_pct, order.by = dates)
  prob_high_vol_xts  <- xts(p_high_vec, order.by = dates)
  prob_low_vol_xts   <- xts(p_low_vec, order.by = dates)
  smooth_high_xts    <- xts(smooth_high_vec, order.by = dates)
  regime_state_xts   <- xts(regime_state_vec, order.by = dates)
  
  # Standardized column names
  colnames(sigma_xts)         <- "GARCH_Sigma"
  colnames(ann_vol_xts)       <- "GARCH_AnnVol"
  colnames(shocks_xts)        <- "GARCH_Shock"
  colnames(vol_pct_xts)       <- "GARCH_VolPct"
  colnames(prob_high_vol_xts) <- "MSGARCH_ProbHighVol"
  colnames(prob_low_vol_xts)  <- "MSGARCH_ProbLowVol"
  colnames(smooth_high_xts)   <- "MSGARCH_SmoothHighVol"
  colnames(regime_state_xts)  <- "MSGARCH_Regime"
  
  return(list(
    model                = fit,
    spec                 = spec,
    regime_params        = regime_params_bundle,
    high_regime_idx      = high_regime_idx,
    low_regime_idx       = low_regime_idx,
    sigma                = sigma_xts,
    annualized_vol       = ann_vol_xts,
    shocks               = shocks_xts,
    vol_percentile       = vol_pct_xts,
    prob_high_vol        = prob_high_vol_xts,
    prob_low_vol         = prob_low_vol_xts,
    smooth_prob_high_vol = smooth_high_xts,
    regime_state         = regime_state_xts
  ))
}

#' Backward compatibility alias
#' @export
compute_garch_volatility <- compute_msgarch_volatility
