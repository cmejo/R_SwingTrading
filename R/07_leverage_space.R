#' Ralph Vince Leverage Space Model (LSPM) for Portfolio Sizing
#'
#' Implements Ralph Vince's Leverage Space Portfolio Model to compute
#' the optimal leverage vector f* that maximizes the portfolio's
#' Geometric Holding Period Return (GHPR) across joint historical return scenarios.
#'
#' Key Concepts:
#'   1. Joint Scenario Matrix: Empirical multi-asset daily return distributions.
#'   2. Optimal f: The fraction of equity allocated to each asset to maximize long-term growth.
#'   3. Safe f (Fractional f): Scaled by a conservative safety multiplier to control drawdown risk.
#'   4. Capital Allocation: Translates optimal f into exact position sizing and share counts.

suppressMessages({
  library(xts)
  library(zoo)
  library(quantmod)
})

source("R/01_data_loader.R")

#' Build Joint Return Scenario Matrix Across Active Assets
#'
#' @param symbols Character vector of ticker symbols.
#' @param lookback_days Number of trailing trading bars to include (default: 120).
#' @return A clean numeric matrix of aligned daily returns (rows = dates, cols = symbols).
#' @export
build_joint_scenario_matrix <- function(symbols, lookback_days = 120) {
  symbols <- unique(toupper(trimws(symbols)))
  if (length(symbols) == 0) stop("No symbols provided")
  
  ret_list <- list()
  
  for (sym in symbols) {
    tryCatch({
      ohlcv <- load_stock_data(sym)
      px <- Cl(ohlcv)
      daily_r <- na.omit(diff(px) / lag.xts(px, 1))
      colnames(daily_r) <- sym
      ret_list[[sym]] <- daily_r
    }, error = function(e) {
      cat(sprintf("[LeverageSpace] Warning: Failed to load returns for %s (%s)\n", sym, e$message))
    })
  }
  
  if (length(ret_list) == 0) stop("No valid price data found for any symbol")
  
  # Merge by Date
  merged_rets <- do.call(merge, ret_list)
  # Take complete observations over the lookback window
  clean_rets <- na.omit(merged_rets)
  
  n_rows <- nrow(clean_rets)
  if (n_rows < 20) {
    # If merged has too few rows, fill NAs with 0 for assets with shorter history
    filled_rets <- na.fill(merged_rets, fill = 0)
    clean_rets <- tail(filled_rets, min(lookback_days, nrow(filled_rets)))
  } else {
    clean_rets <- tail(clean_rets, min(lookback_days, n_rows))
  }
  
  event_matrix <- as.matrix(clean_rets)
  return(event_matrix)
}

#' Solve Ralph Vince Leverage Space Optimal f Vector
#'
#' Maximizes the logarithmic Geometric Holding Period Return:
#'   max_f  (1/T) * sum_t ln( 1 + sum_i f_i * (E_ti / |MaxLoss_i|) )
#'
#' @param events Numeric matrix of joint return scenarios (T rows, K cols).
#' @param max_leverage Maximum aggregate portfolio leverage allowed (default: 1.0 = 100% equity).
#' @param safety_factor Fraction of optimal f to apply for downside protection (default: 0.50).
#' @return A list containing optimal_f, safe_f, ghpr, max_losses, weights, and diagnostics.
#' @export
vince_optimal_f <- function(events, max_leverage = 1.0, safety_factor = 0.50) {
  events <- as.matrix(events)
  T_obs  <- nrow(events)
  K      <- ncol(events)
  
  if (T_obs < 10) stop("Insufficient scenario observations (T < 10)")
  if (K == 0) stop("Scenario matrix has 0 columns")
  
  # 1. Identify empirical worst-case loss (MaxLoss_i) for each asset
  max_losses <- apply(events, 2, min)
  
  # Ensure all max_losses are negative; if an asset had only positive returns, set fallback worst loss
  for (i in 1:K) {
    if (is.na(max_losses[i]) || max_losses[i] >= 0) {
      max_losses[i] <- -0.05 # 5% minimum conservative worst loss
    }
  }
  abs_max_losses <- abs(max_losses)
  
  # 2. Objective Function: Negative Log Geometric Holding Period Return
  obj_fun <- function(f_vec) {
    # Portfolio holding period return per period: HPR_t = 1 + sum_i f_i * (E_ti / |MaxLoss_i|)
    hpr_contributions <- events %*% (f_vec / abs_max_losses)
    hpr <- 1 + hpr_contributions
    
    # Severe penalty if any scenario produces bankruptcy (HPR <= 0)
    if (any(hpr <= 1e-6)) {
      min_hpr <- min(hpr)
      return(1e6 + abs(min_hpr) * 1e5)
    }
    
    # Calculate geometric mean return
    log_hpr <- log(hpr)
    mean_log_hpr <- mean(log_hpr)
    
    # Soft penalty if aggregate leverage exceeds max_leverage
    lev_penalty <- 0
    tot_lev <- sum(f_vec)
    if (tot_lev > max_leverage) {
      lev_penalty <- (tot_lev - max_leverage)^2 * 100
    }
    
    return(-mean_log_hpr + lev_penalty)
  }
  
  # 3. Optimize via L-BFGS-B with bounds [0, 1] per asset
  init_f <- rep(min(0.2, max_leverage / K), K)
  
  opt <- tryCatch({
    optim(
      par = init_f,
      fn = obj_fun,
      method = "L-BFGS-B",
      lower = rep(0, K),
      upper = rep(1, K),
      control = list(maxit = 500, factr = 1e7)
    )
  }, error = function(e) {
    # Fallback to Nelder-Mead if L-BFGS-B encounters boundary issues
    optim(
      par = init_f,
      fn = obj_fun,
      method = "Nelder-Mead",
      control = list(maxit = 500)
    )
  })
  
  raw_opt_f <- pmax(0, opt$par)
  names(raw_opt_f) <- colnames(events)
  
  # Normalize raw optimal f if it exceeds max leverage
  if (sum(raw_opt_f) > max_leverage) {
    opt_f_normalized <- raw_opt_f * (max_leverage / sum(raw_opt_f))
  } else {
    opt_f_normalized <- raw_opt_f
  }
  
  # 4. Compute Safe f (Fractional f)
  safe_f <- raw_opt_f * safety_factor
  if (sum(safe_f) > max_leverage) {
    safe_f <- safe_f * (max_leverage / sum(safe_f))
  }
  
  # Compute resulting GHPR (Geometric Holding Period Return)
  hpr_opt <- 1 + (events %*% (opt_f_normalized / abs_max_losses))
  ghpr_val <- if (all(hpr_opt > 0)) exp(mean(log(hpr_opt))) else 1.0
  
  # Compute relative capital allocation weights
  if (sum(safe_f) > 0) {
    weights <- safe_f / sum(safe_f)
  } else {
    weights <- rep(1 / K, K)
    names(weights) <- colnames(events)
  }
  
  return(list(
    optimal_f = raw_opt_f,
    optimal_f_constrained = opt_f_normalized,
    safe_f = safe_f,
    weights = weights,
    ghpr = ghpr_val,
    expected_growth_pct = (ghpr_val - 1) * 100,
    max_losses = max_losses,
    safety_factor = safety_factor,
    convergence = opt$convergence
  ))
}

#' Calculate Ralph Vince Leverage Space Dollar Allocations and Share Sizes
#'
#' @param opt_res Result object returned by `vince_optimal_f()`.
#' @param total_cash Available cash capital to allocate.
#' @param current_prices Named numeric vector of current stock prices.
#' @return A data.frame detailing allocation dollars, percentage weights, and share sizes.
#' @export
vince_portfolio_allocation <- function(opt_res, total_cash, current_prices) {
  syms <- names(opt_res$weights)
  
  alloc_df <- data.frame(
    Symbol = syms,
    Current_Price = as.numeric(current_prices[syms]),
    Optimal_f = as.numeric(opt_res$optimal_f[syms]),
    Safe_f = as.numeric(opt_res$safe_f[syms]),
    Max_Loss = sprintf("%.2f%%", abs(opt_res$max_losses[syms]) * 100),
    Weight = as.numeric(opt_res$weights[syms]),
    stringsAsFactors = FALSE
  )
  
  # Dollar allocation
  alloc_df$Dollar_Allocation <- round(total_cash * alloc_df$Weight, 2)
  # Share counts
  alloc_df$Shares <- floor(alloc_df$Dollar_Allocation / alloc_df$Current_Price)
  alloc_df$Actual_Outlay <- round(alloc_df$Shares * alloc_df$Current_Price, 2)
  alloc_df$Cash_Left <- round(alloc_df$Dollar_Allocation - alloc_df$Actual_Outlay, 2)
  
  return(alloc_df)
}
