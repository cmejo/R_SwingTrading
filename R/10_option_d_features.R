#' ==============================================================================
#' R/10_option_d_features.R
#' Shared, side-effect-free building blocks for the Option D momentum system.
#' Sourced by BOTH core_satellite_signal.R (live) and R/11_option_d_engine.R
#' (backtest) so that live and simulated logic cannot silently diverge.
#'
#' Feature 1  Risk-targeted, fractional-Kelly leverage
#'            (Barroso & Santa-Clara 2015; Moreira & Muir 2017; Kelly 1956)
#' Feature 2  Residual (beta-adjusted) momentum ranking
#'            (Blitz, Huij & Martens 2011; Daniel & Moskowitz 2016)
#' Feature 3  Skip-recent-days momentum window (Jegadeesh 1990)
#' Utilities  EWMA covariance, margin-call arithmetic, deflated Sharpe ratio
#'            (Bailey & Lopez de Prado 2014), long-only min-variance weights
#' ==============================================================================

#' Annualized EWMA covariance matrix of daily returns.
#' @param X matrix of daily simple returns, rows ordered oldest -> newest.
#' @return k x k annualized covariance, or NULL when fewer than 2 complete rows.
ewma_cov <- function(X, lambda = 0.94, annualize = 252) {
  X <- as.matrix(X)
  X <- X[stats::complete.cases(X), , drop = FALSE]
  n <- nrow(X)
  if (n < 2) return(NULL)
  w  <- lambda^((n - 1):0)
  w  <- w / sum(w)
  mu <- colSums(X * w)
  Xc <- sweep(X, 2, mu)
  crossprod(Xc * sqrt(w)) * annualize
}

#' Portfolio volatility sqrt(w' Sigma w); NA when Sigma is unavailable.
portfolio_vol <- function(w, Sigma) {
  if (is.null(Sigma)) return(NA_real_)
  v <- drop(t(w) %*% Sigma %*% w)
  if (!is.finite(v) || v < 0) return(NA_real_)
  sqrt(v)
}

#' Feature 1: risk-targeted gross leverage with a fractional-Kelly borrowing cap.
#'
#' For L > 1, log growth is g(L) = r_b + L (mu - r_b) - L^2 sigma^2 / 2, maximized at
#' L* = (mu - r_b) / sigma^2. Borrowing is only allowed up to kelly_fraction * L*.
#'
#' @param sigma_p    ex-ante annualized volatility of the unlevered book.
#' @param mu_hat     annualized ARITHMETIC expected return of the unlevered book
#'                   (NA = unknown; leverage above 1 is then capped at no_mu_cap).
#' @param borrow_rate annualized broker margin rate actually paid.
#' @param bull       FALSE caps leverage at 1 (bear regime never borrows).
#' @return list(L, L_risk, L_kelly, binding)
risk_kelly_leverage <- function(sigma_p, sigma_target = 0.40, max_leverage = 2.0,
                                bull = TRUE, mu_hat = NA_real_, borrow_rate = NA_real_,
                                kelly_fraction = 0.5, no_mu_cap = 1.5) {
  if (!is.finite(sigma_p) || sigma_p <= 0) {
    return(list(L = 1.0, L_risk = NA_real_, L_kelly = NA_real_, binding = "fallback_no_vol"))
  }
  L_risk <- sigma_target / sigma_p
  if (!isTRUE(bull)) {
    L <- min(1.0, L_risk)
    return(list(L = L, L_risk = L_risk, L_kelly = NA_real_,
                binding = if (L_risk < 1) "risk" else "bear_cap"))
  }
  L <- min(max_leverage, L_risk)
  binding <- if (L_risk < max_leverage) "risk" else "max_leverage"
  L_kelly <- NA_real_
  if (L > 1) {
    if (is.finite(mu_hat) && is.finite(borrow_rate)) {
      L_kelly <- kelly_fraction * (mu_hat - borrow_rate) / sigma_p^2
      cap <- max(1.0, L_kelly)
      if (cap < L) { L <- cap; binding <- "kelly" }
    } else if (L > no_mu_cap) {
      L <- no_mu_cap
      binding <- "no_mu_cap"
    }
  }
  list(L = L, L_risk = L_risk, L_kelly = L_kelly, binding = binding)
}

#' Fractional decline in gross assets that triggers a maintenance call when the
#' book starts at gross leverage G (assets / equity) with maintenance ratio m.
#' d* = 1 - (G - 1) / (G (1 - m)). Returns 1 (total loss) when G <= 1.
margin_call_decline <- function(G, m = 0.30) {
  if (G <= 1) return(1.0)
  1 - (G - 1) / (G * (1 - m))
}

#' TRUE when equity / assets has fallen below maintenance m. With long-only
#' weights expressed as fractions of equity, equity / assets = 1 / gross.
is_margin_breach <- function(gross, m = 0.30) {
  is.finite(gross) && is.finite(m) && m > 0 && gross > 1 / m
}

#' Feature 3: dual-lookback raw RS vs benchmark, measured to (n - skip).
#' skip = 0 reproduces the original live formula exactly. Each series is indexed
#' from its own end, as in the original live scanner.
dual_rs_raw <- function(cl, qcl, long = 126, short = 63, w_long = 0.5, skip = 0) {
  n <- length(cl); nq <- length(qcl)
  if (n <= long || nq <= long || skip >= short) return(NA_real_)
  e <- n - skip; eq <- nq - skip
  r_l <- (cl[e] / cl[n - long] - 1) - (qcl[eq] / qcl[nq - long] - 1)
  r_s <- (cl[e] / cl[n - short] - 1) - (qcl[eq] / qcl[nq - short] - 1)
  w_long * r_l + (1 - w_long) * r_s
}

#' Feature 2: residual momentum information ratio over one formation window.
#' Beta is estimated on the last `est` aligned observations, excluding the last
#' `skip` rows so skipped days cannot influence the rank at all. Residuals carry
#' NO intercept term (alpha is the signal). The score is the annualized mean / sd
#' of residuals over the last `lookback` days excluding the last `skip`, which is
#' comparable across formation lengths.
#' @param r stock daily simple returns; @param m benchmark returns (aligned).
residual_momentum_score <- function(r, m, est = 504, lookback = 126, skip = 0,
                                    min_obs = 252) {
  ok <- is.finite(r) & is.finite(m)
  r <- r[ok]; m <- m[ok]
  n <- length(r)
  if (n < max(min_obs, lookback + 2) || skip >= lookback - 1) return(NA_real_)
  if (n > est) { r <- utils::tail(r, est); m <- utils::tail(m, est); n <- est }
  fit <- seq_len(n - skip)
  vm <- stats::var(m[fit])
  if (!is.finite(vm) || vm <= 0) return(NA_real_)
  b <- stats::cov(r[fit], m[fit]) / vm
  e <- r - b * m
  w <- (n - lookback + 1):(n - skip)
  s <- stats::sd(e[w])
  if (!is.finite(s) || s <= 0) return(NA_real_)
  mean(e[w]) / s * sqrt(252)
}

#' Dual-lookback residual score (same 50/50 blend as the raw RS composite).
residual_dual_score <- function(r, m, est = 504, long = 126, short = 63,
                                w_long = 0.5, skip = 0, min_obs = 252) {
  s_l <- residual_momentum_score(r, m, est, long,  skip, min_obs)
  s_s <- residual_momentum_score(r, m, est, short, skip, min_obs)
  w_long * s_l + (1 - w_long) * s_s
}

#' Long-only minimum-variance weights (copied from scratch/test_crisis_min_variance.R
#' so the engine does not have to source a script with side effects).
solve_min_var <- function(cov_mat, max_w = 0.35) {
  N <- ncol(cov_mat)
  if (N == 1) return(c(1.0))
  inv_var <- 1 / diag(cov_mat)
  inv_var[!is.finite(inv_var) | inv_var < 0] <- 0
  if (sum(inv_var) <= 0) return(rep(1 / N, N))
  w <- inv_var / sum(inv_var)
  if (requireNamespace("quadprog", quietly = TRUE)) {
    tryCatch({
      Dmat <- cov_mat + diag(1e-5, N)
      Amat <- cbind(rep(1, N), diag(N), -diag(N))
      bvec <- c(1, rep(0, N), rep(-max_w, N))
      qp <- quadprog::solve.QP(Dmat, rep(0, N), Amat, bvec, meq = 1)
      if (!is.null(qp$solution)) w <- pmax(0, qp$solution) / sum(pmax(0, qp$solution))
    }, error = function(e) NULL)
  }
  w
}

#' Deflated Sharpe ratio (Bailey & Lopez de Prado 2014).
#' @param r          per-period (daily) excess returns of the selected strategy.
#' @param n_trials   total number of configurations tried (including prior research).
#' @param sr_var     variance of the per-period Sharpe ratios across those trials.
#' @return probability that the true SR exceeds the expected maximum SR of
#'         n_trials unskilled strategies.
deflated_sharpe_ratio <- function(r, n_trials = 1, sr_var = 0) {
  r <- r[is.finite(r)]
  T_ <- length(r)
  if (T_ < 3 || stats::sd(r) <= 0) return(NA_real_)
  sr <- mean(r) / stats::sd(r)
  z  <- (r - mean(r)) / stats::sd(r)
  g3 <- mean(z^3)
  g4 <- mean(z^4)
  sr0 <- 0
  if (n_trials > 1 && is.finite(sr_var) && sr_var > 0) {
    em <- 0.5772156649
    sr0 <- sqrt(sr_var) * ((1 - em) * stats::qnorm(1 - 1 / n_trials) +
                             em * stats::qnorm(1 - 1 / (n_trials * exp(1))))
  }
  den <- 1 - g3 * sr + (g4 - 1) / 4 * sr^2
  if (!is.finite(den) || den <= 0) return(NA_real_)
  stats::pnorm((sr - sr0) * sqrt(T_ - 1) / sqrt(den))
}

#' Feature 4: Frog-in-the-Pan (FIP) Continuous Momentum (Da, Gurun, Warachka 2014).
#' Measures the proportion of positive daily returns over a trailing window (default 63 days).
#' Persistent, continuous price drift signals institutional accumulation and lower crash risk.
#' @param cl numeric vector of closing prices.
#' @param lookback number of trading days (default 63).
#' @return fraction in [0, 1] of positive return days (defaults to 0.50 if insufficient data).
calc_fip_ratio <- function(cl, lookback = 63) {
  cl <- cl[is.finite(cl)]
  n <- length(cl)
  if (n <= 1) return(0.50)
  k <- min(n - 1, lookback)
  ret <- diff(tail(cl, k + 1))
  if (length(ret) == 0) return(0.50)
  mean(ret > 0, na.rm = TRUE)
}

#' Feature 5: Tax-Aware Winner Retention (LTCG Bridge).
#' Determines whether an existing winning holding should be retained past its 365-day mark
#' when approaching 1-year holding period (within bridge_days) and still ranked <= rank_max.
#'
#' @param entry_date Date or string representation of lot acquisition.
#' @param current_date Date of current rebalance evaluation.
#' @param entry_price numeric acquisition cost per share.
#' @param current_price numeric current mark-to-market price.
#' @param current_rank integer momentum rank in current candidate universe.
#' @param bridge_days integer number of days prior to 365-day mark to protect (default 45).
#' @param rank_max integer maximum allowable momentum rank to retain (default 12).
#' @return logical TRUE if holding should be preserved for LTCG tax qualification.
is_tax_protected_winner <- function(entry_date, current_date, entry_price, current_price, current_rank,
                                    bridge_days = 45, rank_max = 12) {
  if (is.null(entry_date) || is.na(entry_date)) return(FALSE)
  days_held <- as.numeric(as.Date(current_date) - as.Date(entry_date))
  if (is.na(days_held) || is.na(entry_price) || is.na(current_price) || is.na(current_rank)) return(FALSE)
  is_winner <- (current_price > entry_price)
  within_bridge <- (days_held >= (365 - bridge_days) && days_held <= 365)
  is_winner && within_bridge && (current_rank <= rank_max)
}

#' Check if a candidate symbol was sold at a loss within the IRS wash-sale window (30 days).
#' @param symbol character ticker.
#' @param closed_trades list of closed trade objects.
#' @param current_date Date of current rebalance.
#' @param window_days integer wash sale lockout days (default 30).
#' @return logical TRUE if repurchase would trigger wash-sale loss disallowance under IRC § 1091.
is_wash_sale_risk <- function(symbol, closed_trades, current_date, window_days = 30) {
  if (is.null(closed_trades) || length(closed_trades) == 0) return(FALSE)
  cur_dt <- as.Date(current_date)
  for (tr in closed_trades) {
    if (identical(tr$symbol, symbol) && !is.null(tr$realized_pnl) && as.numeric(tr$realized_pnl) < 0) {
      if (!is.null(tr$exit_date)) {
        diff_d <- as.numeric(cur_dt - as.Date(tr$exit_date))
        if (!is.na(diff_d) && diff_d <= window_days) return(TRUE)
      }
    }
  }
  FALSE
}

#' Feature 6: Industry Concentration Limit (Correlation Cap)
#'
#' Retrieves the mapped industry for a stock ticker from sector_map.json.
#' @param sym character stock symbol.
#' @param sm named list or environment mapping symbol -> industry.
#' @return character industry name, defaulting to "Other" if unmapped.
get_stock_industry <- function(sym, sm = NULL) {
  if (is.null(sm)) {
    if (file.exists("sector_map.json")) {
      sm <- jsonlite::fromJSON("sector_map.json")
    } else {
      return("Other")
    }
  }
  if (!is.null(sm[[sym]]) && nzchar(sm[[sym]])) sm[[sym]] else "Other"
}

#' Filters candidate symbols to enforce a maximum number of positions per industry.
#'
#' @param candidates character vector of candidate symbols in priority/ranking order.
#' @param max_per_industry integer maximum allowed positions per industry (default: 5).
#' @param initial_held character vector of currently retained symbols (optional).
#' @param sector_map named list mapping symbol -> industry (optional).
#' @return filtered character vector of candidate symbols respecting max_per_industry.
filter_by_industry_cap <- function(candidates, max_per_industry = 5, initial_held = character(), sector_map = NULL) {
  if (max_per_industry >= (length(candidates) + length(initial_held))) {
    return(candidates)
  }
  if (is.null(sector_map) && file.exists("sector_map.json")) {
    sector_map <- jsonlite::fromJSON("sector_map.json")
  }
  
  get_ind <- function(s) {
    if (!is.null(sector_map[[s]]) && nzchar(sector_map[[s]])) sector_map[[s]] else "Other"
  }
  
  # Initialize counts from initial_held
  ind_counts <- list()
  for (h in initial_held) {
    ind <- get_ind(h)
    cur <- if (!is.null(ind_counts[[ind]])) ind_counts[[ind]] else 0
    ind_counts[[ind]] <- cur + 1
  }
  
  selected <- character()
  for (cand in candidates) {
    ind <- get_ind(cand)
    cur_count <- if (!is.null(ind_counts[[ind]])) ind_counts[[ind]] else 0
    if (cur_count < max_per_industry) {
      selected <- c(selected, cand)
      ind_counts[[ind]] <- cur_count + 1
    }
  }
  selected
}

#' Feature 7: Reallocate Stop Loss Proceeds to Surviving Winners
#'
#' Takes the total proceeds from stopped-out positions and calculates equal additions
#' to each surviving holding.
#'
#' @param total_proceeds numeric total cash proceeds from stopped-out trades.
#' @param surviving_positions list of active satellite position objects.
#' @param cur_prices named list or numeric vector of current prices.
#' @param disaster_stop numeric disaster stop loss percentage (default: 0.25).
#' @return list containing updated_positions, order_records, and tickets.
reallocate_stop_to_survivors <- function(total_proceeds, surviving_positions, cur_prices = list(), disaster_stop = 0.25) {
  if (total_proceeds < 10 || length(surviving_positions) == 0) {
    return(list(updated_positions = surviving_positions, order_records = list(), tickets = character()))
  }
  
  n_survivors <- length(surviving_positions)
  per_survivor_cash <- total_proceeds / n_survivors
  
  updated_positions <- surviving_positions
  order_records <- list()
  tickets <- character()
  
  for (i in seq_along(updated_positions)) {
    pos <- updated_positions[[i]]
    sym <- pos$symbol
    px <- if (!is.null(cur_prices[[sym]]) && cur_prices[[sym]] > 0) as.numeric(cur_prices[[sym]]) else as.numeric(pos$current_price)
    if (is.na(px) || px <= 0) px <- as.numeric(pos$entry_price)
    
    add_shs <- round(per_survivor_cash / px, 3)
    if (add_shs > 0) {
      outlay <- round(add_shs * px, 2)
      prev_shs <- as.numeric(pos$shares)
      prev_entry <- as.numeric(pos$entry_price)
      new_shs <- prev_shs + add_shs
      new_entry <- round((prev_shs * prev_entry + outlay) / new_shs, 4)
      
      ticket <- sprintf("BUY %s shares of %s @ ~$%.2f ($%.2f outlay - Reallocate Disaster Stop Proceeds to Surviving Winner)",
                        add_shs, sym, px, outlay)
      tickets <- c(tickets, ticket)
      
      order_records[[length(order_records) + 1]] <- list(
        symbol = sym, side = "BUY", shares = round(add_shs, 3),
        shares_whole = as.integer(floor(add_shs)), estimated_price = round(px, 2),
        estimated_outlay = outlay, order_type = "MARKET",
        reason = "STOP_REALLOCATION_SURVIVING_WINNER",
        stop_loss_price = round(px * (1.0 - disaster_stop), 2)
      )
      
      updated_positions[[i]]$shares <- new_shs
      updated_positions[[i]]$entry_price <- new_entry
      updated_positions[[i]]$current_price <- px
      updated_positions[[i]]$market_value <- round(new_shs * px, 2)
    }
  }
  
  list(
    updated_positions = updated_positions,
    order_records = order_records,
    tickets = tickets
  )
}

#' Feature 8: Tactical Safe-Haven Bear Regime Asset Selection
#'
#' Evaluates whether GLD (Gold) is in a confirmed uptrend (rolling regression slope > 0).
#' If GLD is in an uptrend, returns "GLD" as the safe-haven asset.
#' If GLD is in a downtrend/flat, returns "SGOV" (Treasury Bills / Cash preservation).
#'
#' @param gld_prices numeric vector or xts of historical GLD close prices.
#' @param window integer rolling window for linear regression slope (default: 50).
#' @return list containing asset ("GLD" or "SGOV"), slope, and rationale.
evaluate_safe_haven_bear <- function(gld_prices, window = 50) {
  if (is.null(gld_prices) || length(gld_prices) < window) {
    return(list(asset = "SGOV", slope = 0.0, trend_up = FALSE, rationale = "Insufficient GLD data -> Default to SGOV"))
  }
  
  cl <- as.numeric(gld_prices)
  tail_cl <- tail(cl, window)
  lm_res <- lmMA(tail_cl, n = window)
  slope <- tail(as.numeric(lm_res$slope), 1)
  trend_up <- (slope > 0)
  
  if (trend_up) {
    list(asset = "GLD", slope = slope, trend_up = TRUE, rationale = sprintf("GLD 50d Slope (%+.2f) > 0 -> Gold Safe Haven Active", slope))
  } else {
    list(asset = "SGOV", slope = slope, trend_up = FALSE, rationale = sprintf("GLD 50d Slope (%+.2f) <= 0 -> Park in SGOV / Cash Yield", slope))
  }
}


