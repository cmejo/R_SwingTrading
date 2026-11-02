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
