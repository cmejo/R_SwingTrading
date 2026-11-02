#!/usr/bin/env Rscript
# ==============================================================================
# tests/test_option_d_features.R
# Unit tests for R/10_option_d_features.R, an exact-parity test of
# R/11_option_d_engine.R (legacy preset) against the original sim_buffer_engine()
# in scratch/test_buffer_leverage_synergies.R, and smoke tests of every new flag.
# All data is synthetic; no network or price caches required.
#   Rscript tests/test_option_d_features.R
# ==============================================================================
suppressPackageStartupMessages({ library(xts); library(zoo); library(TTR) })
source("R/02_trend_lmMA.R")
source("R/10_option_d_features.R")
source("R/11_option_d_engine.R")

n_pass <- 0; n_fail <- 0
check <- function(cond, name) {
  if (isTRUE(cond)) { n_pass <<- n_pass + 1; cat(sprintf("  [PASS] %s\n", name)) }
  else { n_fail <<- n_fail + 1; cat(sprintf("  [FAIL] %s\n", name)) }
}
near <- function(a, b, tol = 1e-8) isTRUE(all(abs(a - b) <= tol))

# ------------------------------------------------------------------------------
cat("\n[1] EWMA covariance and portfolio volatility\n")
set.seed(1)
X <- matrix(rnorm(3000, 0, 0.02), ncol = 3)
S1 <- ewma_cov(X, lambda = 1)                      # lambda = 1 -> equal weights
S_ref <- cov(X) * (nrow(X) - 1) / nrow(X) * 252    # population covariance, annualized
check(near(S1, S_ref, 1e-10), "lambda = 1 reduces to population covariance x 252")
check(is.null(ewma_cov(X[1, , drop = FALSE])), "fewer than 2 rows returns NULL")
check(near(portfolio_vol(c(1, 0, 0), S1), sqrt(S1[1, 1])), "single-asset portfolio vol")
check(is.na(portfolio_vol(c(1, 0, 0), NULL)), "NULL covariance gives NA vol")

# ------------------------------------------------------------------------------
cat("\n[2] Risk-targeted fractional-Kelly leverage\n")
a <- risk_kelly_leverage(sigma_p = 0.20, sigma_target = 0.40, max_leverage = 2.0, bull = TRUE,
                         mu_hat = 0.50, borrow_rate = 0.05, kelly_fraction = 0.5)
check(near(a$L, 2.0) && a$binding == "max_leverage", "calm book with strong mu hits max leverage")
b <- risk_kelly_leverage(0.45, 0.40, 2.0, TRUE, mu_hat = 0.25, borrow_rate = 0.12075, kelly_fraction = 0.5)
check(near(b$L, 0.40 / 0.45) && b$binding == "risk", "high vol book is risk-bound below 1x")
c3 <- risk_kelly_leverage(0.25, 0.40, 2.0, TRUE, mu_hat = 0.20, borrow_rate = 0.12075, kelly_fraction = 0.5)
check(near(c3$L, 1.0) && c3$binding == "kelly", "Kelly forbids borrowing when 0.5(mu - r_b)/sigma^2 < 1")
d4 <- risk_kelly_leverage(0.25, 0.40, 2.0, TRUE, mu_hat = 0.25, borrow_rate = 0.054, kelly_fraction = 0.5)
check(near(d4$L, 0.5 * (0.25 - 0.054) / 0.25^2) && d4$binding == "kelly", "Kelly cap binds between 1 and L_risk")
e5 <- risk_kelly_leverage(0.20, 0.40, 2.0, bull = FALSE)
check(near(e5$L, 1.0), "bear regime never borrows")
f6 <- risk_kelly_leverage(0.20, 0.40, 2.0, TRUE, mu_hat = NA, no_mu_cap = 1.5)
check(near(f6$L, 1.5) && f6$binding == "no_mu_cap", "unknown mu caps leverage at no_mu_cap")
check(near(risk_kelly_leverage(NA)$L, 1.0), "missing volatility falls back to 1x")

# ------------------------------------------------------------------------------
cat("\n[3] Margin-call arithmetic\n")
check(near(margin_call_decline(2.0, 0.30), 1 - 1 / 1.4), "2x gross, 30% maintenance -> 28.57% decline")
check(near(margin_call_decline(2.0, 0.25), 1 / 3), "2x gross, 25% maintenance -> 33.3% decline")
check(near(margin_call_decline(2.0, 0.50), 0), "2x gross, 50% house requirement -> immediate call")
check(margin_call_decline(1.0, 0.30) == 1, "unlevered book is never called")
G <- 2; d <- margin_call_decline(G, 0.30) + 1e-6           # gross after a decline d
gross_after <- G * (1 - d) / (G * (1 - d) - (G - 1))
check(is_margin_breach(gross_after, 0.30) && !is_margin_breach(1.9, 0.30) && !is_margin_breach(3, NA),
      "breach test consistent with decline threshold")

# ------------------------------------------------------------------------------
cat("\n[4] Momentum scores\n")
cl  <- 100 * cumprod(c(1, rep(1.002, 300)))
qcl <- 100 * cumprod(c(1, rep(1.001, 300)))
n <- length(cl); nq <- length(qcl)
orig <- 0.5 * ((cl[n] / cl[n - 126] - 1) - (qcl[nq] / qcl[nq - 126] - 1)) +
        0.5 * ((cl[n] / cl[n - 63]  - 1) - (qcl[nq] / qcl[nq - 63]  - 1))
check(near(dual_rs_raw(cl, qcl, skip = 0), orig), "dual_rs_raw(skip = 0) equals the live formula")
cl_spike <- cl; cl_spike[(n - 4):n] <- cl_spike[(n - 4):n] * 1.30
check(near(dual_rs_raw(cl_spike, qcl, skip = 5), dual_rs_raw(cl, qcl, skip = 5)),
      "skip = 5 ignores a last-week spike")

set.seed(7)
m <- rnorm(600, 0.0004, 0.012)
eps <- rnorm(600, 0, 0.01)
r_hi_beta <- 2.0 * m + eps                      # pure beta, no alpha
r_alpha   <- 0.6 * m + eps + 0.0015             # low beta, positive alpha
check(residual_momentum_score(r_alpha, m) > residual_momentum_score(r_hi_beta, m),
      "residual score prefers alpha over leverage-to-market")
m_up <- abs(m) + 0.001                          # strong bull tape
check(dual_rs_raw(cumprod(1 + 2.0 * m_up + eps), cumprod(1 + m_up)) >
        dual_rs_raw(cumprod(1 + 0.6 * m_up + eps + 0.0015), cumprod(1 + m_up)),
      "raw RS prefers the high-beta name in a bull tape (the bias Feature 2 removes)")
check(is.na(residual_momentum_score(r_alpha[1:100], m[1:100])), "insufficient history returns NA")
check(is.finite(residual_dual_score(r_alpha, m, skip = 5)), "dual residual score with skip is finite")
cl_steady <- 100 * cumprod(c(1, rep(c(1.01, 1.01, 0.99), 30)))
check(near(calc_fip_ratio(cl_steady, lookback = 63), 42 / 63, 1e-4), "calc_fip_ratio correctly measures fraction of positive days")
check(calc_fip_ratio(c(100), lookback = 63) == 0.50, "single price returns 0.50 neutral fallback")

# ------------------------------------------------------------------------------
cat("\n[5] Deflated Sharpe ratio\n")
set.seed(3)
rr <- rnorm(2520, 0.0008, 0.01)
psr <- deflated_sharpe_ratio(rr, n_trials = 1)
dsr <- deflated_sharpe_ratio(rr, n_trials = 300, sr_var = 0.03^2)
check(psr > 0.99, "single strong strategy has PSR near 1")
check(dsr < psr, "more trials deflate the probability")
check(is.na(deflated_sharpe_ratio(c(0.01, 0.01, 0.01))), "zero-variance input returns NA")

# ------------------------------------------------------------------------------
cat("\n[6] Synthetic market for engine tests\n")
make_market <- function(seed = 11, n_days = 1100, n_syms = 40) {
  set.seed(seed)
  cal <- seq(as.Date("2010-01-04"), by = "day", length.out = n_days * 7 / 5 + 10)
  cal <- cal[!format(cal, "%u") %in% c("6", "7")][1:n_days]
  mu_m <- c(rep(0.0009, 450), rep(-0.0025, 120), rep(0.0012, n_days - 570))   # bull, bear, bull
  rq <- c(0, rnorm(n_days - 1, mu_m[-1], 0.012))
  q <- 100 * cumprod(1 + rq)
  beta  <- runif(n_syms, 0.5, 2.2)
  alpha <- rnorm(n_syms, 0.0002, 0.0006)
  ivol  <- runif(n_syms, 0.012, 0.04)
  R <- sapply(seq_len(n_syms), function(j) alpha[j] + beta[j] * rq + rnorm(n_days, 0, ivol[j]))
  R[1, ] <- 0
  P <- 50 * apply(1 + R, 2, cumprod)
  syms <- sprintf("S%02d", seq_len(n_syms)); colnames(P) <- colnames(R) <- syms
  late <- 1:6                                                # late listings
  for (j in late) { f <- 200 + 60 * j; P[1:f, j] <- NA }
  R <- rbind(NA, P[-1, , drop = FALSE] / P[-nrow(P), , drop = FALSE] - 1); R[is.na(R)] <- 0
  MEM <- matrix(TRUE, n_days, n_syms, dimnames = list(NULL, syms))
  MEM[700:n_days, 7:8] <- FALSE                               # index deletions
  VOL63 <- apply(R, 2, function(v) as.numeric(zoo::rollapply(v, 63, sd, align = "right", fill = NA)) * sqrt(252))
  vol20 <- zoo::rollapply(rq, 20, function(x) sd(x) * sqrt(252), align = "right", fill = 0.20)
  month_end <- c(format(cal[-1], "%m") != format(cal[-n_days], "%m"), TRUE)
  r_gld <- c(0, rnorm(n_days - 1, 0.0002, 0.009))
  rf_daily <- rep(0.03 / 252, n_days)
  list(cal = cal, syms = syms, P = P, R = R, MEM = MEM, q = q, rq = rq, r_gld = r_gld,
       rf_daily = rf_daily, VOL63 = VOL63, vol20 = vol20, month_end = month_end)
}
D <- prepare_option_d_data(make_market())
check(any(D$super_bull) && any(!D$benchmark_bull[300:1100], na.rm = TRUE),
      "synthetic market contains super-bull and bear regimes")
START <- D$cal[300]; END <- D$cal[length(D$cal)]

# ------------------------------------------------------------------------------
cat("\n[7] Exact parity with the original sim_buffer_engine()\n")
orig_env <- new.env()
exprs <- parse("scratch/test_buffer_leverage_synergies.R", keep.source = FALSE)
fn_expr <- Filter(function(e) is.call(e) && identical(e[[1]], as.name("<-")) &&
                    identical(e[[2]], as.name("sim_buffer_engine")), as.list(exprs))[[1]]
with(orig_env, {
  cal <- D$cal; U <- D$syms; P <- D$P; R <- D$R; MEM <- D$MEM; rq <- D$rq; r_gld <- D$r_gld
  rf_daily <- D$rf_daily; borrow_daily <- D$rf_daily + 0.0175 / 252; COST <- 0.0010
  VOL63 <- D$VOL63; vol20 <- D$vol20; month_end <- D$month_end
  super_bull_q <- D$super_bull; benchmark_bull <- D$benchmark_bull
  q <- D$q
  q_r126 <- q / c(rep(NA, 126), q[1:(length(q) - 126)]) - 1
  q_r63  <- q / c(rep(NA, 63),  q[1:(length(q) - 63)]) - 1
  r126_mat <- sapply(U, function(s) { px <- P[, s]; (px / c(rep(NA, 126), px[1:(length(px) - 126)]) - 1) - q_r126 })
  r63_mat  <- sapply(U, function(s) { px <- P[, s]; (px / c(rep(NA, 63),  px[1:(length(px) - 63)])  - 1) - q_r63 })
  S20 <- START; E20 <- END
  breadth_50 <- rep(0.5, length(cal))
  solve_min_var <- get("solve_min_var", globalenv())
})
eval(fn_expr, orig_env)
cases <- list(
  list(buffer = 8, stop = 0.25, lev = 2.00),
  list(buffer = 5, stop = 0.20, lev = 1.75),
  list(buffer = 10, stop = 0.15, lev = 2.00))
for (cs in cases) {
  r_orig <- orig_env$sim_buffer_engine(buffer_rank = cs$buffer, disaster_stop_pct = cs$stop,
                                       super_bull_leverage = cs$lev, start = START, end = END)
  r_new <- sim_option_d(D, option_d_config(buffer_rank = cs$buffer, stop_pct = cs$stop,
                                           turbo_margin = cs$lev), START, END)
  check(near(as.numeric(r_orig), as.numeric(r_new), 1e-12),
        sprintf("legacy engine == original (buffer %d, stop %.2f, turbo %.2fx); terminal %.4f vs %.4f",
                cs$buffer, cs$stop, cs$lev, prod(1 + r_orig), prod(1 + r_new)))
}

# ------------------------------------------------------------------------------
cat("\n[8] New flags: smoke tests and directional checks\n")
base <- sim_option_d(D, option_d_config(), START, END)
run <- function(...) sim_option_d(D, option_d_config(...), START, END)
fin <- function(r) all(is.finite(r)) && length(r) == length(base)

r_ibkr   <- run(borrow_spread = 0.015, maintenance = 0.30)
r_schwab <- run(borrow_spread = 0.082, maintenance = 0.30)
check(fin(r_ibkr) && fin(r_schwab), "broker presets run")
check(prod(1 + r_schwab) < prod(1 + r_ibkr), "Schwab financing compounds below IBKR financing")
r_tight <- run(maintenance = 0.55)
check(attr(r_tight, "margin_calls") > 0, "a 55% maintenance ratio produces margin calls at 1.5-2x")
r_parity <- run(drift_band = 0.15, residual_to = "gld", stop_replace_size = "proceeds")
check(fin(r_parity), "live-parity switches run")
r_rk <- run(leverage_mode = "risk_kelly", sigma_target = 0.40)
ll <- attr(r_rk, "leverage_log")
check(fin(r_rk) && !is.null(ll) && all(ll$L <= 2.0 + 1e-12), "risk_kelly runs and respects max leverage")
check(all(ll$L[!D$benchmark_bull[match(ll$date, D$cal) - 1] %in% TRUE] <= 1 + 1e-12),
      "risk_kelly never borrows when the signal bar is bear")
r_fix <- run(leverage_mode = "risk_kelly", mu_mode = "fixed", mu_hat = 0.10)
check(all(attr(r_fix, "leverage_log")$L <= 1 + 1e-12), "mu_hat below borrow cost + risk premium forbids borrowing")
r_res <- run(rank_mode = "residual")
r_skip <- run(skip = 5)
r_all <- run(leverage_mode = "risk_kelly", rank_mode = "residual", skip = 5,
             borrow_spread = 0.015, maintenance = 0.30)
check(fin(r_res) && fin(r_skip) && fin(r_all), "residual rank, skip and combined configs run")
check(!near(as.numeric(r_res), as.numeric(base)), "residual ranking changes selections")
r_ruin <- run(base_margin = 12, turbo_margin = 12, stop_pct = 5)
k_ruin <- which(r_ruin == -1)
check(length(k_ruin) == 1 && all(r_ruin[-(1:k_ruin)] == 0) && option_d_metrics(r_ruin)[["Ruined"]] == 1,
      "ruin is absorbing and flagged")
m_base <- option_d_metrics(base, D$rf_daily[D$cal >= START & D$cal <= END])
check(all(is.finite(m_base[c("CAGR", "MaxDD", "Vol", "Sharpe")])), "metrics are finite")

cat(sprintf("\n%d passed, %d failed\n", n_pass, n_fail))
if (n_fail > 0) quit(status = 1)
