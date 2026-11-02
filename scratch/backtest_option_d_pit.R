#!/usr/bin/env Rscript
#' ==============================================================================
#' scratch/backtest_option_d_pit.R
#' Option D on the survivorship-free point-in-time Nasdaq-100 universe, with
#' broker-accurate financing, margin-call liquidation, live-parity execution
#' rules, a train/test split and the deflated Sharpe ratio, plus Features 1-3
#' (risk-targeted Kelly leverage, residual momentum, skip-recent-days) as
#' separate and combined variants.
#'
#' Usage (from repo root):
#'   bash scratch/build_option_d_caches.sh        # once: builds the gitignored price caches
#'   Rscript scratch/backtest_option_d_pit.R [universe] [--broker=schwab|ibkr|legacy]
#'                                          [--maint=0.30] [--prior_trials=250]
#'     universe: pit_n100 (default) | broad (hindsight list, for comparison only)
#'
#' Row 1 ("LEGACY replication") uses R/11_option_d_engine.R with the original
#' sim_buffer_engine() semantics (bit-identical, see tests/test_option_d_features.R).
#' On the `broad` universe it should reproduce the "Ultimate Alpha Master" row of
#' scratch/scorecard_*_buffer_combos.csv; check that before trusting any other row.
#'
#' Requires the regenerable caches built by scratch/build_option_d_caches.sh.
#' Reports 3/10/20-year windows ending 2026-10-06 (same dates as the *_3_10_20yr scorecards)
#' plus the 2007-16 / 2017-26 train/test split used by the acceptance rule.
#' ==============================================================================
suppressPackageStartupMessages({ library(xts); library(zoo); library(TTR) })

args <- commandArgs(trailingOnly = TRUE)
UNIV <- if (length(args) && !grepl("^--", args[1])) args[1] else "pit_n100"
opt <- function(name, default) {
  a <- grep(sprintf("^--%s=", name), args, value = TRUE)
  if (length(a)) sub(sprintf("^--%s=", name), "", a[1]) else default
}
BROKER       <- opt("broker", "schwab")
MAINT        <- as.numeric(opt("maint", "0.30"))
PRIOR_TRIALS <- as.integer(opt("prior_trials", "250"))   # configs already tried on this history

need <- c("data/n100_pit/prices_adjclose.csv", "data/universe_prices_adjclose.csv",
          file.path("data/data_historical", c("data_qqq.rds", "data_qld.rds", "data_gld_full.rds", "data_^irx.rds")))
miss <- need[!file.exists(need)]
if (length(miss)) stop("Missing price caches. Build them once with:  bash scratch/build_option_d_caches.sh\n  missing: ",
                       paste(miss, collapse = ", "))

# Load data exactly as the existing studies do (PIT globals preserved in PIT_G)
src <- readLines("scratch/compare_symbol_lists.R"); idx <- grep("^lists <- c\\(", src)
eval(parse(text = src[1:(idx - 1)]))
if (UNIV == "pit_n100") {
  for (nm in names(PIT_G)) assign(nm, PIT_G[[nm]])
} else if (UNIV == "broad") {
  invisible(set_universe(read_list("symbols_broad.txt")))
} else stop("unknown universe: ", UNIV)

source("R/02_trend_lmMA.R")
source("R/10_option_d_features.R")
source("R/11_option_d_engine.R")

D <- prepare_option_d_data(list(cal = cal, syms = syms, P = P, R = R, MEM = MEM, q = q, rq = rq,
                                r_gld = r_gld, rf_daily = rf_daily, VOL63 = VOL63, vol20 = vol20,
                                month_end = month_end))

S_FULL <- as.Date("2007-01-02"); E_FULL <- as.Date("2026-10-06"); SPLIT <- as.Date("2017-01-01")
# Same windows as scratch/test_convex_sizing_audit_3_10_20yr.R, so rows are directly comparable
WINDOWS <- list(Y3 = as.Date("2023-10-03"), Y10 = as.Date("2016-10-03"), Y20 = as.Date("2007-01-02"))

bp <- broker_preset(BROKER)
real <- list(turbo_margin = 2.50,   # live default since 547dc49 (row 1 keeps 2.00x for scorecard parity)
             borrow_spread = bp$borrow_spread, maintenance = MAINT,
             # live-parity execution: trim/top-up only on >= 15% drift, 1 - vol_scale into GLD,
             # stop proceeds (not a fresh slot) redeployed into the replacement
             drift_band = 0.15, residual_to = "gld", stop_replace_size = "proceeds")
cfgs <- list(
  "LEGACY replication (T-bill+1.75%, no margin calls)" = list(),
  "Option D realistic (broker borrow + calls + parity)" = real,
  "Option D realistic, turbo OFF (1.5x)"                = c(real, list(use_turbo = FALSE)),
  "F3 skip 5d"                                          = c(real, list(skip = 5)),
  "F3 skip 21d"                                         = c(real, list(skip = 21)),
  "F2 residual momentum"                                = c(real, list(rank_mode = "residual")),
  "F2 residual + F3 skip 5d"                            = c(real, list(rank_mode = "residual", skip = 5)),
  "F1 risk target 25% (expanding mu, half-Kelly)"       = c(real, list(leverage_mode = "risk_kelly", sigma_target = 0.25)),
  "F1 risk target 30% (expanding mu, half-Kelly)"       = c(real, list(leverage_mode = "risk_kelly", sigma_target = 0.30)),
  "F1 risk target 35% (expanding mu, half-Kelly)"       = c(real, list(leverage_mode = "risk_kelly", sigma_target = 0.35)),
  "F1+F2+F3 (30%, residual, skip 5d)"                   = c(real, list(leverage_mode = "risk_kelly", sigma_target = 0.30,
                                                                       rank_mode = "residual", skip = 5))
)

run_period <- function(cfg, s, e) {
  r <- sim_option_d(D, do.call(option_d_config, cfg), s, e)
  ev <- which(cal >= s & cal <= e); ev <- ev[ev >= 2]
  list(r = r, m = option_d_metrics(r, rf_daily[ev]))
}

cat(sprintf("\nUniverse: %s | Broker: %s (spread %.2f%% over T-bill) | Maintenance: %.0f%%\n\n",
            UNIV, BROKER, bp$borrow_spread * 100, MAINT * 100))
res <- lapply(names(cfgs), function(nm) {
  cat(" running:", nm, "\n")
  cfg <- cfgs[[nm]]
  out <- list(train = run_period(cfg, S_FULL, SPLIT - 1),
              test  = run_period(cfg, SPLIT, E_FULL),
              full  = run_period(cfg, S_FULL, E_FULL))
  for (w in c("Y3", "Y10")) out[[w]] <- run_period(cfg, WINDOWS[[w]], E_FULL)
  out$Y20 <- out$full   # 20-year window == full period (2007-01-02 .. 2026-10-06)
  out
})
names(res) <- names(cfgs)

# Deflated Sharpe on the full period. Cross-trial variance of per-period SR is
# estimated from the realistic rows of this run; trials = prior research + this run.
ex_r  <- function(x) { ev <- which(cal >= S_FULL & cal <= E_FULL); ev <- ev[ev >= 2]; x$full$r - rf_daily[ev] }
sr_d  <- sapply(res, function(x) { r <- ex_r(x); mean(r) / sd(r) })
var_sr <- var(sr_d[-1])
n_trials <- PRIOR_TRIALS + length(cfgs)
sr0_ann <- sqrt(var_sr) * ((1 - 0.5772156649) * qnorm(1 - 1 / n_trials) +
                           0.5772156649 * qnorm(1 - 1 / (n_trials * exp(1)))) * sqrt(252)

tbl <- do.call(rbind, lapply(names(res), function(nm) {
  x <- res[[nm]]
  data.frame(Config = nm,
             CAGR_3y = x$Y3$m[["CAGR"]], MaxDD_3y = x$Y3$m[["MaxDD"]],
             CAGR_10y = x$Y10$m[["CAGR"]], MaxDD_10y = x$Y10$m[["MaxDD"]],
             CAGR_20y = x$Y20$m[["CAGR"]], MaxDD_20y = x$Y20$m[["MaxDD"]],
             Train_CAGR = x$train$m[["CAGR"]], Test_CAGR = x$test$m[["CAGR"]],
             Full_CAGR = x$full$m[["CAGR"]], Full_MaxDD = x$full$m[["MaxDD"]],
             Full_Vol = x$full$m[["Vol"]], Full_Sharpe = x$full$m[["Sharpe"]],
             DSR = deflated_sharpe_ratio(ex_r(x), n_trials, var_sr),
             AvgGross = x$full$m[["AvgGross"]], MarginCalls = x$full$m[["MarginCalls"]],
             Ruined = x$full$m[["Ruined"]], Turnover = x$full$m[["Turnover"]],
             check.names = FALSE)
}))

num <- vapply(tbl, is.numeric, logical(1))
print(cbind(tbl[!num], round(tbl[num], 3)), row.names = FALSE)
cat(sprintf("\nDSR uses n_trials = %d (prior %d + %d here); SR0 (annualised) = %.2f\n",
            n_trials, PRIOR_TRIALS, length(cfgs), sr0_ann))
cat("Acceptance rule: a feature ships only if Test_CAGR improves on the realistic baseline (row 2),\n",
    "neighbouring parameter values move the same direction, and DSR stays > 0.95.\n")

out <- sprintf("scratch/scorecard_option_d_%s_%s.csv", UNIV, BROKER)
write.csv(tbl, out, row.names = FALSE)
cat("Saved:", out, "\n")
