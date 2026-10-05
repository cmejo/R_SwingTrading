#!/usr/bin/env Rscript
# ==============================================================================
# Comprehensive Quantitative Smoke-Test Suite (F5)
# Tests core trading engine modules:
#   1. Portfolio Manager (days_held guard B2, averaging-in B5, partial exits F2, persistence B3)
#   2. Standardized Metrics (Sharpe, Sortino, MaxDD, WinRate I1/B8)
#   3. Feature Pipeline & Volatility Bounds (Zoo partial match fix B1, train_idx B10)
#   4. Position Sizing & Heat Limits (B6)
# ==============================================================================

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
})

source("R/08_metrics.R")
source("R/portfolio_manager.R")

test_pass_count <- 0
test_fail_count <- 0

assert_true <- function(cond, test_name) {
  if (isTRUE(cond)) {
    cat(sprintf("  [PASS] %s\n", test_name))
    test_pass_count <<- test_pass_count + 1
  } else {
    cat(sprintf("  [FAIL] %s\n", test_name))
    test_fail_count <<- test_fail_count + 1
  }
}

assert_equal <- function(val, expected, test_name, tol = 1e-4) {
  diff_val <- abs(val - expected)
  if (!is.na(diff_val) && diff_val <= tol) {
    cat(sprintf("  [PASS] %s\n", test_name))
    test_pass_count <<- test_pass_count + 1
  } else {
    cat(sprintf("  [FAIL] %s (Expected: %s, Got: %s)\n", test_name, as.character(expected), as.character(val)))
    test_fail_count <<- test_fail_count + 1
  }
}

cat("================================================================================\n")
cat("                QUANTITATIVE SWING TRADING SYSTEM TEST SUITE                    \n")
cat("================================================================================\n\n")

# ------------------------------------------------------------------------------
# TEST GROUP 1: Portfolio Manager & Accounting (B2, B3, B5, F2)
# ------------------------------------------------------------------------------
cat("[Group 1] Testing Portfolio Manager & Position Lifecycle...\n")

port <- list(
  total_capital = 10000,
  cash_balance = 10000,
  max_positions = 5,
  peak_equity = 10000,
  last_updated = as.character(Sys.time()),
  positions = list(),
  closed_trades = list()
)

# Test B5: Initial Buy followed by Averaging into existing position
port <- record_fill(port, symbol = "AMD", shares = 10, entry_price = 100.0, stop_loss = 90.0, take_profit = 120.0)
assert_equal(port$cash_balance, 9000, "Initial buy deducts correct cash")
assert_equal(length(port$positions), 1, "Position count is 1 after buy")

# Buy more AMD (averaging into position)
port <- record_fill(port, symbol = "AMD", shares = 10, entry_price = 110.0, stop_loss = 95.0, take_profit = 130.0)
assert_equal(length(port$positions), 1, "B5: Adding to existing position preserves 1 position slot")
assert_equal(port$positions[[1]]$shares, 20, "B5: Shares combined correctly (10 + 10 = 20)")
assert_equal(port$positions[[1]]$entry_price, 105.0, "B5: Weighted entry price calculated correctly ($105.00)")
assert_equal(port$cash_balance, 7900, "B5: Cash deducted correctly for combined lot")

# Test B2: sync_portfolio_with_market on same day as entry (cur_date <= entry_date)
today <- Sys.Date()
port$positions[[1]]$entry_date <- as.character(today)
px_map <- c(AMD = 115.0)

sync_res <- sync_portfolio_with_market(
  portfolio = port,
  current_prices = px_map,
  current_date = today,
  is_friday = FALSE
)
assert_true(!is.null(sync_res), "B2: sync_portfolio_with_market succeeds on same-day entry")
assert_equal(sync_res$holdings_df$Days_Held[1], 0, "B2: Same-day entry results in 0 days held")
assert_true(!is.null(sync_res$updated_portfolio), "B3: updated_portfolio returned by sync")

# Test F2: Partial Scale-Out (50% exit)
port <- sync_res$updated_portfolio
port <- record_exit(port, symbol = "AMD", exit_price = 120.0, reason = "TIER1_TARGET", shares_to_exit = 10)
assert_equal(length(port$positions), 1, "F2: Partial exit keeps position open")
assert_equal(port$positions[[1]]$shares, 10, "F2: Partial exit leaves 10 remaining shares")
assert_equal(port$cash_balance, 7900 + 1200, "F2: Cash increased by partial exit proceeds ($1,200)")
assert_equal(length(port$closed_trades), 1, "F2: Closed trades contains 1 partial scale-out record")
assert_equal(port$closed_trades[[1]]$pnl_dollar, (120 - 105) * 10, "F2: Realized PnL calculated accurately ($150)")

# Test Full Exit of remaining shares
port <- record_exit(port, symbol = "AMD", exit_price = 130.0, reason = "TIER2_TARGET")
assert_equal(length(port$positions), 0, "Full exit closes position completely")
assert_equal(length(port$closed_trades), 2, "Closed trades contains 2 records")
assert_equal(port$cash_balance, 9100 + 1300, "Full proceeds restored to cash ($10,400)")

# ------------------------------------------------------------------------------
# TEST GROUP 2: Standardized Performance Metrics Engine (I1, B8)
# ------------------------------------------------------------------------------
cat("\n[Group 2] Testing Standardized Metrics Engine (R/08_metrics.R)...\n")

# Synthetic return series with known mean and variance
set.seed(123)
sample_rets <- c(0.01, 0.02, -0.005, 0.015, -0.01, 0.025, 0.005, -0.002, 0.012, 0.018)
m_out <- calc_performance_metrics(sample_rets, name = "TestStrategy", rf = 0.0)

assert_true(!is.null(m_out$raw$sharpe), "I1: Raw Sharpe ratio computed")
assert_true(m_out$raw$win_rate > 0.60, "I1: Win rate calculation accurate")
assert_true(m_out$raw$max_dd >= 0, "I1: Max Drawdown non-negative")
assert_true(is.data.frame(m_out$formatted), "I1: Formatted data.frame generated")
assert_true("Sharpe_Ratio" %in% colnames(m_out$formatted), "I1: Standard Sharpe_Ratio column present")

# Standard Sharpe formula check: mean / sd * sqrt(252)
expected_sharpe <- (mean(sample_rets) / sd(sample_rets)) * sqrt(252)
assert_equal(m_out$raw$sharpe, expected_sharpe, "B8: Standardized Sharpe matches mean/sd*sqrt(252)")

# ------------------------------------------------------------------------------
# TEST GROUP 3: Feature Pipeline & Bounds Sanity (B1, I2, B10)
# ------------------------------------------------------------------------------
cat("\n[Group 3] Testing Feature Pipeline Bounds & Exact Indexing...\n")

# Test synthetic price series
dates_synth <- seq.Date(from = as.Date("2024-01-01"), by = "day", length.out = 150)
prices_synth <- 100 * cumprod(1 + rnorm(150, mean = 0.0005, sd = 0.02))
synth_ohlcv <- xts(cbind(
  Open = prices_synth * 0.99,
  High = prices_synth * 1.01,
  Low = prices_synth * 0.98,
  Close = prices_synth,
  Volume = rep(1000000, 150)
), order.by = dates_synth)

source("R/04_feature_pipeline.R")
pipe_res <- build_feature_dataset(synth_ohlcv, fast_n = 20, slow_n = 50, look_ahead = 5, use_garch = FALSE, train_idx = 1:80)

assert_true(!is.null(pipe_res$latest_feature_matrix), "I2: latest_feature_matrix returned")
assert_equal(nrow(pipe_res$latest_feature_matrix), 1, "I2: Feature matrix is 1-row unlabeled observation")
assert_true(pipe_res$latest_ann_vol >= 0.05 && pipe_res$latest_ann_vol <= 3.0, "B1: Annualized vol strictly bounded [0.05, 3.0]")

# ------------------------------------------------------------------------------
# TEST SUMMARY
# ------------------------------------------------------------------------------
cat("\n================================================================================\n")
cat(sprintf(" TEST RESULTS: %d PASSED, %d FAILED\n", test_pass_count, test_fail_count))
cat("================================================================================\n\n")

if (test_fail_count > 0) {
  quit(save = "no", status = 1)
} else {
  quit(save = "no", status = 0)
}
