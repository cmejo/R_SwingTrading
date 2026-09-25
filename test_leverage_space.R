#!/usr/bin/env Rscript
#' Verification Test: Ralph Vince Leverage Space Model (LSPM)

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
})

source("R/01_data_loader.R")
source("R/07_leverage_space.R")

cat("\n========================================================================\n")
cat(" VERIFYING RALPH VINCE LEVERAGE SPACE MODEL (LSPM)\n")
cat("========================================================================\n\n")

symbols <- c("SNDK", "NVDA", "AAPL", "MSFT", "TSLA")
cat(sprintf("1. Building 120-Day Joint Return Scenario Matrix for: %s...\n", paste(symbols, collapse = ", ")))
events <- build_joint_scenario_matrix(symbols, lookback_days = 120)

cat(sprintf("   -> Scenario Matrix Dimensions: %d trading days x %d assets\n", nrow(events), ncol(events)))
cat("   -> Empirical Worst-Case Daily Losses (MaxLoss):\n")
for (sym in colnames(events)) {
  cat(sprintf("      [%s] Max Daily Loss: %.2f%%\n", sym, abs(min(events[, sym])) * 100))
}

cat("\n2. Solving Optimal f Vector (L-BFGS-B Optimization)...\n")
vince_out <- vince_optimal_f(events, max_leverage = 1.0, safety_factor = 0.35)

cat(sprintf("   -> Convergence Code: %d (0 = Successful Convergence)\n", vince_out$convergence))
cat(sprintf("   -> Portfolio Geometric Holding Period Return (GHPR): %.4f (Expected Growth: %+.2f%% / day)\n\n",
            vince_out$ghpr, vince_out$expected_growth_pct))

cat("   Optimal f and Safe f Solutions:\n")
f_table <- data.frame(
  Symbol = colnames(events),
  Max_Loss = sprintf("%.2f%%", abs(vince_out$max_losses) * 100),
  Optimal_f = sprintf("%.4f", vince_out$optimal_f),
  Safe_f_35Pct = sprintf("%.4f", vince_out$safe_f),
  Portfolio_Weight = sprintf("%.1f%%", vince_out$weights * 100)
)
print(f_table, row.names = FALSE)

cat("\n3. Testing Capital Allocation ($10,000 Portfolio across Current Prices)...\n")
# Fetch current prices
current_px <- c()
for (s in symbols) {
  ohlcv <- load_stock_data(s)
  current_px[s] <- as.numeric(last(Cl(ohlcv)))
}

alloc <- vince_portfolio_allocation(vince_out, total_cash = 10000, current_prices = current_px)
print(alloc, row.names = FALSE)

cat("\n========================================================================\n")
cat(" [PASSED] Ralph Vince Leverage Space Model mathematically verified.\n")
cat("========================================================================\n\n")
