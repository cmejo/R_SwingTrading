#!/usr/bin/env Rscript
# ==============================================================================
# run_clean_10yr_walkforward.R
# Definitive 10-Year Clean Zero-Lookahead Walk-Forward Backtest & Comparison
# ==============================================================================

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(TTR)
  library(jsonlite)
})

source("R/08_metrics.R")
source("R/09_portfolio_backtest_clean.R")

cat("\n========================================================================================\n")
cat("      10-YEAR WALK-FORWARD OUT-OF-SAMPLE AUDIT (CLEAN ZERO-LOOKAHEAD ENGINE)             \n")
cat("========================================================================================\n\n")

# 1. Load Data
env_file <- "scratch/clean_backtest_env.rds"
if (file.exists(env_file)) {
  cat("[1/4] Loading precomputed clean environment from cache...\n")
  env_data <- readRDS(env_file)
  stocks_list <- env_data$stocks_list
  macro_data  <- env_data$macro_data
  eval_dates  <- env_data$eval_dates
} else {
  stop("Precomputed environment file scratch/clean_backtest_env.rds not found.")
}

cat(sprintf("      Successfully loaded %d stocks across %d trading sessions.\n\n",
            length(stocks_list), length(eval_dates)))

# 2. Define Configurations
configs <- list(
  list(name = "1. Baseline Clean (3 Slots, 1.0x Cash, Unhedged)",
       mp = 3, lev = 1.00, hedge = FALSE, vt = FALSE, pv = 0.35, nw = FALSE, dd = FALSE),
  list(name = "2. Baseline Clean Margin (3 Slots, 1.5x Margin, Unhedged)",
       mp = 3, lev = 1.50, hedge = FALSE, vt = FALSE, pv = 0.35, nw = FALSE, dd = FALSE),
  list(name = "3. Diversified Core (5 Slots, 1.0x Cash, Unhedged)",
       mp = 5, lev = 1.00, hedge = FALSE, vt = FALSE, pv = 0.35, nw = FALSE, dd = FALSE),
  list(name = "4. Diversified + Smart Dual-Bear Hedge (5 Slots, 1.0x Cash)",
       mp = 5, lev = 1.00, hedge = TRUE,  vt = FALSE, pv = 0.35, nw = FALSE, dd = FALSE),
  list(name = "5. Upgraded Alpha + Net Wick Demand (5 Slots, 1.0x Cash)",
       mp = 5, lev = 1.00, hedge = TRUE,  vt = FALSE, pv = 0.35, nw = TRUE,  dd = FALSE),
  list(name = "6. Production Master Model: 5 Slots + Dual-Bear + Vol Target 35% (1.25x)",
       mp = 5, lev = 1.25, hedge = TRUE,  vt = TRUE,  pv = 0.35, nw = TRUE,  dd = FALSE),
  list(name = "7. Production Robust Model: 6 Slots + Dual-Bear + Vol Target 35% (1.25x)",
       mp = 6, lev = 1.25, hedge = TRUE,  vt = TRUE,  pv = 0.35, nw = TRUE,  dd = FALSE)
)

cat("[2/4] Running clean discrete backtest simulations across 7 configurations...\n")
results_list <- list()

for (cfg in configs) {
  cat(sprintf("      Running: %-65s ... ", cfg$name))
  t0 <- Sys.time()
  res <- run_clean_portfolio_backtest(
    stocks_list          = stocks_list,
    macro_data           = macro_data,
    eval_dates           = eval_dates,
    initial_capital      = 10000,
    max_positions        = cfg$mp,
    max_per_sector       = 2,
    leverage             = cfg$lev,
    target_vol           = 0.35,
    use_vol_targeting    = cfg$vt,
    target_portfolio_vol = cfg$pv,
    cost_bps             = 10,
    margin_rate          = 0.070,
    p_long               = 0.58,
    min_history_bars     = 150,
    use_bear_hedge       = cfg$hedge,
    hedge_stop_loss      = -0.10,
    hedge_time_stop      = 15,
    sizing_mode          = "sortino_er",
    drawdown_throttle    = cfg$dd,
    use_net_wick         = cfg$nw
  )
  elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  cat(sprintf("Done (%s s)\n", elapsed))
  
  results_list[[cfg$name]] <- list(
    terminal_wealth = res$terminal_wealth,
    cagr            = res$cagr,
    max_dd          = res$max_dd,
    sharpe          = res$sharpe,
    sortino         = res$sortino,
    calmar          = res$calmar,
    annual_vol      = res$annual_vol,
    win_rate        = res$win_rate,
    total_trades    = res$total_trades,
    margin_interest = res$total_margin_paid,
    equity_curve    = res$equity_curve
  )
}

# 3. Add Benchmarks (QQQ and SPY)
cat("\n[3/4] Compiling benchmark performance (QQQ & SPY Buy & Hold)...\n")
spy_ret <- as.numeric((macro_data$spy_close / lag.xts(macro_data$spy_close, 1)) - 1)
spy_ret[is.na(spy_ret)] <- 0
qqq_ret <- as.numeric((macro_data$qqq_close / lag.xts(macro_data$qqq_close, 1)) - 1)
qqq_ret[is.na(qqq_ret)] <- 0

m_spy <- calc_performance_metrics(xts(spy_ret, order.by = eval_dates), name = "SPY_Buy_Hold")$raw
m_qqq <- calc_performance_metrics(xts(qqq_ret, order.by = eval_dates), name = "QQQ_Buy_Hold")$raw

results_list[["Benchmark: Nasdaq-100 (QQQ Buy & Hold)"]] <- list(
  terminal_wealth = 10000 * tail(cumprod(1 + qqq_ret), 1),
  cagr            = m_qqq$ann_ret * 100,
  max_dd          = m_qqq$max_dd * 100,
  sharpe          = m_qqq$sharpe,
  sortino         = m_qqq$sortino,
  calmar          = if (!is.na(m_qqq$max_dd) && m_qqq$max_dd > 0) m_qqq$ann_ret / m_qqq$max_dd else NA,
  annual_vol      = m_qqq$ann_vol * 100,
  win_rate        = m_qqq$win_rate * 100,
  total_trades    = 1,
  margin_interest = 0,
  equity_curve    = 10000 * cumprod(1 + qqq_ret)
)

results_list[["Benchmark: S&P 500 (SPY Buy & Hold)"]] <- list(
  terminal_wealth = 10000 * tail(cumprod(1 + spy_ret), 1),
  cagr            = m_spy$ann_ret * 100,
  max_dd          = m_spy$max_dd * 100,
  sharpe          = m_spy$sharpe,
  sortino         = m_spy$sortino,
  calmar          = if (!is.na(m_spy$max_dd) && m_spy$max_dd > 0) m_spy$ann_ret / m_spy$max_dd else NA,
  annual_vol      = m_spy$ann_vol * 100,
  win_rate        = m_spy$win_rate * 100,
  total_trades    = 1,
  margin_interest = 0,
  equity_curve    = 10000 * cumprod(1 + spy_ret)
)

# 4. Format and Display Scorecard
cat("\n[4/4] Generating Scorecard...\n")
scorecard_rows <- list()

for (nm in names(results_list)) {
  item <- results_list[[nm]]
  scorecard_rows[[length(scorecard_rows) + 1]] <- data.frame(
    Configuration   = nm,
    Terminal_Wealth = sprintf("$%s", format(round(item$terminal_wealth, 2), big.mark = ",", scientific = FALSE)),
    CAGR            = sprintf("%+.2f%%", item$cagr),
    Max_Drawdown    = sprintf("-%.2f%%", item$max_dd),
    Sharpe_Ratio    = sprintf("%.2f", item$sharpe),
    Sortino_Ratio   = sprintf("%.2f", item$sortino),
    Calmar_Ratio    = if (!is.null(item$calmar) && !is.na(item$calmar)) sprintf("%.2f", item$calmar) else "N/A",
    Annual_Vol      = sprintf("%.2f%%", item$annual_vol),
    Win_Rate        = sprintf("%.1f%%", item$win_rate),
    Total_Trades    = item$total_trades,
    Margin_Paid     = sprintf("$%.2f", item$margin_interest),
    stringsAsFactors = FALSE
  )
}

scorecard_df <- do.call(rbind, scorecard_rows)

cat("\n========================================================================================================================\n")
cat("                           AUDITED 10-YEAR WALK-FORWARD PERFORMANCE SCORECARD (2016-2026)                               \n")
cat("========================================================================================================================\n")
print(scorecard_df, row.names = FALSE)
cat("========================================================================================================================\n\n")

write.csv(scorecard_df, "output/clean_10yr_walkforward_scorecard.csv", row.names = FALSE)
cat("[Saved scorecard to output/clean_10yr_walkforward_scorecard.csv]\n")

# Save detailed results object
saveRDS(results_list, "output/clean_10yr_walkforward_results.rds")
cat("[Saved detailed results to output/clean_10yr_walkforward_results.rds]\n")
