#!/usr/bin/env Rscript
# ==============================================================================
# scratch/test_2_and_3_slots.R
# 10-Year Clean Discrete Walk-Forward Backtest with 2 and 3 Slots vs. 5 and 6 Slots
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

cat("[Loading precomputed clean environment...]\n")
env_data <- readRDS("scratch/clean_backtest_env.rds")
stocks_list <- env_data$stocks_list
macro_data  <- env_data$macro_data
eval_dates  <- env_data$eval_dates

# Load Benchmarks
vti_raw <- readRDS("data/data_10yr/data_vti.rds")
vti_cl  <- Cl(vti_raw)[eval_dates]
vti_ret <- as.numeric((vti_cl / lag.xts(vti_cl, 1)) - 1); vti_ret[is.na(vti_ret)] <- 0

qqq_cl  <- macro_data$qqq_close
qqq_ret <- as.numeric((qqq_cl / lag.xts(qqq_cl, 1)) - 1); qqq_ret[is.na(qqq_ret)] <- 0

spy_cl  <- macro_data$spy_close
spy_ret <- as.numeric((spy_cl / lag.xts(spy_cl, 1)) - 1); spy_ret[is.na(spy_ret)] <- 0

blend_70_30_ret <- 0.70 * qqq_ret + 0.30 * vti_ret

cat(sprintf("[Loaded %d stocks, %d sessions (2016-2026)]\n\n", length(stocks_list), length(eval_dates)))

configs <- list(
  # 2 Slots Configurations
  list(name = "1. 2 Slots (1.0x Cash, Unhedged)",                   mp = 2, ms = 1, lev = 1.00, hedge = FALSE, vt = FALSE, pv = 0.35),
  list(name = "2. 2 Slots + Smart Dual-Bear Hedge (1.0x Cash)",     mp = 2, ms = 1, lev = 1.00, hedge = TRUE,  vt = FALSE, pv = 0.35),
  list(name = "3. 2 Slots + Dual-Bear + Vol Target 35% (1.25x Lev)", mp = 2, ms = 1, lev = 1.25, hedge = TRUE,  vt = TRUE,  pv = 0.35),
  
  # 3 Slots Configurations
  list(name = "4. 3 Slots (1.0x Cash, Unhedged)",                   mp = 3, ms = 2, lev = 1.00, hedge = FALSE, vt = FALSE, pv = 0.35),
  list(name = "5. 3 Slots + Smart Dual-Bear Hedge (1.0x Cash)",     mp = 3, ms = 2, lev = 1.00, hedge = TRUE,  vt = FALSE, pv = 0.35),
  list(name = "6. 3 Slots + Dual-Bear + Vol Target 35% (1.25x Lev)", mp = 3, ms = 2, lev = 1.25, hedge = TRUE,  vt = TRUE,  pv = 0.35),

  # Comparison Reference: 5 & 6 Slots
  list(name = "7. 5 Slots + Dual-Bear + Vol Target 35% (1.25x Lev)", mp = 5, ms = 2, lev = 1.25, hedge = TRUE,  vt = TRUE,  pv = 0.35),
  list(name = "8. 6 Slots + Dual-Bear + Vol Target 35% (1.25x Lev)", mp = 6, ms = 2, lev = 1.25, hedge = TRUE,  vt = TRUE,  pv = 0.35)
)

results <- list()

for (cfg in configs) {
  cat(sprintf("[Running] %-55s ... ", cfg$name))
  t0 <- Sys.time()
  res <- run_clean_portfolio_backtest(
    stocks_list          = stocks_list,
    macro_data           = macro_data,
    eval_dates           = eval_dates,
    initial_capital      = 10000,
    max_positions        = cfg$mp,
    max_per_sector       = cfg$ms,
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
    drawdown_throttle    = FALSE,
    use_net_wick         = TRUE
  )
  elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  cat(sprintf("Done (%s s)\n", elapsed))

  results[[cfg$name]] <- list(
    terminal_wealth = res$terminal_wealth,
    cagr            = res$cagr,
    max_dd          = res$max_dd,
    sharpe          = res$sharpe,
    sortino         = res$sortino,
    calmar          = res$calmar,
    annual_vol      = res$annual_vol,
    win_rate        = res$win_rate,
    total_trades    = res$total_trades,
    margin_paid     = res$total_margin_paid,
    equity_curve    = res$equity_curve,
    daily_returns   = res$daily_returns
  )
}

# Add Benchmarks
m_qqq   <- calc_performance_metrics(xts(qqq_ret, order.by = eval_dates), name = "QQQ")$raw
m_blend <- calc_performance_metrics(xts(blend_70_30_ret, order.by = eval_dates), name = "70_30")$raw
m_spy   <- calc_performance_metrics(xts(spy_ret, order.by = eval_dates), name = "SPY")$raw

results[["Benchmark: Nasdaq-100 (QQQ Buy & Hold)"]] <- list(
  terminal_wealth = 10000 * tail(cumprod(1 + qqq_ret), 1),
  cagr            = m_qqq$ann_ret * 100,
  max_dd          = m_qqq$max_dd * 100,
  sharpe          = m_qqq$sharpe,
  sortino         = m_qqq$sortino,
  calmar          = m_qqq$ann_ret / m_qqq$max_dd,
  annual_vol      = m_qqq$ann_vol * 100,
  win_rate        = m_qqq$win_rate * 100,
  total_trades    = 1,
  margin_paid     = 0,
  equity_curve    = 10000 * cumprod(1 + qqq_ret),
  daily_returns   = qqq_ret
)

results[["Benchmark: 70% QQQ / 30% VTI (Balanced)"]] <- list(
  terminal_wealth = 10000 * tail(cumprod(1 + blend_70_30_ret), 1),
  cagr            = m_blend$ann_ret * 100,
  max_dd          = m_blend$max_dd * 100,
  sharpe          = m_blend$sharpe,
  sortino         = m_blend$sortino,
  calmar          = m_blend$ann_ret / m_blend$max_dd,
  annual_vol      = m_blend$ann_vol * 100,
  win_rate        = m_blend$win_rate * 100,
  total_trades    = 1,
  margin_paid     = 0,
  equity_curve    = 10000 * cumprod(1 + blend_70_30_ret),
  daily_returns   = blend_70_30_ret
)

results[["Benchmark: S&P 500 (SPY Buy & Hold)"]] <- list(
  terminal_wealth = 10000 * tail(cumprod(1 + spy_ret), 1),
  cagr            = m_spy$ann_ret * 100,
  max_dd          = m_spy$max_dd * 100,
  sharpe          = m_spy$sharpe,
  sortino         = m_spy$sortino,
  calmar          = m_spy$ann_ret / m_spy$max_dd,
  annual_vol      = m_spy$ann_vol * 100,
  win_rate        = m_spy$win_rate * 100,
  total_trades    = 1,
  margin_paid     = 0,
  equity_curve    = 10000 * cumprod(1 + spy_ret),
  daily_returns   = spy_ret
)

scorecard_rows <- list()
for (nm in names(results)) {
  item <- results[[nm]]
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
    Margin_Paid     = sprintf("$%.2f", item$margin_paid),
    stringsAsFactors = FALSE
  )
}

scorecard_df <- do.call(rbind, scorecard_rows)

cat("\n========================================================================================================================\n")
cat("                10-YEAR CLEAN DISCRETE WALK-FORWARD SCORECARD: 2 & 3 SLOTS vs. 5 & 6 SLOTS                              \n")
cat("========================================================================================================================\n")
print(scorecard_df, row.names = FALSE)
cat("========================================================================================================================\n\n")

write.csv(scorecard_df, "output/slots_2_and_3_comparison_scorecard.csv", row.names = FALSE)
cat("[Done] Saved to output/slots_2_and_3_comparison_scorecard.csv\n")
