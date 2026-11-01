# scratch/compute_exact_taxes.R
suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(TTR)
})

source("R/08_metrics.R")
source("R/09_portfolio_backtest_clean.R")

cat("[Loading clean environment...]\n")
clean_env <- readRDS("scratch/clean_backtest_env.rds")
stocks_list <- clean_env$stocks_list
macro_data  <- clean_env$macro_data
eval_dates  <- clean_env$eval_dates

# Run 35/35/30 model
cat("[Running 35/35/30 Asymmetric Scale Backtest...]\n")
res_35 <- run_clean_portfolio_backtest(
  stocks_list      = stocks_list,
  macro_data       = macro_data,
  eval_dates       = eval_dates,
  initial_capital  = 10000,
  max_positions    = 5,
  leverage         = 1.0,
  use_bear_hedge   = TRUE,
  t1_r             = 2.0,
  t2_r             = 4.0,
  t1_scale_pct     = 0.35,
  t2_scale_pct     = 0.35
)

# Run 50/25/25 model
cat("[Running 50/25/25 Balanced Model Backtest...]\n")
res_50 <- run_clean_portfolio_backtest(
  stocks_list      = stocks_list,
  macro_data       = macro_data,
  eval_dates       = eval_dates,
  initial_capital  = 10000,
  max_positions    = 5,
  leverage         = 1.0,
  use_bear_hedge   = TRUE,
  t1_r             = 2.0,
  t2_r             = 4.0,
  t1_scale_pct     = 0.50,
  t2_scale_pct     = 0.25
)

calc_taxable_compounding <- function(daily_ret_vec, dates_vec, tax_rate, starting_cap = 10000) {
  n <- length(daily_ret_vec)
  equity_curve <- numeric(n)
  cap_curr <- starting_cap
  years <- as.integer(format(dates_vec, "%Y"))
  unique_years <- unique(years)
  
  loss_carryforward <- 0.0
  
  for (yr in unique_years) {
    yr_mask <- (years == yr)
    yr_indices <- which(yr_mask)
    if (length(yr_indices) == 0) next
    
    yr_cap_start <- cap_curr
    for (i in yr_indices) {
      cap_curr <- cap_curr * (1 + daily_ret_vec[i])
      equity_curve[i] <- cap_curr
    }
    yr_cap_end <- cap_curr
    net_gain <- yr_cap_end - yr_cap_start
    
    if (tax_rate > 0) {
      if (net_gain > 0) {
        taxable_gain <- max(0, net_gain - loss_carryforward)
        loss_carryforward <- max(0, loss_carryforward - net_gain)
        tax_due <- taxable_gain * tax_rate
        cap_curr <- cap_curr - tax_due
      } else {
        loss_carryforward <- loss_carryforward + abs(net_gain)
      }
    }
    equity_curve[tail(yr_indices, 1)] <- cap_curr
  }
  
  cagr <- ((cap_curr / starting_cap)^(252 / n) - 1) * 100
  cum_max <- cummax(equity_curve)
  dd <- (equity_curve - cum_max) / cum_max
  max_dd <- abs(min(dd)) * 100
  
  list(
    terminal_wealth = cap_curr,
    cagr            = cagr,
    max_dd          = max_dd
  )
}

# Taxes for 35/35/30
t35_gross <- calc_taxable_compounding(res_35$daily_returns, eval_dates, tax_rate = 0.0)
t35_tts   <- calc_taxable_compounding(res_35$daily_returns, eval_dates, tax_rate = 0.284)
t35_nyc   <- calc_taxable_compounding(res_35$daily_returns, eval_dates, tax_rate = 0.512)

# Taxes for 50/25/25
t50_gross <- calc_taxable_compounding(res_50$daily_returns, eval_dates, tax_rate = 0.0)
t50_tts   <- calc_taxable_compounding(res_50$daily_returns, eval_dates, tax_rate = 0.284)
t50_nyc   <- calc_taxable_compounding(res_50$daily_returns, eval_dates, tax_rate = 0.512)

cat("\n========================================================================================\n")
cat("                       EXACT TAX SCORECARD (AUDITED 2016-2026)                          \n")
cat("========================================================================================\n")
cat(sprintf("35/35/30 Model:\n"))
cat(sprintf("  Gross (0.0%%):  Terminal: $%.2f | CAGR: %+.2f%% | MaxDD: -%.2f%%\n", t35_gross$terminal_wealth, t35_gross$cagr, t35_gross$max_dd))
cat(sprintf("  TTS   (28.4%%): Terminal: $%.2f | CAGR: %+.2f%% | MaxDD: -%.2f%%\n", t35_tts$terminal_wealth, t35_tts$cagr, t35_tts$max_dd))
cat(sprintf("  NYC   (51.2%%): Terminal: $%.2f | CAGR: %+.2f%% | MaxDD: -%.2f%%\n\n", t35_nyc$terminal_wealth, t35_nyc$cagr, t35_nyc$max_dd))

cat(sprintf("50/25/25 Model:\n"))
cat(sprintf("  Gross (0.0%%):  Terminal: $%.2f | CAGR: %+.2f%% | MaxDD: -%.2f%%\n", t50_gross$terminal_wealth, t50_gross$cagr, t50_gross$max_dd))
cat(sprintf("  TTS   (28.4%%): Terminal: $%.2f | CAGR: %+.2f%% | MaxDD: -%.2f%%\n", t50_tts$terminal_wealth, t50_tts$cagr, t50_tts$max_dd))
cat(sprintf("  NYC   (51.2%%): Terminal: $%.2f | CAGR: %+.2f%% | MaxDD: -%.2f%%\n", t50_nyc$terminal_wealth, t50_nyc$cagr, t50_nyc$max_dd))
cat("========================================================================================\n")
