# Check portfolio returns in run_10yr_matrix_simulation.R
scorecard_7y <- read.csv("output/seven_year_backtest_scorecard.csv", stringsAsFactors = FALSE)
print(scorecard_7y[, c("Configuration", "Terminal_Wealth", "Ann_Return", "Sharpe_Ratio", "Max_Drawdown")])
