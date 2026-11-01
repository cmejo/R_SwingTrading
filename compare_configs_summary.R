scorecard_5y <- read.csv("output/low_max_pos_matrix_scorecard.csv", stringsAsFactors = FALSE)
scorecard_7y <- read.csv("output/seven_year_backtest_scorecard.csv", stringsAsFactors = FALSE)
scorecard_10y <- read.csv("output/ten_year_backtest_scorecard.csv", stringsAsFactors = FALSE)

# Focus configurations:
# MAX_POS in c(3, 4, 5)
# Sector Cap in c(3, 4) (where cap <= max_pos, so POS 3 has Cap 3; POS 4 has Cap 3, 4; POS 5 has Cap 3, 4)

cat("=== 5-YEAR MATRIX (symbols_broad.txt) ===\n")
sub_5y <- scorecard_5y[scorecard_5y$Max_Positions %in% c(3, 4, 5) & scorecard_5y$Sector_Cap %in% c(3, 4), ]
# View Vince and Equal for 12M, 24M, 60M
sub_5y_60m <- sub_5y[sub_5y$Horizon == "60 Months", c("Max_Positions", "Sector_Cap", "Sizing_Mode", "Terminal_Wealth", "Sharpe_Ratio", "Max_Drawdown")]
print(sub_5y_60m)

cat("\n=== 7-YEAR MATRIX (symbols_broad.txt) ===\n")
print(scorecard_7y[, c("Configuration", "Terminal_Wealth", "Sharpe_Ratio", "Max_Drawdown")])

cat("\n=== 10-YEAR MATRIX (symbols_broad.txt) ===\n")
print(scorecard_10y[, c("Configuration", "Terminal_Wealth", "Sharpe_Ratio", "Max_Drawdown")])
