suppressPackageStartupMessages(library(xts))
source("R/08_metrics.R")

# Let's inspect test_low_max_pos_matrix_backtest.R lines where 60M and 24M were computed!
scorecard <- read.csv("output/low_max_pos_matrix_scorecard.csv", stringsAsFactors = FALSE)
p2_v <- scorecard[scorecard$Max_Positions == 2 & scorecard$Sector_Cap == 2 & scorecard$Sizing_Mode == "vince", ]
p2_e <- scorecard[scorecard$Max_Positions == 2 & scorecard$Sector_Cap == 2 & scorecard$Sizing_Mode == "equal", ]
p5_v <- scorecard[scorecard$Max_Positions == 5 & scorecard$Sector_Cap == 5 & scorecard$Sizing_Mode == "vince", ]
p5_e <- scorecard[scorecard$Max_Positions == 5 & scorecard$Sector_Cap == 5 & scorecard$Sizing_Mode == "equal", ]

cat("P2 Vince:\n")
print(p2_v[, c("Horizon", "Terminal_Wealth", "Cum_Return", "Sharpe_Ratio", "Max_Drawdown")])
cat("\nP5 Vince:\n")
print(p5_v[, c("Horizon", "Terminal_Wealth", "Cum_Return", "Sharpe_Ratio", "Max_Drawdown")])
cat("\nP2 Equal:\n")
print(p2_e[, c("Horizon", "Terminal_Wealth", "Cum_Return", "Sharpe_Ratio", "Max_Drawdown")])
cat("\nP5 Equal:\n")
print(p5_e[, c("Horizon", "Terminal_Wealth", "Cum_Return", "Sharpe_Ratio", "Max_Drawdown")])
