suppressPackageStartupMessages(library(xts))

# Let's inspect the specific correlation and trade concentration between POS 3 and POS 4
scorecard_10y <- read.csv("output/ten_year_backtest_scorecard.csv", stringsAsFactors = FALSE)
p3 <- scorecard_10y[scorecard_10y$Configuration == "MAX_POS=3, CAP=3 (Vince)", ]
p4 <- scorecard_10y[scorecard_10y$Configuration == "MAX_POS=4, CAP=4 (Vince)", ]

cat(sprintf("10-Year Terminal Wealth: POS 3 = %s vs POS 4 = %s\n", p3$Terminal_Wealth, p4$Terminal_Wealth))
cat(sprintf("10-Year Max Drawdown: POS 3 = %s vs POS 4 = %s\n", p3$Max_Drawdown, p4$Max_Drawdown))
cat(sprintf("10-Year Sharpe Ratio: POS 3 = %s vs POS 4 = %s\n", p3$Sharpe_Ratio, p4$Sharpe_Ratio))
