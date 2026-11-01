source("test_low_max_pos_matrix_backtest.R")

res_cap3 <- simulate_portfolio_matrix(1260, max_pos = 3, max_per_sector = 3, sizing_mode = "vince")
res_cap4 <- simulate_portfolio_matrix(1260, max_pos = 3, max_per_sector = 4, sizing_mode = "vince")

cat(sprintf("MAX_POS=3, CAP=3 60M Wealth: $%.2f (DD: %.2f%%)\n", res_cap3$wealth_num, res_cap3$max_dd))
cat(sprintf("MAX_POS=3, CAP=4 60M Wealth: $%.2f (DD: %.2f%%)\n", res_cap4$wealth_num, res_cap4$max_dd))
cat(sprintf("Difference: $%.2f\n", abs(res_cap3$wealth_num - res_cap4$wealth_num)))
