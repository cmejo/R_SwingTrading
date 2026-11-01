suppressPackageStartupMessages(library(xts))
source("test_low_max_pos_matrix_backtest.R")

cat("\n--- Running 60M for MaxPos=2/Cap=2 (Vince) ---\n")
r2_60 <- simulate_portfolio_matrix(1260, max_pos = 2, max_per_sector = 2, sizing_mode = "vince")
cat("\n--- Running 60M for MaxPos=5/Cap=5 (Vince) ---\n")
r5_60 <- simulate_portfolio_matrix(1260, max_pos = 5, max_per_sector = 5, sizing_mode = "vince")

# Let's see the 24M for both:
r2_24 <- simulate_portfolio_matrix(504, max_pos = 2, max_per_sector = 2, sizing_mode = "vince")
r5_24 <- simulate_portfolio_matrix(504, max_pos = 5, max_per_sector = 5, sizing_mode = "vince")

cat(sprintf("24M Wealth: P2=$%.2f vs P5=$%.2f\n", r2_24$wealth_num, r5_24$wealth_num))
cat(sprintf("60M Wealth: P2=$%.2f vs P5=$%.2f\n", r2_60$wealth_num, r5_60$wealth_num))

# Notice that 24M is 504 bars, which is the LAST 504 bars of the 1260 bars!
# So 60M contains 756 older bars (2021-2024) PLUS 504 recent bars (2024-2026).
# If at 24M, P2 = $213,115 and P5 = $142,492: P2 actually beat P5 over the 24M window!
