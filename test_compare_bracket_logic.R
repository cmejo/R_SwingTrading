suppressPackageStartupMessages({
  library(xts)
  library(quantmod)
})

# Let's inspect the bracket return generation in run_10yr_walkforward.R vs test_low_max_pos_matrix_backtest.R
# In run_10yr_walkforward.R:
# entry_p <- curr_c
# stop_p <- entry_p - 1.5 * curr_atr
# pt_p <- entry_p + 2.5 * curr_atr
# Notice: In run_10yr_walkforward.R, lines 120-127:
# pos_active <- TRUE; entry_p <- curr_c; bracket_daily_ret[i] <- 0
# EVERY TIME A POSITION EXITS, IT IMMEDIATELY ENTERS AGAIN ON THE VERY SAME BAR!
# AND IT DOES NOT CHECK PROBABILITY OR RS BEFORE ENTERING THE PRECOMPUTED BRACKET!
