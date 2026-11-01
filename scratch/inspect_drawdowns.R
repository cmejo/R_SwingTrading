#!/usr/bin/env Rscript
# inspect_drawdowns.R

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
})

env_data <- readRDS("scratch/clean_backtest_env.rds")
stocks_list <- env_data$stocks_list
macro_data  <- env_data$macro_data
eval_dates  <- env_data$eval_dates

# Modify function to return equity curve
source("scratch/test_refined_features.R")

# Let's inspect daily return curve
# We can do this by running a quick variation that returns portfolio_equity
run_and_get_curve <- function(target_vol = 0.35, lev = 1.25) {
  # Same as run_refined_sim but returns equity xts
  source("R/08_metrics.R")
  # Return equity
}
