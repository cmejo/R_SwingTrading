suppressPackageStartupMessages({
  library(quantmod)
  library(TTR)
  library(parallel)
})

source("R/01_data_loader.R")
source("R/02_trend_lmMA.R")
source("R/03_volatility_garch.R")
source("R/04_feature_pipeline.R")
source("R/05_logistic_model.R")
source("R/06_swing_backtest.R")

cat("[RegimeTest] Starting dynamic regime backtest analysis...\n")
