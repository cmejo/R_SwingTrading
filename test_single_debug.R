suppressPackageStartupMessages({
  library(xts)
  library(quantmod)
  library(TTR)
})

source("R/01_data_loader.R")
source("R/02_trend_lmMA.R")
source("R/03_volatility_garch.R")
source("R/04_feature_pipeline.R")

spy <- readRDS("data/data_10yr/data_spy.rds")
aapl <- readRDS("data/data_10yr/data_aapl.rds")

c_idx <- intersect(index(aapl), index(spy))
aapl_sub <- aapl[c_idx]
spy_sub  <- spy[c_idx]

res <- tryCatch(build_feature_dataset(aapl_sub, spy_data=spy_sub, forward_window=7), error=function(e) e)
if (inherits(res, "error")) {
  cat("Error:", res$message, "\n")
} else {
  cat("Success! Dataset names:", names(res), "\n")
  cat("Rows:", nrow(res$dataset), "\n")
}
