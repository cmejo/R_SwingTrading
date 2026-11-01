suppressPackageStartupMessages({
  library(xts)
  library(quantmod)
})
obj <- readRDS("data/cache_10yr/aapl_10yr_wf.rds")
cat("AAPL bracket_ret mean:", mean(obj$bracket_ret), "sd:", sd(obj$bracket_ret), "\n")
cat("Compounded 10yr bracket_ret:", prod(1 + obj$bracket_ret), "\n")

ohlcv <- readRDS("data/data_10yr/data_aapl.rds")
cl <- Cl(ohlcv)
cat("AAPL actual buy&hold return:", as.numeric(tail(cl, 1) / head(cl, 1)), "\n")
