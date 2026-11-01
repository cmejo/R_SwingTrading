suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(TTR)
})

source("R/01_data_loader.R")
source("R/08_metrics.R")

qqq_ohlcv <- tryCatch(load_symbol_data("QQQ", start_date = "2014-01-01"), error = function(e) NULL)
if (is.null(qqq_ohlcv)) {
  cat("[Error] Could not load QQQ from 2014-01-01\n")
  quit(status = 1)
}
cat(sprintf("[Loaded] QQQ historical bars: %d from %s to %s\n", 
            nrow(qqq_ohlcv), as.character(index(qqq_ohlcv)[1]), as.character(tail(index(qqq_ohlcv), 1))))
