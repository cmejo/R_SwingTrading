suppressPackageStartupMessages({
  library(quantmod)
  library(xts)
})

etf_dir <- "data/data_etf"
dir.create(etf_dir, showWarnings = FALSE, recursive = TRUE)

user_etfs <- c("IYC", "IYK", "IYE", "IYF", "IYH", "IYJ", "IYM", "IYW", "IYZ", "IDU", "EFA", "EEM", "IEF", "SPY", "QQQ")

cat("[ETF-Download] Fetching historical OHLCV data from 2000-01-01...\n")

for (s in user_etfs) {
  cf <- file.path(etf_dir, sprintf("data_%s.rds", tolower(s)))
  d <- tryCatch(suppressWarnings(getSymbols(s, src="yahoo", from="2000-01-01", auto.assign=FALSE)), error=function(e) NULL)
  if (!is.null(d)) {
    saveRDS(d, cf)
    cat(sprintf("  Saved %s: %d bars from %s to %s\n", s, nrow(d), as.character(index(d)[1]), as.character(tail(index(d), 1))))
  } else {
    cat(sprintf("  FAILED %s\n", s))
  }
}
cat("[ETF-Download] Complete.\n")
