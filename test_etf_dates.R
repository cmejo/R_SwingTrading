suppressPackageStartupMessages({
  library(quantmod)
  library(xts)
})

user_etfs <- c("IYC", "IYK", "IYE", "IYF", "IYH", "IYJ", "IYM", "IYW", "IYZ", "IDU", "EFA", "EEM", "IEF", "SPY", "QQQ")

cat("Checking historical availability for user ETFs from 2000-01-01...\n")

results <- data.frame(Symbol = character(), StartDate = character(), Rows = numeric(), stringsAsFactors = FALSE)

for (s in user_etfs) {
  d <- tryCatch(suppressWarnings(getSymbols(s, src="yahoo", from="2000-01-01", auto.assign=FALSE)), error=function(e) NULL)
  if (!is.null(d)) {
    results <- rbind(results, data.frame(
      Symbol = s,
      StartDate = as.character(index(d)[1]),
      Rows = nrow(d),
      stringsAsFactors = FALSE
    ))
  } else {
    results <- rbind(results, data.frame(Symbol = s, StartDate = "NOT_FOUND", Rows = 0, stringsAsFactors = FALSE))
  }
}

print(results)
