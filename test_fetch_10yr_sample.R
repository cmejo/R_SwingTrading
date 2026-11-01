suppressPackageStartupMessages(library(quantmod))
symbols_to_test <- c("AAPL", "AMD", "NVDA", "PANW", "MRVL", "ALAB", "AMUU", "PLTR", "CRWD", "QBTS")

results <- data.frame(Symbol=character(), StartDate=character(), YearsAvailable=numeric(), stringsAsFactors=FALSE)

for (s in symbols_to_test) {
  d <- tryCatch(suppressWarnings(getSymbols(s, src="yahoo", from="2016-01-01", auto.assign=FALSE)), error=function(e) NULL)
  if (!is.null(d)) {
    first_d <- as.Date(index(d)[1])
    yrs <- round(as.numeric(Sys.Date() - first_d) / 365.25, 1)
    results <- rbind(results, data.frame(Symbol=s, StartDate=as.character(first_d), YearsAvailable=yrs))
  } else {
    results <- rbind(results, data.frame(Symbol=s, StartDate="NOT_FOUND", YearsAvailable=0))
  }
}
print(results)
