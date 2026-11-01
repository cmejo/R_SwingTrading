suppressPackageStartupMessages({
  library(quantmod)
  library(xts)
})

broad_lines <- readLines("symbols_broad.txt", warn=FALSE)
broad_syms <- unique(toupper(trimws(unlist(strsplit(gsub("#.*", "", broad_lines), "[, \\t\\r\\n]+")))))
broad_syms <- broad_syms[broad_syms != ""]

cat(sprintf("Checking 10-year (2016-10-01 to 2026-10-06) history for all %d symbols in symbols_broad.txt...\n", length(broad_syms)))

# Check SPY and QQQ
spy <- tryCatch(suppressWarnings(getSymbols("SPY", src="yahoo", from="2016-01-01", auto.assign=FALSE)), error=function(e) NULL)
qqq <- tryCatch(suppressWarnings(getSymbols("QQQ", src="yahoo", from="2016-01-01", auto.assign=FALSE)), error=function(e) NULL)

cat(sprintf("SPY bars: %d, QQQ bars: %d\n", if (is.null(spy)) 0 else nrow(spy), if (is.null(qqq)) 0 else nrow(qqq)))
