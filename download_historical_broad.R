suppressPackageStartupMessages({
  library(quantmod)
  library(xts)
  library(parallel)
})

broad_lines <- readLines("symbols_broad.txt", warn=FALSE)
broad_syms <- unique(toupper(trimws(unlist(strsplit(gsub("#.*", "", broad_lines), "[, \\t\\r\\n]+")))))
broad_syms <- broad_syms[broad_syms != ""]

data_dir <- "data/data_historical"
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

cat(sprintf("[Historical-Data] Fetching multi-decade OHLCV for %d symbols from 1985...\n", length(broad_syms)))

fetch_symbol <- function(s) {
  cf <- file.path(data_dir, sprintf("data_%s.rds", tolower(gsub("[^A-Za-z0-9]", "_", s))))
  if (file.exists(cf)) {
    d <- tryCatch(readRDS(cf), error = function(e) NULL)
    if (!is.null(d) && nrow(d) > 50) return(list(symbol = s, status = "cached", bars = nrow(d)))
  }
  
  d <- tryCatch(suppressWarnings(getSymbols(s, src="yahoo", from="1985-01-01", auto.assign=FALSE)), error=function(e) NULL)
  if (!is.null(d) && nrow(d) > 50) {
    saveRDS(d, cf)
    return(list(symbol = s, status = "ok", bars = nrow(d), start = as.character(index(d)[1]), end = as.character(tail(index(d), 1))))
  } else {
    return(list(symbol = s, status = "failed"))
  }
}

num_cores <- min(8, detectCores())
cat(sprintf("Using %d parallel workers...\n", num_cores))
res <- mclapply(broad_syms, fetch_symbol, mc.cores = num_cores)

ok_count <- 0
fail_count <- 0
for (r in res) {
  if (r$status %in% c("ok", "cached")) ok_count <- ok_count + 1 else fail_count <- fail_count + 1
}
cat(sprintf("\n[Historical-Data] Complete: %d successful, %d failed/unavailable.\n", ok_count, fail_count))
