suppressPackageStartupMessages({
  library(quantmod)
  library(xts)
  library(parallel)
})

broad_lines <- readLines("symbols_broad.txt", warn=FALSE)
broad_syms <- unique(toupper(trimws(unlist(strsplit(gsub("#.*", "", broad_lines), "[, \\t\\r\\n]+")))))
broad_syms <- broad_syms[broad_syms != ""]

data_dir_10y <- "data/data_10yr"
dir.create(data_dir_10y, showWarnings = FALSE, recursive = TRUE)

# 1. Download Benchmarks
cat("[10Yr-Data] Downloading Benchmarks SPY, QQQ, VTI...\n")
for (bm in c("SPY", "QQQ", "VTI")) {
  cf <- file.path(data_dir_10y, sprintf("data_%s.rds", tolower(bm)))
  if (!file.exists(cf)) {
    d <- tryCatch(suppressWarnings(getSymbols(bm, src="yahoo", from="2016-01-01", auto.assign=FALSE)), error=function(e) NULL)
    if (!is.null(d)) {
      saveRDS(d, cf)
      cat(sprintf("  Saved %s: %d bars\n", bm, nrow(d)))
    }
  }
}

# 2. Download broad universe
cat(sprintf("[10Yr-Data] Fetching 10-year OHLCV for %d symbols in parallel...\n", length(broad_syms)))

fetch_symbol <- function(s) {
  cf <- file.path(data_dir_10y, sprintf("data_%s.rds", tolower(gsub("[^A-Za-z0-9]", "_", s))))
  if (file.exists(cf)) return(list(symbol = s, status = "cached"))
  
  d <- tryCatch(suppressWarnings(getSymbols(s, src="yahoo", from="2016-01-01", auto.assign=FALSE)), error=function(e) NULL)
  if (!is.null(d) && nrow(d) > 50) {
    saveRDS(d, cf)
    return(list(symbol = s, status = "ok", bars = nrow(d), start = as.character(index(d)[1])))
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
cat(sprintf("\n[10Yr-Data] Complete: %d successful, %d failed/unavailable.\n", ok_count, fail_count))
