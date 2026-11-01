suppressPackageStartupMessages({
  library(quantmod)
  library(xts)
})

broad_lines <- readLines("symbols_broad.txt", warn=FALSE)
broad_syms <- unique(toupper(trimws(unlist(strsplit(gsub("#.*", "", broad_lines), "[, \\t\\r\\n]+")))))
broad_syms <- broad_syms[broad_syms != ""]

cat(sprintf("Checking 120-month (10-year, 2016-2026) data for %d symbols in symbols_broad.txt...\n", length(broad_syms)))

# Check which symbols exist from 2016
check_sym <- function(s) {
  cf <- file.path("data", sprintf("data_%s.rds", tolower(gsub("[^A-Za-z0-9]", "_", s))))
  if (file.exists(cf)) {
    d <- readRDS(cf)
    if (nrow(d) > 0 && as.Date(index(d)[1]) <= as.Date("2016-10-01")) {
      return(TRUE)
    }
  }
  # Try quick fetch check for 5 symbols as sample
  return(FALSE)
}

cached_10yr <- sapply(broad_syms, check_sym)
cat(sprintf("Cached symbols with 10yr data already on disk: %d / %d\n", sum(cached_10yr), length(broad_syms)))
