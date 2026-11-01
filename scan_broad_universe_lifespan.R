suppressPackageStartupMessages({
  library(quantmod)
  library(xts)
})

broad_lines <- readLines("symbols_broad.txt", warn=FALSE)
broad_syms <- unique(toupper(trimws(unlist(strsplit(gsub("#.*", "", broad_lines), "[, \\t\\r\\n]+")))))
broad_syms <- broad_syms[broad_syms != ""]

# Check locally cached data in data/data_*.rds
cached_files <- list.files("data", pattern="^data_.*\\.rds$", full.names=TRUE)

start_dates <- character()
for (s in broad_syms) {
  cf <- file.path("data", sprintf("data_%s.rds", tolower(gsub("[^A-Za-z0-9]", "_", s))))
  if (file.exists(cf)) {
    d <- readRDS(cf)
    if (nrow(d) > 0) {
      start_dates[s] <- as.character(index(d)[1])
    }
  }
}

cat(sprintf("Total symbols in symbols_broad.txt: %d\n", length(broad_syms)))
cat(sprintf("Symbols with local cache in data/: %d\n", length(start_dates)))
tbl <- table(substr(start_dates, 1, 4))
cat("\nDistribution of Earliest Date in Local Cache (by Year):\n")
print(tbl)

