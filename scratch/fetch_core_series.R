#!/usr/bin/env Rscript
# ==============================================================================
# scratch/fetch_core_series.R
# Downloads the four benchmark / macro series that scratch/pit_feature_search.R
# (and therefore scratch/backtest_option_d_pit.R) reads from data/data_historical/,
# saved under exactly the file names that loader expects:
#   data_qqq.rds, data_qld.rds, data_gld_full.rds, data_^irx.rds
# Existing files are never overwritten. GLD_FULL had no generator in the repo; when
# it is missing it is built from GLD (listed 2004-11-18), which covers the backtest
# calendar (2005-01 onward).
# ==============================================================================
suppressPackageStartupMessages({ library(quantmod); library(xts) })
dir <- "data/data_historical"
dir.create(dir, showWarnings = FALSE, recursive = TRUE)

series <- c(qqq = "QQQ", qld = "QLD", gld_full = "GLD", "^irx" = "^IRX")
ok <- TRUE
for (key in names(series)) {
  f <- file.path(dir, sprintf("data_%s.rds", key))
  if (file.exists(f)) { cat(sprintf("  keep   %-22s (already present)\n", basename(f))); next }
  d <- tryCatch(suppressWarnings(getSymbols(series[[key]], src = "yahoo", from = "1985-01-01",
                                            auto.assign = FALSE)), error = function(e) NULL)
  if (is.null(d) || nrow(d) < 250) { cat(sprintf("  FAILED %s\n", series[[key]])); ok <- FALSE; next }
  if (key == "gld_full") colnames(d) <- sub("^GLD\\.", "GLD_FULL.", colnames(d))
  saveRDS(d, f)
  cat(sprintf("  saved  %-22s %d bars, %s .. %s\n", basename(f), nrow(d),
              format(start(d)), format(end(d))))
}
if (!ok) quit(status = 1)
