#!/usr/bin/env Rscript
# ==============================================================================
# scratch/wf_build_feature_panel.R
# Builds the pooled 27-feature panel for every stock in data/data_historical
# using the PRODUCTION pipeline (R/04_feature_pipeline.R). Cached to
# scratch/wf_feature_panel.rds for the walk-forward ElasticNet backtest.
# GARCH disabled (use_garch = FALSE) -> rolling-vol fallback, for tractability.
# ==============================================================================
suppressPackageStartupMessages({ library(xts); library(quantmod) })
source("R/04_feature_pipeline.R")

HIST_DIR <- "data/data_historical"
spy <- readRDS(file.path(HIST_DIR, "data_spy.rds"))
br  <- readRDS("data/intraday_breadth_10d.rds")

stock_files <- list.files(HIST_DIR, pattern = "^data_[a-z0-9_]+\\.rds$", full.names = TRUE)
bench_patterns <- c("data_\\^", "data_vti", "data_qqq", "data_spy", "data_vfinx", "data_gspc", "data_ndx", "data_irx", "data_gld")
for (bp in bench_patterns) stock_files <- stock_files[!grepl(bp, stock_files)]

panel <- list()
t_start <- Sys.time()
for (k in seq_along(stock_files)) {
  f <- stock_files[k]
  sym <- toupper(sub("^data_", "", sub("\\.rds$", "", basename(f))))
  d <- tryCatch(readRDS(f), error = function(e) NULL)
  if (is.null(d) || nrow(d) < 300) next
  d <- tryCatch(na.locf(d, na.rm = FALSE), error = function(e) d)
  res <- tryCatch(
    suppressWarnings(capture.output(
      p <- build_feature_dataset(d, benchmark_ohlcv = spy, breadth_series = br,
                                 look_ahead = 7, use_garch = FALSE))),
    error = function(e) NULL)
  if (is.null(res) || !exists("p")) next
  fx <- p$features_xts[, p$feature_names]
  df <- data.frame(Date = as.Date(index(fx)), Symbol = sym, coredata(fx), check.names = FALSE)
  md <- p$model_data
  m  <- match(df$Date, md$Date)
  df$TargetBinary <- md$TargetBinary[m]
  df$TargetRet    <- md$TargetRet[m]
  panel[[sym]] <- df
  rm(p)
  if (k %% 10 == 0) cat(sprintf("[%d/%d] %s done (%.1f min elapsed)\n", k, length(stock_files), sym,
                                as.numeric(difftime(Sys.time(), t_start, units = "mins"))))
}
panel_df <- do.call(rbind, panel)
rownames(panel_df) <- NULL
saveRDS(list(panel = panel_df, feature_names = setdiff(colnames(panel_df), c("Date", "Symbol", "TargetBinary", "TargetRet"))),
        "scratch/wf_feature_panel.rds")
cat(sprintf("Saved panel: %d rows, %d symbols, %s to %s\n", nrow(panel_df), length(panel),
            min(panel_df$Date), max(panel_df$Date)))
