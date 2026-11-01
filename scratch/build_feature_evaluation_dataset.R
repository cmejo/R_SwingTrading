#!/usr/bin/env Rscript
# ==============================================================================
# scratch/build_feature_evaluation_dataset.R
# Builds a comprehensive 10-year longitudinal feature matrix across liquid symbols
# ==============================================================================

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(TTR)
  library(jsonlite)
})

source("R/04_feature_pipeline.R")

cat("========================================================================\n")
cat(" BUILDING 10-YEAR MULTI-ASSET FEATURE EVALUATION DATASET (2016 - 2026)\n")
cat("========================================================================\n\n")

DATA_DIR <- "data/data_10yr"
OUTPUT_FILE <- "data/feature_evaluation_dataset.rds"

spy_ohlcv <- readRDS(file.path(DATA_DIR, "data_spy.rds"))
qqq_ohlcv <- readRDS(file.path(DATA_DIR, "data_qqq.rds"))

# 200-day SMA for SPY for Macro Bull/Bear indicator
spy_close <- Cl(spy_ohlcv)
spy_sma200 <- SMA(spy_close, n = 200)
spy_ret1d <- diff(log(spy_close))
spy_vol20 <- runSD(spy_ret1d, n = 20) * sqrt(252)

# Macro regime xts
macro_regimes <- merge(
  SPY_Close  = spy_close,
  SPY_SMA200 = spy_sma200,
  Macro_Bull = ifelse(spy_close > spy_sma200, 1, 0),
  SPY_Vol20  = spy_vol20,
  High_Vol   = ifelse(spy_vol20 >= 0.18, 1, 0)
)
macro_regimes <- na.locf(macro_regimes)

# Core liquid universe (diverse across Tech, Semis, Cloud, Healthcare, Consumer, Industrials, Energy)
symbols_basket <- c(
  # Mega-Cap Tech
  "AAPL", "MSFT", "GOOGL", "AMZN", "META",
  # Semiconductors
  "NVDA", "AMD", "TSM", "AVGO", "ASML", "AMAT", "QCOM", "MU", "LRCX", "ADI",
  # Software & Cyber
  "CRM", "ADBE", "PANW", "FTNT", "NOW", "CRWD", "NET", "DDOG", "TEAM",
  # FinTech & Crypto
  "COIN", "MSTR", "PYPL", "GS", "V",
  # Consumer & Cyclicals
  "TSLA", "NFLX", "DASH", "SHOP", "ORLY", "DAL",
  # Industrials & Energy
  "GE", "ETN", "CAT", "COP", "SLB", "VRT",
  # Biotech & Healthcare (excluding UNH)
  "VRTX", "ARGX", "RMD", "SNN"
)

# Filter for available files
available_syms <- c()
for (s in symbols_basket) {
  f <- file.path(DATA_DIR, sprintf("data_%s.rds", tolower(s)))
  if (file.exists(f)) available_syms <- c(available_syms, s)
}

cat(sprintf("Selected %d liquid symbols with 10-year continuous data.\n", length(available_syms)))

all_dfs <- list()

for (s in available_syms) {
  f <- file.path(DATA_DIR, sprintf("data_%s.rds", tolower(s)))
  ohlcv <- tryCatch(readRDS(f), error = function(e) NULL)
  if (is.null(ohlcv) || nrow(ohlcv) < 500) next
  
  c_idx <- intersect(index(ohlcv), index(spy_ohlcv))
  if (length(c_idx) < 500) next
  
  ohlcv_sub <- ohlcv[c_idx]
  spy_sub   <- spy_ohlcv[c_idx]
  
  cat(sprintf("  Processing %-6s (%d bars)...", s, nrow(ohlcv_sub)))
  
  t0 <- Sys.time()
  suppressWarnings({
    ds <- tryCatch(
      build_feature_dataset(ohlcv_sub, benchmark_ohlcv = spy_sub, fast_n = 20, slow_n = 50,
                            look_ahead = 7, use_garch = FALSE),
      error = function(e) NULL
    )
  })
  
  if (is.null(ds) || is.null(ds$model_data) || nrow(ds$model_data) < 100) {
    cat(" [FAILED / SKIPPED]\n")
    next
  }
  
  df <- ds$model_data
  df$Symbol <- s
  df$Date   <- as.Date(ds$dates)
  
  # Merge macro regimes
  m_idx <- match(df$Date, as.Date(index(macro_regimes)))
  valid_m <- !is.na(m_idx)
  
  df$Macro_Bull <- as.numeric(macro_regimes$Macro_Bull)[m_idx]
  df$High_Vol   <- as.numeric(macro_regimes$High_Vol)[m_idx]
  df$SPY_Vol20  <- as.numeric(macro_regimes$SPY_Vol20)[m_idx]
  
  df <- df[valid_m, ]
  
  elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)
  cat(sprintf(" %d rows in %.2fs\n", nrow(df), elapsed))
  
  all_dfs[[s]] <- df
}

master_df <- do.call(rbind, all_dfs)
rownames(master_df) <- NULL

cat(sprintf("\n[Master Dataset Assembled] %d total observations across %d symbols.\n",
            nrow(master_df), length(unique(master_df$Symbol))))
cat(sprintf("Date Range: %s to %s\n", min(master_df$Date), max(master_df$Date)))

saveRDS(master_df, OUTPUT_FILE)
cat(sprintf("Saved successfully to %s\n", OUTPUT_FILE))
