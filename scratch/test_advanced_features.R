#!/usr/bin/env Rscript
# ==============================================================================
# scratch/test_advanced_features.R
# Systematically Test Alpha & Vol-Targeting / Market Breadth on Clean Engine
# ==============================================================================

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(TTR)
  library(jsonlite)
})

source("R/08_metrics.R")
source("R/09_portfolio_backtest_clean.R")

# 1. Load Macro & Stocks
spy_raw  <- readRDS("data/data_10yr/data_spy.rds")
qqq_raw  <- readRDS("data/data_10yr/data_qqq.rds")
gld_path <- "data/data_gld.rds"
gld_raw  <- if (file.exists(gld_path)) readRDS(gld_path) else NULL
irx_path <- "data/data_historical/data_^irx.rds"
irx_raw  <- if (file.exists(irx_path)) readRDS(irx_path) else NULL
rf_daily <- if (!is.null(irx_raw)) {
  xts((as.numeric(na.locf(irx_raw[, 4], na.rm = FALSE)) / 100) / 252, order.by = index(irx_raw))
} else {
  xts(rep(0.02 / 252, nrow(spy_raw)), order.by = index(spy_raw))
}

common_dates <- sort(intersect(index(spy_raw), index(qqq_raw)))
eval_dates   <- tail(common_dates, 2520)

macro_data <- list(
  spy_close = Cl(spy_raw)[eval_dates],
  qqq_close = Cl(qqq_raw)[eval_dates],
  gld_close = if (!is.null(gld_raw)) Cl(gld_raw)[eval_dates] else NULL,
  rf_daily  = rf_daily[eval_dates]
)

SECTOR_MAP <- if (file.exists("sector_map.json")) jsonlite::fromJSON("sector_map.json") else list()
get_sym_sector <- function(s) {
  if (!is.null(SECTOR_MAP[[s]])) return(SECTOR_MAP[[s]])
  return("General_Tech")
}

cache_files <- list.files("data/cache_10yr_stage3", pattern = "_10yr_s3_wf", full.names = TRUE)
stocks_list <- list()

# Precompute Market Breadth: 50-day SMA for all stocks
stock_sma50_matrix <- matrix(FALSE, nrow = length(eval_dates), ncol = length(cache_files))
colnames(stock_sma50_matrix) <- character(length(cache_files))
rownames(stock_sma50_matrix) <- as.character(eval_dates)

stk_idx <- 0
for (cf in cache_files) {
  obj <- tryCatch(readRDS(cf), error = function(e) NULL)
  if (is.null(obj) || is.null(obj$symbol)) next
  sym <- toupper(obj$symbol)
  if (sym == "UNH") next
  
  df_f <- file.path("data/data_10yr", sprintf("data_%s.rds", tolower(sym)))
  if (!file.exists(df_f)) next
  ohlcv <- tryCatch(readRDS(df_f), error = function(e) NULL)
  if (is.null(ohlcv) || nrow(ohlcv) < 150) next
  
  c_idx <- intersect(index(ohlcv), eval_dates)
  if (length(c_idx) < 100) next
  sub_ohlcv <- ohlcv[c_idx]
  atr_obj <- tryCatch(ATR(HLC(sub_ohlcv), n = 14)$atr, error = function(e) Cl(sub_ohlcv) * 0.02)
  atr_v <- as.numeric(atr_obj)
  atr_v[is.na(atr_v) | atr_v <= 0] <- as.numeric(Cl(sub_ohlcv))[is.na(atr_v) | atr_v <= 0] * 0.02
  
  m_p <- match(c_idx, index(obj$pred_prob_s3))
  prob_v <- as.numeric(obj$pred_prob_s3)[m_p]
  prob_v[is.na(prob_v)] <- 0.50
  
  m_rs <- match(c_idx, index(obj$rs_spy))
  rs_v <- as.numeric(obj$rs_spy)[m_rs]
  rs_v[is.na(rs_v)] <- 0.0
  
  m_v <- match(c_idx, index(obj$garch_vol))
  vol_v <- as.numeric(obj$garch_vol)[m_v]
  vol_v[is.na(vol_v) | vol_v < 0.05] <- 0.25
  
  eff_v <- rep(0.30, length(c_idx))
  if (!is.null(obj$eff_ratio)) {
    m_e <- match(c_idx, index(obj$eff_ratio))
    eff_v <- as.numeric(obj$eff_ratio)[m_e]
    eff_v[is.na(eff_v)] <- 0.30
  }
  
  c_sub <- as.numeric(Cl(sub_ohlcv))
  sma20_v <- as.numeric(tryCatch(SMA(Cl(sub_ohlcv), n = 20), error = function(e) Cl(sub_ohlcv)))
  sma20_v[is.na(sma20_v)] <- c_sub[is.na(sma20_v)]
  atr_ext_v <- (c_sub - sma20_v) / (atr_v + 1e-5)
  atr_ext_v[is.na(atr_ext_v)] <- 0.0
  
  # 50-day SMA for breadth
  sma50_v <- as.numeric(tryCatch(SMA(Cl(sub_ohlcv), n = 50), error = function(e) Cl(sub_ohlcv)))
  sma50_v[is.na(sma50_v)] <- c_sub[is.na(sma50_v)]
  is_above_sma50 <- (c_sub > sma50_v)
  
  # Net Wick Demand (proxy: (Close - Low) / (High - Low + 1e-4) over 10D SMA)
  hl_range <- as.numeric(Hi(sub_ohlcv)) - as.numeric(Lo(sub_ohlcv))
  wick_loc <- (c_sub - as.numeric(Lo(sub_ohlcv))) / (hl_range + 1e-4)
  wick_loc[is.na(wick_loc)] <- 0.50
  nwick_v <- as.numeric(tryCatch(SMA(wick_loc, n = 10), error = function(e) rep(0.5, length(c_sub))))
  nwick_v[is.na(nwick_v)] <- 0.50
  # Centered around 0: >0 means closing in upper half of bar consistently
  nwick_score <- (nwick_v - 0.50) * 2.0
  
  stk_idx <- stk_idx + 1
  m_eval <- match(c_idx, eval_dates)
  valid_eval <- !is.na(m_eval)
  stock_sma50_matrix[m_eval[valid_eval], stk_idx] <- is_above_sma50[valid_eval]
  colnames(stock_sma50_matrix)[stk_idx] <- sym

  stocks_list[[sym]] <- list(
    symbol   = sym,
    sector   = get_sym_sector(sym),
    dates    = as.Date(c_idx),
    open     = as.numeric(Op(sub_ohlcv)),
    high     = as.numeric(Hi(sub_ohlcv)),
    low      = as.numeric(Lo(sub_ohlcv)),
    close    = c_sub,
    atr      = atr_v,
    prob     = prob_v,
    rs       = rs_v,
    vol      = vol_v,
    atr_ext  = atr_ext_v,
    sortino  = rep(1.0, length(c_idx)),
    eff_ratio= eff_v,
    net_wick = nwick_score
  )
}

stock_sma50_matrix <- stock_sma50_matrix[, 1:stk_idx, drop = FALSE]
# Market Breadth = fraction of universe above 50-day SMA
market_breadth <- rowMeans(stock_sma50_matrix, na.rm = TRUE)
macro_data$market_breadth <- market_breadth

cat(sprintf("[Loaded %d stocks | Market Breadth precomputed for %d sessions]\n\n",
            length(stocks_list), length(eval_dates)))

# Save Breadth and advanced metrics for engine enhancement
saveRDS(list(stocks_list = stocks_list, macro_data = macro_data, eval_dates = eval_dates),
        "scratch/clean_backtest_env.rds")
cat("[Saved environment to scratch/clean_backtest_env.rds]\n")
