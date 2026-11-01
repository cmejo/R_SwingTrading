#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(TTR)
  library(jsonlite)
})

source("R/08_metrics.R")
source("R/09_portfolio_backtest_clean.R")

cat("[Prep] Loading 10-year macro benchmarks...\n")
spy_raw <- readRDS("data/data_10yr/data_spy.rds")
qqq_raw <- readRDS("data/data_10yr/data_qqq.rds")
gld_path <- "data/data_gld.rds"
gld_raw <- if (file.exists(gld_path)) readRDS(gld_path) else NULL

# IRX daily risk-free rate
irx_path <- "data/data_historical/data_^irx.rds"
irx_raw <- if (file.exists(irx_path)) readRDS(irx_path) else NULL
rf_daily <- if (!is.null(irx_raw)) {
  xts((as.numeric(na.locf(irx_raw[, 4], na.rm = FALSE)) / 100) / 252, order.by = index(irx_raw))
} else {
  xts(rep(0.02 / 252, nrow(spy_raw)), order.by = index(spy_raw))
}

common_dates <- sort(intersect(index(spy_raw), index(qqq_raw)))
eval_dates <- tail(common_dates, 2520)

cat(sprintf("[Prep] Evaluation window: %d sessions (%s to %s)\n",
            length(eval_dates), as.character(eval_dates[1]), as.character(tail(eval_dates, 1))))

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

cat("[Prep] Loading and formatting 161 stocks from data/data_10yr & cache...\n")
cache_files <- list.files("data/cache_10yr_stage3", pattern = "_10yr_s3_wf", full.names = TRUE)
stocks_list <- list()

for (cf in cache_files) {
  obj <- tryCatch(readRDS(cf), error = function(e) NULL)
  if (is.null(obj) || is.null(obj$symbol)) next
  sym <- toupper(obj$symbol)
  if (sym == "UNH") next # UNH excluded
  
  df_f <- file.path("data/data_10yr", sprintf("data_%s.rds", tolower(sym)))
  if (!file.exists(df_f)) next
  ohlcv <- tryCatch(readRDS(df_f), error = function(e) NULL)
  if (is.null(ohlcv) || nrow(ohlcv) < 150) next
  
  # Align to eval_dates
  c_idx <- intersect(index(ohlcv), eval_dates)
  if (length(c_idx) < 100) next
  
  sub_ohlcv <- ohlcv[c_idx]
  atr_obj <- tryCatch(ATR(HLC(sub_ohlcv), n = 14)$atr, error = function(e) Cl(sub_ohlcv) * 0.02)
  atr_v <- as.numeric(atr_obj)
  atr_v[is.na(atr_v) | atr_v <= 0] <- as.numeric(Cl(sub_ohlcv))[is.na(atr_v) | atr_v <= 0] * 0.02
  
  # Extract features aligned to c_idx
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
  
  stocks_list[[sym]] <- list(
    symbol   = sym,
    sector   = get_sym_sector(sym),
    dates    = as.Date(c_idx),
    open     = as.numeric(Op(sub_ohlcv)),
    high     = as.numeric(Hi(sub_ohlcv)),
    low      = as.numeric(Lo(sub_ohlcv)),
    close    = as.numeric(Cl(sub_ohlcv)),
    atr      = atr_v,
    prob     = prob_v,
    rs       = rs_v,
    vol      = vol_v,
    atr_ext  = rep(1.0, length(c_idx)),
    sortino  = rep(1.0, length(c_idx)),
    eff_ratio= eff_v,
    net_wick = rep(0.0, length(c_idx))
  )
}

cat(sprintf("[Prep] Successfully indexed %d stocks.\n", length(stocks_list)))

# Run clean backtest with 1.0x cash
t0 <- Sys.time()
res_clean_10x <- run_clean_portfolio_backtest(
  stocks_list      = stocks_list,
  macro_data       = macro_data,
  eval_dates       = eval_dates,
  initial_capital  = 10000,
  max_positions    = 3,
  max_per_sector   = 2,
  leverage         = 1.0,
  target_vol       = 0.45,
  cost_bps         = 10,
  use_bear_hedge   = FALSE, # pure equity swing
  p_long           = 0.58
)
t1 <- Sys.time()
cat(sprintf("[Timing] Backtest took %.2f seconds.\n", as.numeric(difftime(t1, t0, units="secs"))))

cat(sprintf("\n=== CLEAN UNLEVERED (1.0x CASH) SWING RESULTS (2016 - 2026) ===\n"))
cat(sprintf("CAGR:         %+.2f%%\n", res_clean_10x$cagr))
cat(sprintf("Max Drawdown: -%.2f%%\n", res_clean_10x$max_dd))
cat(sprintf("Sharpe:       %.2f\n", res_clean_10x$sharpe))
cat(sprintf("Sortino:      %.2f\n", res_clean_10x$sortino))
cat(sprintf("Annual Vol:   %.2f%%\n", res_clean_10x$annual_vol))
cat(sprintf("Win Rate:     %.1f%%\n", res_clean_10x$win_rate))
cat(sprintf("Total Trades: %d\n", res_clean_10x$total_trades))
cat(sprintf("Terminal:     $%.2f\n", res_clean_10x$terminal_wealth))
cat(sprintf("QQQ CAGR:     %+.2f%% (Max DD: -%.2f%%, Sharpe: %.2f)\n",
            res_clean_10x$benchmark_qqq$ann_ret * 100, res_clean_10x$benchmark_qqq$max_dd * 100, res_clean_10x$benchmark_qqq$sharpe))
cat(sprintf("SPY CAGR:     %+.2f%% (Max DD: -%.2f%%, Sharpe: %.2f)\n",
            res_clean_10x$benchmark_spy$ann_ret * 100, res_clean_10x$benchmark_spy$max_dd * 100, res_clean_10x$benchmark_spy$sharpe))
