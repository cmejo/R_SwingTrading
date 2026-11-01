#!/usr/bin/env Rscript
# ==============================================================================
# scratch/test_feature_improvements.R
# Systematically Test Alpha & Drawdown-Reducing Features on Clean Engine
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
    net_wick = rep(0.0, length(c_idx))
  )
}

cat(sprintf("[Loaded %d stocks for feature experiment testing]\n\n", length(stocks_list)))

# Feature Experiment Configurations
exp_configs <- list(
  list(name = "1. Baseline Clean (3 Slots, 1.0x Cash, Unhedged)",      mp = 3, ms = 2, lev = 1.0, hedge = FALSE, sm = "sortino_er", dd = FALSE),
  list(name = "2. Diversified (5 Slots, 1.0x Cash, Unhedged)",         mp = 5, ms = 2, lev = 1.0, hedge = FALSE, sm = "sortino_er", dd = FALSE),
  list(name = "3. Diversified (6 Slots, 1.0x Cash, Unhedged)",         mp = 6, ms = 2, lev = 1.0, hedge = FALSE, sm = "sortino_er", dd = FALSE),
  list(name = "4. Diversified (5 Slots + Smart Dual-Bear, 1.0x Cash)", mp = 5, ms = 2, lev = 1.0, hedge = TRUE,  sm = "sortino_er", dd = FALSE),
  list(name = "5. Diversified (5 Slots + Smart Dual-Bear, 1.25x Lev)", mp = 5, ms = 2, lev = 1.25, hedge = TRUE, sm = "sortino_er", dd = FALSE),
  list(name = "6. Diversified (5 Slots + Dual-Bear + DD Cushion 1.0x)", mp = 5, ms = 2, lev = 1.0, hedge = TRUE,  sm = "sortino_er", dd = TRUE),
  list(name = "7. Diversified (5 Slots + Dual-Bear + DD Cushion 1.25x)", mp = 5, ms = 2, lev = 1.25, hedge = TRUE, sm = "sortino_er", dd = TRUE)
)

exp_rows <- list()

for (cfg in exp_configs) {
  res <- run_clean_portfolio_backtest(
    stocks_list       = stocks_list,
    macro_data        = macro_data,
    eval_dates        = eval_dates,
    initial_capital   = 10000,
    max_positions     = cfg$mp,
    max_per_sector    = cfg$ms,
    leverage          = cfg$lev,
    target_vol        = 0.35, # Moderate target volatility
    cost_bps          = 10,
    use_bear_hedge    = cfg$hedge,
    hedge_stop_loss   = -0.10,
    hedge_time_stop   = 15,
    sizing_mode       = cfg$sm,
    drawdown_throttle = cfg$dd,
    p_long            = 0.58
  )
  
  exp_rows[[length(exp_rows) + 1]] <- data.frame(
    Feature_Config  = cfg$name,
    Terminal_Wealth = sprintf("$%s", format(round(res$terminal_wealth, 2), big.mark = ",", scientific = FALSE)),
    CAGR            = sprintf("%+.2f%%", res$cagr),
    Max_Drawdown    = sprintf("-%.2f%%", res$max_dd),
    Sharpe_Ratio    = sprintf("%.2f", res$sharpe),
    Sortino_Ratio   = sprintf("%.2f", res$sortino),
    Calmar_Ratio    = if (!is.na(res$calmar)) sprintf("%.2f", res$calmar) else "N/A",
    Annual_Vol      = sprintf("%.2f%%", res$annual_vol),
    Win_Rate        = sprintf("%.1f%%", res$win_rate),
    Total_Trades    = res$total_trades,
    Margin_Paid     = sprintf("$%.2f", res$total_margin_paid),
    stringsAsFactors = FALSE
  )
}

exp_df <- do.call(rbind, exp_rows)

cat("\n========================================================================================\n")
cat("          FEATURE EXPERIMENT SCORECARD: DIVERSIFICATION, LEVERAGE & RISK CONTROLS        \n")
cat("========================================================================================\n")
print(exp_df, row.names = FALSE)
cat("========================================================================================\n\n")

write.csv(exp_df, "output/feature_experiment_scorecard.csv", row.names = FALSE)
cat("[Done] Results saved to output/feature_experiment_scorecard.csv\n")
