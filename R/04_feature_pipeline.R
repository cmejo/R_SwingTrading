#' Feature Engineering Pipeline
#'
#' Rolling regression trend features and conditional volatility pipeline:
#'   1. Fast lmMA Slope (direction/momentum of fast trend)
#'   2. Slow lmMA Slope (direction/momentum of slow trend)
#'   3. Divergence % ((Fast Fit - Slow Fit) / Slow Fit * 100)
#'   4. Trend Quality (R^2 of fast regression)
#'   5. Z-Score (Price - Fast Fit) / rolling residual std dev
#'   6. GARCH(1,1) Volatility (Conditional annualized volatility)
#'   7. GARCH Standardized Shock (z_t = r_t / sigma_t)
#'
#' Target:
#'   Forward return k periods ahead: (Price_{t+k} - Price_t) / Price_t

suppressMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(TTR)
})

source("R/02_trend_lmMA.R")
source("R/03_volatility_garch.R")

#' Build Feature Matrix and Target Series
#'
#' @param ohlcv xts object containing OHLCV price series.
#' @param benchmark_ohlcv Optional xts object of benchmark OHLCV (e.g. SPY) for Relative Strength.
#' @param fast_n Window length for fast lmMA trend (default: 20 for swing trading, or 50).
#' @param slow_n Window length for slow lmMA trend (default: 50 for swing trading, or 200).
#' @param look_ahead Forward prediction horizon in days (default: 5 days for swing trading).
#' @param use_garch Logical; whether to include GARCH(1,1) volatility features (default: TRUE).
#' @param train_idx Optional training indices for GARCH parameter estimation without lookahead.
#' @return A list containing:
#'   - model_data: Clean data.frame of aligned features and target
#'   - dates: Date vector corresponding to each row
#'   - price: Price xts series used for modeling
#'   - feature_names: Character vector of predictor feature column names
#' @export
build_feature_dataset <- function(ohlcv, 
                                  benchmark_ohlcv = NULL,
                                  fast_n = 20, 
                                  slow_n = 50, 
                                  look_ahead = 5,
                                  use_garch = TRUE,
                                  train_idx = NULL) {
  
  price <- Cl(ohlcv)
  colnames(price) <- "Close"
  n_rows <- nrow(price)
  
  cat(sprintf("[Pipeline] Engineering features with fast_n=%d, slow_n=%d, look_ahead=%d...\n",
              fast_n, slow_n, look_ahead))
  
  # 1. Dual Linear Model Moving Averages (Fast & Slow)
  dual_lm <- calculate_dual_lmMA(price, fast_n = fast_n, slow_n = slow_n)
  
  feat_slope_fast <- dual_lm$fast_lm$slope
  feat_slope_slow <- dual_lm$slow_lm$slope
  feat_rsq_fast   <- dual_lm$fast_lm$r.squared
  feat_dist_pct   <- dual_lm$dist_pct
  
  colnames(feat_slope_fast) <- "SlopeFast"
  colnames(feat_slope_slow) <- "SlopeSlow"
  colnames(feat_rsq_fast)   <- "TrendQuality"
  colnames(feat_dist_pct)   <- "DistPct"
  
  # 2. Multi-Timeframe Weekly Trend Synergy
  feat_slope_weekly <- xts(rep(0, nrow(price)), order.by = index(price))
  colnames(feat_slope_weekly) <- "SlopeWeeklyPct"
  tryCatch({
    weekly_ohlcv <- to.weekly(ohlcv, indexAt = "endof", OHLC = TRUE)
    w_price <- Cl(weekly_ohlcv)
    if (nrow(w_price) >= 6) {
      w_n <- min(10, nrow(w_price) - 1)
      w_lm <- lmMA(w_price, n = w_n)
      w_slope_pct <- (w_lm$slope / w_price) * 100
      colnames(w_slope_pct) <- "SlopeWeeklyPct"
      merged_w <- merge(price, w_slope_pct)
      filled_w <- na.locf(merged_w$SlopeWeeklyPct, na.rm = FALSE)
      feat_slope_weekly <- filled_w[index(price)]
      colnames(feat_slope_weekly) <- "SlopeWeeklyPct"
    }
  }, error = function(e) NULL)

  # 3. Z-Score (Residual distance from fast trend standardized by residual volatility)
  residuals <- price - dual_lm$fast_lm$fit
  resid_vol <- TTR::runSD(residuals, n = fast_n)
  feat_zscore <- residuals / resid_vol
  colnames(feat_zscore) <- "ZScore"
  
  # 4. GARCH(1,1) Volatility Features
  if (use_garch) {
    garch_out <- compute_garch_volatility(price, train_idx = train_idx)
    feat_garch_vol   <- garch_out$annualized_vol
    feat_garch_shock <- garch_out$shocks
    feat_garch_pct   <- garch_out$vol_percentile
  } else {
    # Simple rolling volatility fallback
    rets <- na.omit(diff(log(price)))
    feat_garch_vol   <- TTR::runSD(rets, n = 20) * sqrt(252)
    feat_garch_shock <- rets / (feat_garch_vol / sqrt(252))
    feat_garch_pct   <- runPercentRank(feat_garch_vol, n = 50)
  }
  
  colnames(feat_garch_vol)   <- "GARCH_Vol"
  colnames(feat_garch_shock) <- "GARCH_Shock"
  colnames(feat_garch_pct)   <- "GARCH_VolPct"
  
  # 5. Volume & Institutional Accumulation Features
  vol <- tryCatch({
    Vo(ohlcv)
  }, error = function(e) {
    xts(rep(1, nrow(price)), order.by = index(price))
  })
  colnames(vol) <- "Volume"
  
  # 20-day Volume Surge Ratio
  vol_ma <- TTR::runMean(vol, n = fast_n)
  feat_vol_ratio <- vol / (vol_ma + 1e-6)
  colnames(feat_vol_ratio) <- "VolumeRatio"
  
  # On-Balance Volume (OBV) Standardized Trend Slope
  obv <- tryCatch({
    TTR::OBV(price, vol)
  }, error = function(e) {
    xts(rep(0, nrow(price)), order.by = index(price))
  })
  obv_lm <- calculate_dual_lmMA(obv, fast_n = fast_n, slow_n = slow_n)$fast_lm
  obv_sd <- TTR::runSD(obv, n = fast_n)
  feat_obv_slope <- obv_lm$slope / (obv_sd + 1e-6)
  colnames(feat_obv_slope) <- "OBV_Slope"

  # 6. Benchmark-Relative Strength (RS vs SPY/Benchmark)
  feat_rs_20 <- xts(rep(0, nrow(price)), order.by = index(price))
  colnames(feat_rs_20) <- "RS_20"
  if (!is.null(benchmark_ohlcv)) {
    tryCatch({
      bmk_price <- Cl(benchmark_ohlcv)
      merged_bmk <- merge(price, bmk_price)
      merged_bmk <- na.locf(merged_bmk, na.rm = FALSE)
      p_stock <- merged_bmk[, 1]
      p_bmk   <- merged_bmk[, 2]
      ret_stock_20 <- (p_stock / lag.xts(p_stock, k = fast_n)) - 1
      ret_bmk_20   <- (p_bmk   / lag.xts(p_bmk,   k = fast_n)) - 1
      rs_diff <- ret_stock_20 - ret_bmk_20
      feat_rs_20 <- rs_diff[index(price)]
      colnames(feat_rs_20) <- "RS_20"
    }, error = function(e) NULL)
  }

  # 7. Forward Target: Return k periods into future
  # (Price_{t+k} / Price_t - 1)
  fwd_price <- lag.xts(price, k = -look_ahead)
  target_ret <- (fwd_price - price) / price
  colnames(target_ret) <- "TargetRet"
  
  # 8. Merge all into single xts
  merged_xts <- merge(
    target_ret,
    feat_slope_fast,
    feat_slope_slow,
    feat_slope_weekly,
    feat_rsq_fast,
    feat_dist_pct,
    feat_zscore,
    feat_garch_vol,
    feat_garch_shock,
    feat_garch_pct,
    feat_vol_ratio,
    feat_obv_slope,
    feat_rs_20
  )
  
  # Clean NA created by rolling windows & lookahead
  clean_xts <- na.omit(merged_xts)
  dates <- index(clean_xts)
  
  # Convert to standard data.frame for model training
  df_model <- data.frame(
    Date = dates,
    TargetRet = as.numeric(clean_xts$TargetRet),
    TargetBinary = ifelse(as.numeric(clean_xts$TargetRet) > 0, 1, 0),
    SlopeFast = as.numeric(clean_xts$SlopeFast),
    SlopeSlow = as.numeric(clean_xts$SlopeSlow),
    SlopeWeeklyPct = as.numeric(clean_xts$SlopeWeeklyPct),
    TrendQuality = as.numeric(clean_xts$TrendQuality),
    DistPct = as.numeric(clean_xts$DistPct),
    ZScore = as.numeric(clean_xts$ZScore),
    GARCH_Vol = as.numeric(clean_xts$GARCH_Vol),
    GARCH_Shock = as.numeric(clean_xts$GARCH_Shock),
    GARCH_VolPct = as.numeric(clean_xts$GARCH_VolPct),
    VolumeRatio = as.numeric(clean_xts$VolumeRatio),
    OBV_Slope = as.numeric(clean_xts$OBV_Slope),
    RS_20 = as.numeric(clean_xts$RS_20)
  )
  
  feature_names <- c("SlopeFast", "SlopeSlow", "SlopeWeeklyPct", "TrendQuality", "DistPct", 
                     "ZScore", "GARCH_Vol", "GARCH_Shock", "GARCH_VolPct", "VolumeRatio", "OBV_Slope", "RS_20")
  
  cat(sprintf("[Pipeline] Complete. Result: %d valid observation rows across %d features.\n",
              nrow(df_model), length(feature_names)))
  
  return(list(
    model_data = df_model,
    dates = dates,
    price = price[dates],
    dual_lm = dual_lm,
    latest_weekly_slope_pct = as.numeric(tail(feat_slope_weekly, 1)),
    latest_rs_20 = as.numeric(tail(feat_rs_20, 1)),
    feature_names = feature_names
  ))
}
