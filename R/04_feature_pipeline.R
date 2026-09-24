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
  
  # 2. Z-Score (Residual distance from fast trend standardized by residual volatility)
  residuals <- price - dual_lm$fast_lm$fit
  resid_vol <- TTR::runSD(residuals, n = fast_n)
  feat_zscore <- residuals / resid_vol
  colnames(feat_zscore) <- "ZScore"
  
  # 3. GARCH(1,1) Volatility Features
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
  
  # 4. Forward Target: Return k periods into future
  # (Price_{t+k} / Price_t - 1)
  fwd_price <- lag.xts(price, k = -look_ahead)
  target_ret <- (fwd_price - price) / price
  colnames(target_ret) <- "TargetRet"
  
  # 5. Merge all into single xts
  merged_xts <- merge(
    target_ret,
    feat_slope_fast,
    feat_slope_slow,
    feat_rsq_fast,
    feat_dist_pct,
    feat_zscore,
    feat_garch_vol,
    feat_garch_shock,
    feat_garch_pct
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
    TrendQuality = as.numeric(clean_xts$TrendQuality),
    DistPct = as.numeric(clean_xts$DistPct),
    ZScore = as.numeric(clean_xts$ZScore),
    GARCH_Vol = as.numeric(clean_xts$GARCH_Vol),
    GARCH_Shock = as.numeric(clean_xts$GARCH_Shock),
    GARCH_VolPct = as.numeric(clean_xts$GARCH_VolPct)
  )
  
  feature_names <- c("SlopeFast", "SlopeSlow", "TrendQuality", "DistPct", 
                     "ZScore", "GARCH_Vol", "GARCH_Shock", "GARCH_VolPct")
  
  cat(sprintf("[Pipeline] Complete. Result: %d valid observation rows across %d features.\n",
              nrow(df_model), length(feature_names)))
  
  return(list(
    model_data = df_model,
    dates = dates,
    price = price[dates],
    dual_lm = dual_lm,
    feature_names = feature_names
  ))
}
