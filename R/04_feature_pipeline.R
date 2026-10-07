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
#' @param look_ahead Forward prediction horizon in days (default: 7 days for swing trading).
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
                                  sector_ohlcv = NULL,
                                  fast_n = 20, 
                                  slow_n = 50, 
                                  look_ahead = 7,
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

  # 6b. RSI(14) - Relative Strength Index (Overbought / Oversold Mean-Reversion)
  feat_rsi <- tryCatch({
    rsi_raw <- TTR::RSI(price, n = 14)
    rsi_scaled <- (rsi_raw - 50) / 50  # Center at 0, range [-1, 1]
    colnames(rsi_scaled) <- "RSI_14"
    rsi_scaled
  }, error = function(e) {
    r <- xts(rep(0, nrow(price)), order.by = index(price))
    colnames(r) <- "RSI_14"
    r
  })

  # 6c. MACD Histogram (Momentum Acceleration / Deceleration)
  feat_macd_hist <- tryCatch({
    macd_out <- TTR::MACD(price, nFast = 12, nSlow = 26, nSig = 9)
    macd_h <- macd_out[, "macd"] - macd_out[, "signal"]
    # Normalize by price to make cross-asset comparable
    macd_norm <- macd_h / (price + 1e-6) * 100
    colnames(macd_norm) <- "MACD_Hist"
    macd_norm
  }, error = function(e) {
    r <- xts(rep(0, nrow(price)), order.by = index(price))
    colnames(r) <- "MACD_Hist"
    r
  })

  # 6d. Bollinger %B (Price Position Relative to Volatility Bands)
  feat_bbpct <- tryCatch({
    bb <- TTR::BBands(price, n = 20, sd = 2)
    pctb <- (price - bb[, "dn"]) / (bb[, "up"] - bb[, "dn"] + 1e-6)
    colnames(pctb) <- "BB_PctB"
    pctb
  }, error = function(e) {
    r <- xts(rep(0.5, nrow(price)), order.by = index(price))
    colnames(r) <- "BB_PctB"
    r
  })

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

  # 6e. ATR-Normalized Distance to Trend (Mean-Reversion & Overextension Guard)
  feat_atr_ext <- tryCatch({
    atr_14 <- TTR::ATR(HLC(ohlcv), n = 14)$atr
    ext <- (price - dual_lm$fast_lm$fit) / (atr_14 + 1e-6)
    colnames(ext) <- "ATRExt"
    ext
  }, error = function(e) {
    sd_p <- TTR::runSD(price, n = fast_n)
    ext <- (price - dual_lm$fast_lm$fit) / (sd_p + 1e-6)
    colnames(ext) <- "ATRExt"
    ext
  })

  # 6f. Sector Relative Strength Momentum Spread (Sector ETF vs SPY Benchmark)
  feat_sector_spread <- xts(rep(0, nrow(price)), order.by = index(price))
  colnames(feat_sector_spread) <- "SectorSpread"
  if (!is.null(sector_ohlcv) && !is.null(benchmark_ohlcv)) {
    tryCatch({
      sec_p <- Cl(sector_ohlcv)
      bmk_p <- Cl(benchmark_ohlcv)
      merged_sec <- merge(price, sec_p, bmk_p)
      merged_sec <- na.locf(merged_sec, na.rm = FALSE)
      s_ret20 <- (merged_sec[, 2] / lag.xts(merged_sec[, 2], k = fast_n)) - 1
      b_ret20 <- (merged_sec[, 3] / lag.xts(merged_sec[, 3], k = fast_n)) - 1
      spread <- s_ret20 - b_ret20
      feat_sector_spread <- spread[index(price)]
      colnames(feat_sector_spread) <- "SectorSpread"
    }, error = function(e) NULL)
  }

  # 6g. Asymmetric Downside Volatility Ratio (Upside Semi-variance vs Downside Semi-variance)
  feat_asym_vol <- tryCatch({
    log_rets <- diff(log(price))
    log_rets[1] <- 0
    pos_rets2 <- pmax(log_rets, 0)^2
    neg_rets2 <- pmin(log_rets, 0)^2
    pos_var <- TTR::runMean(pos_rets2, n = fast_n)
    neg_var <- TTR::runMean(neg_rets2, n = fast_n)
    ratio <- sqrt(pos_var) / (sqrt(neg_var) + 1e-5)
    log_ratio <- log(pmax(ratio, 1e-3))
    clamped_ratio <- pmin(pmax(log_ratio, -3.0), 3.0)
    colnames(clamped_ratio) <- "AsymVolRatio"
    clamped_ratio
  }, error = function(e) {
    r <- xts(rep(0, nrow(price)), order.by = index(price))
    colnames(r) <- "AsymVolRatio"
    r
  })

  # 7. Forward Target: Return k periods into future
  # (Price_{t+k} / Price_t - 1)
  fwd_price <- lag.xts(price, k = -look_ahead)
  target_ret <- (fwd_price - price) / price
  colnames(target_ret) <- "TargetRet"
  
  # 8. Merge all 18 predictor features
  features_xts <- merge(
    SlopeFast      = feat_slope_fast,
    SlopeSlow      = feat_slope_slow,
    SlopeWeeklyPct = feat_slope_weekly,
    TrendQuality   = feat_rsq_fast,
    DistPct        = feat_dist_pct,
    ZScore         = feat_zscore,
    GARCH_Vol      = feat_garch_vol,
    GARCH_Shock    = feat_garch_shock,
    GARCH_VolPct   = feat_garch_pct,
    VolumeRatio    = feat_vol_ratio,
    OBV_Slope      = feat_obv_slope,
    RSI_14         = feat_rsi,
    MACD_Hist      = feat_macd_hist,
    BB_PctB        = feat_bbpct,
    RS_20          = feat_rs_20,
    ATRExt         = feat_atr_ext,
    SectorSpread   = feat_sector_spread,
    AsymVolRatio   = feat_asym_vol
  )
  
  feature_names <- c("SlopeFast", "SlopeSlow", "SlopeWeeklyPct", "TrendQuality", "DistPct", 
                     "ZScore", "GARCH_Vol", "GARCH_Shock", "GARCH_VolPct", "VolumeRatio", "OBV_Slope",
                     "RSI_14", "MACD_Hist", "BB_PctB", "RS_20", "ATRExt", "SectorSpread", "AsymVolRatio")
  
  # Clean warm-up NAs across features
  clean_feats_xts <- na.omit(features_xts)
  
  # Latest available feature observation (unlabeled, up to latest price bar)
  latest_feat_row <- tail(clean_feats_xts, 1)
  latest_feature_matrix <- matrix(as.numeric(latest_feat_row), nrow = 1)
  colnames(latest_feature_matrix) <- feature_names
  
  # Exact column indexing (prevent zoo partial name matching bug)
  latest_ann_vol <- as.numeric(latest_feat_row[, "GARCH_Vol"])
  if (is.na(latest_ann_vol) || latest_ann_vol < 0.05 || latest_ann_vol > 3.0) {
    # Sanity fallback to 20-day realized volatility if GARCH estimate is outside reasonable bounds
    realized_vol <- as.numeric(tail(na.omit(TTR::runSD(diff(log(price)), n = 20) * sqrt(252)), 1))
    latest_ann_vol <- if (!is.na(realized_vol) && realized_vol > 0.05) realized_vol else 0.25
  }
  
  latest_vol_pct <- as.numeric(latest_feat_row[, "GARCH_VolPct"])
  
  # 9. Supervised learning target: align with forward returns
  merged_with_target <- merge(target_ret, clean_feats_xts)
  clean_model_xts <- na.omit(merged_with_target)
  dates <- index(clean_model_xts)
  
  # Convert to standard data.frame for model training
  df_model <- data.frame(
    Date           = dates,
    TargetRet      = as.numeric(clean_model_xts[, "TargetRet"]),
    TargetBinary   = ifelse(as.numeric(clean_model_xts[, "TargetRet"]) > 0, 1, 0),
    SlopeFast      = as.numeric(clean_model_xts[, "SlopeFast"]),
    SlopeSlow      = as.numeric(clean_model_xts[, "SlopeSlow"]),
    SlopeWeeklyPct = as.numeric(clean_model_xts[, "SlopeWeeklyPct"]),
    TrendQuality   = as.numeric(clean_model_xts[, "TrendQuality"]),
    DistPct        = as.numeric(clean_model_xts[, "DistPct"]),
    ZScore         = as.numeric(clean_model_xts[, "ZScore"]),
    GARCH_Vol      = as.numeric(clean_model_xts[, "GARCH_Vol"]),
    GARCH_Shock    = as.numeric(clean_model_xts[, "GARCH_Shock"]),
    GARCH_VolPct   = as.numeric(clean_model_xts[, "GARCH_VolPct"]),
    VolumeRatio    = as.numeric(clean_model_xts[, "VolumeRatio"]),
    OBV_Slope      = as.numeric(clean_model_xts[, "OBV_Slope"]),
    RSI_14         = as.numeric(clean_model_xts[, "RSI_14"]),
    MACD_Hist      = as.numeric(clean_model_xts[, "MACD_Hist"]),
    BB_PctB        = as.numeric(clean_model_xts[, "BB_PctB"]),
    RS_20          = as.numeric(clean_model_xts[, "RS_20"]),
    ATRExt         = as.numeric(clean_model_xts[, "ATRExt"]),
    SectorSpread   = as.numeric(clean_model_xts[, "SectorSpread"]),
    AsymVolRatio   = as.numeric(clean_model_xts[, "AsymVolRatio"])
  )
  
  cat(sprintf("[Pipeline] Complete. Result: %d valid observation rows across %d features.\n",
              nrow(df_model), length(feature_names)))
  
  return(list(
    model_data              = df_model,
    dates                   = dates,
    price                   = price[dates],
    dual_lm                 = dual_lm,
    latest_feature_matrix   = latest_feature_matrix,
    latest_ann_vol          = latest_ann_vol,
    latest_vol_pct          = latest_vol_pct,
    latest_weekly_slope_pct = as.numeric(tail(feat_slope_weekly, 1)),
    latest_rs_20            = as.numeric(tail(feat_rs_20, 1)),
    latest_atr_ext          = as.numeric(tail(feat_atr_ext, 1)),
    latest_sector_spread    = as.numeric(tail(feat_sector_spread, 1)),
    latest_asym_vol         = as.numeric(tail(feat_asym_vol, 1)),
    feature_names           = feature_names,
    features_xts            = clean_feats_xts,
    garch_out               = if (use_garch) garch_out else NULL
  ))
}
