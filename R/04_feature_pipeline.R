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
  
  # 1. Dual Linear Model Moving Averages (Fast & Slow) — Price-Normalized (%/day)
  dual_lm <- calculate_dual_lmMA(price, fast_n = fast_n, slow_n = slow_n)
  
  feat_slope_fast <- (dual_lm$fast_lm$slope / (price + 1e-6)) * 100
  feat_slope_slow <- (dual_lm$slow_lm$slope / (price + 1e-6)) * 100
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
  
  # 4. Two-Regime Markov-Switching GARCH (MS-GARCH) Volatility Features
  garch_out <- NULL
  if (use_garch) {
    garch_out <- compute_msgarch_volatility(price, train_idx = train_idx)
    feat_garch_vol      <- garch_out$annualized_vol
    feat_garch_shock    <- garch_out$shocks
    feat_garch_pct      <- garch_out$vol_percentile
    feat_prob_high_vol  <- garch_out$prob_high_vol
  } else {
    # Simple rolling volatility fallback
    rets <- na.omit(diff(log(price)))
    feat_garch_vol      <- TTR::runSD(rets, n = 20) * sqrt(252)
    feat_garch_shock    <- rets / (feat_garch_vol / sqrt(252))
    feat_garch_pct      <- runPercentRank(feat_garch_vol, n = 50)
    feat_prob_high_vol  <- xts(rep(0.5, length(feat_garch_vol)), order.by = index(feat_garch_vol))
  }
  
  colnames(feat_garch_vol)     <- "GARCH_Vol"
  colnames(feat_garch_shock)   <- "GARCH_Shock"
  colnames(feat_garch_pct)     <- "GARCH_VolPct"
  colnames(feat_prob_high_vol) <- "MSGARCH_ProbHighVol"
  
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

  # 6h. Volatility Contraction / TTM Squeeze Ratio (BB Width / Keltner Width) & Squeeze Release
  feat_vol_squeeze <- xts(rep(1.0, nrow(price)), order.by = index(price))
  feat_squeeze_rel <- xts(rep(0.0, nrow(price)), order.by = index(price))
  tryCatch({
    bb_sq  <- TTR::BBands(price, n = 20, sd = 2)
    atr_sq <- TTR::ATR(HLC(ohlcv), n = 14)$atr
    sq_vec <- as.numeric((bb_sq[, "up"] - bb_sq[, "dn"]) / (4.0 * atr_sq + 1e-6))
    sq_vec[is.na(sq_vec)] <- 1.0
    sq_vec <- pmin(pmax(sq_vec, 0.2), 3.0)
    feat_vol_squeeze <- xts(sq_vec, order.by = index(price))

    sq_lag <- c(1.0, head(sq_vec, -1))
    sf_vec <- as.numeric(feat_slope_fast)
    sf_vec[is.na(sf_vec)] <- 0
    sq_rel_vec <- pmax(0, 1.25 - sq_lag) * pmax(0, sf_vec)
    feat_squeeze_rel <- xts(sq_rel_vec, order.by = index(price))
  }, error = function(e) NULL)
  colnames(feat_vol_squeeze) <- "VolSqueeze"
  colnames(feat_squeeze_rel) <- "SqueezeRelease"

  # 6i. Intraday Institutional Accumulation: 5D Closing Location Value (CLV_5D) & Intraday vs Overnight Drift
  feat_clv_5d <- xts(rep(0.0, nrow(price)), order.by = index(price))
  feat_intra_vs_gap <- xts(rep(0.0, nrow(price)), order.by = index(price))
  tryCatch({
    op_v <- as.numeric(Op(ohlcv))
    hi_v <- as.numeric(Hi(ohlcv))
    lo_v <- as.numeric(Lo(ohlcv))
    cl_v <- as.numeric(Cl(ohlcv))
    clv_raw <- ((cl_v - lo_v) - (hi_v - cl_v)) / (hi_v - lo_v + 1e-6)
    clv_5 <- as.numeric(TTR::runMean(clv_raw, n = 5))
    clv_5[is.na(clv_5)] <- 0
    feat_clv_5d <- xts(clv_5, order.by = index(price))

    prev_cl <- c(cl_v[1], head(cl_v, -1))
    intra_r <- (cl_v / (op_v + 1e-6)) - 1
    gap_r   <- (op_v / (prev_cl + 1e-6)) - 1
    gap_r[is.na(gap_r)] <- 0
    diff_ig <- (intra_r - gap_r) * 100
    ig_10 <- as.numeric(TTR::runMean(diff_ig, n = 10))
    ig_10[is.na(ig_10)] <- 0
    feat_intra_vs_gap <- xts(ig_10, order.by = index(price))
  }, error = function(e) NULL)
  colnames(feat_clv_5d) <- "CLV_5D"
  colnames(feat_intra_vs_gap) <- "IntradayVsGap_10D"

  # 6j. Post-Earnings / High-Volume Gap Catalyst (PEAD_Catalyst)
  feat_pead <- xts(rep(0.0, nrow(price)), order.by = index(price))
  tryCatch({
    op_v <- as.numeric(Op(ohlcv))
    lo_v <- as.numeric(Lo(ohlcv))
    cl_v <- as.numeric(Cl(ohlcv))
    vr_v <- as.numeric(feat_vol_ratio)
    atr_v <- as.numeric(TTR::ATR(HLC(ohlcv), n = 14)$atr)
    atr_v[is.na(atr_v) | atr_v <= 0] <- cl_v[is.na(atr_v) | atr_v <= 0] * 0.02

    pead_vec <- numeric(n_rows)
    last_cat_idx <- -999L
    last_cat_low <- 0.0
    last_cat_mag <- 0.0
    for (ii in 2:n_rows) {
      gap_up <- op_v[ii] - cl_v[ii - 1]
      is_cat <- (!is.na(gap_up) && !is.na(vr_v[ii]) &&
                 gap_up >= 1.2 * atr_v[ii - 1] &&
                 vr_v[ii] >= 1.75 &&
                 cl_v[ii] >= op_v[ii])
      if (is_cat) {
        last_cat_idx <- ii
        last_cat_low <- lo_v[ii]
        last_cat_mag <- min(3.0, (gap_up / (atr_v[ii - 1] + 1e-6)) * (vr_v[ii] / 2.0))
      }
      age <- ii - last_cat_idx
      if (age >= 0 && age <= 5 && !is.na(cl_v[ii]) && cl_v[ii] >= last_cat_low) {
        pead_vec[ii] <- last_cat_mag * (1.0 - age / 6.0)
      }
    }
    feat_pead <- xts(pead_vec, order.by = index(price))
  }, error = function(e) NULL)
  colnames(feat_pead) <- "PEAD_Catalyst"

  # 6k. Explicit Non-Linear Regime Interactions (PullbackInUptrend & BreakoutWithVolume)
  feat_pullback_up <- xts(rep(0.0, nrow(price)), order.by = index(price))
  feat_breakout_vol <- xts(rep(0.0, nrow(price)), order.by = index(price))
  tryCatch({
    cl_v <- as.numeric(price)
    slow_fit_v <- as.numeric(dual_lm$slow_lm$fit)
    ss_v <- as.numeric(feat_slope_slow)
    zs_v <- as.numeric(feat_zscore)
    above_slow <- ifelse(!is.na(cl_v) & !is.na(slow_fit_v) & cl_v >= slow_fit_v, 1.0, 0.0)
    pb_up <- pmax(0, ifelse(is.na(ss_v), 0, ss_v)) * pmax(0, ifelse(is.na(zs_v), 0, -zs_v)) * above_slow
    feat_pullback_up <- xts(pb_up, order.by = index(price))

    sf_v <- as.numeric(feat_slope_fast)
    vr_v <- as.numeric(feat_vol_ratio)
    rs_v <- as.numeric(feat_rs_20)
    bo_vol <- pmax(0, ifelse(is.na(sf_v), 0, sf_v)) *
              pmax(0, ifelse(is.na(vr_v), 0, vr_v - 1.0)) *
              pmax(0, ifelse(is.na(rs_v), 0, rs_v * 100))
    bo_vol <- pmin(bo_vol, 10.0)
    feat_breakout_vol <- xts(bo_vol, order.by = index(price))
  }, error = function(e) NULL)
  colnames(feat_pullback_up) <- "PullbackInUptrend"
  colnames(feat_breakout_vol) <- "BreakoutWithVolume"

  # 7. Forward Target & Path-Dependent Triple-Barrier Label (Look-Ahead k Days)
  # Upper Profit Barrier: +1.5R (+3.0 * sigma_daily)
  # Lower Stop Barrier:   -1.0R (-2.0 * sigma_daily)
  # Vertical Time Barrier: k = look_ahead trading days
  fwd_price <- lag.xts(price, k = -look_ahead)
  target_ret <- (fwd_price - price) / price
  colnames(target_ret) <- "TargetRet"
  
  hi_vec <- tryCatch(as.numeric(Hi(ohlcv)), error = function(e) as.numeric(price))
  lo_vec <- tryCatch(as.numeric(Lo(ohlcv)), error = function(e) as.numeric(price))
  cl_vec <- as.numeric(price)
  
  # Align daily conditional volatility to price index
  vol_aligned <- merge(price, feat_garch_vol)[, "GARCH_Vol"]
  vol_vec <- as.numeric(vol_aligned) / sqrt(252)
  vol_vec[is.na(vol_vec) | vol_vec < 0.005 | vol_vec > 0.15] <- 0.02
  
  tb_labels <- rep(NA_real_, n_rows)
  if (n_rows > look_ahead) {
    for (t_idx in 1:(n_rows - look_ahead)) {
      p0  <- cl_vec[t_idx]
      sig <- vol_vec[t_idx]
      if (is.na(p0) || p0 <= 0) next
      
      upper_bar <- p0 * (1.0 + 3.0 * sig) # +1.5R Tier 1 Target
      lower_bar <- p0 * (1.0 - 2.0 * sig) # -1.0R Stop-Loss
      
      win_slice <- (t_idx + 1):(t_idx + look_ahead)
      hit_up <- which(hi_vec[win_slice] >= upper_bar)[1]
      hit_dn <- which(cl_vec[win_slice] <= lower_bar)[1]
      
      if (!is.na(hit_up) && (is.na(hit_dn) || hit_up < hit_dn)) {
        tb_labels[t_idx] <- 1 # Hit +1.5R target before stop
      } else if (!is.na(hit_dn)) {
        tb_labels[t_idx] <- 0 # Stopped out at -1.0R before target
      } else {
        # Neither barrier breached: evaluate vertical time barrier at t + look_ahead
        tb_labels[t_idx] <- ifelse(cl_vec[t_idx + look_ahead] > p0, 1, 0)
      }
    }
  }
  target_tb <- xts(tb_labels, order.by = index(price))
  colnames(target_tb) <- "TargetBinary"
  
  # 8. Merge all 26 predictor features (including MS-GARCH & High-Alpha Microstructure/Squeeze Features)
  features_xts <- merge(
    SlopeFast          = feat_slope_fast,
    SlopeSlow          = feat_slope_slow,
    SlopeWeeklyPct     = feat_slope_weekly,
    TrendQuality       = feat_rsq_fast,
    DistPct            = feat_dist_pct,
    ZScore             = feat_zscore,
    GARCH_Vol          = feat_garch_vol,
    GARCH_Shock        = feat_garch_shock,
    GARCH_VolPct       = feat_garch_pct,
    MSGARCH_ProbHighVol= feat_prob_high_vol,
    VolumeRatio        = feat_vol_ratio,
    OBV_Slope          = feat_obv_slope,
    RSI_14             = feat_rsi,
    MACD_Hist          = feat_macd_hist,
    BB_PctB            = feat_bbpct,
    RS_20              = feat_rs_20,
    ATRExt             = feat_atr_ext,
    SectorSpread       = feat_sector_spread,
    AsymVolRatio       = feat_asym_vol,
    VolSqueeze         = feat_vol_squeeze,
    SqueezeRelease     = feat_squeeze_rel,
    CLV_5D             = feat_clv_5d,
    IntradayVsGap_10D  = feat_intra_vs_gap,
    PEAD_Catalyst      = feat_pead,
    PullbackInUptrend  = feat_pullback_up,
    BreakoutWithVolume = feat_breakout_vol
  )
  
  # Orthogonalized 22-Feature Predictor Set for ElasticNet:
  # Prunes the 4 near-duplicate / zero-selection collinear features:
  #   - SlopeWeeklyPct (r = 0.986 with SlopeSlow; retained as top-down gate in latest_weekly_slope_pct)
  #   - ATRExt         (r = 0.953 with ZScore; retained as overextension gate in latest_atr_ext)
  #   - BB_PctB        (r = 0.891 with RSI_14; 0.8% selection rate)
  #   - GARCH_Shock    (r = 0.600 with ZScore; 0.0% selection rate)
  feature_names <- c("SlopeFast", "SlopeSlow", "TrendQuality", "DistPct", 
                     "ZScore", "GARCH_Vol", "GARCH_VolPct", "MSGARCH_ProbHighVol",
                     "VolumeRatio", "OBV_Slope", "RSI_14", "MACD_Hist", "RS_20", 
                     "SectorSpread", "AsymVolRatio",
                     "VolSqueeze", "SqueezeRelease", "CLV_5D", "IntradayVsGap_10D",
                     "PEAD_Catalyst", "PullbackInUptrend", "BreakoutWithVolume")
  
  # Clean warm-up NAs across features
  clean_feats_xts <- na.omit(features_xts)
  
  # Latest available feature observation (unlabeled, up to latest price bar)
  latest_feat_row <- tail(clean_feats_xts, 1)
  latest_feature_matrix <- matrix(as.numeric(latest_feat_row[, feature_names]), nrow = 1)
  colnames(latest_feature_matrix) <- feature_names
  
  # Exact column indexing (prevent zoo partial name matching bug)
  latest_ann_vol <- as.numeric(latest_feat_row[, "GARCH_Vol"])
  if (is.na(latest_ann_vol) || latest_ann_vol < 0.05 || latest_ann_vol > 3.0) {
    # Sanity fallback to 20-day realized volatility if GARCH estimate is outside reasonable bounds
    realized_vol <- as.numeric(tail(na.omit(TTR::runSD(diff(log(price)), n = 20) * sqrt(252)), 1))
    latest_ann_vol <- if (!is.na(realized_vol) && realized_vol > 0.05) realized_vol else 0.25
  }
  
  latest_vol_pct <- as.numeric(latest_feat_row[, "GARCH_VolPct"])
  latest_prob_high_vol <- as.numeric(latest_feat_row[, "MSGARCH_ProbHighVol"])
  if (is.na(latest_prob_high_vol)) latest_prob_high_vol <- 0.5
  
  # 9. Supervised learning target: align with forward returns & triple-barrier label
  merged_with_target <- merge(target_ret, target_tb, clean_feats_xts)
  clean_model_xts <- na.omit(merged_with_target)
  dates <- index(clean_model_xts)
  
  # Convert to standard data.frame for model training
  df_model <- data.frame(
    Date               = dates,
    TargetRet          = as.numeric(clean_model_xts[, "TargetRet"]),
    TargetBinary       = as.numeric(clean_model_xts[, "TargetBinary"]),
    SlopeFast          = as.numeric(clean_model_xts[, "SlopeFast"]),
    SlopeSlow          = as.numeric(clean_model_xts[, "SlopeSlow"]),
    SlopeWeeklyPct     = as.numeric(clean_model_xts[, "SlopeWeeklyPct"]),
    TrendQuality       = as.numeric(clean_model_xts[, "TrendQuality"]),
    DistPct            = as.numeric(clean_model_xts[, "DistPct"]),
    ZScore             = as.numeric(clean_model_xts[, "ZScore"]),
    GARCH_Vol          = as.numeric(clean_model_xts[, "GARCH_Vol"]),
    GARCH_Shock        = as.numeric(clean_model_xts[, "GARCH_Shock"]),
    GARCH_VolPct       = as.numeric(clean_model_xts[, "GARCH_VolPct"]),
    MSGARCH_ProbHighVol= as.numeric(clean_model_xts[, "MSGARCH_ProbHighVol"]),
    VolumeRatio        = as.numeric(clean_model_xts[, "VolumeRatio"]),
    OBV_Slope          = as.numeric(clean_model_xts[, "OBV_Slope"]),
    RSI_14             = as.numeric(clean_model_xts[, "RSI_14"]),
    MACD_Hist          = as.numeric(clean_model_xts[, "MACD_Hist"]),
    BB_PctB            = as.numeric(clean_model_xts[, "BB_PctB"]),
    RS_20              = as.numeric(clean_model_xts[, "RS_20"]),
    ATRExt             = as.numeric(clean_model_xts[, "ATRExt"]),
    SectorSpread       = as.numeric(clean_model_xts[, "SectorSpread"]),
    AsymVolRatio       = as.numeric(clean_model_xts[, "AsymVolRatio"]),
    VolSqueeze         = as.numeric(clean_model_xts[, "VolSqueeze"]),
    SqueezeRelease     = as.numeric(clean_model_xts[, "SqueezeRelease"]),
    CLV_5D             = as.numeric(clean_model_xts[, "CLV_5D"]),
    IntradayVsGap_10D  = as.numeric(clean_model_xts[, "IntradayVsGap_10D"]),
    PEAD_Catalyst      = as.numeric(clean_model_xts[, "PEAD_Catalyst"]),
    PullbackInUptrend  = as.numeric(clean_model_xts[, "PullbackInUptrend"]),
    BreakoutWithVolume = as.numeric(clean_model_xts[, "BreakoutWithVolume"])
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
    latest_prob_high_vol    = latest_prob_high_vol,
    regime_params           = if (!is.null(garch_out)) garch_out$regime_params else NULL,
    high_regime_idx         = if (!is.null(garch_out)) garch_out$high_regime_idx else NULL,
    low_regime_idx          = if (!is.null(garch_out)) garch_out$low_regime_idx else NULL,
    latest_weekly_slope_pct = as.numeric(tail(feat_slope_weekly, 1)),
    latest_trend_quality    = as.numeric(tail(feat_rsq_fast, 1)),
    latest_rs_20            = as.numeric(tail(feat_rs_20, 1)),
    latest_atr_ext          = as.numeric(tail(feat_atr_ext, 1)),
    latest_sector_spread    = as.numeric(tail(feat_sector_spread, 1)),
    latest_asym_vol         = as.numeric(tail(feat_asym_vol, 1)),
    feature_names           = feature_names,
    features_xts            = clean_feats_xts,
    garch_out               = if (use_garch) garch_out else NULL
  ))
}
