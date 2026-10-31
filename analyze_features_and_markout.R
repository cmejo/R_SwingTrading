#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(TTR)
  library(glmnet)
  library(jsonlite)
})

source("R/01_data_loader.R")
source("R/02_trend_lmMA.R")
source("R/03_volatility_garch.R")
source("R/04_feature_pipeline.R")
source("R/05_logistic_model.R")

cat("================================================================================\n")
cat(" 1. ROLLING ELASTICNET BETA FEATURE IMPORTANCE OVER TIME & COLLINEARITY AUDIT\n")
cat("================================================================================\n")

# Load SPY & QQQ benchmarks
spy_ohlcv <- if (file.exists("data/data_spy.rds")) readRDS("data/data_spy.rds") else readRDS("data/data_qqq.rds")
qqq_ohlcv <- if (file.exists("data/data_qqq.rds")) readRDS("data/data_qqq.rds") else spy_ohlcv

# Representative liquid cross-section across sectors with long history in data/ or data/data_10yr/
rep_syms <- c("AAPL", "MSFT", "NVDA", "AMD", "AMZN", "GOOGL", "META", "TSLA",
              "AVGO", "MU", "AMAT", "LRCX", "KLAC", "CRM", "ADBE", "PANW",
              "NFLX", "JPM", "GS", "UNH", "LLY", "XOM", "CAT", "COST",
              "HD", "V", "MA", "ISRG", "NOW", "INTU")

load_sym_ohlcv <- function(sym) {
  f_10 <- file.path("data/data_10yr", sprintf("data_%s.rds", tolower(sym)))
  f_std <- file.path("data", sprintf("data_%s.rds", tolower(sym)))
  if (file.exists(f_10)) return(readRDS(f_10))
  if (file.exists(f_std)) return(readRDS(f_std))
  return(NULL)
}

feature_names <- c("SlopeFast", "SlopeSlow", "SlopeWeeklyPct", "TrendQuality", "DistPct", 
                   "ZScore", "GARCH_Vol", "GARCH_Shock", "GARCH_VolPct", "MSGARCH_ProbHighVol",
                   "VolumeRatio", "OBV_Slope", "RSI_14", "MACD_Hist", "BB_PctB", "RS_20", 
                   "ATRExt", "SectorSpread", "AsymVolRatio",
                   "VolSqueeze", "SqueezeRelease", "CLV_5D", "IntradayVsGap_10D",
                   "PEAD_Catalyst", "PullbackInUptrend", "BreakoutWithVolume")

# Define evaluation regime windows (500-bar rolling training windows ending at key dates)
epoch_ends <- list(
  "2018-2019 (Late-Cycle / Vol Shock)"   = as.Date("2019-12-31"),
  "2020-2021 (COVID & Liquidity Boom)"   = as.Date("2021-11-30"),
  "2022 (Inflation & Bear Market)"       = as.Date("2022-12-30"),
  "2023-2024 (AI & Momentum Expansion)"  = as.Date("2024-06-30"),
  "2025-2026 (Current Live Regime)"      = as.Date("2026-10-01")
)

all_feature_dfs <- list()
cat("Building 26-feature matrices across representative stocks...\n")
for (sym in rep_syms) {
  ohlcv <- load_sym_ohlcv(sym)
  if (is.null(ohlcv) || nrow(ohlcv) < 600) next
  # Use fast rolling vol fallback in bulk diagnostic for speed while preserving exact feature definitions
  sink("/dev/null")
  res <- tryCatch(
    build_feature_dataset(ohlcv, benchmark_ohlcv = spy_ohlcv, sector_ohlcv = qqq_ohlcv,
                          fast_n = 20, slow_n = 50, look_ahead = 7, use_garch = FALSE),
    error = function(e) NULL
  )
  sink()
  if (!is.null(res) && nrow(res$model_data) >= 500) {
    df_m <- res$model_data
    df_m$Symbol <- sym
    all_feature_dfs[[sym]] <- df_m
  }
}
cat(sprintf("Successfully built feature datasets for %d symbols.\n", length(all_feature_dfs)))

# 1A. Compute Feature Collinearity & Univariate IC (Information Coefficient with 7D TargetRet & TargetBinary)
pooled_df <- do.call(rbind, all_feature_dfs)
X_pooled  <- as.matrix(pooled_df[, feature_names])
cor_mat   <- cor(X_pooled, use = "pairwise.complete.obs")

# Find top collinear feature pairs (|r| >= 0.65)
high_cor_pairs <- data.frame(Feat1 = character(), Feat2 = character(), Corr = numeric(), stringsAsFactors = FALSE)
for (i in 1:(ncol(cor_mat) - 1)) {
  for (j in (i + 1):ncol(cor_mat)) {
    r_val <- cor_mat[i, j]
    if (!is.na(r_val) && abs(r_val) >= 0.60) {
      high_cor_pairs <- rbind(high_cor_pairs, data.frame(
        Feat1 = colnames(cor_mat)[i],
        Feat2 = colnames(cor_mat)[j],
        Corr  = round(r_val, 3),
        stringsAsFactors = FALSE
      ))
    }
  }
}
high_cor_pairs <- high_cor_pairs[order(-abs(high_cor_pairs$Corr)), ]
cat("\n--- High Collinearity Pairs (|r| >= 0.60) ---\n")
print(high_cor_pairs, row.names = FALSE)

# 1B. Rolling ElasticNet Beta Importance Over Time (Standardized Betas: beta_j * sd(X_j))
# Note: glmnet standardizes X internally during fitting and returns unstandardized coefficients on the original scale.
# Multiplying coef_j * sd(X_j) recovers the exact standardized beta (log-odds change per +1 SD shift in feature j)!
regime_beta_list <- list()
regime_sel_list  <- list()
raw_vs_std_list  <- list()

for (ep_name in names(epoch_ends)) {
  cutoff_date <- epoch_ends[[ep_name]]
  std_betas_mat <- matrix(0, nrow = length(all_feature_dfs), ncol = length(feature_names))
  raw_betas_mat <- matrix(0, nrow = length(all_feature_dfs), ncol = length(feature_names))
  nonzero_mat   <- matrix(0, nrow = length(all_feature_dfs), ncol = length(feature_names))
  colnames(std_betas_mat) <- colnames(raw_betas_mat) <- colnames(nonzero_mat) <- feature_names
  valid_rows <- 0
  
  for (sym in names(all_feature_dfs)) {
    df_s <- all_feature_dfs[[sym]]
    sub_df <- df_s[df_s$Date <= cutoff_date, ]
    if (nrow(sub_df) < 350) next
    train_slice <- tail(sub_df, 500)
    X_tr <- as.matrix(train_slice[, feature_names])
    y_tr <- train_slice$TargetBinary
    if (length(unique(y_tr)) < 2) next
    
    fit_obj <- tryCatch(
      train_swing_model(X_tr, y_tr, feature_names = feature_names, alpha = 0.5, calibrate = FALSE),
      error = function(e) NULL
    )
    if (!is.null(fit_obj)) {
      valid_rows <- valid_rows + 1
      raw_b <- as.numeric(fit_obj$coef_matrix[-1, 1])
      sds   <- apply(X_tr, 2, sd, na.rm = TRUE)
      sds[is.na(sds) | sds == 0] <- 1
      std_b <- raw_b * sds
      
      raw_betas_mat[valid_rows, ] <- raw_b
      std_betas_mat[valid_rows, ] <- std_b
      nonzero_mat[valid_rows, ]   <- as.numeric(abs(raw_b) > 1e-8)
    }
  }
  
  if (valid_rows > 0) {
    std_betas_mat <- std_betas_mat[1:valid_rows, , drop = FALSE]
    raw_betas_mat <- raw_betas_mat[1:valid_rows, , drop = FALSE]
    nonzero_mat   <- nonzero_mat[1:valid_rows, , drop = FALSE]
    
    regime_beta_list[[ep_name]] <- colMeans(std_betas_mat, na.rm = TRUE)
    regime_sel_list[[ep_name]]  <- colMeans(nonzero_mat, na.rm = TRUE) * 100
    raw_vs_std_list[[ep_name]]  <- list(
      mean_abs_std = colMeans(abs(std_betas_mat), na.rm = TRUE),
      mean_abs_raw = colMeans(abs(raw_betas_mat), na.rm = TRUE),
      mean_signed_std = colMeans(std_betas_mat, na.rm = TRUE),
      sel_freq_pct = colMeans(nonzero_mat, na.rm = TRUE) * 100
    )
  }
}

# Build summary table of Standardized Beta Evolution across the 5 eras
beta_time_df <- data.frame(Feature = feature_names, stringsAsFactors = FALSE)
for (ep_name in names(regime_beta_list)) {
  beta_time_df[[paste0("SignedBeta_", ep_name)]] <- round(regime_beta_list[[ep_name]], 4)
  beta_time_df[[paste0("AbsBeta_", ep_name)]]    <- round(raw_vs_std_list[[ep_name]]$mean_abs_std, 4)
  beta_time_df[[paste0("SelPct_", ep_name)]]     <- round(regime_sel_list[[ep_name]], 1)
}

# Compute overall average absolute standardized beta across all eras
abs_cols <- grep("^AbsBeta_", colnames(beta_time_df), value = TRUE)
beta_time_df$MeanAbsStdBeta <- round(rowMeans(beta_time_df[, abs_cols, drop = FALSE]), 4)
sel_cols <- grep("^SelPct_", colnames(beta_time_df), value = TRUE)
beta_time_df$MeanSelPct <- round(rowMeans(beta_time_df[, sel_cols, drop = FALSE]), 1)

# Add latest era Raw Abs Beta vs Standardized Abs Beta to show scale distortion
latest_ep <- tail(names(raw_vs_std_list), 1)
beta_time_df$LatestRawAbsBeta <- round(raw_vs_std_list[[latest_ep]]$mean_abs_raw, 4)
beta_time_df$LatestStdAbsBeta <- round(raw_vs_std_list[[latest_ep]]$mean_abs_std, 4)

beta_time_df <- beta_time_df[order(-beta_time_df$MeanAbsStdBeta), ]
cat("\n--- Top Features by Standardized ElasticNet |Beta| & Selection Rate Across Time ---\n")
print(beta_time_df[, c("Feature", "MeanAbsStdBeta", "MeanSelPct", "LatestRawAbsBeta", "LatestStdAbsBeta")], row.names = FALSE)

cat("\n--- Signed Standardized Beta Evolution Across Market Regimes (2018 -> 2026) ---\n")
signed_cols <- c("Feature", grep("^SignedBeta_", colnames(beta_time_df), value = TRUE))
print(beta_time_df[, signed_cols], row.names = FALSE)

cat("\n================================================================================\n")
cat(" 2. FULL-UNIVERSE OUT-OF-SAMPLE MARK-OUT ANALYSIS (T+1 TO T+20 TRADING DAYS)\n")
cat("================================================================================\n")

# Load cached walk-forward predictions across the universe from data/cache_wf
wf_files <- list.files("data/cache_wf", pattern = "_1260wf\\.rds$", full.names = TRUE)
cat(sprintf("Scanning %d out-of-sample walk-forward symbol caches...\n", length(wf_files)))

spy_cl <- Cl(spy_ohlcv)
spy_dates <- index(spy_cl)
spy_px_vec <- as.numeric(spy_cl)

# Compute QQQ 50-day lmMA for Bull vs Defensive/Bear regime tagging
qqq_cl <- Cl(qqq_ohlcv)
qqq_n <- 50
qqq_x <- as.numeric(qqq_cl)
qqq_fit <- rep(NA_real_, length(qqq_x))
t_seq <- seq_len(qqq_n)
for (i in qqq_n:length(qqq_x)) {
  w <- qqq_x[(i - qqq_n + 1):i]
  if (!any(is.na(w))) {
    qqq_fit[i] <- tail(fitted(lm(w ~ t_seq)), 1)
  }
}
qqq_bull_map <- setNames(qqq_x >= qqq_fit, as.character(index(qqq_cl)))

horizons <- c(1, 2, 3, 5, 7, 10, 15, 20)
max_h <- max(horizons)

# Buckets to track:
# 1. High_Conv_Long: P >= 0.65
# 2. Hybrid_Leader_Long: P >= 0.60 & RS_20 >= 0.05
# 3. Standard_Long: 0.58 <= P < 0.65
# 4. All_Long: P >= 0.58
# 5. Neutral: 0.42 < P < 0.58
# 6. All_Short: P <= 0.42
# 7. Standard_Short: 0.35 < P <= 0.42
# 8. High_Conv_Short: P <= 0.35
# Plus regime-conditioned buckets:
# 9. Long_Bull_Regime (P >= 0.58 & Bull)
# 10. Long_Def_Regime (P >= 0.60 & RS >= 0.05 & Def)
# 11. Short_Bull_Regime (P <= 0.42 & Bull)
# 12. Short_Def_Regime (P <= 0.42 & Def)

bucket_names <- c(
  "High_Conv_Long (P >= 0.65)",
  "Hybrid_Leader_Long (P >= 0.60, RS >= +5%)",
  "Standard_Long (0.58 <= P < 0.65)",
  "All_Long (P >= 0.58)",
  "Neutral (0.42 < P < 0.58)",
  "All_Short (P <= 0.42)",
  "Standard_Short (0.35 < P <= 0.42)",
  "High_Conv_Short (P <= 0.35)",
  "Long_in_Bull_Regime (P >= 0.58)",
  "Long_in_Def_Regime (P >= 0.65)",
  "Short_in_Bull_Regime (P <= 0.42)",
  "Short_in_Def_Regime (P <= 0.42)"
)

# Accumulators for sum of raw returns, sum of excess returns vs SPY, sum of positive hits, and count
acc_raw_sum <- matrix(0, nrow = length(bucket_names), ncol = length(horizons),
                      dimnames = list(bucket_names, paste0("T+", horizons)))
acc_exc_sum <- matrix(0, nrow = length(bucket_names), ncol = length(horizons),
                      dimnames = list(bucket_names, paste0("T+", horizons)))
acc_win_sum <- matrix(0, nrow = length(bucket_names), ncol = length(horizons),
                      dimnames = list(bucket_names, paste0("T+", horizons)))
acc_cnt     <- matrix(0, nrow = length(bucket_names), ncol = length(horizons),
                      dimnames = list(bucket_names, paste0("T+", horizons)))

# Also track MFE (Max Favorable Excursion) and MAE (Max Adverse Excursion) over 7D and 15D
mfe_7d_sum  <- setNames(numeric(length(bucket_names)), bucket_names)
mae_7d_sum  <- setNames(numeric(length(bucket_names)), bucket_names)
mfe_15d_sum <- setNames(numeric(length(bucket_names)), bucket_names)
mae_15d_sum <- setNames(numeric(length(bucket_names)), bucket_names)
exc_cnt     <- setNames(numeric(length(bucket_names)), bucket_names)

for (wf_f in wf_files) {
  obj <- tryCatch(readRDS(wf_f), error = function(e) NULL)
  if (is.null(obj) || is.null(obj$cl) || is.null(obj$pred_prob)) next
  
  cl_v   <- as.numeric(obj$cl)
  prob_v <- as.numeric(obj$pred_prob)
  rs_v   <- if (!is.null(obj$rs_spy)) as.numeric(obj$rs_spy) else rep(0, length(cl_v))
  dts    <- as.Date(index(obj$cl))
  n_bars <- length(cl_v)
  if (n_bars <= max_h + 5) next
  
  # Align SPY prices
  spy_idx <- match(dts, spy_dates)
  spy_v   <- spy_px_vec[spy_idx]
  
  # Align Macro Bull regime
  bull_v  <- qqq_bull_map[as.character(dts)]
  bull_v[is.na(bull_v)] <- TRUE
  
  valid_t <- 1:(n_bars - max_h)
  p_sub   <- prob_v[valid_t]
  rs_sub  <- rs_v[valid_t]
  b_sub   <- bull_v[valid_t]
  c0      <- cl_v[valid_t]
  s0      <- spy_v[valid_t]
  
  # Filter out invalid prices or uninitialized 0.500 exact probabilities
  ok <- !is.na(c0) & c0 > 0 & !is.na(p_sub) & p_sub != 0.5
  if (sum(ok) < 10) next
  
  # Precompute masks for each bucket on valid_t
  masks <- list(
    "High_Conv_Long (P >= 0.65)"                = ok & (p_sub >= 0.65),
    "Hybrid_Leader_Long (P >= 0.60, RS >= +5%)" = ok & (p_sub >= 0.60 & rs_sub >= 0.05),
    "Standard_Long (0.58 <= P < 0.65)"          = ok & (p_sub >= 0.58 & p_sub < 0.65),
    "All_Long (P >= 0.58)"                      = ok & (p_sub >= 0.58),
    "Neutral (0.42 < P < 0.58)"                 = ok & (p_sub > 0.42 & p_sub < 0.58),
    "All_Short (P <= 0.42)"                     = ok & (p_sub <= 0.42),
    "Standard_Short (0.35 < P <= 0.42)"         = ok & (p_sub > 0.35 & p_sub <= 0.42),
    "High_Conv_Short (P <= 0.35)"               = ok & (p_sub <= 0.35),
    "Long_in_Bull_Regime (P >= 0.58)"           = ok & (p_sub >= 0.58 & b_sub),
    "Long_in_Def_Regime (P >= 0.65)"            = ok & (p_sub >= 0.65 & !b_sub),
    "Short_in_Bull_Regime (P <= 0.42)"          = ok & (p_sub <= 0.42 & b_sub),
    "Short_in_Def_Regime (P <= 0.42)"           = ok & (p_sub <= 0.42 & !b_sub)
  )
  
  # Forward returns at each horizon h
  for (h_idx in seq_along(horizons)) {
    h <- horizons[h_idx]
    ch <- cl_v[valid_t + h]
    sh <- spy_v[valid_t + h]
    
    raw_ret <- (ch - c0) / c0
    spy_ret <- (sh - s0) / s0
    exc_ret <- raw_ret - ifelse(is.na(spy_ret), 0, spy_ret)
    
    # Clamp extreme data glitches (> +200% or < -80% in h days)
    valid_r <- !is.na(raw_ret) & raw_ret < 2.0 & raw_ret > -0.80
    
    for (b_name in bucket_names) {
      m <- masks[[b_name]] & valid_r
      cnt_m <- sum(m)
      if (cnt_m > 0) {
        r_m <- raw_ret[m]
        e_m <- exc_ret[m]
        is_short_bucket <- grepl("Short", b_name)
        acc_raw_sum[b_name, h_idx] <- acc_raw_sum[b_name, h_idx] + sum(r_m)
        acc_exc_sum[b_name, h_idx] <- acc_exc_sum[b_name, h_idx] + sum(e_m)
        # Win rate: for Long/Neutral, raw_ret > 0; for Short, raw_ret < 0
        wins <- if (is_short_bucket) sum(r_m < 0) else sum(r_m > 0)
        acc_win_sum[b_name, h_idx] <- acc_win_sum[b_name, h_idx] + wins
        acc_cnt[b_name, h_idx]     <- acc_cnt[b_name, h_idx] + cnt_m
      }
    }
  }
}

mean_raw_pct <- round((acc_raw_sum / pmax(acc_cnt, 1)) * 100, 3)
mean_exc_pct <- round((acc_exc_sum / pmax(acc_cnt, 1)) * 100, 3)
win_rate_pct <- round((acc_win_sum / pmax(acc_cnt, 1)) * 100, 1)

cat("\n--- Signal Sample Counts Across 5-Year Out-of-Sample Universe ---\n")
print( data.frame(Bucket = bucket_names, Signals_Count = acc_cnt[, 1]), row.names = FALSE)

cat("\n--- Mark-Out Table 1: Mean Forward Raw Cumulative Return (%) [T+1 to T+20] ---\n")
print(mean_raw_pct)

cat("\n--- Mark-Out Table 2: Mean Forward Benchmark-Adjusted Excess Return vs SPY (%) [T+1 to T+20] ---\n")
print(mean_exc_pct)

cat("\n--- Mark-Out Table 3: Directional Hit Rate (%) [Long: R > 0 | Short: R < 0] ---\n")
print(win_rate_pct)

# Save all diagnostic results to JSON/CSV in output/
dir.create("output", showWarnings = FALSE)
write.csv(beta_time_df, "output/feature_importance_over_time.csv", row.names = FALSE)
write.csv(high_cor_pairs, "output/feature_collinearity_pairs.csv", row.names = FALSE)
write.csv(cbind(Bucket = rownames(mean_raw_pct), Signals = acc_cnt[, 1], as.data.frame(mean_raw_pct)),
          "output/markout_raw_returns_pct.csv", row.names = FALSE)
write.csv(cbind(Bucket = rownames(mean_exc_pct), Signals = acc_cnt[, 1], as.data.frame(mean_exc_pct)),
          "output/markout_excess_returns_pct.csv", row.names = FALSE)
write.csv(cbind(Bucket = rownames(win_rate_pct), Signals = acc_cnt[, 1], as.data.frame(win_rate_pct)),
          "output/markout_directional_winrate_pct.csv", row.names = FALSE)

cat("\nSaved all tables to output/feature_importance_over_time.csv and output/markout_*.csv\n")
