#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(TTR)
  library(glmnet)
})

source("R/01_data_loader.R")
source("R/02_trend_lmMA.R")
source("R/03_volatility_garch.R")
source("R/04_feature_pipeline.R")
source("R/05_logistic_model.R")

cat("================================================================================\n")
cat(" EMPIRICAL ALPHA AUDIT OF 6 CANDIDATE ORTHOGONAL FEATURES (2018 - 2026)\n")
cat("================================================================================\n")

spy_ohlcv <- if (file.exists("data/data_spy.rds")) readRDS("data/data_spy.rds") else readRDS("data/data_qqq.rds")
qqq_ohlcv <- if (file.exists("data/data_qqq.rds")) readRDS("data/data_qqq.rds") else spy_ohlcv

rep_syms <- c("AAPL", "MSFT", "NVDA", "AMD", "AMZN", "GOOGL", "META", "TSLA",
              "AVGO", "MU", "AMAT", "LRCX", "KLAC", "CRM", "ADBE", "PANW",
              "NFLX", "JPM", "GS", "LLY", "XOM", "CAT", "COST",
              "HD", "V", "MA", "ISRG", "NOW", "INTU")

load_sym_ohlcv <- function(sym) {
  f_10 <- file.path("data/data_10yr", sprintf("data_%s.rds", tolower(sym)))
  f_std <- file.path("data", sprintf("data_%s.rds", tolower(sym)))
  if (file.exists(f_10)) return(readRDS(f_10))
  if (file.exists(f_std)) return(readRDS(f_std))
  return(NULL)
}

# Candidate New Features to Engineer & Test:
# 1. IdioAlpha_20D:      20-day beta-adjusted residual Information Ratio vs SPY (Blitz/Jegadeesh Residual Momentum)
# 2. Prox52WHigh:        Log proximity to 252-day (52-week) high (George & Hwang 2004 overhead supply factor)
# 3. EfficiencyRatio_10D: Kaufman 10-day Fractal Efficiency Ratio |Net Change| / Sum(|Daily Changes|)
# 4. UpDownVolRatio_20D: 20-day return-weighted Up-Volume vs Down-Volume log accumulation ratio
# 5. StockVsSector_20D:  20-day excess return of Stock vs its Sector ETF (True intra-sector leadership)
# 6. RS_Accel_5v20:      Short-term Relative Strength Acceleration (5D RS vs SPY minus 20D RS / 4)

compute_candidate_features <- function(ohlcv, benchmark_ohlcv, sector_ohlcv) {
  price <- Cl(ohlcv)
  hi    <- Hi(ohlcv)
  lo    <- Lo(ohlcv)
  vol   <- Vo(ohlcv)
  n     <- nrow(price)
  idx   <- index(price)
  
  cl_v  <- as.numeric(price)
  hi_v  <- as.numeric(hi)
  lo_v  <- as.numeric(lo)
  vol_v <- as.numeric(vol)
  vol_v[is.na(vol_v) | vol_v <= 0] <- 1
  
  ret_1d <- c(0, diff(log(pmax(cl_v, 1e-4))))
  ret_1d[is.na(ret_1d)] <- 0
  
  # Benchmark returns
  bmk_ret_1d <- rep(0, n)
  if (!is.null(benchmark_ohlcv)) {
    m_b <- merge(price, Cl(benchmark_ohlcv))
    m_b <- na.locf(m_b, na.rm = FALSE)
    b_cl <- as.numeric(m_b[idx, 2])
    bmk_ret_1d <- c(0, diff(log(pmax(b_cl, 1e-4))))
    bmk_ret_1d[is.na(bmk_ret_1d)] <- 0
  }
  
  # Sector returns
  sec_ret_1d <- bmk_ret_1d
  if (!is.null(sector_ohlcv)) {
    m_s <- merge(price, Cl(sector_ohlcv))
    m_s <- na.locf(m_s, na.rm = FALSE)
    s_cl <- as.numeric(m_s[idx, 2])
    sec_ret_1d <- c(0, diff(log(pmax(s_cl, 1e-4))))
    sec_ret_1d[is.na(sec_ret_1d)] <- 0
  }
  
  # 1. IdioAlpha_20D (20D Beta-Adjusted Residual Information Ratio)
  cov_60 <- as.numeric(TTR::runCov(ret_1d, bmk_ret_1d, n = 60))
  var_60 <- as.numeric(TTR::runVar(bmk_ret_1d, n = 60))
  beta_60 <- cov_60 / (var_60 + 1e-6)
  beta_60[is.na(beta_60)] <- 1.0
  beta_60 <- pmin(pmax(beta_60, 0.2), 2.5)
  
  resid_1d <- ret_1d - beta_60 * bmk_ret_1d
  resid_mean_20 <- as.numeric(TTR::runMean(resid_1d, n = 20))
  resid_sd_20   <- as.numeric(TTR::runSD(resid_1d, n = 20))
  idio_ir_20    <- (resid_mean_20 / (resid_sd_20 + 1e-5)) * sqrt(20)
  idio_ir_20[is.na(idio_ir_20)] <- 0
  idio_ir_20 <- pmin(pmax(idio_ir_20, -4.0), 4.0)
  
  # 2. Prox52WHigh (Proximity to 252-day High, bounded [-1, 0] where 0 = at 52W High)
  w_52 <- min(252, max(20, floor(n * 0.5)))
  hi_252 <- as.numeric(TTR::runMax(hi_v, n = w_52))
  prox_52w <- (cl_v - hi_252) / (hi_252 + 1e-4)
  prox_52w[is.na(prox_52w)] <- -0.10
  prox_52w <- pmin(pmax(prox_52w, -0.60), 0.0)
  
  # 3. EfficiencyRatio_10D (Kaufman 10D Fractal Efficiency Ratio: |Net 10D Change| / Sum(|1D Changes|))
  abs_1d <- abs(c(0, diff(cl_v)))
  net_10d <- abs(cl_v - c(rep(cl_v[1], 10), head(cl_v, -10)))
  sum_10d <- as.numeric(TTR::runSum(abs_1d, n = 10))
  er_10d  <- net_10d / (sum_10d + 1e-5)
  # Signed by 10D direction so positive ER = smooth uptrend, negative ER = smooth downtrend
  dir_10d <- sign(cl_v - c(rep(cl_v[1], 10), head(cl_v, -10)))
  er_10d_signed <- er_10d * dir_10d
  er_10d_signed[is.na(er_10d_signed)] <- 0
  
  # 4. UpDownVolRatio_20D (Return-Weighted Up-Volume vs Down-Volume Log Ratio over 20D)
  up_dv   <- vol_v * pmax(ret_1d, 0)
  dn_dv   <- vol_v * pmax(-ret_1d, 0)
  sum_up  <- as.numeric(TTR::runSum(up_dv, n = 20))
  sum_dn  <- as.numeric(TTR::runSum(dn_dv, n = 20))
  ud_vol_ratio <- log((sum_up + 1e-4) / (sum_dn + 1e-4))
  ud_vol_ratio[is.na(ud_vol_ratio)] <- 0
  ud_vol_ratio <- pmin(pmax(ud_vol_ratio, -3.0), 3.0)
  
  # 5. StockVsSector_20D (20D Excess Return vs Sector ETF)
  ret_20_stk <- as.numeric(TTR::runSum(ret_1d, n = 20))
  ret_20_sec <- as.numeric(TTR::runSum(sec_ret_1d, n = 20))
  stk_vs_sec <- ret_20_stk - ret_20_sec
  stk_vs_sec[is.na(stk_vs_sec)] <- 0
  
  # 6. RS_Accel_5v20 (5D Relative Strength vs SPY minus 20D pace)
  rs_1d <- ret_1d - bmk_ret_1d
  rs_5d <- as.numeric(TTR::runSum(rs_1d, n = 5))
  rs_20d <- as.numeric(TTR::runSum(rs_1d, n = 20))
  rs_accel <- rs_5d - (rs_20d / 4.0)
  rs_accel[is.na(rs_accel)] <- 0
  
  out_xts <- xts(cbind(
    IdioAlpha_20D       = idio_ir_20,
    Prox52WHigh         = prox_52w,
    EfficiencyRatio_10D = er_10d_signed,
    UpDownVolRatio_20D  = ud_vol_ratio,
    StockVsSector_20D   = stk_vs_sec,
    RS_Accel_5v20       = rs_accel
  ), order.by = idx)
  return(out_xts)
}

cand_names <- c("IdioAlpha_20D", "Prox52WHigh", "EfficiencyRatio_10D",
                "UpDownVolRatio_20D", "StockVsSector_20D", "RS_Accel_5v20")

all_dfs <- list()
for (sym in rep_syms) {
  ohlcv <- load_sym_ohlcv(sym)
  if (is.null(ohlcv) || nrow(ohlcv) < 600) next
  sink("/dev/null")
  res <- tryCatch(
    build_feature_dataset(ohlcv, benchmark_ohlcv = spy_ohlcv, sector_ohlcv = qqq_ohlcv,
                          fast_n = 20, slow_n = 50, look_ahead = 7, use_garch = FALSE),
    error = function(e) NULL
  )
  sink()
  if (!is.null(res) && nrow(res$model_data) >= 500) {
    df_m <- res$model_data
    cand_xts <- compute_candidate_features(ohlcv, spy_ohlcv, qqq_ohlcv)
    cand_aligned <- cand_xts[df_m$Date]
    for (cn in cand_names) {
      df_m[[cn]] <- as.numeric(cand_aligned[, cn])
    }
    # Also compute 15-day forward return for mark-out test
    cl_s <- as.numeric(Cl(ohlcv)[df_m$Date])
    fwd_15 <- c(tail(cl_s, -15), rep(NA, 15))
    df_m$TargetRet15D <- (fwd_15 - cl_s) / cl_s
    df_m$Symbol <- sym
    all_dfs[[sym]] <- na.omit(df_m)
  }
}

pooled <- do.call(rbind, all_dfs)
base_feats <- c("SlopeFast", "SlopeSlow", "TrendQuality", "DistPct", 
                "ZScore", "GARCH_Vol", "GARCH_VolPct", "MSGARCH_ProbHighVol",
                "VolumeRatio", "OBV_Slope", "RSI_14", "MACD_Hist", "RS_20", 
                "SectorSpread", "AsymVolRatio",
                "VolSqueeze", "SqueezeRelease", "CLV_5D", "IntradayVsGap_10D",
                "PEAD_Catalyst", "PullbackInUptrend", "BreakoutWithVolume")

# 1. Check max correlation of each candidate feature with existing 22 features
cor_check <- cor(pooled[, cand_names], pooled[, base_feats], use = "pairwise.complete.obs")
max_cor_df <- data.frame(
  Candidate = cand_names,
  MaxAbsCorrWithExisting = round(apply(abs(cor_check), 1, max, na.rm = TRUE), 3),
  MostSimilarExisting    = colnames(cor_check)[apply(abs(cor_check), 1, which.max)],
  IC_7D_Binary           = round(sapply(cand_names, function(cn) cor(pooled[[cn]], pooled$TargetBinary, use = "complete.obs")), 4),
  IC_7D_Return           = round(sapply(cand_names, function(cn) cor(pooled[[cn]], pooled$TargetRet, use = "complete.obs")), 4),
  IC_15D_Return          = round(sapply(cand_names, function(cn) cor(pooled[[cn]], pooled$TargetRet15D, use = "complete.obs")), 4),
  stringsAsFactors = FALSE
)

# Compute Top Quintile (Q5) vs Bottom Quintile (Q1) 15D Return Spread (in bps)
q_spread_15d <- numeric(length(cand_names))
tb_spread_7d <- numeric(length(cand_names))
for (i in seq_along(cand_names)) {
  v <- pooled[[cand_names[i]]]
  qs <- quantile(v, probs = c(0.20, 0.80), na.rm = TRUE)
  q1_mask <- v <= qs[1]
  q5_mask <- v >= qs[2]
  q_spread_15d[i] <- round((mean(pooled$TargetRet15D[q5_mask], na.rm = TRUE) - mean(pooled$TargetRet15D[q1_mask], na.rm = TRUE)) * 10000, 1)
  tb_spread_7d[i] <- round((mean(pooled$TargetBinary[q5_mask], na.rm = TRUE) - mean(pooled$TargetBinary[q1_mask], na.rm = TRUE)) * 100, 2)
}
max_cor_df$Q5_minus_Q1_WinRate_Pct <- tb_spread_7d
max_cor_df$Q5_minus_Q1_15D_Bps     <- q_spread_15d

cat("\n--- Candidate Feature Orthogonality, Information Coefficient (IC), & Quintile Spread ---\n")
print(max_cor_df, row.names = FALSE)

# 2. Walk-Forward Out-of-Sample Model Comparison (22 Base Features vs. 22 Base + Top Candidate Features)
all_test_feats <- c(base_feats, cand_names)
auc_base_vec <- numeric(0)
auc_enh_vec  <- numeric(0)
top_ret_base <- numeric(0)
top_ret_enh  <- numeric(0)
std_beta_mat <- matrix(0, nrow = length(all_dfs), ncol = length(all_test_feats),
                       dimnames = list(names(all_dfs), all_test_feats))

for (sym in names(all_dfs)) {
  df_s <- all_dfs[[sym]]
  n_s  <- nrow(df_s)
  if (n_s < 700) next
  # Use last 500 bars before the final 250 bars as train, and final 250 bars as strictly OOS test
  tr_idx <- (n_s - 749):(n_s - 255)
  te_idx <- (n_s - 249):n_s
  
  X_tr_base <- as.matrix(df_s[tr_idx, base_feats])
  X_te_base <- as.matrix(df_s[te_idx, base_feats])
  X_tr_enh  <- as.matrix(df_s[tr_idx, all_test_feats])
  X_te_enh  <- as.matrix(df_s[te_idx, all_test_feats])
  y_tr      <- df_s$TargetBinary[tr_idx]
  y_te      <- df_s$TargetBinary[te_idx]
  r15_te    <- df_s$TargetRet15D[te_idx]
  
  m_base <- tryCatch(train_swing_model(X_tr_base, y_tr, X_te_base, feature_names = base_feats), error = function(e) NULL)
  m_enh  <- tryCatch(train_swing_model(X_tr_enh,  y_tr, X_te_enh,  feature_names = all_test_feats), error = function(e) NULL)
  
  if (!is.null(m_base) && !is.null(m_enh) && length(unique(y_te)) == 2) {
    # Record standardized betas from enhanced model
    imp_df <- m_enh$feature_importance
    std_beta_mat[sym, imp_df$Feature] <- imp_df$AbsWeight
    
    # Compute OOS 15D return of top-decile predictions for Base vs Enhanced
    q90_b <- quantile(m_base$pred_probs, 0.85, na.rm = TRUE)
    q90_e <- quantile(m_enh$pred_probs,  0.85, na.rm = TRUE)
    top_ret_base <- c(top_ret_base, mean(r15_te[m_base$pred_probs >= q90_b], na.rm = TRUE))
    top_ret_enh  <- c(top_ret_enh,  mean(r15_te[m_enh$pred_probs  >= q90_e], na.rm = TRUE))
  }
}

cat("\n--- Out-of-Sample Standardized |Beta| Ranking (Existing + 6 Candidate Features) ---\n")
mean_std_betas <- sort(colMeans(std_beta_mat, na.rm = TRUE), decreasing = TRUE)
sel_rates      <- colMeans(std_beta_mat > 1e-6, na.rm = TRUE) * 100
rank_df <- data.frame(
  Feature        = names(mean_std_betas),
  IsNewCandidate = ifelse(names(mean_std_betas) %in% cand_names, ">>> NEW <<<", ""),
  MeanStdAbsBeta = round(as.numeric(mean_std_betas), 4),
  SelectionRate  = sprintf("%.1f%%", sel_rates[names(mean_std_betas)]),
  stringsAsFactors = FALSE
)
print(head(rank_df, 18), row.names = FALSE)

cat(sprintf("\n--- Out-of-Sample Top-Conviction 15-Day Forward Return across %d Stocks ---\n", length(top_ret_base)))
cat(sprintf("  22-Feature Baseline Model:  %+.3f%% per 15-day trade (Annualized ~%+.1f%%)\n",
            mean(top_ret_base, na.rm = TRUE) * 100, ((1 + mean(top_ret_base, na.rm = TRUE))^(252/15) - 1) * 100))
cat(sprintf("  Enhanced Feature Model:     %+.3f%% per 15-day trade (Annualized ~%+.1f%%)\n",
            mean(top_ret_enh, na.rm = TRUE) * 100, ((1 + mean(top_ret_enh, na.rm = TRUE))^(252/15) - 1) * 100))
cat(sprintf("  Out-of-Sample Alpha Lift:   %+.1f bps per trade!\n",
            (mean(top_ret_enh, na.rm = TRUE) - mean(top_ret_base, na.rm = TRUE)) * 10000))
