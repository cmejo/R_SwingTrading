#!/usr/bin/env Rscript
# ==============================================================================
# Stage-2 Candidate Feature Audit: Out-of-Sample IC, Collinearity & Portfolio Lift
# Tests 6 new candidate features against the current 26-feature orthogonal model:
#   1. CMF_20D            : 20-Day Chaikin Money Flow (volume-weighted CLV accumulation)
#   2. EfficiencyRatio_20D: 20-Day Kaufman Efficiency Ratio (net move / total path length)
#   3. UpDownVolRatio_20D : 20-Day Log Ratio of Mean Up-Day Volume to Mean Down-Day Volume
#   4. RS_60              : 60-Day (Quarterly) Structural Relative Strength vs SPY
#   5. RS_Accel_5v20      : 5-Day vs 20-Day Relative Strength Acceleration vs SPY
#   6. ATR_Compression_5v20: 5-Day ATR / 20-Day ATR Short-Term Volatility Contraction
# ==============================================================================

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(TTR)
  library(glmnet)
  library(parallel)
})

source("R/01_data_loader.R")
source("R/02_trend_lmMA.R")
source("R/03_volatility_garch.R")
source("R/04_feature_pipeline.R")
source("R/05_logistic_model.R")

spy_ohlcv <- if (file.exists("data/data_10yr/data_spy.rds")) readRDS("data/data_10yr/data_spy.rds") else readRDS("data/data_spy.rds")
qqq_ohlcv <- if (file.exists("data/data_10yr/data_qqq.rds")) readRDS("data/data_10yr/data_qqq.rds") else spy_ohlcv

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

compute_stage2_candidates <- function(ohlcv, spy_ohlcv) {
  cl <- as.numeric(Cl(ohlcv))
  hi <- as.numeric(Hi(ohlcv))
  lo <- as.numeric(Lo(ohlcv))
  vo <- tryCatch(as.numeric(Vo(ohlcv)), error = function(e) rep(1e6, length(cl)))
  vo[is.na(vo) | vo <= 0] <- 1e6
  n  <- length(cl)
  idx <- index(ohlcv)

  ret_1d <- c(0, diff(log(pmax(cl, 1e-4))))
  ret_1d[is.na(ret_1d)] <- 0

  # Align SPY
  m_spy <- merge(Cl(ohlcv), Cl(spy_ohlcv))
  m_spy <- na.locf(m_spy, na.rm = FALSE)
  spy_cl <- as.numeric(m_spy[idx, 2])
  spy_cl[is.na(spy_cl) | spy_cl <= 0] <- cl[is.na(spy_cl) | spy_cl <= 0]

  # 1. CMF_20D: 20-Day Chaikin Money Flow = sum_20(CLV_t * Vol_t) / sum_20(Vol_t)
  hl_range <- pmax(hi - lo, 1e-4)
  clv_1d   <- ((cl - lo) - (hi - cl)) / hl_range
  clv_1d[is.na(clv_1d)] <- 0
  mf_vol   <- clv_1d * vo
  cmf_20   <- as.numeric(TTR::runSum(mf_vol, n = 20)) / (as.numeric(TTR::runSum(vo, n = 20)) + 1e-4)
  cmf_20[is.na(cmf_20)] <- 0
  cmf_20   <- pmin(pmax(cmf_20, -1.0), 1.0)

  # 2. EfficiencyRatio_20D: Kaufman 20-Day Efficiency Ratio = |C_t - C_{t-20}| / sum_{k=1..20} |C_t - C_{t-1}|
  abs_diff_1d <- c(0, abs(diff(cl)))
  net_move_20 <- abs(cl - as.numeric(lag.xts(Cl(ohlcv), k = 20)))
  path_len_20 <- as.numeric(TTR::runSum(abs_diff_1d, n = 20))
  er_20       <- net_move_20 / (path_len_20 + 1e-4)
  er_20[is.na(er_20)] <- 0.30
  er_20       <- pmin(pmax(er_20, 0.0), 1.0)

  # 3. UpDownVolRatio_20D: Log ratio of average volume on Up days vs Down days over 20 bars
  up_vol   <- ifelse(ret_1d > 0, vo, 0)
  dn_vol   <- ifelse(ret_1d < 0, vo, 0)
  up_cnt   <- ifelse(ret_1d > 0, 1, 0)
  dn_cnt   <- ifelse(ret_1d < 0, 1, 0)
  avg_up_v <- as.numeric(TTR::runSum(up_vol, n = 20)) / pmax(1, as.numeric(TTR::runSum(up_cnt, n = 20)))
  avg_dn_v <- as.numeric(TTR::runSum(dn_vol, n = 20)) / pmax(1, as.numeric(TTR::runSum(dn_cnt, n = 20)))
  ud_vol_r <- log((avg_up_v + 1.0) / (avg_dn_v + 1.0))
  ud_vol_r[is.na(ud_vol_r)] <- 0.0
  ud_vol_r <- pmin(pmax(ud_vol_r, -2.5), 2.5)

  # 4. RS_60: 60-Day Quarterly Structural Relative Strength vs SPY
  ret_60_stk <- (cl / as.numeric(lag.xts(Cl(ohlcv), k = 60))) - 1
  ret_60_spy <- (spy_cl / as.numeric(lag.xts(xts(spy_cl, order.by = idx), k = 60))) - 1
  rs_60      <- ret_60_stk - ret_60_spy
  rs_60[is.na(rs_60)] <- 0.0
  rs_60      <- pmin(pmax(rs_60, -1.0), 1.5)

  # 5. RS_Accel_5v20: 5-Day Relative Strength minus (20-Day RS / 4)
  ret_5_stk  <- (cl / as.numeric(lag.xts(Cl(ohlcv), k = 5))) - 1
  ret_5_spy  <- (spy_cl / as.numeric(lag.xts(xts(spy_cl, order.by = idx), k = 5))) - 1
  rs_5       <- ret_5_stk - ret_5_spy
  ret_20_stk <- (cl / as.numeric(lag.xts(Cl(ohlcv), k = 20))) - 1
  ret_20_spy <- (spy_cl / as.numeric(lag.xts(xts(spy_cl, order.by = idx), k = 20))) - 1
  rs_20      <- ret_20_stk - ret_20_spy
  rs_accel   <- rs_5 - (rs_20 / 4.0)
  rs_accel[is.na(rs_accel)] <- 0.0
  rs_accel   <- pmin(pmax(rs_accel, -0.35), 0.35)

  # 6. ATR_Compression_5v20: 5-Day True Range Mean / 20-Day True Range Mean
  tr_xts   <- TTR::ATR(HLC(ohlcv), n = 14)$tr
  tr_v     <- as.numeric(tr_xts)
  tr_v[is.na(tr_v) | tr_v <= 0] <- cl[is.na(tr_v) | tr_v <= 0] * 0.02
  atr_5    <- as.numeric(TTR::runMean(tr_v, n = 5))
  atr_20   <- as.numeric(TTR::runMean(tr_v, n = 20))
  atr_comp <- atr_5 / (atr_20 + 1e-5)
  atr_comp[is.na(atr_comp)] <- 1.0
  atr_comp <- pmin(pmax(atr_comp, 0.3), 2.5)

  xts(cbind(
    CMF_20D             = cmf_20,
    EfficiencyRatio_20D = er_20,
    UpDownVolRatio_20D  = ud_vol_r,
    RS_60               = rs_60,
    RS_Accel_5v20       = rs_accel,
    ATR_Compression_5v20= atr_comp
  ), order.by = idx)
}

new_cand_names <- c("CMF_20D", "EfficiencyRatio_20D", "UpDownVolRatio_20D",
                    "RS_60", "RS_Accel_5v20", "ATR_Compression_5v20")

cat(sprintf("[1/2] Evaluating 6 Stage-2 Candidate Features across %d stocks...\n", length(rep_syms)))

process_sym <- function(sym) {
  ohlcv <- load_sym_ohlcv(sym)
  if (is.null(ohlcv) || nrow(ohlcv) < 800) return(NULL)

  pipe <- tryCatch(
    suppressWarnings(build_feature_dataset(
      ohlcv = ohlcv, benchmark_ohlcv = spy_ohlcv, sector_ohlcv = qqq_ohlcv,
      fast_n = 20, slow_n = 50, look_ahead = 7, use_garch = FALSE
    )),
    error = function(e) NULL
  )
  if (is.null(pipe)) return(NULL)

  cand_xts <- compute_stage2_candidates(ohlcv, spy_ohlcv)
  df_base  <- pipe$model_data
  base_feats <- pipe$feature_names

  cand_df <- as.data.frame(cand_xts[as.Date(df_base$Date)])
  if (nrow(cand_df) != nrow(df_base)) return(NULL)

  df_all <- cbind(df_base, cand_df)
  cl_xts <- Cl(ohlcv)
  fwd_15 <- as.numeric((lag.xts(cl_xts, k = -15) - cl_xts) / cl_xts)
  names(fwd_15) <- as.character(index(cl_xts))
  df_all$Fwd15D <- fwd_15[as.character(df_all$Date)]
  df_all <- df_all[!is.na(df_all$Fwd15D), ]
  if (nrow(df_all) < 700) return(NULL)

  list(sym = sym, df = df_all, base_feats = base_feats)
}

res_list <- mclapply(rep_syms, process_sym, mc.cores = min(6, detectCores()))
res_list <- Filter(Negate(is.null), res_list)
cat(sprintf("  -> Built datasets for %d symbols.\n", length(res_list)))

pooled_df <- do.call(rbind, lapply(res_list, function(x) x$df))
base_feats <- res_list[[1]]$base_feats

ic_rows <- list()
for (fn in new_cand_names) {
  v <- pooled_df[[fn]]
  y_bin <- pooled_df$TargetBinary
  y_15d <- pooled_df$Fwd15D

  ic_bin <- suppressWarnings(cor(v, y_bin, use = "complete.obs", method = "spearman"))
  ic_15d <- suppressWarnings(cor(v, y_15d, use = "complete.obs", method = "spearman"))

  cor_with_base <- sapply(base_feats, function(bf) abs(suppressWarnings(cor(v, pooled_df[[bf]], use = "complete.obs"))))
  max_cor <- max(cor_with_base, na.rm = TRUE)
  most_cor_feat <- names(which.max(cor_with_base))

  qs <- quantile(v, probs = c(0.2, 0.8), na.rm = TRUE)
  q1_idx <- which(v <= qs[1])
  q5_idx <- which(v >= qs[2])
  q1_ret <- mean(y_15d[q1_idx], na.rm = TRUE) * 100
  q5_ret <- mean(y_15d[q5_idx], na.rm = TRUE) * 100
  q1_wr  <- mean(y_bin[q1_idx] == 1, na.rm = TRUE) * 100
  q5_wr  <- mean(y_bin[q5_idx] == 1, na.rm = TRUE) * 100

  ic_rows[[length(ic_rows) + 1]] <- data.frame(
    Feature         = fn,
    IC_Binary       = round(ic_bin, 4),
    IC_15D_Ret      = round(ic_15d, 4),
    Q5_minus_Q1_Bps = round((q5_ret - q1_ret) * 100, 1),
    Q5_minus_Q1_WR  = round(q5_wr - q1_wr, 2),
    MaxCorr_Existing= round(max_cor, 3),
    Closest_Existing= most_cor_feat,
    stringsAsFactors = FALSE
  )
}
ic_table <- do.call(rbind, ic_rows)
ic_table <- ic_table[order(-abs(ic_table$IC_15D_Ret)), ]

cat("\n========================================================================================\n")
cat(" 1. STAGE-2 CANDIDATE FEATURE INFORMATION COEFFICIENT (IC) & ORTHOGONALITY AUDIT\n")
cat("========================================================================================\n")
print(ic_table, row.names = FALSE)
