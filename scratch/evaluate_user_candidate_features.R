#!/usr/bin/env Rscript
# ==============================================================================
# scratch/evaluate_candidate_features_deep_dive.R
# Evaluates User Candidate Features:
# 1. Intraday Breadth Ratio (Net Advance-Decline Momentum across Tech Equities)
# 2. Short-Term Mean-Reversion / Overnight Gap Reversal Factor
# ==============================================================================

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(TTR)
  library(glmnet)
})

source("R/evaluate_candidate_feature.R")

cat("========================================================================\n")
cat(" EVALUATING USER CANDIDATE ALPHA FEATURES ON 10-YEAR ENGINE (2016-2026)\n")
cat("========================================================================\n\n")

DATA_FILE <- "data/feature_evaluation_dataset.rds"
master_df <- readRDS(DATA_FILE)

cat(sprintf("[Loaded Dataset] %d observations from %s to %s across %d symbols.\n\n",
            nrow(master_df), min(master_df$Date), max(master_df$Date),
            length(unique(master_df$Symbol))))

DATA_DIR <- "data/data_10yr"
symbols <- unique(master_df$Symbol)
all_dates <- sort(unique(master_df$Date))

cat("1. Computing Market Breadth: Intraday Advance-Decline Ratio across Universe...\n")

# Load all OHLCVs to compute market-wide intraday advance/decline
ohlcv_list <- list()
for (s in symbols) {
  f <- file.path(DATA_DIR, sprintf("data_%s.rds", tolower(s)))
  if (file.exists(f)) {
    d <- tryCatch(readRDS(f), error = function(e) NULL)
    if (!is.null(d)) ohlcv_list[[s]] <- d
  }
}

# Matrix of daily intraday signs: +1 if Close > Open, -1 if Close < Open, 0 if equal
intraday_matrix <- matrix(NA_real_, nrow = length(all_dates), ncol = length(ohlcv_list))
rownames(intraday_matrix) <- as.character(all_dates)
colnames(intraday_matrix) <- names(ohlcv_list)

for (s in names(ohlcv_list)) {
  d <- ohlcv_list[[s]]
  m_idx <- match(all_dates, as.Date(index(d)))
  valid <- !is.na(m_idx)
  if (sum(valid) > 0) {
    op <- as.numeric(Op(d))[m_idx[valid]]
    cl <- as.numeric(Cl(d))[m_idx[valid]]
    diff_oc <- cl - op
    sign_oc <- ifelse(diff_oc > 0, 1, ifelse(diff_oc < 0, -1, 0))
    intraday_matrix[valid, s] <- sign_oc
  }
}

# Net Advance-Decline Ratio: (N_adv - N_dec) / N_tot for each day
n_adv <- rowSums(intraday_matrix == 1, na.rm = TRUE)
n_dec <- rowSums(intraday_matrix == -1, na.rm = TRUE)
n_tot <- rowSums(!is.na(intraday_matrix), na.rm = TRUE)
n_tot[n_tot == 0] <- 1

net_adv_dec_ratio <- (n_adv - n_dec) / n_tot
# 10-day smoothed intraday breadth momentum
breadth_xts <- xts(net_adv_dec_ratio, order.by = all_dates)
breadth_10d <- as.numeric(TTR::SMA(breadth_xts, n = 10))
breadth_10d[is.na(breadth_10d)] <- 0.0

# 5-day smoothed intraday breadth momentum
breadth_5d <- as.numeric(TTR::SMA(breadth_xts, n = 5))
breadth_5d[is.na(breadth_5d)] <- 0.0

breadth_10d_xts <- xts(breadth_10d, order.by = all_dates)
breadth_5d_xts  <- xts(breadth_5d, order.by = all_dates)

# Map breadth to master_df by date
m_date_idx <- match(master_df$Date, all_dates)
master_df$IntradayBreadthRatio_10D <- as.numeric(breadth_10d_xts)[m_date_idx]
master_df$IntradayBreadthRatio_5D  <- as.numeric(breadth_5d_xts)[m_date_idx]

cat("  -> Intraday Breadth Ratio computed across all 2,629 trading sessions.\n\n")

cat("2. Computing Short-Term Mean-Reversion & Overnight Gap Reversal Factors...\n")

# For each stock, compute:
# A. OvernightGapReversal_ATR: -(Open_t - Close_{t-1}) / ATR_{t-1}
# B. ST_Reversal_3D: -(Close_t - Close_{t-3}) / (ATR_t * sqrt(3))
gap_rev_list <- list()
st_rev_list  <- list()

for (s in names(ohlcv_list)) {
  d <- ohlcv_list[[s]]
  cl <- as.numeric(Cl(d))
  op <- as.numeric(Op(d))
  dates_d <- as.Date(index(d))
  n_d <- length(cl)
  
  atr_obj <- tryCatch(TTR::ATR(HLC(d), n = 14)$atr, error = function(e) cl * 0.02)
  atr_v <- as.numeric(atr_obj)
  atr_v[is.na(atr_v) | atr_v <= 0] <- cl[is.na(atr_v) | atr_v <= 0] * 0.02
  
  # Lagged close
  prev_cl <- c(cl[1], head(cl, -1))
  prev_atr <- c(atr_v[1], head(atr_v, -1))
  
  # Overnight Gap: (Open - PrevClose) / PrevATR
  gap_norm <- (op - prev_cl) / (prev_atr + 1e-5)
  # Gap Reversal Factor: fade the gap (negative gap is positive reversal score)
  gap_rev <- -gap_norm
  gap_rev[is.na(gap_rev)] <- 0.0
  gap_rev <- pmin(pmax(gap_rev, -4.0), 4.0)
  
  # 3-Day Short-Term Return Reversal: -(Close_t - Close_{t-3}) / (ATR_t * sqrt(3))
  cl_lag3 <- c(rep(cl[1], 3), head(cl, -3))
  st_rev3 <- -(cl - cl_lag3) / (atr_v * sqrt(3) + 1e-5)
  st_rev3[is.na(st_rev3)] <- 0.0
  st_rev3 <- pmin(pmax(st_rev3, -4.0), 4.0)
  
  # 5-day smoothed gap reversal
  gap_rev_5d <- as.numeric(TTR::SMA(gap_rev, n = 5))
  gap_rev_5d[is.na(gap_rev_5d)] <- 0.0
  
  gap_rev_list[[s]] <- data.frame(
    Symbol = s,
    Date   = dates_d,
    OvernightGapReversal = gap_rev,
    OvernightGapRev_5D   = gap_rev_5d,
    ST_Reversal_3D       = st_rev3,
    stringsAsFactors     = FALSE
  )
}

stock_rev_df <- do.call(rbind, gap_rev_list)

# Merge reversal factors to master_df by Symbol and Date
key_m <- paste(master_df$Symbol, master_df$Date, sep = "_")
key_r <- paste(stock_rev_df$Symbol, stock_rev_df$Date, sep = "_")
match_r <- match(key_m, key_r)

master_df$OvernightGapReversal <- stock_rev_df$OvernightGapReversal[match_r]
master_df$OvernightGapRev_5D   <- stock_rev_df$OvernightGapRev_5D[match_r]
master_df$ST_Reversal_3D       <- stock_rev_df$ST_Reversal_3D[match_r]

cat("  -> Overnight Gap Reversal and 3-Day Reversal Factors computed.\n\n")

# Baseline 26 continuous features
feat_cols <- c(
  "SlopeFast", "SlopeSlow", "TrendQuality",
  "ZScore", "GARCH_Vol", "GARCH_VolPct", "MSGARCH_ProbHighVol",
  "RS_20", "SectorSpread", "AsymVolRatio",
  "VolSqueeze", "SqueezeRelease", "CLV_5D", "IntradayVsGap_10D",
  "Prox52WHigh", "IdioAlpha_20D", "RangeToCloseVol_20D", "LowerWickDemand_10D",
  "CMF_20D", "RS_60", "EfficiencyRatio_20D",
  "ATR_Comp_5v20", "VolumeExp_5v20", "UpDownVolRatio_20D", "NetWickDemand_10D", "LeftTailShock_20D"
)

b_mat <- as.matrix(master_df[, feat_cols])
target_ret <- master_df$TargetRet
target_bin <- master_df$TargetBinary
dates      <- master_df$Date
macro_bull <- master_df$Macro_Bull
macro_vol  <- master_df$High_Vol

# ==============================================================================
# RUN 5-GATE EVALUATIONS
# ==============================================================================

cat("========================================================================\n")
cat(" CANDIDATE 1: INTRADAY BREADTH RATIO (10-DAY SMOOTHED)\n")
cat("========================================================================\n")
eval_breadth <- evaluate_candidate_feature(
  candidate_name  = "IntradayBreadthRatio_10D",
  candidate_vec   = master_df$IntradayBreadthRatio_10D,
  target_ret      = target_ret,
  target_bin      = target_bin,
  baseline_matrix = b_mat,
  dates           = dates,
  macro_bull      = macro_bull,
  macro_vol       = macro_vol,
  run_walkforward = TRUE
)

cat("========================================================================\n")
cat(" CANDIDATE 2: OVERNIGHT GAP REVERSAL (1-DAY FADE / ATR)\n")
cat("========================================================================\n")
eval_gap_rev <- evaluate_candidate_feature(
  candidate_name  = "OvernightGapReversal",
  candidate_vec   = master_df$OvernightGapReversal,
  target_ret      = target_ret,
  target_bin      = target_bin,
  baseline_matrix = b_mat,
  dates           = dates,
  macro_bull      = macro_bull,
  macro_vol       = macro_vol,
  run_walkforward = TRUE
)

cat("========================================================================\n")
cat(" CANDIDATE 3: SHORT-TERM 3-DAY MEAN-REVERSION (ST_Reversal_3D)\n")
cat("========================================================================\n")
eval_st_rev3 <- evaluate_candidate_feature(
  candidate_name  = "ST_Reversal_3D",
  candidate_vec   = master_df$ST_Reversal_3D,
  target_ret      = target_ret,
  target_bin      = target_bin,
  baseline_matrix = b_mat,
  dates           = dates,
  macro_bull      = macro_bull,
  macro_vol       = macro_vol,
  run_walkforward = TRUE
)

# Comparison Summary Table
summary_results <- data.frame(
  Candidate_Feature = c("IntradayBreadthRatio_10D", "OvernightGapReversal", "ST_Reversal_3D"),
  Type              = c("Market Internal Breadth", "1-Day Overnight Gap Fade", "3-Day Price Mean-Reversion"),
  Rank_IC           = c(round(eval_breadth$Metrics$Rank_IC, 4),
                        round(eval_gap_rev$Metrics$Rank_IC, 4),
                        round(eval_st_rev3$Metrics$Rank_IC, 4)),
  Monthly_t_stat    = c(round(eval_breadth$Metrics$t_stat, 2),
                        round(eval_gap_rev$Metrics$t_stat, 2),
                        round(eval_st_rev3$Metrics$t_stat, 2)),
  p_value           = c(format.pval(eval_breadth$Metrics$p_value, digits = 3),
                        format.pval(eval_gap_rev$Metrics$p_value, digits = 3),
                        format.pval(eval_st_rev3$Metrics$p_value, digits = 3)),
  Quintile_Spread   = c(sprintf("%+.3f%%", eval_breadth$Metrics$Quintile_Spread),
                        sprintf("%+.3f%%", eval_gap_rev$Metrics$Quintile_Spread),
                        sprintf("%+.3f%%", eval_st_rev3$Metrics$Quintile_Spread)),
  Monotonicity      = c(round(eval_breadth$Metrics$Monotonicity, 2),
                        round(eval_gap_rev$Metrics$Monotonicity, 2),
                        round(eval_st_rev3$Metrics$Monotonicity, 2)),
  Max_Collinear_Feat= c(sprintf("%s (%+.2f)", eval_breadth$Metrics$Max_Cor_Feat, eval_breadth$Metrics$Max_Cor_Val),
                        sprintf("%s (%+.2f)", eval_gap_rev$Metrics$Max_Cor_Feat, eval_gap_rev$Metrics$Max_Cor_Val),
                        sprintf("%s (%+.2f)", eval_st_rev3$Metrics$Max_Cor_Feat, eval_st_rev3$Metrics$Max_Cor_Val)),
  VIF               = c(round(eval_breadth$Metrics$VIF, 2),
                        round(eval_gap_rev$Metrics$VIF, 2),
                        round(eval_st_rev3$Metrics$VIF, 2)),
  Gate1_StatPower   = c(ifelse(eval_breadth$Gate1_Passed, "PASS", "FAIL"),
                        ifelse(eval_gap_rev$Gate1_Passed, "PASS", "FAIL"),
                        ifelse(eval_st_rev3$Gate1_Passed, "PASS", "FAIL")),
  Gate2_Orthogonal  = c(ifelse(eval_breadth$Gate2_Passed, "PASS", "FAIL"),
                        ifelse(eval_gap_rev$Gate2_Passed, "PASS", "FAIL"),
                        ifelse(eval_st_rev3$Gate2_Passed, "PASS", "FAIL")),
  Gate3_Stability   = c(ifelse(eval_breadth$Gate3_Passed, "PASS", "FAIL"),
                        ifelse(eval_gap_rev$Gate3_Passed, "PASS", "FAIL"),
                        ifelse(eval_st_rev3$Gate3_Passed, "PASS", "FAIL")),
  Gate4_ModelAdopt  = c(ifelse(eval_breadth$Gate4_Passed, "PASS", "FAIL"),
                        ifelse(eval_gap_rev$Gate4_Passed, "PASS", "FAIL"),
                        ifelse(eval_st_rev3$Gate4_Passed, "PASS", "FAIL")),
  Recommendation    = c(eval_breadth$Recommendation,
                        eval_gap_rev$Recommendation,
                        eval_st_rev3$Recommendation),
  stringsAsFactors  = FALSE
)

cat("\n========================================================================\n")
cat(" FINAL 5-GATE CANDIDATE FEATURE EVALUATION SUMMARY:\n")
cat("========================================================================\n")
print(summary_results[, c("Candidate_Feature", "Rank_IC", "Monthly_t_stat", "Quintile_Spread", "VIF", "Gate1_StatPower", "Gate2_Orthogonal", "Gate4_ModelAdopt")])

write.csv(summary_results, "output/user_candidate_features_evaluation.csv", row.names = FALSE)
cat(sprintf("\nSaved evaluation results to output/user_candidate_features_evaluation.csv\n"))
