#!/usr/bin/env Rscript
# ==============================================================================
# scratch/test_feature_evaluation_framework.R
# Benchmarking the Candidate Feature Evaluation Framework on 3 Candidate Types
# ==============================================================================

suppressPackageStartupMessages({
  library(glmnet)
  library(xts)
  library(zoo)
})

source("R/evaluate_candidate_feature.R")

cat("========================================================================\n")
cat(" BENCHMARKING THE CANDIDATE FEATURE EVALUATION FRAMEWORK\n")
cat("========================================================================\n\n")

DATA_FILE <- "data/feature_evaluation_dataset.rds"
master_df <- readRDS(DATA_FILE)

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

# ------------------------------------------------------------------------------
# Test 1: Synthetic Gaussian Noise (Simulated Junk Feature)
# ------------------------------------------------------------------------------
cat("\n>>> TEST 1: SYNTHETIC GAUSSIAN NOISE (Junk Feature) <<<\n")
set.seed(42)
noise_feature <- rnorm(nrow(master_df))

res_noise <- evaluate_candidate_feature(
  candidate_name  = "Synthetic_Gaussian_Noise",
  candidate_vec   = noise_feature,
  target_ret      = target_ret,
  target_bin      = target_bin,
  baseline_matrix = b_mat,
  dates           = dates,
  macro_bull      = macro_bull,
  macro_vol       = macro_vol
)

# ------------------------------------------------------------------------------
# Test 2: Collinear / Redundant Feature (SlopeWeeklyPct)
# ------------------------------------------------------------------------------
cat("\n>>> TEST 2: COLLINEAR / REDUNDANT FEATURE (SlopeWeeklyPct) <<<\n")
slope_weekly <- master_df$SlopeWeeklyPct

res_collinear <- evaluate_candidate_feature(
  candidate_name  = "SlopeWeeklyPct",
  candidate_vec   = slope_weekly,
  target_ret      = target_ret,
  target_bin      = target_bin,
  baseline_matrix = b_mat,
  dates           = dates,
  macro_bull      = macro_bull,
  macro_vol       = macro_vol
)

# ------------------------------------------------------------------------------
# Test 3: Genuine Structural Alpha Feature (IntradayVsGap_10D as Candidate)
# ------------------------------------------------------------------------------
cat("\n>>> TEST 3: VALID CANDIDATE BENCHMARK (IntradayVsGap_10D) <<<\n")
# Remove IntradayVsGap_10D from baseline for this test
b_mat_no_ig <- b_mat[, colnames(b_mat) != "IntradayVsGap_10D"]
ig_feature  <- master_df$IntradayVsGap_10D

res_valid <- evaluate_candidate_feature(
  candidate_name  = "IntradayVsGap_10D",
  candidate_vec   = ig_feature,
  target_ret      = target_ret,
  target_bin      = target_bin,
  baseline_matrix = b_mat_no_ig,
  dates           = dates,
  macro_bull      = macro_bull,
  macro_vol       = macro_vol
)

# Aggregate Evaluation Summary
summary_table <- data.frame(
  Candidate_Feature = c("Synthetic_Gaussian_Noise", "SlopeWeeklyPct", "IntradayVsGap_10D"),
  Type              = c("Pure Random Noise", "Collinear Redundant", "Genuine Microstructure Alpha"),
  Gate1_Statistical = c(ifelse(res_noise$Gate1_Passed, "PASS", "FAIL"),
                        ifelse(res_collinear$Gate1_Passed, "PASS", "FAIL"),
                        ifelse(res_valid$Gate1_Passed, "PASS", "FAIL")),
  Gate2_Orthogonality= c(ifelse(res_noise$Gate2_Passed, "PASS", "FAIL"),
                        ifelse(res_collinear$Gate2_Passed, "PASS", "FAIL"),
                        ifelse(res_valid$Gate2_Passed, "PASS", "FAIL")),
  Gate3_Stability   = c(ifelse(res_noise$Gate3_Passed, "PASS", "FAIL"),
                        ifelse(res_collinear$Gate3_Passed, "PASS", "FAIL"),
                        ifelse(res_valid$Gate3_Passed, "PASS", "FAIL")),
  Gate4_ModelAdopt  = c(ifelse(res_noise$Gate4_Passed, "PASS", "FAIL"),
                        ifelse(res_collinear$Gate4_Passed, "PASS", "FAIL"),
                        ifelse(res_valid$Gate4_Passed, "PASS", "FAIL")),
  Final_Verdict     = c("REJECT (Noise)", "REJECT (Collinear)", "ACCEPT (Alpha Driver)"),
  stringsAsFactors  = FALSE
)

cat("========================================================================\n")
cat(" FRAMEWORK BENCHMARK SUMMARY COMPARISON:\n")
cat("========================================================================\n")
print(summary_table)

write.csv(summary_table, "output/candidate_feature_evaluation_benchmark.csv", row.names = FALSE)
