#!/usr/bin/env Rscript
# ==============================================================================
# scratch/analyze_feature_importance_over_time.R
# Walk-Forward ElasticNet Retraining & Feature Importance Time Series (2017-2026)
# ==============================================================================

suppressPackageStartupMessages({
  library(glmnet)
  library(xts)
  library(zoo)
})

source("R/05_logistic_model.R")

cat("========================================================================\n")
cat(" WALK-FORWARD ELASTICNET RETRAINING & FEATURE IMPORTANCE TIME SERIES\n")
cat("========================================================================\n\n")

DATA_FILE <- "data/feature_evaluation_dataset.rds"
if (!file.exists(DATA_FILE)) {
  stop("Master feature dataset not found at: ", DATA_FILE)
}

master_df <- readRDS(DATA_FILE)
cat(sprintf("[Loaded Dataset] %d observations from %s to %s across %d symbols.\n",
            nrow(master_df), min(master_df$Date), max(master_df$Date),
            length(unique(master_df$Symbol))))

# Orthogonalized 26-Feature Continuous Predictor Set
feat_cols <- c(
  "SlopeFast", "SlopeSlow", "TrendQuality",
  "ZScore", "GARCH_Vol", "GARCH_VolPct", "MSGARCH_ProbHighVol",
  "RS_20", "SectorSpread", "AsymVolRatio",
  "VolSqueeze", "SqueezeRelease", "CLV_5D", "IntradayVsGap_10D",
  "Prox52WHigh", "IdioAlpha_20D", "RangeToCloseVol_20D", "LowerWickDemand_10D",
  "CMF_20D", "RS_60", "EfficiencyRatio_20D",
  "ATR_Comp_5v20", "VolumeExp_5v20", "UpDownVolRatio_20D", "NetWickDemand_10D", "LeftTailShock_20D"
)

# Feature Category Mapping for Structured Attribution
FEATURE_CATEGORIES <- list(
  Microstructure_Demand = c("LowerWickDemand_10D", "NetWickDemand_10D", "CLV_5D", "IntradayVsGap_10D", "CMF_20D", "UpDownVolRatio_20D"),
  Volatility_Squeeze    = c("VolSqueeze", "SqueezeRelease", "ATR_Comp_5v20", "RangeToCloseVol_20D", "AsymVolRatio", "LeftTailShock_20D"),
  Trend_Quality         = c("SlopeFast", "SlopeSlow", "TrendQuality", "ZScore", "EfficiencyRatio_20D", "Prox52WHigh"),
  Macro_Volatility      = c("GARCH_Vol", "GARCH_VolPct", "MSGARCH_ProbHighVol"),
  Alpha_RelStrength     = c("RS_20", "RS_60", "IdioAlpha_20D", "SectorSpread", "VolumeExp_5v20")
)

get_feature_cat <- function(f) {
  for (cat_name in names(FEATURE_CATEGORIES)) {
    if (f %in% FEATURE_CATEGORIES[[cat_name]]) return(cat_name)
  }
  return("Other")
}

# Define Semi-Annual Walk-Forward Retraining Cutoffs (2017 to 2026)
retrain_schedule <- list(
  list(epoch = "2017-H1", train_end = as.Date("2017-06-30"), test_start = as.Date("2017-07-01"), test_end = as.Date("2017-12-31")),
  list(epoch = "2017-H2", train_end = as.Date("2017-12-31"), test_start = as.Date("2018-01-01"), test_end = as.Date("2018-06-30")),
  list(epoch = "2018-H1", train_end = as.Date("2018-06-30"), test_start = as.Date("2018-07-01"), test_end = as.Date("2018-12-31")),
  list(epoch = "2018-H2", train_end = as.Date("2018-12-31"), test_start = as.Date("2019-01-01"), test_end = as.Date("2019-06-30")),
  list(epoch = "2019-H1", train_end = as.Date("2019-06-30"), test_start = as.Date("2019-07-01"), test_end = as.Date("2019-12-31")),
  list(epoch = "2019-H2", train_end = as.Date("2019-12-31"), test_start = as.Date("2020-01-01"), test_end = as.Date("2020-06-30")),
  list(epoch = "2020-H1", train_end = as.Date("2020-06-30"), test_start = as.Date("2020-07-01"), test_end = as.Date("2020-12-31")),
  list(epoch = "2020-H2", train_end = as.Date("2020-12-31"), test_start = as.Date("2021-01-01"), test_end = as.Date("2021-06-30")),
  list(epoch = "2021-H1", train_end = as.Date("2021-06-30"), test_start = as.Date("2021-07-01"), test_end = as.Date("2021-12-31")),
  list(epoch = "2021-H2", train_end = as.Date("2021-12-31"), test_start = as.Date("2022-01-01"), test_end = as.Date("2022-06-30")),
  list(epoch = "2022-H1", train_end = as.Date("2022-06-30"), test_start = as.Date("2022-07-01"), test_end = as.Date("2022-12-31")),
  list(epoch = "2022-H2", train_end = as.Date("2022-12-31"), test_start = as.Date("2023-01-01"), test_end = as.Date("2023-06-30")),
  list(epoch = "2023-H1", train_end = as.Date("2023-06-30"), test_start = as.Date("2023-07-01"), test_end = as.Date("2023-12-31")),
  list(epoch = "2023-H2", train_end = as.Date("2023-12-31"), test_start = as.Date("2024-01-01"), test_end = as.Date("2024-06-30")),
  list(epoch = "2024-H1", train_end = as.Date("2024-06-30"), test_start = as.Date("2024-07-01"), test_end = as.Date("2024-12-31")),
  list(epoch = "2024-H2", train_end = as.Date("2024-12-31"), test_start = as.Date("2025-01-01"), test_end = as.Date("2025-06-30")),
  list(epoch = "2025-H1", train_end = as.Date("2025-06-30"), test_start = as.Date("2025-07-01"), test_end = as.Date("2025-12-31")),
  list(epoch = "2025-H2", train_end = as.Date("2025-12-31"), test_start = as.Date("2026-01-01"), test_end = as.Date("2026-06-30")),
  list(epoch = "2026-H1", train_end = as.Date("2026-06-30"), test_start = as.Date("2026-07-01"), test_end = as.Date("2026-09-25"))
)

cat(sprintf("Configured %d walk-forward retraining epochs across 2017-2026.\n\n", length(retrain_schedule)))

epoch_records <- list()
model_summary_records <- list()

for (sched in retrain_schedule) {
  ep <- sched$epoch
  tr_end <- sched$train_end
  te_start <- sched$test_start
  te_end <- sched$test_end
  
  # Causal training split: observations strictly on or before tr_end
  # Note: A 7-bar embargo is respected between training end and forward testing
  tr_mask <- (master_df$Date <= tr_end)
  te_mask <- (master_df$Date >= te_start & master_df$Date <= te_end)
  
  n_tr <- sum(tr_mask)
  n_te <- sum(te_mask)
  
  if (n_tr < 500) {
    cat(sprintf("Skipping %s: insufficient training samples (%d)\n", ep, n_tr))
    next
  }
  
  cat(sprintf("[Epoch %-7s] Training: %d obs (to %s) | Out-of-Sample Test: %d obs (%s to %s)...",
              ep, n_tr, tr_end, n_te, te_start, te_end))
  
  X_tr <- as.matrix(master_df[tr_mask, feat_cols])
  y_tr <- master_df$TargetBinary[tr_mask]
  
  X_te <- if (n_te > 0) as.matrix(master_df[te_mask, feat_cols]) else NULL
  y_te <- if (n_te > 0) master_df$TargetBinary[te_mask] else NULL
  
  t0 <- Sys.time()
  mod <- train_swing_model(
    X_train       = X_tr,
    y_train       = y_tr,
    X_test        = X_te,
    feature_names = feat_cols,
    alpha         = 0.5,
    p_long        = 0.58,
    p_short       = 0.42,
    calibrate     = TRUE,
    embargo_days  = 7,
    min_active_features = 3
  )
  fit_sec <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)
  
  # Out-of-sample skill diagnostics
  oos_auc <- NA_real_
  oos_brier <- NA_real_
  if (n_te > 0 && length(unique(y_te)) == 2) {
    pos_p <- mod$pred_probs[y_te == 1]
    neg_p <- mod$pred_probs[y_te == 0]
    if (length(pos_p) > 0 && length(neg_p) > 0) {
      r_all <- rank(c(pos_p, neg_p))
      n_pos <- length(pos_p)
      n_neg <- length(neg_p)
      sum_r_pos <- sum(r_all[1:n_pos])
      oos_auc <- (sum_r_pos - (n_pos * (n_pos + 1)) / 2) / (n_pos * n_neg)
    }
    oos_brier <- mean((mod$pred_probs - y_te)^2)
  }
  
  cat(sprintf(" Done (%.2fs) | Active: %2d/%2d | Lambda: %.5f | OOS AUC: %.3f\n",
              fit_sec, mod$active_features, length(feat_cols), mod$best_lambda,
              if (!is.na(oos_auc)) oos_auc else 0.50))
  
  # Feature importance metrics for this epoch
  feat_imp <- mod$feature_importance
  # Total absolute standardized weight
  sum_abs_std <- sum(feat_imp$AbsWeight)
  if (sum_abs_std == 0) sum_abs_std <- 1e-6
  
  feat_imp$RelWeightPct <- (feat_imp$AbsWeight / sum_abs_std) * 100
  feat_imp$Epoch <- ep
  feat_imp$TrainDate <- tr_end
  feat_imp$Category <- sapply(feat_imp$Feature, get_feature_cat)
  feat_imp$IsActive <- ifelse(feat_imp$AbsWeight > 1e-6, 1, 0)
  
  epoch_records[[ep]] <- feat_imp
  
  model_summary_records[[ep]] <- data.frame(
    Epoch           = ep,
    TrainEndDate    = as.character(tr_end),
    TrainSamples    = n_tr,
    TestSamples     = n_te,
    BestLambda      = mod$best_lambda,
    ActiveFeatures  = mod$active_features,
    OOS_AUC         = round(oos_auc, 4),
    OOS_Brier       = round(oos_brier, 4),
    Calibrated      = ifelse(mod$is_calibrated, 1, 0),
    stringsAsFactors = FALSE
  )
}

# Aggregate full feature importance time series table
all_feat_importance <- do.call(rbind, epoch_records)
rownames(all_feat_importance) <- NULL

all_model_summaries <- do.call(rbind, model_summary_records)
rownames(all_model_summaries) <- NULL

# Ensure output directory exists
dir.create("output", showWarnings = FALSE)

write.csv(all_feat_importance, "output/walkforward_feature_importance.csv", row.names = FALSE)
write.csv(all_model_summaries, "output/walkforward_model_epochs_summary.csv", row.names = FALSE)

# Compute Feature Retention Rate and Summary Across Epochs
n_epochs <- length(unique(all_feat_importance$Epoch))
feat_retention <- do.call(rbind, lapply(feat_cols, function(f) {
  sub <- all_feat_importance[all_feat_importance$Feature == f, ]
  active_count <- sum(sub$IsActive)
  retention_rate <- (active_count / n_epochs) * 100
  mean_std_beta <- mean(sub$StdBeta)
  mean_abs_weight <- mean(sub$AbsWeight)
  mean_rel_weight <- mean(sub$RelWeightPct)
  bullish_count <- sum(sub$Direction == "BULLISH")
  bearish_count <- sum(sub$Direction == "BEARISH")
  neutral_count <- sum(sub$Direction == "NEUTRAL")
  
  dom_dir <- if (bullish_count > bearish_count && bullish_count > neutral_count) {
    "BULLISH"
  } else if (bearish_count > bullish_count && bearish_count > neutral_count) {
    "BEARISH"
  } else if (active_count == 0) {
    "NEUTRAL (Zero)"
  } else {
    "MIXED / ADAPTIVE"
  }
  
  data.frame(
    Feature          = f,
    Category         = get_feature_cat(f),
    RetentionRatePct = round(retention_rate, 1),
    ActiveEpochs     = active_count,
    TotalEpochs      = n_epochs,
    MeanStdBeta      = round(mean_std_beta, 5),
    MeanAbsWeight    = round(mean_abs_weight, 5),
    MeanRelWeightPct = round(mean_rel_weight, 2),
    DominantDirection= dom_dir,
    BullishEpochs    = bullish_count,
    BearishEpochs    = bearish_count,
    ZeroEpochs       = neutral_count,
    stringsAsFactors = FALSE
  )
}))

feat_retention <- feat_retention[order(-feat_retention$MeanRelWeightPct, -feat_retention$RetentionRatePct), ]
rownames(feat_retention) <- NULL

write.csv(feat_retention, "output/feature_retention_and_weights_summary.csv", row.names = FALSE)

cat("\n========================================================================\n")
cat(" TOP 10 RETAINED FEATURES ACROSS RETRAINING EPOCHS:\n")
cat("========================================================================\n")
print(head(feat_retention[, c("Feature", "Category", "RetentionRatePct", "MeanRelWeightPct", "DominantDirection")], 10))

cat(sprintf("\nArtifacts generated:\n - output/walkforward_feature_importance.csv\n - output/walkforward_model_epochs_summary.csv\n - output/feature_retention_and_weights_summary.csv\n"))
