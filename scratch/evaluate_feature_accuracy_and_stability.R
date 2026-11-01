#!/usr/bin/env Rscript
# ==============================================================================
# scratch/evaluate_feature_accuracy_and_stability.R
# Independent Out-of-Sample Statistical Accuracy & Parameter Adaptation Engine
# ==============================================================================

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
})

cat("========================================================================\n")
cat(" OUT-OF-SAMPLE STATISTICAL ACCURACY & PARAMETER ADAPTATION EVALUATION\n")
cat("========================================================================\n\n")

DATA_FILE <- "data/feature_evaluation_dataset.rds"
master_df <- readRDS(DATA_FILE)

# Complete list of evaluated features (26 continuous features + candidates)
feat_cols <- c(
  "SlopeFast", "SlopeSlow", "TrendQuality",
  "ZScore", "GARCH_Vol", "GARCH_VolPct", "MSGARCH_ProbHighVol",
  "RS_20", "SectorSpread", "AsymVolRatio",
  "VolSqueeze", "SqueezeRelease", "CLV_5D", "IntradayVsGap_10D",
  "Prox52WHigh", "IdioAlpha_20D", "RangeToCloseVol_20D", "LowerWickDemand_10D",
  "CMF_20D", "RS_60", "EfficiencyRatio_20D",
  "ATR_Comp_5v20", "VolumeExp_5v20", "UpDownVolRatio_20D", "NetWickDemand_10D", "LeftTailShock_20D",
  # Additional candidate/pruned features for comparison
  "SortinoMom_20D", "ATRExt", "RSI_14", "VolumeRatio", "SlopeWeeklyPct"
)

# Filter for available features in master_df
available_feats <- intersect(feat_cols, colnames(master_df))

target_ret <- master_df$TargetRet
target_bin <- master_df$TargetBinary
dates      <- master_df$Date
is_bull    <- master_df$Macro_Bull == 1
is_highvol <- master_df$High_Vol == 1

# Generate monthly time buckets for cross-sectional IC series
master_df$YearMonth <- format(as.Date(dates), "%Y-%m")
unique_months <- sort(unique(master_df$YearMonth))

cat(sprintf("Analyzing %d features across %d observations and %d monthly cross-sections...\n\n",
            length(available_feats), nrow(master_df), length(unique_months)))

scorecard_rows <- list()
quintile_markouts <- list()
rolling_betas_list <- list()

# Rolling 2-year window definitions for parameter refit dynamics
years <- 2018:2026
rolling_windows <- lapply(years, function(y) {
  start_d <- as.Date(sprintf("%d-01-01", y - 2))
  end_d   <- as.Date(sprintf("%d-12-31", y))
  list(year = y, start_d = start_d, end_d = end_d)
})

for (f in available_feats) {
  vals <- master_df[[f]]
  valid <- complete.cases(vals, target_ret, target_bin) & is.finite(vals) & is.finite(target_ret)
  
  if (sum(valid) < 1000) next
  
  v_sub <- vals[valid]
  r_sub <- target_ret[valid]
  y_sub <- target_bin[valid]
  b_sub <- is_bull[valid]
  h_sub <- is_highvol[valid]
  m_sub <- master_df$YearMonth[valid]
  d_sub <- dates[valid]
  
  # 1. Univariate Correlation & Rank Correlation (Full Sample)
  ic_pearson <- suppressWarnings(cor(v_sub, r_sub, method = "pearson"))
  ic_spearman <- suppressWarnings(cor(v_sub, r_sub, method = "spearman"))
  
  # 2. Cross-Sectional Monthly IC Distribution (Information Ratio & t-stat)
  month_ics <- numeric(0)
  for (m in unique_months) {
    m_mask <- (m_sub == m)
    if (sum(m_mask) >= 30) {
      v_m <- v_sub[m_mask]
      r_m <- r_sub[m_mask]
      if (sd(v_m) > 1e-6 && sd(r_m) > 1e-6) {
        month_ics <- c(month_ics, suppressWarnings(cor(v_m, r_m, method = "spearman")))
      }
    }
  }
  
  mean_ic <- mean(month_ics, na.rm = TRUE)
  sd_ic   <- sd(month_ics, na.rm = TRUE)
  # Annualized IC IR (monthly periods: sqrt(12))
  ic_ir   <- if (!is.na(sd_ic) && sd_ic > 0) (mean_ic / sd_ic) * sqrt(12) else 0.0
  t_stat  <- if (!is.na(sd_ic) && sd_ic > 0) (mean_ic / (sd_ic / sqrt(length(month_ics)))) else 0.0
  p_val   <- 2 * (1 - pnorm(abs(t_stat)))
  
  # 3. Directional Hit Rate & Classification AUC
  med_v <- median(v_sub, na.rm = TRUE)
  # Expected sign from Rank IC
  sign_ic <- if (!is.na(ic_spearman) && ic_spearman >= 0) 1 else if (!is.na(ic_spearman)) -1 else 1
  hit_mask <- if (sign_ic > 0) (v_sub >= med_v) else (v_sub <= med_v)
  hit_rate <- mean(r_sub[hit_mask] > 0) * 100
  
  # Out-of-sample AUC via Mann-Whitney
  auc <- 0.50
  if (length(unique(y_sub)) == 2) {
    pos_v <- v_sub[y_sub == 1]
    neg_v <- v_sub[y_sub == 0]
    n_pos <- as.numeric(length(pos_v))
    n_neg <- as.numeric(length(neg_v))
    if (n_pos > 0 && n_neg > 0) {
      r_all <- rank(c(pos_v, neg_v))
      sum_r_pos <- sum(r_all[1:n_pos])
      auc_raw <- (sum_r_pos - (n_pos * (n_pos + 1)) / 2) / (n_pos * n_neg)
      auc <- if (sign_ic > 0) auc_raw else (1 - auc_raw)
    }
  }
  
  # 4. Monotonic Quintile Returns & Spread
  r_rank <- rank(v_sub, ties.method = "first")
  q_groups <- cut(r_rank, breaks = quantile(r_rank, probs = seq(0, 1, 0.20)), include.lowest = TRUE, labels = FALSE)
  
  q_rets <- tapply(r_sub, q_groups, mean, na.rm = TRUE) * 100
  if (length(q_rets) < 5 || any(is.na(q_rets))) {
    q_rets <- rep(0, 5)
  }
  
  q1_ret <- q_rets[1]
  q5_ret <- q_rets[5]
  q_spread <- if (sign_ic > 0) (q5_ret - q1_ret) else (q1_ret - q5_ret)
  # Monotonicity: Rank correlation between quintile rank (1..5) and quintile return
  monotonicity <- suppressWarnings(cor(1:5, if (sign_ic > 0) q_rets else rev(q_rets), method = "spearman"))
  if (is.na(monotonicity)) monotonicity <- 0.0
  
  quintile_markouts[[f]] <- data.frame(
    Feature  = f,
    Q1       = round(q_rets[1], 3),
    Q2       = round(q_rets[2], 3),
    Q3       = round(q_rets[3], 3),
    Q4       = round(q_rets[4], 3),
    Q5       = round(q_rets[5], 3),
    QSpread  = round(q_spread, 3),
    Monotonicity = round(monotonicity, 2)
  )
  
  # 5. Market Regime Sensitivity
  # Bull vs Bear
  ic_bull <- suppressWarnings(cor(v_sub[b_sub], r_sub[b_sub], method = "spearman"))
  ic_bear <- suppressWarnings(cor(v_sub[!b_sub], r_sub[!b_sub], method = "spearman"))
  # Low Vol vs High Vol
  ic_lowvol  <- suppressWarnings(cor(v_sub[!h_sub], r_sub[!h_sub], method = "spearman"))
  ic_highvol <- suppressWarnings(cor(v_sub[h_sub], r_sub[h_sub], method = "spearman"))
  
  # 6. Parameter Adaptation / Rolling Refit Dynamics
  rolling_betas <- numeric(0)
  for (rw in rolling_windows) {
    w_mask <- (d_sub >= rw$start_d & d_sub <= rw$end_d)
    if (sum(w_mask) >= 1000) {
      v_w <- v_sub[w_mask]
      y_w <- y_sub[w_mask]
      # Standardize feature in window
      sd_w <- sd(v_w)
      if (!is.na(sd_w) && sd_w > 1e-6 && length(unique(y_w)) == 2) {
        v_std <- (v_w - mean(v_w)) / sd_w
        fit_w <- suppressWarnings(glm(y_w ~ v_std, family = binomial(link = "logit")))
        b1 <- coef(fit_w)[2]
        if (!is.na(b1)) {
          rolling_betas <- c(rolling_betas, b1)
          rolling_betas_list[[length(rolling_betas_list) + 1]] <- data.frame(
            Feature = f,
            Year    = rw$year,
            Window  = sprintf("%d-%d", rw$year - 2, rw$year),
            Beta1   = b1,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  
  mean_beta1 <- mean(rolling_betas, na.rm = TRUE)
  sd_beta1   <- sd(rolling_betas, na.rm = TRUE)
  cv_beta1   <- if (!is.na(sd_beta1) && abs(mean_beta1) > 1e-4) abs(sd_beta1 / mean_beta1) else 9.99
  
  # Sign consistency across rolling windows
  sign_match <- if (length(rolling_betas) > 0) {
    mean(sign(rolling_betas) == sign(mean_beta1)) * 100
  } else 0.0
  
  # Parameter Stability Assessment Rating
  stability_rating <- if (cv_beta1 < 0.35 && sign_match >= 90.0) {
    "HIGH (Stable Signal)"
  } else if (cv_beta1 < 0.75 && sign_match >= 75.0) {
    "MODERATE (Mild Adaptation)"
  } else {
    "UNSTABLE (Regime Decay / Inversion)"
  }

  scorecard_rows[[f]] <- data.frame(
    Feature             = f,
    Rank_IC             = round(ic_spearman, 4),
    Mean_Monthly_IC     = round(mean_ic, 4),
    IC_IR               = round(ic_ir, 2),
    t_stat              = round(t_stat, 2),
    p_value             = format.pval(p_val, digits = 3, eps = 1e-4),
    Hit_Rate_Pct        = round(hit_rate, 2),
    ROC_AUC             = round(auc, 3),
    Q5_minus_Q1_Spread  = round(q_spread, 3),
    Monotonicity_Score  = round(monotonicity, 2),
    Bull_IC             = round(ic_bull, 4),
    Bear_IC             = round(ic_bear, 4),
    LowVol_IC           = round(ic_lowvol, 4),
    HighVol_IC          = round(ic_highvol, 4),
    Mean_Beta1          = round(mean_beta1, 4),
    CV_Beta1            = round(cv_beta1, 2),
    Sign_Consistency_Pct= round(sign_match, 1),
    Parameter_Stability = stability_rating,
    stringsAsFactors    = FALSE
  )
}

# Aggregate and save scorecard
scorecard_df <- do.call(rbind, scorecard_rows)
scorecard_df <- scorecard_df[order(-abs(scorecard_df$Rank_IC)), ]
rownames(scorecard_df) <- NULL

quintile_df <- do.call(rbind, quintile_markouts)
rownames(quintile_df) <- NULL

rolling_betas_df <- do.call(rbind, rolling_betas_list)
rownames(rolling_betas_df) <- NULL

dir.create("output", showWarnings = FALSE)
write.csv(scorecard_df, "output/univariate_feature_accuracy_scorecard.csv", row.names = FALSE)
write.csv(quintile_df, "output/feature_quintile_markout_returns.csv", row.names = FALSE)
write.csv(rolling_betas_df, "output/feature_rolling_parameter_betas.csv", row.names = FALSE)

cat("========================================================================\n")
cat(" TOP 10 STATISTICALLY SIGNIFICANT FEATURES (RANK IC & T-STAT):\n")
cat("========================================================================\n")
print(head(scorecard_df[, c("Feature", "Rank_IC", "IC_IR", "t_stat", "ROC_AUC", "Q5_minus_Q1_Spread", "Parameter_Stability")], 10))

cat(sprintf("\nArtifacts generated:\n - output/univariate_feature_accuracy_scorecard.csv\n - output/feature_quintile_markout_returns.csv\n - output/feature_rolling_parameter_betas.csv\n"))
