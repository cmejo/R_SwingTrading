#!/usr/bin/env Rscript
# Append Walk-Forward Out-of-Sample Backtest to test_stage2_features.R
source("test_stage2_features.R")

top4_stage2 <- c("CMF_20D", "RS_60", "EfficiencyRatio_20D", "ATR_Compression_5v20")

cat("\n[2/2] Running Purged Walk-Forward ElasticNet Backtest (26 Current vs. 30 Enhanced Features)...\n")

eval_sym_wf <- function(item) {
  sym <- item$sym
  df  <- item$df
  f_base <- item$base_feats
  f_enh  <- c(f_base, top4_stage2)

  n_obs <- nrow(df)
  start_idx <- max(505, floor(n_obs * 0.35))
  rebal_pts <- seq(start_idx, n_obs - 15, by = 60)
  if (length(rebal_pts) < 3) return(NULL)

  p_base_vec <- rep(NA_real_, n_obs)
  p_enh_vec  <- rep(NA_real_, n_obs)
  beta_enh_list <- list()

  for (k in seq_along(rebal_pts)) {
     idx_t <- rebal_pts[k]
     idx_next <- if (k < length(rebal_pts)) rebal_pts[k + 1] - 1 else n_obs
     train_rows <- max(1, idx_t - 500):(idx_t - 5)

     X_tr_b <- as.matrix(df[train_rows, f_base])
     X_tr_e <- as.matrix(df[train_rows, f_enh])
     y_tr   <- df$TargetBinary[train_rows]
     if (length(unique(y_tr)) < 2) next

     X_te_b <- as.matrix(df[idx_t:idx_next, f_base, drop = FALSE])
     X_te_e <- as.matrix(df[idx_t:idx_next, f_enh,  drop = FALSE])

     m_b <- tryCatch(suppressWarnings(train_swing_model(
      X_train = X_tr_b, y_train = y_tr, X_test = X_te_b,
      feature_names = f_base, alpha = 0.5, calibrate = TRUE, embargo_days = 7
    )), error = function(e) NULL)

     m_e <- tryCatch(suppressWarnings(train_swing_model(
      X_train = X_tr_e, y_train = y_tr, X_test = X_te_e,
      feature_names = f_enh, alpha = 0.5, calibrate = TRUE, embargo_days = 7
    )), error = function(e) NULL)

     if (!is.null(m_b)) p_base_vec[idx_t:idx_next] <- m_b$pred_probs
     if (!is.null(m_e)) {
      p_enh_vec[idx_t:idx_next] <- m_e$pred_probs
      beta_enh_list[[length(beta_enh_list) + 1]] <- m_e$feature_importance
    }
  }

  valid <- which(!is.na(p_base_vec) & !is.na(p_enh_vec) & !is.na(df$Fwd15D))
  if (length(valid) < 50) return(NULL)

  list(
    oos = data.frame(
      Symbol   = sym,
      Target   = df$TargetBinary[valid],
      Fwd15D   = df$Fwd15D[valid],
      RS_20    = df$RS_20[valid],
      RS_60    = df$RS_60[valid],
      EffRatio = df$EfficiencyRatio_20D[valid],
      P_Base   = p_base_vec[valid],
      P_Enh    = p_enh_vec[valid],
      stringsAsFactors = FALSE
    ),
    betas = do.call(rbind, beta_enh_list)
  )
}

wf_res <- mclapply(res_list, eval_sym_wf, mc.cores = min(6, detectCores()))
wf_res <- Filter(Negate(is.null), wf_res)

oos_all  <- do.call(rbind, lapply(wf_res, function(x) x$oos))
beta_all <- do.call(rbind, lapply(wf_res, function(x) x$betas))

beta_agg <- aggregate(cbind(AbsStdBeta = abs(StdBeta), StdBeta = StdBeta, NonZero = (abs(StdBeta) > 1e-6)) ~ Feature,
                      data = beta_all, FUN = mean)
beta_agg$AbsStdBeta     <- round(beta_agg$AbsStdBeta, 5)
beta_agg$StdBeta        <- round(beta_agg$StdBeta, 5)
beta_agg$Selection_Pct  <- round(beta_agg$NonZero * 100, 1)
beta_agg$Is_Stage2_New  <- ifelse(beta_agg$Feature %in% top4_stage2, "★ NEW STAGE-2", "")
beta_agg <- beta_agg[order(-beta_agg$AbsStdBeta), c("Feature", "Is_Stage2_New", "AbsStdBeta", "StdBeta", "Selection_Pct")]

cat("\n========================================================================================\n")
cat(" 2. ELASTICNET STANDARDIZED BETA IMPORTANCE (30-FEATURE MODEL WITH STAGE-2 CANDIDATES)\n")
cat("========================================================================================\n")
print(head(beta_agg, 18), row.names = FALSE)
cat("\n--- Specifically the 4 Stage-2 Candidate Features ---\n")
print(beta_agg[beta_agg$Feature %in% top4_stage2, ], row.names = FALSE)

# Compare OOS Signal Performance
idx_base_long <- which(oos_all$P_Base >= 0.58 & oos_all$RS_20 > 0)
idx_enh_long  <- which(oos_all$P_Enh  >= 0.58 & oos_all$RS_20 > 0)
idx_enh_rs60  <- which(oos_all$P_Enh  >= 0.58 & oos_all$RS_20 > 0 & oos_all$RS_60 > -0.02)

idx_base_hyb  <- which(oos_all$P_Base >= 0.60 & oos_all$RS_20 >= 0.05)
idx_enh_hyb   <- which(oos_all$P_Enh  >= 0.60 & oos_all$RS_20 >= 0.05)
idx_enh_hyb60 <- which(oos_all$P_Enh  >= 0.60 & oos_all$RS_20 >= 0.05 & oos_all$RS_60 >= 0.0)

comp_df <- data.frame(
  Model_Gate = c(
    "Current 26-Feature: Bull Long (P >= 0.58 & RS20 > 0)",
    "Enhanced 30-Feature: Bull Long (P >= 0.58 & RS20 > 0)",
    "Enhanced 30-Feature + RS60 Gate (P >= 0.58 & RS20 > 0 & RS60 > -2%)",
    "Current 26-Feature: Hybrid Leader (P >= 0.60 & RS20 >= +5%)",
    "Enhanced 30-Feature: Hybrid Leader (P >= 0.60 & RS20 >= +5%)",
    "Enhanced 30-Feature + RS60 Leader (P >= 0.60 & RS20 >= +5% & RS60 >= 0%)"
  ),
  Signals = c(length(idx_base_long), length(idx_enh_long), length(idx_enh_rs60),
              length(idx_base_hyb), length(idx_enh_hyb), length(idx_enh_hyb60)),
  TB_WinRate_Pct = round(c(
    mean(oos_all$Target[idx_base_long]) * 100,
    mean(oos_all$Target[idx_enh_long]) * 100,
    mean(oos_all$Target[idx_enh_rs60]) * 100,
    mean(oos_all$Target[idx_base_hyb]) * 100,
    mean(oos_all$Target[idx_enh_hyb]) * 100,
    mean(oos_all$Target[idx_enh_hyb60]) * 100
  ), 2),
  Mean_15D_Ret_Bps = round(c(
    mean(oos_all$Fwd15D[idx_base_long]) * 10000,
    mean(oos_all$Fwd15D[idx_enh_long]) * 10000,
    mean(oos_all$Fwd15D[idx_enh_rs60]) * 10000,
    mean(oos_all$Fwd15D[idx_base_hyb]) * 10000,
    mean(oos_all$Fwd15D[idx_enh_hyb]) * 10000,
    mean(oos_all$Fwd15D[idx_enh_hyb60]) * 10000
  ), 1),
  Pos_15D_WinRate_Pct = round(c(
    mean(oos_all$Fwd15D[idx_base_long] > 0) * 100,
    mean(oos_all$Fwd15D[idx_enh_long] > 0) * 100,
    mean(oos_all$Fwd15D[idx_enh_rs60] > 0) * 100,
    mean(oos_all$Fwd15D[idx_base_hyb] > 0) * 100,
    mean(oos_all$Fwd15D[idx_enh_hyb] > 0) * 100,
    mean(oos_all$Fwd15D[idx_enh_hyb60] > 0) * 100
  ), 2),
  stringsAsFactors = FALSE
)

cat("\n========================================================================================\n")
cat(" 3. OUT-OF-SAMPLE WALK-FORWARD SIGNAL LIFT (CURRENT 26F VS. ENHANCED 30F + RS60 GATE)\n")
cat("========================================================================================\n")
print(comp_df, row.names = FALSE)
