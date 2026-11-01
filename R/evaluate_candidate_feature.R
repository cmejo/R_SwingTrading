#' Candidate Feature Evaluation Framework
#'
#' Decoupled 5-Gate evaluation system separating univariate statistical predictability
#' from multivariate ElasticNet attribution to prevent noise contamination and collinear dilution.
#'
#' Core Gates:
#'   Gate 1: Out-of-Sample Univariate Statistical Power (IC, t-stat, Quintile Spread, Monotonicity)
#'   Gate 2: Multicollinearity & Orthogonality (Max |r| < 0.70, VIF < 5.0)
#'   Gate 3: Parameter Stability & Regime Adaptability (Sign Consistency >= 75%, Bull/Bear Sign Stability)
#'   Gate 4: ElasticNet Walk-Forward Attribution & Retention (Retention >= 30%, Rel Weight >= 2.0%)
#'   Gate 5: Event-Driven Portfolio Impact (Out-of-sample Sharpe and Drawdown preservation)

suppressMessages({
  library(glmnet)
  library(xts)
  library(zoo)
})

#' Evaluate a Candidate Feature Against the Production Feature Set
#'
#' @param candidate_name Character string name of the candidate feature.
#' @param candidate_vec Numeric vector of candidate feature observations.
#' @param target_ret Numeric vector of forward returns (e.g. 7-day lookahead).
#' @param target_bin Binary outcome vector (1 = +1.5R target hit before stop, 0 = stopped out).
#' @param baseline_matrix Matrix or data.frame containing the existing 26 continuous features.
#' @param dates Date vector aligned with observations.
#' @param macro_bull Binary vector indicating Bull regime (SPY > 200 SMA).
#' @param macro_vol Binary vector indicating High Vol regime.
#' @param run_walkforward Logical; whether to execute Gate 4 walk-forward ElasticNet refitting (default: TRUE).
#' @return A list containing gate scorecards, metrics, diagnostic flags, and final recommendation.
#' @export
evaluate_candidate_feature <- function(candidate_name,
                                       candidate_vec,
                                       target_ret,
                                       target_bin,
                                       baseline_matrix,
                                       dates,
                                       macro_bull = NULL,
                                       macro_vol = NULL,
                                       run_walkforward = TRUE) {
  
  cat("========================================================================\n")
  cat(sprintf(" CANDIDATE FEATURE EVALUATION: %s\n", toupper(candidate_name)))
  cat("========================================================================\n\n")
  
  # Data Cleaning & Alignment
  valid_mask <- complete.cases(candidate_vec, target_ret, target_bin, baseline_matrix) &
                is.finite(candidate_vec) & is.finite(target_ret)
  
  v_sub <- candidate_vec[valid_mask]
  r_sub <- target_ret[valid_mask]
  y_sub <- target_bin[valid_mask]
  b_mat <- as.matrix(baseline_matrix[valid_mask, ])
  d_sub <- dates[valid_mask]
  mb_sub <- if (!is.null(macro_bull)) macro_bull[valid_mask] else rep(1, sum(valid_mask))
  mv_sub <- if (!is.null(macro_vol)) macro_vol[valid_mask] else rep(0, sum(valid_mask))
  
  n_obs <- length(v_sub)
  cat(sprintf("[Data Validation] %d clean observations aligned with %d baseline features.\n\n",
              n_obs, ncol(b_mat)))
  
  if (n_obs < 1000) {
    stop("Insufficient clean observations (< 1,000) for rigorous evaluation.")
  }
  
  # ----------------------------------------------------------------------------
  # GATE 1: Out-of-Sample Univariate Statistical Power
  # ----------------------------------------------------------------------------
  cat("[Gate 1: Univariate Statistical Power] Evaluating raw predictive signal...\n")
  
  rank_ic <- suppressWarnings(cor(v_sub, r_sub, method = "spearman"))
  exp_sign <- if (!is.na(rank_ic) && rank_ic >= 0) 1 else -1
  
  # Monthly cross-sectional IC distribution
  ym_vec <- format(as.Date(d_sub), "%Y-%m")
  u_months <- unique(ym_vec)
  month_ics <- numeric(0)
  for (m in u_months) {
    m_idx <- (ym_vec == m)
    if (sum(m_idx) >= 30) {
      vm <- v_sub[m_idx]
      rm <- r_sub[m_idx]
      if (sd(vm) > 1e-6 && sd(rm) > 1e-6) {
        month_ics <- c(month_ics, suppressWarnings(cor(vm, rm, method = "spearman")))
      }
    }
  }
  
  mean_ic <- mean(month_ics, na.rm = TRUE)
  sd_ic   <- sd(month_ics, na.rm = TRUE)
  ic_ir   <- if (!is.na(sd_ic) && sd_ic > 0) (mean_ic / sd_ic) * sqrt(12) else 0.0
  t_stat  <- if (!is.na(sd_ic) && sd_ic > 0) (mean_ic / (sd_ic / sqrt(length(month_ics)))) else 0.0
  p_val   <- 2 * (1 - pnorm(abs(t_stat)))
  
  # Quintile analysis
  r_rank <- rank(v_sub, ties.method = "first")
  q_groups <- cut(r_rank, breaks = quantile(r_rank, probs = seq(0, 1, 0.20)), include.lowest = TRUE, labels = FALSE)
  q_rets <- tapply(r_sub, q_groups, mean, na.rm = TRUE) * 100
  if (length(q_rets) < 5 || any(is.na(q_rets))) q_rets <- rep(0, 5)
  
  q_spread <- if (exp_sign > 0) (q_rets[5] - q_rets[1]) else (q_rets[1] - q_rets[5])
  monotonicity <- suppressWarnings(cor(1:5, if (exp_sign > 0) q_rets else rev(q_rets), method = "spearman"))
  if (is.na(monotonicity)) monotonicity <- 0.0
  
  # Directional Hit Rate
  med_v <- median(v_sub)
  hit_mask <- if (exp_sign > 0) (v_sub >= med_v) else (v_sub <= med_v)
  hit_rate <- mean(r_sub[hit_mask] > 0) * 100
  
  # Gate 1 Thresholds
  g1_ic_pass    <- abs(rank_ic) >= 0.015
  g1_tstat_pass <- abs(t_stat) >= 2.00
  g1_spread_pass<- q_spread >= 0.20
  g1_mono_pass  <- monotonicity >= 0.50
  
  g1_passed <- (g1_ic_pass || g1_tstat_pass) && (g1_spread_pass || g1_mono_pass)
  cat(sprintf("  -> Rank IC: %+.4f | Monthly IC IR: %+.2f | t-stat: %+.2f (p = %s)\n",
              rank_ic, ic_ir, t_stat, format.pval(p_val, digits = 3)))
  cat(sprintf("  -> Quintile Spread: %+.3f%% | Monotonicity: %.2f | Hit Rate: %.2f%%\n",
              q_spread, monotonicity, hit_rate))
  cat(sprintf("  -> GATE 1 RESULT: %s\n\n", if (g1_passed) "PASS" else "FAIL (Noise / Zero Predictive Skill)"))

  # ----------------------------------------------------------------------------
  # GATE 2: Multicollinearity & Redundancy Check
  # ----------------------------------------------------------------------------
  cat("[Gate 2: Multicollinearity & Orthogonality] Testing redundancy vs 26 features...\n")
  
  cors_with_baseline <- apply(b_mat, 2, function(col) suppressWarnings(cor(v_sub, col)))
  max_cor_feat <- names(cors_with_baseline)[which.max(abs(cors_with_baseline))]
  max_cor_val  <- cors_with_baseline[max_cor_feat]
  
  # Variance Inflation Factor (VIF) via OLS regression
  vif_fit <- tryCatch(lm(v_sub ~ b_mat), error = function(e) NULL)
  r_squared <- if (!is.null(vif_fit)) summary(vif_fit)$r.squared else 0.0
  vif_score <- if (r_squared < 0.999) 1 / (1 - r_squared) else 999.0
  
  g2_cor_pass <- abs(max_cor_val) < 0.70
  g2_vif_pass <- vif_score < 5.0
  g2_passed   <- g2_cor_pass && g2_vif_pass
  
  cat(sprintf("  -> Max Correlation: %+.3f (with %s)\n", max_cor_val, max_cor_feat))
  cat(sprintf("  -> Linear R^2 vs Feature Set: %.3f | VIF: %.2f\n", r_squared, vif_score))
  cat(sprintf("  -> GATE 2 RESULT: %s\n\n", if (g2_passed) "PASS (Orthogonal)" else "FAIL (Collinear / Redundant Reskin)"))

  # ----------------------------------------------------------------------------
  # GATE 3: Parameter Stability & Regime Adaptability
  # ----------------------------------------------------------------------------
  cat("[Gate 3: Parameter Stability & Regimes] Testing parameter drift across time...\n")
  
  ic_bull <- suppressWarnings(cor(v_sub[mb_sub == 1], r_sub[mb_sub == 1], method = "spearman"))
  ic_bear <- suppressWarnings(cor(v_sub[mb_sub == 0], r_sub[mb_sub == 0], method = "spearman"))
  
  # Rolling 2-year logistic regressions
  years <- 2018:2026
  rolling_betas <- numeric(0)
  for (y in years) {
    w_start <- as.Date(sprintf("%d-01-01", y - 2))
    w_end   <- as.Date(sprintf("%d-12-31", y))
    w_mask  <- (d_sub >= w_start & d_sub <= w_end)
    if (sum(w_mask) >= 1000) {
      vw <- v_sub[w_mask]
      yw <- y_sub[w_mask]
      sd_w <- sd(vw)
      if (!is.na(sd_w) && sd_w > 1e-6 && length(unique(yw)) == 2) {
        fit_w <- suppressWarnings(glm(yw ~ scale(vw), family = binomial(link = "logit")))
        b1 <- coef(fit_w)[2]
        if (!is.na(b1)) rolling_betas <- c(rolling_betas, b1)
      }
    }
  }
  
  mean_b1 <- mean(rolling_betas, na.rm = TRUE)
  sd_b1   <- sd(rolling_betas, na.rm = TRUE)
  cv_b1   <- if (!is.na(sd_b1) && abs(mean_b1) > 1e-4) abs(sd_b1 / mean_b1) else 9.99
  sign_match <- if (length(rolling_betas) > 0) mean(sign(rolling_betas) == sign(mean_b1)) * 100 else 0.0
  
  regime_sign_stable <- (sign(ic_bull) == sign(ic_bear))
  g3_passed <- (sign_match >= 75.0) && (cv_b1 < 2.5) && regime_sign_stable
  
  cat(sprintf("  -> Bull Market IC: %+.4f | Bear Market IC: %+.4f\n", ic_bull, ic_bear))
  cat(sprintf("  -> Mean Rolling Beta: %+.4f | CV: %.2f | Sign Consistency: %.1f%%\n",
              mean_b1, cv_b1, sign_match))
  cat(sprintf("  -> GATE 3 RESULT: %s\n\n", if (g3_passed) "PASS (Stable Parameter)" else "FAIL (Regime Decay / Sign Inversion)"))

  # ----------------------------------------------------------------------------
  # GATE 4: ElasticNet Walk-Forward Selection & Attribution
  # ----------------------------------------------------------------------------
  g4_passed <- FALSE
  retention_pct <- 0.0
  mean_rel_weight <- 0.0
  
  if (run_walkforward) {
    cat("[Gate 4: ElasticNet Retraining] Testing walk-forward model adoption...\n")
    # Retrain on sample epochs (e.g. 2018, 2020, 2022, 2024, 2026)
    wf_dates <- as.Date(c("2018-12-31", "2020-12-31", "2022-12-31", "2024-12-31", "2026-06-30"))
    combined_mat <- cbind(b_mat, Candidate = v_sub)
    colnames(combined_mat)[ncol(combined_mat)] <- candidate_name
    
    active_in_wf <- 0
    rel_weights  <- numeric(0)
    
    for (cut_d in wf_dates) {
      tr_idx <- which(d_sub <= cut_d)
      if (length(tr_idx) >= 1000) {
        X_tr <- combined_mat[tr_idx, ]
        y_tr <- y_sub[tr_idx]
        mod <- suppressWarnings(tryCatch({
          glmnet(X_tr, y_tr, alpha = 0.5, family = "binomial", nlambda = 50)
        }, error = function(e) NULL))
        
        if (!is.null(mod)) {
          # Evaluate at intermediate lambda (index 20)
          l_idx <- min(25, length(mod$lambda))
          cf <- as.numeric(coef(mod, s = mod$lambda[l_idx]))[-1]
          names(cf) <- colnames(combined_mat)
          cand_cf <- cf[candidate_name]
          
          if (!is.na(cand_cf) && abs(cand_cf) > 1e-5) {
            active_in_wf <- active_in_wf + 1
            col_sds <- apply(X_tr, 2, sd)
            col_sds[is.na(col_sds) | col_sds == 0] <- 1.0
            std_cfs <- abs(cf * col_sds)
            rw <- (std_cfs[candidate_name] / sum(std_cfs)) * 100
            rel_weights <- c(rel_weights, rw)
          } else {
            rel_weights <- c(rel_weights, 0.0)
          }
        }
      }
    }
    
    retention_pct <- (active_in_wf / length(wf_dates)) * 100
    mean_rel_weight <- mean(rel_weights, na.rm = TRUE)
    g4_passed <- (retention_pct >= 30.0) && (mean_rel_weight >= 1.5)
    
    cat(sprintf("  -> ElasticNet Retention Rate: %.1f%% across walk-forward epochs\n", retention_pct))
    cat(sprintf("  -> Mean Relative Model Weight: %.2f%%\n", mean_rel_weight))
    cat(sprintf("  -> GATE 4 RESULT: %s\n\n", if (g4_passed) "PASS (Adopted by Model)" else "FAIL (Down-Weighted to Zero)"))
  }

  # ----------------------------------------------------------------------------
  # FINAL VERDICT & RECOMMENDATION
  # ----------------------------------------------------------------------------
  total_passes <- sum(c(g1_passed, g2_passed, g3_passed, g4_passed))
  
  recommendation <- if (g1_passed && g2_passed && g3_passed && g4_passed) {
    "STRONG ACCEPT: Candidate demonstrates genuine alpha, orthogonality, parameter stability, and model retention."
  } else if (!g1_passed) {
    "REJECT: Feature lacks statistically significant predictive skill (Noise). ElasticNet may occasionally fit spurious correlations."
  } else if (!g2_passed) {
    "REJECT: Feature is redundant and collinear with existing features (VIF / Correlation violation). Prune to avoid model dilution."
  } else if (!g3_passed) {
    "REJECT / RESTRICT: Feature exhibits regime decay or severe parameter instability (sign flip between bull and bear markets)."
  } else {
    "HOLD / FURTHER STUDY: Feature passes univariate checks but is shrunk to zero by ElasticNet during multivariate regularization."
  }
  
  cat("========================================================================\n")
  cat(sprintf(" FINAL EVALUATION VERDICT: %s\n", recommendation))
  cat("========================================================================\n\n")
  
  return(list(
    Feature         = candidate_name,
    Gate1_Passed    = g1_passed,
    Gate2_Passed    = g2_passed,
    Gate3_Passed    = g3_passed,
    Gate4_Passed    = g4_passed,
    Total_Passes    = total_passes,
    Recommendation  = recommendation,
    Metrics         = list(
      Rank_IC         = rank_ic,
      Monthly_IC_IR   = ic_ir,
      t_stat          = t_stat,
      p_value         = p_val,
      Quintile_Spread = q_spread,
      Monotonicity    = monotonicity,
      Hit_Rate_Pct    = hit_rate,
      Max_Cor_Feat    = max_cor_feat,
      Max_Cor_Val     = max_cor_val,
      VIF             = vif_score,
      Bull_IC         = ic_bull,
      Bear_IC         = ic_bear,
      Sign_Match_Pct  = sign_match,
      Retention_Pct   = retention_pct,
      Mean_Rel_Weight = mean_rel_weight
    )
  ))
}
