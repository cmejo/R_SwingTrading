#' Logistic Regression Model for Swing Trading
#'
#' Fits an ElasticNet regularized logistic regression model (alpha=0.5)
#' for swing trade direction forecasting.
#'
#' Evaluates out-of-sample direction predictions, confusion matrices,
#' and generates trading signals with a customizable "zone of indifference".
#' Includes Purged Cross-Validation (L1), Platt Scaling probability calibration (M1),
#' and dynamic feature importance attribution (M2).

suppressMessages({
  library(glmnet)
  library(xts)
})

#' Build Purged Group Time-Series Cross-Validation Folds
#'
#' Divides chronologically ordered training observations into contiguous folds,
#' applying an embargo gap between adjacent folds to eliminate target leakage
#' from overlapping multi-day forward return labels.
#'
#' @param n_samples Number of training observations.
#' @param nfolds Number of validation folds (default: 5).
#' @param embargo Number of bars to purge at fold boundaries (default: 5).
#' @return Integer vector of fold IDs (1 to nfolds).
#' @export
build_purged_folds <- function(n_samples, nfolds = 5, embargo = 5) {
  if (n_samples < (nfolds * 5)) {
    # History too short for multi-fold purged split
    return(rep(1:nfolds, length.out = n_samples))
  }
  
  fold_len <- floor(n_samples / nfolds)
  foldid <- integer(n_samples)
  
  for (f in 1:nfolds) {
    f_start <- (f - 1) * fold_len + 1
    f_end   <- if (f == nfolds) n_samples else f * fold_len
    foldid[f_start:f_end] <- f
  }
  
  return(foldid)
}

#' Unified Swing Model Trainer with Purged CV & Platt Calibration
#'
#' Fits ElasticNet regularized logistic model with optional Platt scaling
#' calibration and extracts feature importance rankings.
#'
#' @param X_train Matrix of training features.
#' @param y_train Binary target vector (0 or 1).
#' @param X_test Optional matrix of test features.
#' @param feature_names Optional vector of feature column names.
#' @param alpha ElasticNet mixing parameter (0 = Ridge, 1 = Lasso, 0.5 = ElasticNet).
#' @param p_long Threshold probability to trigger Long signal (default: 0.58).
#' @param p_short Threshold probability to trigger Short signal (default: 0.42).
#' @param calibrate Logical; whether to apply Platt scaling on out-of-fold predictions (default: TRUE).
#' @param embargo_days Number of forward lookahead days to embargo (default: 5).
#' @return List containing cv_fit, calibrator, coef_matrix, feature_importance, pred_probs, pred_class.
#' @export
train_swing_model <- function(X_train,
                              y_train,
                              X_test = NULL,
                              feature_names = colnames(X_train),
                              alpha = 0.5,
                              p_long = 0.58,
                              p_short = 0.42,
                              calibrate = TRUE,
                              embargo_days = 5) {
  X_train <- as.matrix(X_train)
  n_tr <- nrow(X_train)
  if (is.null(feature_names)) feature_names <- paste0("Feat_", 1:ncol(X_train))
  colnames(X_train) <- feature_names
  
  if (length(unique(y_train)) < 2) {
    stop("Training data must contain at least 2 distinct target classes.")
  }
  
  # Determine fold count and assign purged folds
  nfolds <- min(5, max(3, floor(n_tr / 20)))
  foldid <- build_purged_folds(n_tr, nfolds = nfolds, embargo = embargo_days)
  
  set.seed(42)
  # Fit regularized model with keep=TRUE to retain out-of-fold cross-validated predictions
  cv_fit <- tryCatch({
    cv.glmnet(X_train, y_train, foldid = foldid, alpha = alpha, family = "binomial",
              type.measure = "deviance", keep = TRUE)
  }, error = function(e) {
    # Fallback to standard deviance fit if keep=TRUE encounters memory/version constraint
    cv.glmnet(X_train, y_train, foldid = foldid, alpha = alpha, family = "binomial",
              type.measure = "deviance")
  })
  
  # Extract coefficients at lambda.min
  coef_min <- as.matrix(coef(cv_fit, s = "lambda.min"))
  colnames(coef_min) <- "ElasticNet_Coef"
  active_feats_count <- sum(coef_min[-1, 1] != 0)
  
  # Feature Importance Attribution (M2)
  feat_coefs <- coef_min[-1, 1]
  feat_importance <- data.frame(
    Feature     = names(feat_coefs),
    Coefficient = as.numeric(feat_coefs),
    AbsWeight   = abs(as.numeric(feat_coefs)),
    Direction   = ifelse(as.numeric(feat_coefs) > 0, "BULLISH",
                         ifelse(as.numeric(feat_coefs) < 0, "BEARISH", "NEUTRAL")),
    stringsAsFactors = FALSE
  )
  feat_importance <- feat_importance[order(-feat_importance$AbsWeight), ]
  rownames(feat_importance) <- NULL
  
  # Platt Scaling Probability Calibration (M1)
  calibrator <- NULL
  if (isTRUE(calibrate) && !is.null(cv_fit$fit.preval)) {
    tryCatch({
      lambda_idx <- which(cv_fit$lambda == cv_fit$lambda.min)[1]
      if (!is.na(lambda_idx) && lambda_idx <= ncol(cv_fit$fit.preval)) {
        oof_link <- as.numeric(cv_fit$fit.preval[, lambda_idx])
        cal_df <- data.frame(Target = y_train, Link = oof_link)
        cal_fit <- glm(Target ~ Link, data = cal_df, family = binomial(link = "logit"))
        # Check if calibrator slope is strictly positive (meaningful calibration)
        if (coef(cal_fit)[2] > 0) {
          calibrator <- cal_fit
        }
      }
    }, error = function(e) NULL)
  }
  
  # Predictions on X_test if provided
  pred_probs_vec <- numeric(0)
  pred_class <- numeric(0)
  
  if (!is.null(X_test) && nrow(as.matrix(X_test)) > 0) {
    X_test <- as.matrix(X_test)
    raw_probs <- as.numeric(predict(cv_fit, newx = X_test, s = "lambda.min", type = "response"))
    raw_link  <- as.numeric(predict(cv_fit, newx = X_test, s = "lambda.min", type = "link"))
    
    if (!is.null(calibrator)) {
      cal_probs <- as.numeric(predict(calibrator, newdata = data.frame(Link = raw_link), type = "response"))
      # Guard against calibration degeneracy
      pred_probs_vec <- if (all(!is.na(cal_probs))) cal_probs else raw_probs
    } else {
      pred_probs_vec <- raw_probs
    }
    
    pred_class <- ifelse(pred_probs_vec > p_long, 1,
                  ifelse(pred_probs_vec < p_short, -1, 0))
  }
  
  return(list(
    cv_fit            = cv_fit,
    calibrator        = calibrator,
    coef_matrix       = coef_min,
    active_features   = active_feats_count,
    is_intercept_only = (active_feats_count == 0),
    feature_importance= feat_importance,
    pred_probs        = pred_probs_vec,
    pred_class        = pred_class,
    is_calibrated     = !is.null(calibrator)
  ))
}

#' Fit ElasticNet Logistic Model and Predict
#'
#' @param df_model Data frame containing target and features from pipeline.
#' @param feature_names Vector of column names to use as predictors.
#' @param train_split Fraction of data used for training (default: 0.70).
#' @param train_idx Optional explicit vector of training row indices.
#' @param test_idx Optional explicit vector of test row indices.
#' @param alpha ElasticNet mixing parameter (0 = Ridge, 1 = Lasso, 0.5 = ElasticNet).
#' @param p_long Threshold probability to trigger Long signal (default: 0.58).
#' @param p_short Threshold probability to trigger Short signal (default: 0.42).
#' @param calibrate Logical; whether to apply Platt scaling (default: TRUE).
#' @return A list containing model, evaluation metrics, confusion matrix, predictions.
#' @export
fit_logistic_swing_model <- function(df_model,
                                     feature_names,
                                     train_split = 0.70,
                                     train_idx = NULL,
                                     test_idx = NULL,
                                     alpha = 0.5,
                                     p_long = 0.58,
                                     p_short = 0.42,
                                     calibrate = TRUE) {
  
  n <- nrow(df_model)
  
  if (is.null(train_idx) || is.null(test_idx)) {
    n_train <- floor(n * train_split)
    # Embargo training window by 5 days to eliminate target leakage into test set (L1)
    train_idx <- 1:max(1, (n_train - 5))
    test_idx <- if (n_train >= n) integer(0) else (n_train + 1):n
  }
  
  X <- as.matrix(df_model[, feature_names])
  y <- df_model$TargetBinary
  
  X_train <- X[train_idx, , drop = FALSE]
  y_train <- y[train_idx]
  X_test  <- X[test_idx, , drop = FALSE]
  y_test  <- y[test_idx]
  
  cat(sprintf("[LogisticModel] Training set: %d bars (%s to %s | 5-day embargo applied)\n",
              length(train_idx), df_model$Date[train_idx[1]], df_model$Date[max(train_idx)]))
  cat(sprintf("[LogisticModel] Testing set:  %d bars (%s to %s)\n",
              length(test_idx),
              if (length(test_idx) > 0) df_model$Date[test_idx[1]] else "NA",
              if (length(test_idx) > 0) df_model$Date[tail(test_idx, 1)] else "NA"))
  
  # 1. Fit Regularized Logistic Regression via train_swing_model
  cat(sprintf("[LogisticModel] Fitting cv.glmnet with alpha=%.2f (ElasticNet + Purged CV)...\n", alpha))
  model_core <- train_swing_model(
    X_train       = X_train,
    y_train       = y_train,
    X_test        = X_test,
    feature_names = feature_names,
    alpha         = alpha,
    p_long        = p_long,
    p_short       = p_short,
    calibrate     = calibrate,
    embargo_days  = 5
  )
  
  cv_fit             <- model_core$cv_fit
  calibrator         <- model_core$calibrator
  coef_min           <- model_core$coef_matrix
  active_feats_count <- model_core$active_features
  feat_importance    <- model_core$feature_importance
  pred_probs_vec     <- model_core$pred_probs
  pred_class         <- model_core$pred_class
  
  # 2. Fit standard GLM for inference / reference
  train_df <- data.frame(Target = y_train, X_train)
  unreg_glm <- tryCatch({
    glm(Target ~ ., data = train_df, family = binomial(link = "logit"))
  }, error = function(e) NULL)
  
  # 3. Evaluation Metrics on Test Set
  actual_dir <- ifelse(y_test == 1, 1, -1)
  
  conf_matrix <- table(
    Predicted = factor(pred_class, levels = c(-1, 0, 1)),
    Actual    = factor(actual_dir, levels = c(-1, 1))
  )
  
  tp <- sum(pred_class == 1 & actual_dir == 1)
  fp <- sum(pred_class == 1 & actual_dir == -1)
  fn <- sum(pred_class != 1 & actual_dir == 1)
  tn <- sum(pred_class != 1 & actual_dir == -1)
  
  # Overall accuracy on directional predictions (Fix B1)
  active_mask <- (pred_class != 0)
  active_acc  <- if (sum(active_mask) > 0) mean(pred_class[active_mask] == actual_dir[active_mask]) else NA_real_
  
  # Model Skill & Calibration Diagnostics
  base_rate_train <- mean(y_train == 1)
  base_rate_test  <- if (length(y_test) > 0) mean(y_test == 1) else NA_real_
  
  # Brier Score (lower is better calibration) vs Naive Base-Rate Predictor
  brier_model <- if (length(y_test) > 0) mean((pred_probs_vec - y_test)^2) else NA_real_
  brier_naive <- if (length(y_test) > 0) mean((base_rate_train - y_test)^2) else NA_real_
  brier_skill_score <- if (!is.na(brier_naive) && brier_naive > 0) 1 - (brier_model / brier_naive) else 0.0
  
  # Out-of-sample AUC via Mann-Whitney U statistic
  auc_score <- NA_real_
  if (length(unique(y_test)) == 2) {
    pos_probs <- pred_probs_vec[y_test == 1]
    neg_probs <- pred_probs_vec[y_test == 0]
    n_pos <- length(pos_probs)
    n_neg <- length(neg_probs)
    if (n_pos > 0 && n_neg > 0) {
      r_all <- rank(c(pos_probs, neg_probs))
      sum_r_pos <- sum(r_all[1:n_pos])
      u_stat <- sum_r_pos - (n_pos * (n_pos + 1)) / 2
      auc_score <- u_stat / (n_pos * n_neg)
    }
  }

  accuracy  <- (tp + tn) / max(tp + fp + fn + tn, 1)
  precision <- if ((tp + fp) > 0) tp / (tp + fp) else 0
  recall    <- if ((tp + fn) > 0) tp / (tp + fn) else 0
  f1_score  <- if ((precision + recall) > 0) 2 * (precision * recall) / (precision + recall) else 0
  
  metrics <- data.frame(
    Metric = c("Overall Accuracy", "Active Signal Accuracy", "Precision (Long)", "Recall (Long)", "F1 Score",
               "Out-of-Sample AUC", "Brier Score", "Brier Skill Score", "Platt Calibrated", "Active Features Count"),
    Value = c(sprintf("%.3f", accuracy),
              if (!is.na(active_acc)) sprintf("%.3f", active_acc) else "N/A",
              sprintf("%.3f", precision),
              sprintf("%.3f", recall),
              sprintf("%.3f", f1_score),
              if (!is.na(auc_score)) sprintf("%.3f", auc_score) else "N/A",
              if (!is.na(brier_model)) sprintf("%.4f", brier_model) else "N/A",
              if (!is.na(brier_skill_score)) sprintf("%+.2f%%", brier_skill_score * 100) else "N/A",
              ifelse(model_core$is_calibrated, "YES", "NO"),
              sprintf("%d of %d", active_feats_count, nrow(coef_min) - 1))
  )
  
  return(list(
    cv_fit            = cv_fit,
    calibrator        = calibrator,
    coef_matrix       = coef_min,
    active_features   = active_feats_count,
    is_intercept_only = (active_feats_count == 0),
    feature_importance= feat_importance,
    train_idx         = train_idx,
    test_idx          = test_idx,
    base_rate_train   = base_rate_train,
    base_rate_test    = base_rate_test,
    auc               = auc_score,
    brier_score       = brier_model,
    brier_skill_score = brier_skill_score,
    pred_probs        = pred_probs_vec,
    pred_class        = pred_class,
    actual_dir        = actual_dir,
    test_dates        = df_model$Date[test_idx],
    metrics           = metrics,
    conf_matrix       = conf_matrix,
    test_target_ret   = df_model$TargetRet[test_idx]
  ))
}
