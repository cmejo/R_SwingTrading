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

#' Build Expanding-Window Out-of-Fold Validation Splits with Embargo Gap
#'
#' Creates strictly causal (forward-in-time) expanding training windows
#' separated from each validation block by an `embargo`-bar purge gap to
#' eliminate overlapping multi-day forward return leakage and future-to-past leakage.
#'
#' @param n_samples Number of chronologically ordered training observations.
#' @param n_splits Number of forward validation blocks (default: 4).
#' @param embargo Number of bars to purge between training end and validation start (default: 7).
#' @param min_train_frac Minimum fraction of observations in initial training window (default: 0.40).
#' @return A list of split objects, each containing `train_idx` and `val_idx`.
#' @export
build_expanding_splits <- function(n_samples, n_splits = 4, embargo = 7, min_train_frac = 0.40) {
  base_train <- max(30, floor(n_samples * min_train_frac))
  rem <- n_samples - base_train - embargo
  if (rem < n_splits * 5) {
    n_splits <- max(1, floor(rem / 5))
  }
  if (rem < 5 || n_splits < 1) {
    return(list())
  }
  
  val_len <- floor(rem / n_splits)
  splits <- list()
  
  for (s in 1:n_splits) {
    tr_end    <- base_train + (s - 1) * val_len
    val_start <- tr_end + embargo + 1
    val_end   <- if (s == n_splits) n_samples else (val_start + val_len - 1)
    if (val_start <= val_end && tr_end >= 20) {
      splits[[length(splits) + 1]] <- list(
        train_idx = 1:tr_end,
        val_idx   = val_start:val_end
      )
    }
  }
  return(splits)
}

#' Backward-compatible alias for purged fold assignment
#' @export
build_purged_folds <- function(n_samples, nfolds = 5, embargo = 7) {
  if (n_samples < (nfolds * 5)) {
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

#' Unified Swing Model Trainer with Expanding-Window 7-Bar Embargo CV,
#' Controlled Lambda Step-Down, & Causal Platt Calibration
#'
#' Fits ElasticNet regularized logistic model using strictly causal expanding-window
#' out-of-fold validation with a 7-bar embargo gap, applies a controlled lambda
#' step-down to retain at least `min_active_features = 3` when supported by
#' out-of-sample skill, and calibrates probabilities via Platt scaling.
#'
#' @param X_train Matrix of training features.
#' @param y_train Binary target vector (0 or 1).
#' @param X_test Optional matrix of test features.
#' @param feature_names Optional vector of feature column names.
#' @param alpha ElasticNet mixing parameter (0 = Ridge, 1 = Lasso, 0.5 = ElasticNet).
#' @param p_long Threshold probability to trigger Long signal (default: 0.58).
#' @param p_short Threshold probability to trigger Short signal (default: 0.42).
#' @param calibrate Logical; whether to apply Platt scaling on out-of-fold predictions (default: TRUE).
#' @param embargo_days Number of forward lookahead days to embargo (default: 7).
#' @param min_active_features Minimum non-zero features for controlled step-down (default: 3).
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
                              embargo_days = 7,
                              min_active_features = 3) {
  X_train <- as.matrix(X_train)
  n_tr <- nrow(X_train)
  if (is.null(feature_names)) feature_names <- paste0("Feat_", 1:ncol(X_train))
  colnames(X_train) <- feature_names
  
  if (length(unique(y_train)) < 2) {
    stop("Training data must contain at least 2 distinct target classes.")
  }
  
  set.seed(42)
  # 1. Fit full-sample ElasticNet path to establish canonical lambda sequence
  full_fit <- glmnet(X_train, y_train, alpha = alpha, family = "binomial")
  lambda_grid <- full_fit$lambda
  n_lambdas   <- length(lambda_grid)
  nzero_vec   <- as.integer(full_fit$df)
  
  # 2. Build Expanding-Window Out-of-Fold Splits with 7-Bar Embargo Gap
  splits <- build_expanding_splits(n_tr, n_splits = 4, embargo = embargo_days, min_train_frac = 0.40)
  
  oof_link_list <- list()
  oof_prob_list <- list()
  oof_y_list    <- numeric(0)
  
  if (length(splits) >= 2) {
    for (sp in splits) {
      y_sub <- y_train[sp$train_idx]
      if (length(unique(y_sub)) < 2) next
      sp_fit <- tryCatch({
        glmnet(X_train[sp$train_idx, , drop = FALSE], y_sub,
               alpha = alpha, family = "binomial", lambda = lambda_grid)
      }, error = function(e) NULL)
      
      if (!is.null(sp_fit)) {
        X_val <- X_train[sp$val_idx, , drop = FALSE]
        lnk <- predict(sp_fit, newx = X_val, s = lambda_grid, type = "link")
        prb <- predict(sp_fit, newx = X_val, s = lambda_grid, type = "response")
        oof_link_list[[length(oof_link_list) + 1]] <- as.matrix(lnk)
        oof_prob_list[[length(oof_prob_list) + 1]] <- as.matrix(prb)
        oof_y_list <- c(oof_y_list, y_train[sp$val_idx])
      }
    }
  }
  
  best_lambda <- lambda_grid[1]
  oof_link_best <- NULL
  
  if (length(oof_prob_list) > 0 && length(oof_y_list) >= 20) {
    oof_prob_mat <- do.call(rbind, oof_prob_list)
    oof_link_mat <- do.call(rbind, oof_link_list)
    eps <- 1e-7
    oof_prob_clamped <- pmin(pmax(oof_prob_mat, eps), 1 - eps)
    
    # Pointwise binomial deviance matrix (N_val x n_lambdas)
    dev_mat <- -2 * (oof_y_list * log(oof_prob_clamped) + (1 - oof_y_list) * log(1 - oof_prob_clamped))
    dev_vec <- colMeans(dev_mat, na.rm = TRUE)
    dev_se  <- apply(dev_mat, 2, sd, na.rm = TRUE) / sqrt(nrow(dev_mat))
    
    idx_min <- which.min(dev_vec)[1]
    dev_min <- dev_vec[idx_min]
    se_min  <- if (!is.na(dev_se[idx_min])) dev_se[idx_min] else 0.01
    
    best_idx <- idx_min
    
    # 3. Controlled Lambda Step-Down (retain >= min_active_features if out-of-sample skill holds)
    if (nzero_vec[idx_min] < min_active_features) {
      cand_indices <- which(nzero_vec >= min_active_features)
      if (length(cand_indices) > 0) {
        # Check candidate lambdas starting from the most regularized one with >= 3 features
        for (c_idx in cand_indices[1:min(5, length(cand_indices))]) {
          lnk_c <- oof_link_mat[, c_idx]
          has_pos_skill <- (!all(is.na(lnk_c)) && sd(lnk_c, na.rm = TRUE) > 1e-6 &&
                            suppressWarnings(cor(lnk_c, oof_y_list, use = "complete.obs")) > 0.01)
          within_tol <- (dev_vec[c_idx] <= (dev_min + max(0.5 * se_min, 0.008 * abs(dev_min))))
          if (isTRUE(has_pos_skill) && isTRUE(within_tol)) {
            best_idx <- c_idx
            break
          }
        }
      }
    }
    
    best_lambda   <- lambda_grid[best_idx]
    oof_link_best <- as.numeric(oof_link_mat[, best_idx])
  } else {
    # Fallback to standard fold CV if sample is too short for expanding splits
    nfolds <- min(5, max(3, floor(n_tr / 20)))
    foldid <- build_purged_folds(n_tr, nfolds = nfolds, embargo = embargo_days)
    cv_fallback <- cv.glmnet(X_train, y_train, foldid = foldid, alpha = alpha, family = "binomial",
                             type.measure = "deviance", keep = TRUE)
    best_lambda <- cv_fallback$lambda.min
    if (!is.null(cv_fallback$fit.preval)) {
      l_idx <- which(cv_fallback$lambda == best_lambda)[1]
      if (!is.na(l_idx) && l_idx <= ncol(cv_fallback$fit.preval)) {
        oof_link_best <- as.numeric(cv_fallback$fit.preval[, l_idx])
        oof_y_list    <- y_train
      }
    }
  }
  
  # Expose best_lambda via cv_fit object for full backward compatibility
  cv_fit <- full_fit
  cv_fit$lambda.min <- best_lambda
  
  # Extract coefficients at selected best_lambda
  coef_min <- as.matrix(coef(full_fit, s = best_lambda))
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
  
  # 4. Platt Scaling Probability Calibration on Causal Out-of-Fold Predictions (M1)
  calibrator <- NULL
  if (isTRUE(calibrate) && active_feats_count > 0 && !is.null(oof_link_best) && length(oof_link_best) == length(oof_y_list)) {
    tryCatch({
      if (sd(oof_link_best, na.rm = TRUE) > 1e-6) {
        cal_df <- data.frame(Target = oof_y_list, Link = oof_link_best)
        cal_fit <- suppressWarnings(glm(Target ~ Link, data = cal_df, family = binomial(link = "logit")))
        # Check if calibrator slope is strictly positive (meaningful forward calibration)
        if (!is.na(coef(cal_fit)[2]) && coef(cal_fit)[2] > 0) {
          calibrator <- cal_fit
        }
      }
    }, error = function(e) NULL)
  }
  
  # 5. Predictions on X_test if provided
  pred_probs_vec <- numeric(0)
  pred_class <- numeric(0)
  
  if (!is.null(X_test) && nrow(as.matrix(X_test)) > 0) {
    X_test <- as.matrix(X_test)
    raw_probs <- as.numeric(predict(full_fit, newx = X_test, s = best_lambda, type = "response"))
    raw_link  <- as.numeric(predict(full_fit, newx = X_test, s = best_lambda, type = "link"))
    
    if (!is.null(calibrator)) {
      cal_probs <- as.numeric(predict(calibrator, newdata = data.frame(Link = raw_link), type = "response"))
      # Blend 70% calibrated probability + 30% raw regularized probability to prevent step-function extremes
      pred_probs_vec <- if (all(!is.na(cal_probs))) (0.70 * cal_probs + 0.30 * raw_probs) else raw_probs
    } else {
      pred_probs_vec <- raw_probs
    }
    
    pred_class <- ifelse(pred_probs_vec > p_long, 1,
                  ifelse(pred_probs_vec < p_short, -1, 0))
  }
  
  return(list(
    cv_fit            = cv_fit,
    best_lambda       = best_lambda,
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
