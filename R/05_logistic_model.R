#' Logistic Regression Model for Swing Trading
#'
#' Fits an ElasticNet regularized logistic regression model (alpha=0.5)
#' for swing trade direction forecasting.
#'
#' Evaluates out-of-sample direction predictions, confusion matrices,
#' and generates trading signals with a customizable "zone of indifference".

suppressMessages({
  library(glmnet)
  library(xts)
})

#' Fit ElasticNet Logistic Model and Predict
#'
#' @param df_model Data frame containing target and features from pipeline.
#' @param feature_names Vector of column names to use as predictors.
#' @param train_split Fraction of data used for training (default: 0.70).
#' @param alpha ElasticNet mixing parameter (0 = Ridge, 1 = Lasso, 0.5 = ElasticNet).
#' @param p_long Threshold probability to trigger Long signal (default: 0.60).
#' @param p_short Threshold probability to trigger Short signal (default: 0.40).
#' @return A list containing:
#'   - cv_fit: Cross-validated glmnet model object
#'   - unreg_glm: Standard glm fit for statistical comparison
#'   - coef_matrix: Matrix of coefficients at lambda.min
#'   - train_idx: Training row indices
#'   - test_idx: Test row indices
#'   - pred_probs: Out-of-sample predicted probability of positive return
#'   - pred_class: Mapped signal (+1, 0, -1)
#'   - metrics: Evaluation metrics table (Accuracy, Precision, Recall, F1)
#'   - conf_matrix: Confusion matrix table
#' @export
fit_logistic_swing_model <- function(df_model,
                                     feature_names,
                                     train_split = 0.70,
                                     alpha = 0.5,
                                     p_long = 0.60,
                                     p_short = 0.40) {
  
  n <- nrow(df_model)
  n_train <- floor(n * train_split)
  # Embargo training window by 5 days to eliminate target leakage into test set
  train_idx <- 1:max(1, (n_train - 5))
  test_idx <- if (n_train >= n) integer(0) else (n_train + 1):n
  
  X <- as.matrix(df_model[, feature_names])
  y <- df_model$TargetBinary
  
  X_train <- X[train_idx, , drop = FALSE]
  y_train <- y[train_idx]
  X_test  <- X[test_idx, , drop = FALSE]
  y_test  <- y[test_idx]
  
  cat(sprintf("[LogisticModel] Training set: %d bars (%s to %s | 5-day embargo applied)\n",
              length(train_idx), df_model$Date[1], df_model$Date[max(train_idx)]))
  cat(sprintf("[LogisticModel] Testing set:  %d bars (%s to %s)\n",
              length(test_idx),
              if (length(test_idx) > 0) df_model$Date[test_idx[1]] else "NA",
              if (length(test_idx) > 0) df_model$Date[tail(test_idx, 1)] else "NA"))
  
  # 1. Fit Regularized Logistic Regression (ElasticNet)
  cat(sprintf("[LogisticModel] Fitting cv.glmnet with alpha=%.2f (ElasticNet)...\n", alpha))
  set.seed(42) # For reproducible fold assignment
  # Use chronological blocked folds to prevent lookahead leakage
  # (overlapping 5-day forward targets leak across random K-fold boundaries)
  nfolds <- 5
  foldid <- rep(1:nfolds, each = ceiling(length(train_idx) / nfolds))[1:length(train_idx)]
  cv_fit <- cv.glmnet(X_train, y_train, foldid = foldid, alpha = alpha, family = "binomial", type.measure = "deviance")
  
  # Extract coefficients at lambda.min
  coef_min <- as.matrix(coef(cv_fit, s = "lambda.min"))
  colnames(coef_min) <- "ElasticNet_Coef"
  
  # 2. Fit standard GLM for inference / reference
  train_df <- data.frame(Target = y_train, X_train)
  unreg_glm <- tryCatch({
    glm(Target ~ ., data = train_df, family = binomial(link = "logit"))
  }, error = function(e) NULL)
  
  # 3. Generate Out-of-Sample Predictions
  pred_probs <- predict(cv_fit, newx = X_test, s = "lambda.min", type = "response")
  pred_probs_vec <- as.numeric(pred_probs)
  
  # Signal mapping logic:
  # Long (+1) if P(Up) > p_long
  # Short (-1) if P(Up) < p_short
  # Neutral (0) if in between (zone of indifference / noise filter)
  pred_class <- ifelse(pred_probs_vec > p_long, 1,
                ifelse(pred_probs_vec < p_short, -1, 0))
  
  # Evaluation Metrics on Test Set
  actual_dir <- ifelse(y_test == 1, 1, -1)
  
  # Binary evaluation on directional bets (excluding neutral if evaluated purely on signals taken)
  conf_matrix <- table(
    Predicted = factor(pred_class, levels = c(-1, 0, 1)),
    Actual = factor(actual_dir, levels = c(-1, 1))
  )
  
  # Calculate standard metrics for positive class (Up = 1)
  tp <- sum(pred_class == 1 & actual_dir == 1)
  fp <- sum(pred_class == 1 & actual_dir == -1)
  fn <- sum(pred_class != 1 & actual_dir == 1)
  tn <- sum(pred_class != 1 & actual_dir == -1)
  
  # Overall accuracy on directional predictions
  active_mask <- (pred_class != 0)
  active_acc <- if (sum(active_mask) > 0) mean(pred_class[active_mask] == actual_dir[active_mask]) else NA
  
  accuracy  <- (tp + tn) / max(tp + fp + fn + tn, 1)
  precision <- if ((tp + fp) > 0) tp / (tp + fp) else 0
  recall    <- if ((tp + fn) > 0) tp / (tp + fn) else 0
  f1_score  <- if ((precision + recall) > 0) 2 * (precision * recall) / (precision + recall) else 0
  
  metrics <- data.frame(
    Metric = c("Overall Accuracy", "Active Signal Accuracy", "Precision (Long)", "Recall (Long)", "F1 Score"),
    Value = c(accuracy, active_acc, precision, recall, f1_score)
  )
  
  return(list(
    cv_fit = cv_fit,
    unreg_glm = unreg_glm,
    coef_matrix = coef_min,
    train_idx = train_idx,
    test_idx = test_idx,
    pred_probs = pred_probs_vec,
    pred_class = pred_class,
    actual_dir = actual_dir,
    test_dates = df_model$Date[test_idx],
    metrics = metrics,
    conf_matrix = conf_matrix,
    test_target_ret = df_model$TargetRet[test_idx]
  ))
}
