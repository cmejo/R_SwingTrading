suppressMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(glmnet)
})

source("R/01_data_loader.R")
source("R/02_trend_lmMA.R")
source("R/03_volatility_garch.R")
source("R/04_feature_pipeline.R")
source("R/05_logistic_model.R")

cat("========================================================================\n")
cat(" EVALUATING FORWARD ACCURACY OF FEATURES & SIGNALS (7-DAY HORIZON)\n")
cat("========================================================================\n\n")

# Load benchmark
spy_ohlcv <- load_stock_data("SPY", from = "2020-01-01", to = "2026-10-13")

# Representative diverse basket of symbols with cached data
symbols <- c("NVDA", "AMD", "TSM", "AAPL", "MSFT", "AMZN", "META", "GOOGL", 
             "PANW", "CRWD", "QQQ", "SPY", "XLK", "SOXL", "SMH", "COIN", "MSTR", "GE")

cache_rds <- "scratch/cached_eval_master_df.rds"
if (file.exists(cache_rds)) {
  cat(sprintf("[Cache] Loading cached master feature dataset from %s...\n", cache_rds))
  master_df <- readRDS(cache_rds)
} else {
  all_obs <- list()
  for (sym in symbols) {
    ohlcv <- tryCatch(load_stock_data(sym, from = "2020-01-01", to = "2026-10-13"), error = function(e) NULL)
    if (is.null(ohlcv) || nrow(ohlcv) < 200) next
    
    ds <- tryCatch(build_feature_dataset(ohlcv, benchmark_ohlcv = spy_ohlcv, 
                                        fast_n = 20, slow_n = 50, look_ahead = 7, 
                                        use_garch = TRUE), error = function(e) NULL)
    if (is.null(ds)) next
    
    df <- ds$model_data
    df$Symbol <- sym
    df$Date <- as.Date(ds$dates)
    all_obs[[sym]] <- df
  }
  master_df <- do.call(rbind, all_obs)
  saveRDS(master_df, cache_rds)
}
cat(sprintf("Aggregated %d observations across liquid symbols [2020-2026].\n\n", nrow(master_df)))

feature_cols <- setdiff(colnames(master_df), c("TargetRet", "TargetClass", "TargetBinary", "Symbol", "Date", "SectorSpread"))

# 1. Feature Information Coefficient (IC) & Predictive Power
ic_list <- list()
for (feat in feature_cols) {
  vals <- master_df[[feat]]
  target <- master_df$TargetRet
  valid <- complete.cases(vals, target) & is.finite(vals) & is.finite(target)
  
  if (sum(valid) > 100) {
    v_vec <- as.numeric(vals[valid])
    t_vec <- as.numeric(target[valid])
    
    sp_cor <- cor(v_vec, t_vec, method = "spearman")
    pe_cor <- cor(v_vec, t_vec, method = "pearson")
    n_pts <- length(v_vec)
    t_stat <- sp_cor * sqrt((n_pts - 2) / (1 - sp_cor^2))
    p_val <- 2 * (1 - pt(abs(t_stat), df = n_pts - 2))
    
    q <- as.numeric(quantile(v_vec, probs = seq(0, 1, 0.2), na.rm = TRUE))
    uq <- unique(q)
    if (length(uq) >= 3 && sd(v_vec) > 1e-6) {
      q_bin <- cut(v_vec, breaks = uq, include.lowest = TRUE, labels = FALSE)
      q1_ret <- mean(t_vec[q_bin == 1], na.rm = TRUE) * 100
      q5_ret <- mean(t_vec[q_bin == max(q_bin, na.rm=TRUE)], na.rm = TRUE) * 100
      spread <- q5_ret - q1_ret
    } else {
      q1_ret <- NA
      q5_ret <- NA
      spread <- NA
    }
    
    ic_list[[feat]] <- data.frame(
      Feature = feat,
      Spearman_IC = sp_cor,
      Pearson_r = pe_cor,
      t_stat = t_stat,
      p_value = p_val,
      Q1_FwdRet = q1_ret,
      Q5_FwdRet = q5_ret,
      Q5_Q1_Spread = spread,
      stringsAsFactors = FALSE
    )
  }
}

ic_df <- do.call(rbind, ic_list)
ic_df <- ic_df[order(-abs(ic_df$Spearman_IC)), ]

cat("--- 1. INFORMATION COEFFICIENT (IC) & QUINTILE SPREAD RANKING ---\n")
print(ic_df, row.names = FALSE, digits = 3)

# 2. Walk-Forward Logistic Signal Calibration & Accuracy
cat("\n--- 2. WALK-FORWARD OUT-OF-SAMPLE SIGNAL CALIBRATION & ACCURACY ---\n")
# Train walk-forward models on rolling 500-day windows and predict out-of-sample
test_preds <- list()

# Sort by date
master_df <- master_df[order(master_df$Date), ]
unique_dates <- sort(unique(master_df$Date))

# Run monthly walk-forward re-estimation over out-of-sample period (2023 - 2026)
eval_dates <- unique_dates[unique_dates >= as.Date("2023-01-01")]
step_dates <- eval_dates[seq(1, length(eval_dates), by = 21)] # ~monthly steps

for (i in seq_along(step_dates)) {
  cur_dt <- step_dates[i]
  train_data <- master_df[master_df$Date < cur_dt & master_df$Date >= (cur_dt - 730), ] # 2-year rolling train
  # Out-of-sample chunk (next 21 trading days)
  next_dt <- if (i < length(step_dates)) step_dates[i+1] else max(unique_dates)
  test_data <- master_df[master_df$Date >= cur_dt & master_df$Date < next_dt, ]
  
  if (nrow(train_data) < 300 || nrow(test_data) == 0) next
  
  # Purge overlap (7 days lookahead)
  train_data <- train_data[train_data$Date <= (cur_dt - 7), ]
  
  X_train <- as.matrix(train_data[, feature_cols])
  y_train <- as.numeric(train_data$TargetBinary)
  
  X_test  <- as.matrix(test_data[, feature_cols])
  
  # Clean NA/Inf
  for (j in 1:ncol(X_train)) {
    X_train[is.na(X_train[, j]) | !is.finite(X_train[, j]), j] <- 0
    X_test[is.na(X_test[, j]) | !is.finite(X_test[, j]), j] <- 0
  }
  
  fit_cv <- tryCatch(cv.glmnet(X_train, y_train, family = "binomial", alpha = 0.5, nfolds = 5), error = function(e) NULL)
  if (is.null(fit_cv)) next
  
  preds <- as.numeric(predict(fit_cv, newx = X_test, s = "lambda.min", type = "response"))
  test_data$ProbUp <- preds
  test_preds[[i]] <- test_data[, c("Date", "Symbol", "ProbUp", "TargetBinary", "TargetRet", "SlopeFast", "DistPct")]
}

all_test <- do.call(rbind, test_preds)
all_test$ProbUp <- as.numeric(all_test$ProbUp)
all_test$TargetBinary <- as.numeric(all_test$TargetBinary)
all_test$TargetRet <- as.numeric(all_test$TargetRet)
cat(sprintf("Evaluated %d out-of-sample predictions across 2023-2026 walk-forward tests.\n\n", nrow(all_test)))

# Calibration table by predicted probability bucket
all_test$ProbBucket <- cut(
  all_test$ProbUp, 
  breaks = c(0, 0.45, 0.50, 0.55, 0.60, 0.65, 0.70, 1.0),
  labels = c("<45%", "45-50%", "50-55%", "55-60%", "60-65%", "65-70%", ">70%"),
  include.lowest = TRUE
)

calib_summary <- aggregate(
  cbind(TargetBinary, TargetRet) ~ ProbBucket, 
  data = all_test, 
  FUN = function(x) c(Mean = mean(x, na.rm=TRUE), Count = length(x))
)

calib_df <- data.frame(
  ProbBucket = calib_summary$ProbBucket,
  Count      = calib_summary$TargetBinary[, "Count"],
  WinRate    = calib_summary$TargetBinary[, "Mean"] * 100,
  Avg7dRet   = calib_summary$TargetRet[, "Mean"] * 100
)

# Compute Win/Loss ratio and Profit Factor for each bucket
pf_list <- c()
payoff_list <- c()
for (b in calib_df$ProbBucket) {
  sub <- all_test[all_test$ProbBucket == b, ]
  wins <- sub$TargetRet[sub$TargetRet > 0]
  loss <- sub$TargetRet[sub$TargetRet <= 0]
  pf <- if (length(loss) > 0 && sum(abs(loss)) > 0) sum(wins) / sum(abs(loss)) else NA
  payoff <- if (length(loss) > 0 && mean(abs(loss)) > 0) mean(wins) / mean(abs(loss)) else NA
  pf_list <- c(pf_list, pf)
  payoff_list <- c(payoff_list, payoff)
}
calib_df$ProfitFactor <- pf_list
calib_df$PayoffRatio  <- payoff_list

cat("--- PROBABILITY BUCKET CALIBRATION & FORWARD HIT RATE ---\n")
print(calib_df, row.names = FALSE, digits = 3)

# 3. Model Performance at Active Trading Threshold (Prob >= 65% + Trend Filter)
active_signals <- all_test[all_test$ProbUp >= 0.65 & all_test$SlopeFast > 0 & all_test$DistPct > -2.0, ]
baseline_all   <- all_test

cat("\n--- 3. ACTIVE SIGNAL THRESHOLD (Prob >= 65% + Trend Filter) VS BASELINE ---\n")
cat(sprintf("Baseline All Days:     Win Rate: %.1f%% | Avg 7d Return: %+.2f%% | Total Samples: %d\n",
            mean(baseline_all$TargetBinary) * 100, mean(baseline_all$TargetRet) * 100, nrow(baseline_all)))
cat(sprintf("Active Buy Signals:    Win Rate: %.1f%% | Avg 7d Return: %+.2f%% | Total Samples: %d\n",
            mean(active_signals$TargetBinary) * 100, mean(active_signals$TargetRet) * 100, nrow(active_signals)))
cat(sprintf("Edge over Baseline:    Win Rate Delta: %+.1f%% | Return Delta: %+.2f%%\n",
            (mean(active_signals$TargetBinary) - mean(baseline_all$TargetBinary)) * 100,
            (mean(active_signals$TargetRet) - mean(baseline_all$TargetRet)) * 100))

# AUC-ROC and Brier Score
brier <- mean((all_test$ProbUp - all_test$TargetBinary)^2)
# Approximate AUC via Wilcoxon Mann-Whitney U test statistic
n1 <- sum(all_test$TargetBinary == 1)
n0 <- sum(all_test$TargetBinary == 0)
r <- rank(all_test$ProbUp)
auc <- (sum(r[all_test$TargetBinary == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)

cat(sprintf("\nOut-of-Sample AUC-ROC:  %.3f (Benchmark: 0.500 Random, >0.550 Strong in Daily Equities)\n", auc))
cat(sprintf("Out-of-Sample Brier:    %.3f (Lower is better; 0.250 is uninformed coin toss)\n", brier))
