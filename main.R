#!/usr/bin/env Rscript
#' Main Execution Script: Quantitative Swing Trading System
#'
#' Statistical Swing Trading Model with lmMA & GARCH Volatility
#'
#' Pipeline:
#'   1. Load stock data (Yahoo Finance live / cache fallback)
#'   2. Calculate Linear Model Moving Average (lmMA) trends (Fast & Slow)
#'   3. Fit GARCH(1,1) conditional volatility model
#'   4. Engineer scale-invariant features & forward swing target
#'   5. Train ElasticNet regularized logistic regression model
#'   6. Simulate out-of-sample swing trading strategy with volatility targeting
#'   7. Benchmark against Buy & Hold and Classic MA Crossover
#'   8. Export diagnostic charts and performance report

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(TTR)
  library(glmnet)
  library(tseries)
})

# Source module scripts
source("R/01_data_loader.R")
source("R/02_trend_lmMA.R")
source("R/03_volatility_garch.R")
source("R/04_feature_pipeline.R")
source("R/05_logistic_model.R")
source("R/06_swing_backtest.R")

cat("\n========================================================================\n")
cat(" QUANTITATIVE SWING TRADING SYSTEM FOR SNDK (SANDISK) STOCK\n")
cat(" Statistical Indicator Modernization & Volatility Regimes\n")
cat("========================================================================\n\n")

# System Parameters
SYMBOL       <- "SNDK"
FAST_N       <- 20     # Fast lmMA trend window
SLOW_N       <- 50     # Slow lmMA trend window
LOOK_AHEAD   <- 5      # Swing trading target horizon (5 days)
TRAIN_SPLIT  <- 0.70   # In-sample training ratio
P_LONG       <- 0.58   # Probability threshold for Long signal
P_SHORT      <- 0.42   # Probability threshold for Short signal
ALLOW_SHORT  <- FALSE  # Set TRUE for long/short, FALSE for long-only swing
TARGET_VOL   <- 0.30   # Target annualized volatility for sizing
CACHE_PATH   <- "data/data_sndk.rds"
OUTPUT_DIR   <- "output"

dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

# Step 1: Data Acquisition
cat("[Step 1/6] Loading price series...\n")
ohlcv <- load_stock_data(symbol = SYMBOL, cache_file = CACHE_PATH)
price <- Cl(ohlcv)

# Step 2: Feature Engineering (lmMA + GARCH(1,1))
cat("\n[Step 2/6] Building feature pipeline & GARCH(1,1) volatility...\n")
pipeline_out <- build_feature_dataset(
  ohlcv = ohlcv,
  fast_n = FAST_N,
  slow_n = SLOW_N,
  look_ahead = LOOK_AHEAD,
  use_garch = TRUE
)

df_model <- pipeline_out$model_data
feat_names <- pipeline_out$feature_names

# Step 3: Model Estimation (ElasticNet Logistic Regression)
cat("\n[Step 3/6] Fitting ElasticNet Logistic Regression Model...\n")
model_res <- fit_logistic_swing_model(
  df_model = df_model,
  feature_names = feat_names,
  train_split = TRAIN_SPLIT,
  alpha = 0.5,
  p_long = P_LONG,
  p_short = P_SHORT
)

# Step 4: Out-of-Sample Swing Trading Simulation
cat("\n[Step 4/6] Running out-of-sample swing trading backtest...\n")
backtest_res <- run_swing_backtest(
  ohlcv = ohlcv,
  model_res = model_res,
  pipeline_out = pipeline_out,
  allow_short = ALLOW_SHORT,
  target_vol = TARGET_VOL,
  max_leverage = 1.0,
  cost_bps = 10
)

# Step 5: Display Comprehensive Results
cat("\n========================================================================\n")
cat(" STATISTICAL MODEL ESTIMATION & COEFFICIENTS\n")
cat("========================================================================\n")

cat("\n1. Fitted ElasticNet Feature Coefficients (s = 'lambda.min'):\n")
print(model_res$coef_matrix)

cat("\n2. Confusion Matrix (Out-of-Sample Test Set):\n")
print(model_res$conf_matrix)

cat("\n3. Classification Evaluation Metrics:\n")
print(model_res$metrics)

cat("\n========================================================================\n")
cat(" OUT-OF-SAMPLE SWING TRADING PERFORMANCE COMPARISON\n")
cat("========================================================================\n")
print(backtest_res$performance_table, row.names = FALSE)

if (nrow(backtest_res$trade_log) > 0) {
  cat("\nRecent Swing Trades Log (Last 10 trades):\n")
  print(tail(backtest_res$trade_log, 10), row.names = FALSE)
}

# Step 6: Export Publication-Quality Diagnostic Visualizations
cat("\n[Step 6/6] Generating diagnostic and performance charts in 'output/'...\n")

# Chart 1: Price and Dual lmMA Trends
png(file.path(OUTPUT_DIR, "01_price_and_lmMA_trends.png"), width = 1200, height = 700, res = 120)
par(mfrow = c(2, 1), mar = c(3, 4, 3, 2))
plot(index(price), as.numeric(price), type = "l", col = "black", lwd = 1.5,
     main = sprintf("%s: Price with Fast lmMA(%d) and Slow lmMA(%d)", SYMBOL, FAST_N, SLOW_N),
     ylab = "Price (USD)", xlab = "")
lines(index(price), as.numeric(pipeline_out$dual_lm$fast_lm$fit), col = "blue", lwd = 1.5)
lines(index(price), as.numeric(pipeline_out$dual_lm$slow_lm$fit), col = "red", lwd = 1.5)
legend("topleft", legend = c("Price", sprintf("Fast lmMA(%d)", FAST_N), sprintf("Slow lmMA(%d)", SLOW_N)),
       col = c("black", "blue", "red"), lwd = 1.5, bty = "n")

plot(index(price), as.numeric(pipeline_out$dual_lm$dist_pct), type = "l", col = "darkgreen", lwd = 1.5,
     main = "Trend Divergence (%) [(Fast Fit - Slow Fit) / Slow Fit * 100]",
     ylab = "Divergence %", xlab = "Date")
abline(h = 0, lty = 2, col = "gray40")
dev.off()

# Chart 2: GARCH(1,1) Conditional Volatility & Shocks
rets <- na.omit(diff(log(price)))
garch_vol <- compute_garch_volatility(price)
png(file.path(OUTPUT_DIR, "02_garch_volatility.png"), width = 1200, height = 700, res = 120)
par(mfrow = c(2, 1), mar = c(3, 4, 3, 2))
plot(index(rets), as.numeric(rets) * 100, type = "h", col = "gray50",
     main = sprintf("%s: Daily Returns and GARCH(1,1) Conditional Volatility Bounds (95%%)", SYMBOL),
     ylab = "Return (%)", xlab = "")
lines(index(rets), 1.96 * as.numeric(garch_vol$sigma) * 100, col = "red", lwd = 1.5)
lines(index(rets), -1.96 * as.numeric(garch_vol$sigma) * 100, col = "red", lwd = 1.5)
legend("topleft", legend = c("Daily Return", "+/- 1.96 GARCH Sigma"), col = c("gray50", "red"), lty = 1, bty = "n")

plot(index(garch_vol$annualized_vol), as.numeric(garch_vol$annualized_vol) * 100, type = "l", col = "purple", lwd = 2,
     main = "GARCH(1,1) Annualized Conditional Volatility (%)", ylab = "Annualized Vol (%)", xlab = "Date")
abline(h = mean(as.numeric(garch_vol$annualized_vol) * 100), lty = 2, col = "gray40")
dev.off()

# Chart 3: Engineered Features Multi-Panel Dashboard
png(file.path(OUTPUT_DIR, "03_features_dashboard.png"), width = 1200, height = 800, res = 120)
par(mfrow = c(4, 1), mar = c(2.5, 4, 2, 2))
plot(df_model$Date, df_model$SlopeFast, type = "l", col = "blue", lwd = 1.5,
     main = "Feature: Fast lmMA Slope (Momentum)", ylab = "Slope")
abline(h = 0, lty = 2, col = "gray50")

plot(df_model$Date, df_model$TrendQuality, type = "l", col = "darkorange", lwd = 1.5,
     main = "Feature: Trend Quality (Fast Regression R-Squared)", ylab = "R-Squared", ylim = c(0, 1))

plot(df_model$Date, df_model$ZScore, type = "l", col = "darkgreen", lwd = 1.5,
     main = "Feature: Z-Score from Fast lmMA (Standardized Residuals)", ylab = "Z-Score")
abline(h = c(-2, 0, 2), lty = 2, col = c("red", "gray50", "red"))

plot(df_model$Date, df_model$GARCH_Vol * 100, type = "l", col = "purple", lwd = 1.5,
     main = "Feature: GARCH(1,1) Conditional Volatility (%)", ylab = "Vol %", xlab = "Date")
dev.off()

# Chart 4: Out-of-Sample Performance & Drawdown
png(file.path(OUTPUT_DIR, "04_backtest_equity_curves.png"), width = 1200, height = 800, res = 120)
par(mfrow = c(2, 1), mar = c(3, 4, 3, 2))
eq <- backtest_res$equity_curves
plot(index(eq), as.numeric(eq$ML_Swing_Strategy), type = "l", col = "blue", lwd = 2.5,
     ylim = range(as.numeric(eq)),
     main = sprintf("Out-of-Sample Swing Trading Equity Curves (%s to %s) [Base $10,000]",
                    as.character(first(index(eq))), as.character(last(index(eq)))),
     ylab = "Portfolio Value ($)", xlab = "")
lines(index(eq), as.numeric(eq$Buy_and_Hold), col = "gray50", lwd = 1.8, lty = 2)
lines(index(eq), as.numeric(eq$Classic_MA_Cross), col = "red", lwd = 1.8, lty = 3)
legend("topleft", legend = c("ML Swing Strategy (GARCH Vol-Targeted)", paste("Buy & Hold", SYMBOL), "Classic MA Cross"),
       col = c("blue", "gray50", "red"), lwd = c(2.5, 1.8, 1.8), lty = c(1, 2, 3), bty = "n")

# Drawdowns panel
eq_strat <- as.numeric(eq$ML_Swing_Strategy)
dd_strat <- (eq_strat - cummax(eq_strat)) / cummax(eq_strat) * 100
eq_bh <- as.numeric(eq$Buy_and_Hold)
dd_bh <- (eq_bh - cummax(eq_bh)) / cummax(eq_bh) * 100

min_dd <- min(c(dd_strat, dd_bh), na.rm = TRUE)
ylim_min <- if (!is.na(min_dd) && min_dd < 0) min_dd * 1.05 else -5

plot(index(eq), dd_strat, type = "l", col = "blue", lwd = 2,
     ylim = c(ylim_min, 0),
     main = "Drawdown Comparison (%)", ylab = "Drawdown %", xlab = "Date")
lines(index(eq), dd_bh, col = "gray50", lwd = 1.5, lty = 2)
abline(h = 0, lty = 1, col = "black")
legend("bottomleft", legend = c("ML Swing Strategy Drawdown", "Buy & Hold Drawdown"),
       col = c("blue", "gray50"), lwd = c(2, 1.5), lty = c(1, 2), bty = "n")
dev.off()

cat(sprintf("[Complete] All 4 publication-quality charts saved successfully in '%s/'\n\n", OUTPUT_DIR))
