#!/usr/bin/env Rscript
#' Monthly Strategy Performance & Health Evaluation Tool
#'
#' Evaluates the recent 30/60/90-day performance of the quantitative swing strategy,
#' comparing realized return, win rate, profit factor, and maximum drawdown against
#' Buy & Hold to verify if the strategy is operating nominally.
#'
#' Usage:
#'   Rscript monthly_review.R [--symbol=SNDK] [--days=30]

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(glmnet)
  library(tseries)
  library(TTR)
})

source("R/01_data_loader.R")
source("R/02_trend_lmMA.R")
source("R/03_volatility_garch.R")
source("R/04_feature_pipeline.R")
source("R/05_logistic_model.R")
source("R/06_swing_backtest.R")
source("R/08_metrics.R")

SYMBOL <- "SNDK"
DAYS   <- 30
CAPITAL<- 10000

args <- commandArgs(trailingOnly = TRUE)
for (arg in args) {
  if (grepl("^--symbol=", arg)) SYMBOL <- sub("^--symbol=", "", arg)
  if (grepl("^--days=", arg))   DAYS   <- as.numeric(sub("^--days=", "", arg))
}

cat("\n========================================================================\n")
cat(sprintf(" MONTHLY STRATEGY HEALTH EVALUATION | %s (PAST %d DAYS)\n", SYMBOL, DAYS))
cat("========================================================================\n\n")

ohlcv <- load_stock_data(symbol = SYMBOL)
price <- Cl(ohlcv)

# Slicing in-sample training window for GARCH parameter estimation (L1)
n_raw <- nrow(ohlcv)
approx_train_end <- max(50, n_raw - DAYS - 50)
pipeline_out <- build_feature_dataset(ohlcv, fast_n = 20, slow_n = 50, look_ahead = 5, train_idx = 1:approx_train_end)
df_model <- pipeline_out$model_data
feat_names <- pipeline_out$feature_names

n_total <- nrow(df_model)
eval_n <- min(DAYS, floor(n_total * 0.4))
# Embargo training window by 5 days to eliminate target leakage (L1)
train_idx <- 1:(n_total - eval_n - 5)
test_idx <- (n_total - eval_n + 1):n_total

# Fit calibrated swing model using unified model trainer (M1, M2)
model_res <- fit_logistic_swing_model(
  df_model      = df_model,
  feature_names = feat_names,
  train_idx     = train_idx,
  test_idx      = test_idx,
  p_long        = 0.58,
  p_short       = 0.42,
  calibrate     = TRUE
)

backtest_res <- run_swing_backtest(
  ohlcv = ohlcv,
  model_res = model_res,
  pipeline_out = pipeline_out,
  allow_short = FALSE,
  target_vol = 1.00,
  max_leverage = 1.0
)

perf <- backtest_res$performance_table
strat_row <- perf[1, ]
bh_row <- perf[3, ]

strat_ret_num <- as.numeric(sub("%", "", strat_row$Cumulative_Return))
bh_ret_num    <- as.numeric(sub("%", "", bh_row$Cumulative_Return))
strat_dd_num  <- as.numeric(sub("%", "", strat_row$Max_Drawdown))
win_rate_num  <- suppressWarnings(as.numeric(sub("%", "", strat_row$Win_Rate)))
if (is.na(win_rate_num)) win_rate_num <- 0.0
pf_val <- suppressWarnings(as.numeric(strat_row$Profit_Factor))
pf_num <- if (!is.na(pf_val)) pf_val else 999.0

# Strategy Health Diagnosis
health_status <- "HEALTHY (NOMINAL OPERATION)"
diag_notes <- list()

if (pf_num >= 1.30 && win_rate_num >= 50) {
  diag_notes[[length(diag_notes) + 1]] <- "✓ Profit Factor and Win Rate exceed target thresholds (PF >= 1.30, Win Rate >= 50%)."
} else if (pf_num >= 1.0) {
  health_status <- "ACCEPTABLE (CHOPPY / TRANSITIONAL REGIME)"
  diag_notes[[length(diag_notes) + 1]] <- "! Strategy is profitable but experiencing lower edge due to sideways market action."
} else {
  health_status <- "UNDERPERFORMING (REVIEW SIZING / MARKET REGIME)"
  diag_notes[[length(diag_notes) + 1]] <- "X Profit Factor fell below 1.0 over this window. Market may be in prolonged non-trending chop."
}

if (strat_dd_num <= 15) {
  diag_notes[[length(diag_notes) + 1]] <- sprintf("✓ Maximum Drawdown (%.2f%%) remained strictly within normal boundaries.", strat_dd_num)
} else {
  diag_notes[[length(diag_notes) + 1]] <- sprintf("! Drawdown (%.2f%%) exceeded 15%%. Consider lowering position sizing to 70%%.", strat_dd_num)
}

cat("------------------------------------------------------------------------\n")
cat(sprintf(" EVALUATION PERIOD: %s to %s (%d Trading Days)\n", 
            df_model$Date[test_idx[1]], df_model$Date[tail(test_idx, 1)], length(test_idx)))
cat("------------------------------------------------------------------------\n")
print(perf, row.names = FALSE)

cat("\n========================================================================\n")
cat(sprintf(" STRATEGY HEALTH VERDICT: [%s]\n", health_status))
cat("========================================================================\n")
for (note in diag_notes) {
  cat(sprintf(" %s\n", note))
}
cat("========================================================================\n\n")

# Save monthly chart
dir.create("output", showWarnings = FALSE)
png("output/monthly_review.png", width = 1100, height = 650, res = 120)
eq <- backtest_res$equity_curves
plot(index(eq), as.numeric(eq$ML_Swing_Strategy), type = "l", col = "blue", lwd = 2.5,
     ylim = range(as.numeric(eq)),
     main = sprintf("%s: Past %d-Day Strategy Review vs Buy & Hold (Base $10,000)", SYMBOL, length(test_idx)),
     ylab = "Account Value ($)", xlab = "Date")
lines(index(eq), as.numeric(eq$Buy_and_Hold), col = "gray40", lwd = 1.8, lty = 2)
legend("topleft", legend = c("ML Swing Strategy", "Buy & Hold"), col = c("blue", "gray40"),
       lwd = c(2.5, 1.8), lty = c(1, 2), bty = "n")
dev.off()
cat("[Chart] Monthly evaluation plot saved to 'output/monthly_review.png'\n\n")
