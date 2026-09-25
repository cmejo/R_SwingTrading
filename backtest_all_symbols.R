#!/usr/bin/env Rscript
#' Comprehensive Backtest of All Stocks in symbols.txt (Past 2 Months vs S&P 500)

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

cat("\n========================================================================================\n")
cat(" COMPREHENSIVE STRATEGY BACKTEST: ALL STOCKS IN SYMBOLS.TXT (PAST 2 MONTHS)\n")
cat("========================================================================================\n\n")

# Read symbols.txt
lines <- readLines("symbols.txt")
lines <- gsub("#.*", "", lines)
symbols <- unique(toupper(trimws(unlist(strsplit(lines, "[, ]+")))))
symbols <- symbols[symbols != ""]

# Benchmark: S&P 500
spy_ohlcv <- load_stock_data("SPY")
spy_all <- Cl(spy_ohlcv)
spy_ret_all <- na.omit(diff(spy_all) / lag.xts(spy_all, 1))

eval_n <- 43 # Past 2 months (~42-43 trading bars)
results_list <- list()

for (s in symbols) {
  cat(sprintf("[Evaluating %s] Running out-of-sample feature pipeline & swing backtest...\n", s))
  
  res <- tryCatch({
    ohlcv <- load_stock_data(s)
    pipe <- build_feature_dataset(ohlcv, fast_n = 20, slow_n = 50, look_ahead = 5)
    df_m <- pipe$model_data
    
    if (nrow(df_m) < eval_n + 30) {
      cat(sprintf("  -> Skipping %s: History too short (%d rows).\n", s, nrow(df_m)))
      return(NULL)
    }
    
    train_idx <- 1:(nrow(df_m) - eval_n)
    test_idx  <- (nrow(df_m) - eval_n + 1):nrow(df_m)
    test_dates <- df_m$Date[test_idx]
    
    # Train ElasticNet model strictly on in-sample training split
    X_train <- as.matrix(df_m[train_idx, pipe$feature_names])
    y_train <- df_m$TargetBinary[train_idx]
    X_test  <- as.matrix(df_m[test_idx, pipe$feature_names])
    
    set.seed(42)
    cv_fit <- cv.glmnet(X_train, y_train, alpha = 0.5, family = "binomial")
    probs <- predict(cv_fit, newx = X_test, s = "lambda.min", type = "response")
    pred_class <- ifelse(probs > 0.58, 1, ifelse(probs < 0.42, -1, 0))
    
    model_res <- list(test_dates = test_dates, test_idx = test_idx, pred_class = as.numeric(pred_class))
    
    # Run swing backtest
    bt <- run_swing_backtest(ohlcv, model_res, pipe, allow_short = FALSE, target_vol = 1.0, max_leverage = 1.0)
    
    strat_r <- as.numeric(bt$strat_net_ret)
    bh_r    <- as.numeric(bt$bh_ret)
    dates   <- index(bt$strat_net_ret)
    spy_r   <- as.numeric(spy_ret_all[dates])
    
    cum_strat <- prod(1 + strat_r) - 1
    cum_bh    <- prod(1 + bh_r) - 1
    cum_spy   <- prod(1 + spy_r) - 1
    
    ann_vol <- sd(strat_r) * sqrt(252)
    sharpe  <- if (!is.na(ann_vol) && ann_vol > 0) (mean(strat_r) / sd(strat_r)) * sqrt(252) else 0
    cum_c   <- cumprod(1 + strat_r)
    max_dd  <- max((cummax(cum_c) - cum_c) / cummax(cum_c))
    win_r   <- if (sum(strat_r != 0) > 0) mean(strat_r[strat_r != 0] > 0) * 100 else 0
    
    # Alpha vs S&P 500 and vs Buy & Hold
    alpha_spy <- cum_strat - cum_spy
    alpha_bh  <- cum_strat - cum_bh
    
    data.frame(
      Symbol        = s,
      Strategy_Ret  = cum_strat,
      BuyHold_Ret   = cum_bh,
      SP500_Ret     = cum_spy,
      Alpha_vs_SPY  = alpha_spy,
      Alpha_vs_BH   = alpha_bh,
      Sharpe_Ratio  = sharpe,
      Max_Drawdown  = max_dd,
      Win_Rate      = win_r,
      N_Trades      = nrow(bt$trade_log),
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    cat(sprintf("  -> Error on %s: %s\n", s, e$message))
    NULL
  })
  
  if (!is.null(res)) {
    results_list[[length(results_list) + 1]] <- res
  }
}

df_all <- do.call(rbind, results_list)

# Sort by Strategy Return descending
df_all <- df_all[order(-df_all$Strategy_Ret), ]
rownames(df_all) <- 1:nrow(df_all)

# Formatted Table
table_display <- data.frame(
  Rank          = 1:nrow(df_all),
  Symbol        = df_all$Symbol,
  Strategy_2M   = sprintf("%+.2f%%", df_all$Strategy_Ret * 100),
  BuyHold_2M    = sprintf("%+.2f%%", df_all$BuyHold_Ret * 100),
  SP500_2M      = sprintf("%+.2f%%", df_all$SP500_Ret * 100),
  Alpha_vs_SPY  = sprintf("%+.2f%%", df_all$Alpha_vs_SPY * 100),
  Alpha_vs_BH   = sprintf("%+.2f%%", df_all$Alpha_vs_BH * 100),
  Sharpe        = sprintf("%.2f", df_all$Sharpe_Ratio),
  Max_Drawdown  = sprintf("%.2f%%", df_all$Max_Drawdown * 100),
  Win_Rate      = sprintf("%.1f%%", df_all$Win_Rate),
  Trades        = df_all$N_Trades
)

cat("\n========================================================================================\n")
cat("        PAST 2-MONTHS BACKTEST RESULTS ACROSS ALL WATCHLIST STOCKS (VS S&P 500)\n")
cat("========================================================================================\n")
print(table_display, row.names = FALSE)

# Aggregate Summary Statistics
avg_strat <- mean(df_all$Strategy_Ret) * 100
avg_bh    <- mean(df_all$BuyHold_Ret) * 100
spy_val   <- df_all$SP500_Ret[1] * 100
win_count <- sum(df_all$Strategy_Ret > df_all$SP500_Ret)

cat("\n----------------------------------------------------------------------------------------\n")
cat(" UNIVERSE AGGREGATE SUMMARY (PAST 2 MONTHS)\n")
cat("----------------------------------------------------------------------------------------\n")
cat(sprintf(" Average Strategy Return Across All Stocks:   %+.2f%%\n", avg_strat))
cat(sprintf(" Average Buy & Hold Return Across All Stocks: %+.2f%%\n", avg_bh))
cat(sprintf(" S&P 500 (SPY) Benchmark Return:              %+.2f%%\n", spy_val))
cat(sprintf(" Average Strategy Excess Return (vs S&P 500): %+.2f%%\n", avg_strat - spy_val))
cat(sprintf(" Stock Outperformance Rate (Beating SPY):     %d of %d (%.1f%%)\n", 
            win_count, nrow(df_all), win_count / nrow(df_all) * 100))
cat("========================================================================================\n\n")

# Generate Horizontal Bar Chart of 2-Month Returns vs S&P 500
dir.create("output", showWarnings = FALSE)
chart_path <- "output/all_stocks_2month_vs_sp500.png"
png(chart_path, width = 1200, height = 750, res = 120)

par(mar = c(5, 7, 4, 3))
plot_df <- df_all[order(df_all$Strategy_Ret), ]
bar_colors <- ifelse(plot_df$Strategy_Ret >= plot_df$SP500_Ret, "#2ca02c", "#d62728")

bp <- barplot(plot_df$Strategy_Ret * 100, horiz = TRUE, names.arg = plot_df$Symbol,
              las = 1, col = bar_colors, border = NA,
              xlab = "2-Month Cumulative Return (%)",
              main = "Swing Trading Strategy 2-Month Returns by Stock vs S&P 500 Benchmark\n(July 2026 to September 2026)",
              xlim = c(min(c(plot_df$Strategy_Ret * 100, -10)) - 5, max(plot_df$Strategy_Ret * 100) + 8))

grid(nx = NULL, ny = NA, col = "gray85", lty = 1)

# Add SP500 vertical benchmark line
abline(v = spy_val, col = "#1f77b4", lwd = 2.5, lty = 2)
abline(v = 0, col = "gray40", lwd = 1.2)

# Value labels on bars
text(x = ifelse(plot_df$Strategy_Ret >= 0, plot_df$Strategy_Ret * 100 + 1.2, plot_df$Strategy_Ret * 100 - 1.2),
     y = bp,
     labels = sprintf("%+.1f%%", plot_df$Strategy_Ret * 100),
     pos = ifelse(plot_df$Strategy_Ret >= 0, 4, 2),
     cex = 0.85, font = 2)

legend("bottomright",
       legend = c("Strategy Outperformed SPY", "Strategy Underperformed SPY", sprintf("S&P 500 Benchmark (%+.2f%%)", spy_val)),
       fill = c("#2ca02c", "#d62728", NA),
       border = c(NA, NA, NA),
       col = c(NA, NA, "#1f77b4"),
       lwd = c(NA, NA, 2.5),
       lty = c(NA, NA, 2),
       bg = "white", box.col = "gray80", cex = 0.85)

dev.off()
cat(sprintf("[Chart] Comparison chart saved to: %s\n\n", chart_path))
