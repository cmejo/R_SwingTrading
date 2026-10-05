#!/usr/bin/env Rscript
# ==============================================================================
# Comprehensive Walk-Forward Backtest: 1 Year (252 Days) & 18 Months (378 Days)
# Multi-Factor ML Swing Strategy (Walk-Forward Rolling) vs Classic MA vs SP500 vs Buy & Hold
# ==============================================================================

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

OUTPUT_DIR <- "output"
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

calc_stats <- function(r_series, name = "Strategy") {
  calc_performance_metrics(r_series, name = name)$formatted
}

run_walkforward_backtest <- function(symbol = "AMD", test_bars = 252, horizon_name = "1 Year") {
  cat(sprintf("\n>>> Executing Rolling Walk-Forward Backtest for %s (%s | %d Trading Days)...\n",
              symbol, horizon_name, test_bars))
  
  ohlcv <- load_stock_data(symbol)
  n_raw <- nrow(ohlcv)
  approx_test_start <- max(1, n_raw - test_bars - 55)
  pipeline_out <- build_feature_dataset(ohlcv, fast_n = 20, slow_n = 50, look_ahead = 5, train_idx = 1:approx_test_start)
  df_model <- pipeline_out$model_data
  feat_names <- pipeline_out$feature_names
  
  total_bars <- nrow(df_model)
  if (total_bars < test_bars + 40) {
    actual_test_bars <- total_bars - 50
    cat(sprintf("  [Notice] Total history (%d bars) < requested (%d + 40). Adjusting out-of-sample test to %d bars.\n",
                total_bars, test_bars, actual_test_bars))
    test_bars <- actual_test_bars
  }
  
  test_start_idx <- total_bars - test_bars + 1
  test_dates <- df_model$Date[test_start_idx:total_bars]
  
  # Rolling walk-forward retraining loop (retrain every 20 days using rolling window)
  train_window <- min(200, test_start_idx - 1)
  pred_class <- numeric(test_bars)
  pred_probs <- numeric(test_bars)
  current_cv_fit <- NULL
  
  cat(sprintf("  Running walk-forward retraining across %d out-of-sample bars (%s to %s)...\n",
              test_bars, as.character(test_dates[1]), as.character(test_dates[test_bars])))
  
  for (i in 1:test_bars) {
    cur_idx <- test_start_idx + i - 1
    # Retrain model every 20 trading days (~monthly)
    if (i %% 20 == 1 || is.null(current_cv_fit)) {
      tr_start <- max(1, cur_idx - train_window)
      # Embargo training target by look_ahead window (5 days) to eliminate lookahead leakage
      tr_end   <- cur_idx - 5
      if (tr_end > (tr_start + 25)) {
        X_tr <- as.matrix(df_model[tr_start:tr_end, feat_names])
        y_tr <- df_model$TargetBinary[tr_start:tr_end]
        if (length(unique(y_tr)) >= 2) {
          set.seed(42)
          n_tr <- nrow(X_tr)
          nfolds <- min(5, max(3, floor(n_tr / 20)))
          foldid <- rep(1:nfolds, each = ceiling(n_tr / nfolds))[1:n_tr]
          current_cv_fit <- glmnet::cv.glmnet(X_tr, y_tr, foldid = foldid, alpha = 0.5, family = "binomial", type.measure = "deviance")
        }
      }
    }
    x_cur <- matrix(as.numeric(df_model[cur_idx, feat_names]), nrow = 1)
    p <- as.numeric(predict(current_cv_fit, newx = x_cur, s = "lambda.min", type = "response"))
    pred_probs[i] <- p
    pred_class[i] <- ifelse(p >= 0.58, 1, ifelse(p <= 0.42, -1, 0))
  }
  
  model_res <- list(test_dates = test_dates, test_idx = test_start_idx:total_bars, pred_class = pred_class)
  
  # 1. ML Swing Strategy (1.0x Cash)
  bt_10 <- run_swing_backtest(ohlcv, model_res, pipeline_out, allow_short = FALSE, target_vol = 0.30, max_leverage = 1.0)
  
  # 2. ML Swing Strategy (1.5x Margin Leverage)
  bt_15 <- run_swing_backtest(ohlcv, model_res, pipeline_out, allow_short = FALSE, target_vol = 0.30, max_leverage = 1.5)
  
  # Benchmarks (calculate full arithmetic returns first to preserve bar 1)
  spy_ohlcv <- load_stock_data("SPY")
  spy_cl    <- Cl(spy_ohlcv)
  spy_ret_all <- na.omit((spy_cl / lag.xts(spy_cl, 1)) - 1)
  
  qqq_ohlcv <- load_stock_data("QQQ")
  qqq_cl    <- Cl(qqq_ohlcv)
  qqq_ret_all <- na.omit((qqq_cl / lag.xts(qqq_cl, 1)) - 1)
  
  # Extract exact arithmetic returns from backtest
  ml_10_r   <- bt_10$strat_net_ret
  # 1.5x Margin Leveraged return: explicitly scales net return minus 7% annual margin interest on borrowed 0.5x
  ml_15_r   <- ml_10_r * 1.5 - ifelse(ml_10_r != 0, (0.07 / 252) * 0.5, 0.0)
  ma_r      <- bt_10$ma_ret
  bh_r      <- bt_10$bh_ret
  
  # Event-Driven Bracket Simulation (Live rules: 2-tier exits, breakeven ratchet, 5-day exit)
  sim_bracket <- tryCatch({
    simulate_bracket_backtest(ohlcv, model_res, pipeline_out)
  }, error = function(e) NULL)

  common_idx <- index(ml_10_r)
  spy_ret   <- spy_ret_all[common_idx]
  qqq_ret   <- qqq_ret_all[common_idx]
  
  # Metrics Table
  stats_rows <- list(
    calc_stats(ml_10_r, "ML Swing Strategy (1.0x Cash)"),
    calc_stats(ml_15_r, "ML Swing Strategy (1.5x Margin)")
  )
  if (!is.null(sim_bracket) && length(sim_bracket$daily_returns) > 0) {
    bracket_ret <- sim_bracket$daily_returns[common_idx]
    if (length(na.omit(bracket_ret)) > 5) {
      stats_rows[[length(stats_rows) + 1]] <- calc_stats(bracket_ret, "Event-Driven Bracket Execution (Real Rules)")
    }
  }
  stats_rows[[length(stats_rows) + 1]] <- calc_stats(ma_r,    "Classic MA Trend Cross (20/50)")
  stats_rows[[length(stats_rows) + 1]] <- calc_stats(bh_r,    sprintf("Buy & Hold %s", symbol))
  stats_rows[[length(stats_rows) + 1]] <- calc_stats(spy_ret, "S&P 500 Index (SPY)")
  stats_rows[[length(stats_rows) + 1]] <- calc_stats(qqq_ret, "Nasdaq 100 Index (QQQ)")

  stats_df <- do.call(rbind, stats_rows)
  
  # Align equity curves to base $10,000 using arithmetic compounding
  init_cap <- 10000
  eq_ml10 <- init_cap * cumprod(1 + as.numeric(ml_10_r))
  eq_ml15 <- init_cap * cumprod(1 + as.numeric(ml_15_r))
  eq_ma   <- init_cap * cumprod(1 + as.numeric(ma_r))
  eq_bh   <- init_cap * cumprod(1 + as.numeric(bh_r))
  eq_spy  <- init_cap * cumprod(1 + as.numeric(spy_ret))
  eq_qqq  <- init_cap * cumprod(1 + as.numeric(qqq_ret))
  
  eq_all <- xts(cbind(eq_ml10, eq_ml15, eq_ma, eq_bh, eq_spy, eq_qqq), order.by = common_idx)
  colnames(eq_all) <- c("ML_1.0x", "ML_1.5x_Margin", "Classic_MA", "Buy_Hold", "SPY_500", "QQQ_100")
  
  return(list(stats = stats_df, equity = eq_all, symbol = symbol, horizon = horizon_name, dates = test_dates))
}

# Run 1-Year (252 Days) & 18-Month (378 Days) on Core Asset AMD
res_1y_amd  <- run_walkforward_backtest("AMD", test_bars = 252, horizon_name = "1 Year (Past 252 Trading Days)")
res_18m_amd <- run_walkforward_backtest("AMD", test_bars = 378, horizon_name = "18 Months (Past 378 Trading Days)")

# Run 1-Year on SNDK
res_1y_sndk <- run_walkforward_backtest("SNDK", test_bars = 252, horizon_name = "1 Year (Past 252 Trading Days)")

cat("\n========================================================================================\n")
cat(" TABLE 1: 1-YEAR (252-DAY) WALK-FORWARD BACKTEST (AMD Benchmark Leader)\n")
cat("========================================================================================\n")
print(res_1y_amd$stats, row.names = FALSE)

cat("\n========================================================================================\n")
cat(" TABLE 2: 18-MONTH (378-DAY) WALK-FORWARD BACKTEST (AMD Benchmark Leader)\n")
cat("========================================================================================\n")
print(res_18m_amd$stats, row.names = FALSE)

cat("\n========================================================================================\n")
cat(" TABLE 3: 1-YEAR (252-DAY) WALK-FORWARD BACKTEST (SNDK)\n")
cat("========================================================================================\n")
print(res_1y_sndk$stats, row.names = FALSE)

# Generate Chart 1: 1-Year Walk-Forward Backtest
chart_1y_path <- file.path(OUTPUT_DIR, "backtest_1year_equity_curves.png")
png(chart_1y_path, width = 1200, height = 800, res = 120)
par(mfrow = c(2, 1), mar = c(3, 4, 3, 2))

eq1 <- res_1y_amd$equity
plot(index(eq1), as.numeric(eq1$ML_1.5x_Margin), type = "l", col = "blue", lwd = 2.5,
     ylim = range(as.numeric(eq1), na.rm = TRUE),
     main = sprintf("1-Year Rolling Walk-Forward Backtest: %s (%s to %s) [Base $10,000]",
                    res_1y_amd$symbol, as.character(first(index(eq1))), as.character(last(index(eq1)))),
     ylab = "Portfolio Value ($)", xlab = "")
lines(index(eq1), as.numeric(eq1$ML_1.0x), col = "deepskyblue", lwd = 2, lty = 1)
lines(index(eq1), as.numeric(eq1$Classic_MA), col = "red", lwd = 1.8, lty = 3)
lines(index(eq1), as.numeric(eq1$Buy_Hold), col = "gray50", lwd = 1.8, lty = 2)
lines(index(eq1), as.numeric(eq1$SPY_500), col = "darkgreen", lwd = 1.8, lty = 4)
legend("topleft", legend = c("ML Swing (1.5x Margin)", "ML Swing (1.0x Cash)", "Classic MA Cross", "Buy & Hold AMD", "S&P 500 (SPY)"),
       col = c("blue", "deepskyblue", "red", "gray50", "darkgreen"),
       lwd = c(2.5, 2, 1.8, 1.8, 1.8), lty = c(1, 1, 3, 2, 4), bty = "n")

dd_fn <- function(x) { x <- as.numeric(x); (x - cummax(x)) / cummax(x) * 100 }
dd_ml15 <- dd_fn(eq1$ML_1.5x_Margin)
dd_ma   <- dd_fn(eq1$Classic_MA)
dd_bh   <- dd_fn(eq1$Buy_Hold)
dd_spy  <- dd_fn(eq1$SPY_500)

plot(index(eq1), dd_ml15, type = "l", col = "blue", lwd = 2,
     ylim = c(min(c(dd_ml15, dd_ma, dd_bh, dd_spy), na.rm = TRUE), 0),
     main = "1-Year Drawdown Profile (%)", ylab = "Drawdown %", xlab = "Date")
lines(index(eq1), dd_ma, col = "red", lwd = 1.5, lty = 3)
lines(index(eq1), dd_bh, col = "gray50", lwd = 1.5, lty = 2)
lines(index(eq1), dd_spy, col = "darkgreen", lwd = 1.5, lty = 4)
abline(h = 0, lty = 1, col = "black")
legend("bottomleft", legend = c("ML Swing 1.5x DD", "Classic MA DD", "Buy & Hold DD", "SPY Drawdown"),
       col = c("blue", "red", "gray50", "darkgreen"), lwd = c(2, 1.5, 1.5, 1.5), lty = c(1, 3, 2, 4), bty = "n")
dev.off()
cat(sprintf("\n[Chart Saved] 1-Year Backtest chart saved to: %s\n", chart_1y_path))

# Generate Chart 2: 18-Month Walk-Forward Backtest
chart_18m_path <- file.path(OUTPUT_DIR, "backtest_18months_equity_curves.png")
png(chart_18m_path, width = 1200, height = 800, res = 120)
par(mfrow = c(2, 1), mar = c(3, 4, 3, 2))

eq18 <- res_18m_amd$equity
plot(index(eq18), as.numeric(eq18$ML_1.5x_Margin), type = "l", col = "blue", lwd = 2.5,
     ylim = range(as.numeric(eq18), na.rm = TRUE),
     main = sprintf("18-Month Rolling Walk-Forward Backtest: %s (%s to %s) [Base $10,000]",
                    res_18m_amd$symbol, as.character(first(index(eq18))), as.character(last(index(eq18)))),
     ylab = "Portfolio Value ($)", xlab = "")
lines(index(eq18), as.numeric(eq18$ML_1.0x), col = "deepskyblue", lwd = 2, lty = 1)
lines(index(eq18), as.numeric(eq18$Classic_MA), col = "red", lwd = 1.8, lty = 3)
lines(index(eq18), as.numeric(eq18$Buy_Hold), col = "gray50", lwd = 1.8, lty = 2)
lines(index(eq18), as.numeric(eq18$SPY_500), col = "darkgreen", lwd = 1.8, lty = 4)
legend("topleft", legend = c("ML Swing (1.5x Margin)", "ML Swing (1.0x Cash)", "Classic MA Cross", "Buy & Hold AMD", "S&P 500 (SPY)"),
       col = c("blue", "deepskyblue", "red", "gray50", "darkgreen"),
       lwd = c(2.5, 2, 1.8, 1.8, 1.8), lty = c(1, 1, 3, 2, 4), bty = "n")

dd_ml18 <- dd_fn(eq18$ML_1.5x_Margin)
dd_ma18 <- dd_fn(eq18$Classic_MA)
dd_bh18 <- dd_fn(eq18$Buy_Hold)
dd_spy18<- dd_fn(eq18$SPY_500)

plot(index(eq18), dd_ml18, type = "l", col = "blue", lwd = 2,
     ylim = c(min(c(dd_ml18, dd_ma18, dd_bh18, dd_spy18), na.rm = TRUE), 0),
     main = "18-Month Drawdown Profile (%)", ylab = "Drawdown %", xlab = "Date")
lines(index(eq18), dd_ma18, col = "red", lwd = 1.5, lty = 3)
lines(index(eq18), dd_bh18, col = "gray50", lwd = 1.5, lty = 2)
lines(index(eq18), dd_spy18, col = "darkgreen", lwd = 1.5, lty = 4)
abline(h = 0, lty = 1, col = "black")
legend("bottomleft", legend = c("ML Swing 1.5x DD", "Classic MA DD", "Buy & Hold DD", "SPY Drawdown"),
       col = c("blue", "red", "gray50", "darkgreen"), lwd = c(2, 1.5, 1.5, 1.5), lty = c(1, 3, 2, 4), bty = "n")
dev.off()
cat(sprintf("[Chart Saved] 18-Month Backtest chart saved to: %s\n", chart_18m_path))
