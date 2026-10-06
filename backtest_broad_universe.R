#!/usr/bin/env Rscript
# ==============================================================================
# Comprehensive Multi-Asset Walk-Forward Backtest: symbols_broad.txt (170 Assets)
# Evaluates 1-Year (252 Days) Out-of-Sample Performance vs SP500, QQQ, Buy & Hold
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

SYMBOLS_FILE <- "symbols_broad.txt"
lines <- readLines(SYMBOLS_FILE, warn = FALSE)
lines <- gsub("#.*", "", lines)
symbols <- unique(toupper(trimws(unlist(strsplit(lines, "[, \\t\\r\\n]+")))))
symbols <- symbols[symbols != ""]

cat("\n========================================================================================\n")
cat(sprintf("   BROAD UNIVERSE WALK-FORWARD BACKTEST: %d ASSETS (PAST 1 YEAR / 252 DAYS)     \n", length(symbols)))
cat("========================================================================================\n\n")

# Benchmark Data
spy_ohlcv <- load_stock_data("SPY")
qqq_ohlcv <- load_stock_data("QQQ")

TEST_BARS <- 252 # 1 Year

calc_metrics <- function(r_vec, name = "Strategy") {
  res <- calc_performance_metrics(r_vec, name = name)
  if (length(res$raw) == 0) return(NULL)
  raw <- res$raw
  list(
    Strategy      = raw$strategy,
    Cum_Ret       = raw$cum_ret,
    Ann_Ret       = raw$ann_ret,
    Ann_Vol       = raw$ann_vol,
    Sharpe        = raw$sharpe,
    Max_DD        = raw$max_dd,
    Calmar        = raw$calmar,
    Win_Rate      = raw$win_rate,
    Profit_Factor = raw$profit_factor,
    n_days        = raw$n_days
  )
}

sim_single_stock <- function(sym) {
  ohlcv <- load_stock_data(sym)
  n_raw <- nrow(ohlcv)
  approx_tr_end <- max(1, n_raw - TEST_BARS - 50)
  pipe <- build_feature_dataset(ohlcv, fast_n = 20, slow_n = 50, look_ahead = 5, train_idx = 1:approx_tr_end)
  df_m <- pipe$model_data
  feat_names <- pipe$feature_names
  total_bars <- nrow(df_m)
  
  if (total_bars < 150) {
    cat(sprintf("  [-] Skipping %s: History too short (%d bars < 150).\n", sym, total_bars))
    return(NULL)
  }
  
  act_test_bars <- min(TEST_BARS, total_bars - 50)
  test_start_idx <- total_bars - act_test_bars + 1
  test_dates <- df_m$Date[test_start_idx:total_bars]
  
  train_window <- min(200, test_start_idx - 1)
  pred_class <- numeric(act_test_bars)
  current_model <- NULL
  
  # Monthly walk-forward retraining with Purged CV & Platt Calibration (L1, M1)
  for (i in 1:act_test_bars) {
    cur_idx <- test_start_idx + i - 1
    if (i %% 20 == 1 || is.null(current_model)) {
      tr_start <- max(1, cur_idx - train_window)
      # Embargo training target by look_ahead window (5 days) to eliminate lookahead leakage (L1)
      tr_end   <- cur_idx - 5
      if (tr_end > (tr_start + 25)) {
        X_tr <- as.matrix(df_m[tr_start:tr_end, feat_names])
        y_tr <- df_m$TargetBinary[tr_start:tr_end]
        if (length(unique(y_tr)) >= 2) {
          current_model <- train_swing_model(
            X_train       = X_tr,
            y_train       = y_tr,
            feature_names = feat_names,
            alpha         = 0.5,
            calibrate     = TRUE,
            embargo_days  = 5
          )
        }
      }
    }
    if (is.null(current_model)) {
      pred_class[i] <- 0
      next
    }
    x_cur <- matrix(as.numeric(df_m[cur_idx, feat_names]), nrow = 1)
    raw_p <- as.numeric(predict(current_model$cv_fit, newx = x_cur, s = "lambda.min", type = "response"))
    if (!is.null(current_model$calibrator)) {
      raw_link <- as.numeric(predict(current_model$cv_fit, newx = x_cur, s = "lambda.min", type = "link"))
      cal_p <- as.numeric(predict(current_model$calibrator, newdata = data.frame(Link = raw_link), type = "response"))
      p <- if (!is.na(cal_p)) cal_p else raw_p
    } else {
      p <- raw_p
    }
    pred_class[i] <- ifelse(p >= 0.58, 1, ifelse(p <= 0.42, -1, 0))
  }
  
  model_res <- list(test_dates = test_dates, test_idx = test_start_idx:total_bars, pred_class = pred_class)
  bt <- run_swing_backtest(ohlcv, model_res, pipe, allow_short = FALSE, target_vol = 0.30, max_leverage = 1.0)
  
  # Extract exact arithmetic returns from backtest
  ml_r  <- bt$strat_net_ret
  ma_r  <- bt$ma_ret
  bh_r  <- bt$bh_ret
  
  m <- calc_metrics(ml_r, sym)
  if (is.null(m)) return(NULL)
  
  m$BuyHold_Ret <- prod(1 + as.numeric(bh_r)) - 1
  m$MA_Ret      <- prod(1 + as.numeric(ma_r)) - 1
  
  return(list(metric = m, ml_r = ml_r, ma_r = ma_r, bh_r = bh_r))
}

individual_results <- list()
ml_returns_list    <- list()
ma_returns_list    <- list()
bh_returns_list    <- list()

valid_count <- 0
cat(sprintf("Scanning and running walk-forward simulations across %d symbols...\n", length(symbols)))

for (sym in symbols) {
  res <- tryCatch(sim_single_stock(sym), error = function(e) {
    cat(sprintf("  [!] Error processing %s: %s\n", sym, e$message))
    NULL
  })
  
  if (!is.null(res)) {
    m <- res$metric
    individual_results[[sym]] <- m
    ml_returns_list[[sym]]    <- res$ml_r
    ma_returns_list[[sym]]    <- res$ma_r
    bh_returns_list[[sym]]    <- res$bh_r
    valid_count <- valid_count + 1
    cat(sprintf("  [+] %-6s: ML CumRet: %+6.1f%% | Sharpe: %4.2f | MaxDD: -%4.1f%% | WinRate: %4.1f%% | PF: %4.2f\n",
                sym, m$Cum_Ret * 100, m$Sharpe, m$Max_DD * 100, m$Win_Rate * 100, ifelse(is.na(m$Profit_Factor), 0, m$Profit_Factor)))
  }
}

cat(sprintf("\nSuccessfully simulated %d assets across the broad watchlist.\n\n", valid_count))

if (valid_count == 0) {
  stop("No valid assets processed.")
}

# Merge all stock return series by date into an xts matrix
all_ml_xts <- do.call(merge, ml_returns_list)
all_ma_xts <- do.call(merge, ma_returns_list)
all_bh_xts <- do.call(merge, bh_returns_list)

# Take common trading dates with at least 50% coverage
date_coverage <- rowSums(!is.na(all_ml_xts))
common_dates <- index(all_ml_xts)[date_coverage >= max(5, floor(0.5 * valid_count))]

cat(sprintf("Constructing aggregate broad portfolio over %d shared out-of-sample trading days (%s to %s)...\n",
            length(common_dates), as.character(first(common_dates)), as.character(last(common_dates))))

# Portfolio Returns (Equal-weighted cross-asset basket on common trading days)
port_ml_r <- rowMeans(all_ml_xts[common_dates], na.rm = TRUE)
port_ma_r <- rowMeans(all_ma_xts[common_dates], na.rm = TRUE)
port_bh_r <- rowMeans(all_bh_xts[common_dates], na.rm = TRUE)

# Benchmark Returns: calculate full arithmetic returns first
spy_r_all <- na.omit((Cl(spy_ohlcv) / lag.xts(Cl(spy_ohlcv), 1)) - 1)
qqq_r_all <- na.omit((Cl(qqq_ohlcv) / lag.xts(Cl(qqq_ohlcv), 1)) - 1)

eval_dates   <- common_dates
port_ml_vec  <- as.numeric(port_ml_r)
port_ma_vec  <- as.numeric(port_ma_r)
port_bh_vec  <- as.numeric(port_bh_r)
spy_vec      <- as.numeric(spy_r_all[common_dates])
qqq_vec      <- as.numeric(qqq_r_all[common_dates])

# 1.5x Margin Leveraged Portfolio
port_ml15_vec <- port_ml_vec * 1.5 - (0.07 / 252 * 0.5) # Deduct 7% margin interest on borrowed 0.5x

# Compute Portfolio Comparison Table
format_stat_row <- function(m) {
  data.frame(
    Strategy          = m$Strategy,
    Cumulative_Return = sprintf("%+.2f%%", m$Cum_Ret * 100),
    Annualized_Return = sprintf("%+.2f%%", m$Ann_Ret * 100),
    Annualized_Vol    = sprintf("%.2f%%", m$Ann_Vol * 100),
    Sharpe_Ratio      = sprintf("%.2f", m$Sharpe),
    Max_Drawdown      = sprintf("-%.2f%%", m$Max_DD * 100),
    Calmar_Ratio      = if (!is.na(m$Calmar)) sprintf("%.2f", m$Calmar) else "N/A",
    Win_Rate          = sprintf("%.1f%%", m$Win_Rate * 100),
    Profit_Factor     = if (!is.na(m$Profit_Factor)) sprintf("%.2f", m$Profit_Factor) else "N/A",
    stringsAsFactors  = FALSE
  )
}

port_stats <- rbind(
  format_stat_row(calc_metrics(port_ml_vec,   sprintf("ML Broad Swing Portfolio (1.0x Cash, %d Stocks)", valid_count))),
  format_stat_row(calc_metrics(port_ml15_vec, sprintf("ML Broad Swing Portfolio (1.5x Margin, %d Stocks)", valid_count))),
  format_stat_row(calc_metrics(spy_vec,       "S&P 500 Index (SPY Benchmark)")),
  format_stat_row(calc_metrics(qqq_vec,       "Nasdaq 100 Index (QQQ Benchmark)")),
  format_stat_row(calc_metrics(port_ma_vec,   sprintf("Classic MA Trend Cross (%d-Asset Basket)", valid_count))),
  format_stat_row(calc_metrics(port_bh_vec,   sprintf("Broad Universe Equal-Weight Buy & Hold (%d Assets)", valid_count)))
)

cat("\n========================================================================================\n")
cat(sprintf(" TABLE 1: AGGREGATE 1-YEAR PORTFOLIO BACKTEST (%d ASSETS IN symbols_broad.txt)\n", valid_count))
cat("========================================================================================\n")
print(port_stats, row.names = FALSE)

# Top Individual Performers in symbols_broad.txt
indiv_df <- do.call(rbind, lapply(individual_results, function(x) {
  data.frame(
    Symbol            = x$Strategy,
    ML_Cum_Return     = x$Cum_Ret,
    BuyHold_Return    = x$BuyHold_Ret,
    Sharpe            = x$Sharpe,
    Max_DD            = x$Max_DD,
    Win_Rate          = x$Win_Rate,
    Profit_Factor     = x$Profit_Factor,
    stringsAsFactors  = FALSE
  )
}))

indiv_df <- indiv_df[order(-indiv_df$Sharpe), ]

cat("\n========================================================================================\n")
cat(" TABLE 2: TOP 15 RISK-ADJUSTED LEADERS IN BROAD UNIVERSE (BY SHARPE RATIO)\n")
cat("========================================================================================\n")
top_15 <- head(indiv_df, 15)
top_15_display <- data.frame(
  Symbol        = top_15$Symbol,
  ML_Return     = sprintf("%+.1f%%", top_15$ML_Cum_Return * 100),
  BuyHold_Ret   = sprintf("%+.1f%%", top_15$BuyHold_Return * 100),
  Sharpe_Ratio  = sprintf("%.2f", top_15$Sharpe),
  Max_Drawdown  = sprintf("-%.1f%%", top_15$Max_DD * 100),
  Win_Rate      = sprintf("%.1f%%", top_15$Win_Rate * 100),
  Profit_Factor = sprintf("%.2f", top_15$Profit_Factor),
  stringsAsFactors = FALSE
)
print(top_15_display, row.names = FALSE)

# Generate Publication-Quality Portfolio Chart
init_capital <- 10000
eq_port_ml   <- init_capital * cumprod(1 + port_ml_vec)
eq_port_ml15 <- init_capital * cumprod(1 + port_ml15_vec)
eq_port_ma   <- init_capital * cumprod(1 + port_ma_vec)
eq_port_bh   <- init_capital * cumprod(1 + port_bh_vec)
eq_spy       <- init_capital * cumprod(1 + spy_vec)
eq_qqq       <- init_capital * cumprod(1 + qqq_vec)

eq_xts <- xts(
  data.frame(
    ML_1.0x    = eq_port_ml,
    ML_1.5x    = eq_port_ml15,
    Classic_MA = eq_port_ma,
    Buy_Hold   = eq_port_bh,
    SPY        = eq_spy,
    QQQ        = eq_qqq
  ),
  order.by = eval_dates
)

chart_path <- file.path(OUTPUT_DIR, "backtest_broad_equity_curves.png")
png(chart_path, width = 1200, height = 800, res = 120)
par(mfrow = c(2, 1), mar = c(3, 4, 3, 2))

plot(index(eq_xts), as.numeric(eq_xts$ML_1.5x), type = "l", col = "blue", lwd = 2.5,
     ylim = range(as.numeric(eq_xts), na.rm = TRUE),
     main = sprintf("Broad Watchlist (%d Assets) 1-Year Walk-Forward Backtest [Base $10,000]", valid_count),
     ylab = "Portfolio Value ($)", xlab = "")
lines(index(eq_xts), as.numeric(eq_xts$ML_1.0x), col = "deepskyblue", lwd = 2, lty = 1)
lines(index(eq_xts), as.numeric(eq_xts$Classic_MA), col = "red", lwd = 1.8, lty = 3)
lines(index(eq_xts), as.numeric(eq_xts$Buy_Hold), col = "gray50", lwd = 1.8, lty = 2)
lines(index(eq_xts), as.numeric(eq_xts$SPY), col = "darkgreen", lwd = 2, lty = 4)
lines(index(eq_xts), as.numeric(eq_xts$QQQ), col = "purple", lwd = 1.8, lty = 5)
legend("topleft", legend = c(sprintf("ML Broad (1.5x Margin, %d Stocks)", valid_count),
                             sprintf("ML Broad (1.0x Cash, %d Stocks)", valid_count),
                             "Classic MA Basket", "Broad Equal-Weight Buy & Hold", "S&P 500 (SPY)", "Nasdaq 100 (QQQ)"),
       col = c("blue", "deepskyblue", "red", "gray50", "darkgreen", "purple"),
       lwd = c(2.5, 2, 1.8, 1.8, 2, 1.8), lty = c(1, 1, 3, 2, 4, 5), bty = "n")

# Drawdown Comparison Panel
dd_calc <- function(s) { s <- as.numeric(s); (s - cummax(s)) / cummax(s) * 100 }
dd_ml15 <- dd_calc(eq_xts$ML_1.5x)
dd_ml10 <- dd_calc(eq_xts$ML_1.0x)
dd_ma   <- dd_calc(eq_xts$Classic_MA)
dd_bh   <- dd_calc(eq_xts$Buy_Hold)
dd_spy  <- dd_calc(eq_xts$SPY)

plot(index(eq_xts), dd_ml10, type = "l", col = "deepskyblue", lwd = 2,
     ylim = c(min(c(dd_ml15, dd_ma, dd_bh, dd_spy), na.rm = TRUE), 0),
     main = "Broad Portfolio Drawdown Profile (%)", ylab = "Drawdown %", xlab = "Date")
lines(index(eq_xts), dd_ml15, col = "blue", lwd = 2)
lines(index(eq_xts), dd_ma, col = "red", lwd = 1.5, lty = 3)
lines(index(eq_xts), dd_bh, col = "gray50", lwd = 1.5, lty = 2)
lines(index(eq_xts), dd_spy, col = "darkgreen", lwd = 1.8, lty = 4)
abline(h = 0, lty = 1, col = "black")
legend("bottomleft", legend = c("ML 1.0x DD", "ML 1.5x Margin DD", "Classic MA DD", "Buy & Hold DD", "SPY Drawdown"),
       col = c("deepskyblue", "blue", "red", "gray50", "darkgreen"), lwd = c(2, 2, 1.5, 1.5, 1.8), lty = c(1, 1, 3, 2, 4), bty = "n")
dev.off()

cat(sprintf("\n[Chart Saved] Broad Universe backtest chart saved to: %s\n\n", chart_path))
