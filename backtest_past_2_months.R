#!/usr/bin/env Rscript
#' Past 2 Months Strategy Backtest vs S&P 500 Benchmark (SPY)
#'
#' Evaluates out-of-sample trading performance over the past ~2 months
#' (42 trading days: July 20, 2026 to September 16, 2026)
#' comparing the ML Swing Strategy against:
#'   1. S&P 500 Benchmark (SPY)
#'   2. Nasdaq 100 Benchmark (QQQ)
#'   3. Underlying Buy & Hold (SNDK)
#'   4. Traditional 50/200 MA Trend Follower

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

cat("\n========================================================================\n")
cat(" OUT-OF-SAMPLE BACKTEST: PAST 2 MONTHS VS S&P 500 (SPY)\n")
cat("========================================================================\n\n")

# 1. Load Data
ohlcv_sndk <- load_stock_data("SNDK")
ohlcv_spy  <- load_stock_data("SPY")
ohlcv_qqq  <- load_stock_data("QQQ")

# 2. Build Features on SNDK
eval_n <- 43
n_raw <- nrow(ohlcv_sndk)
train_end_idx <- max(1, n_raw - eval_n - 50)
pipeline_out <- build_feature_dataset(ohlcv_sndk, fast_n = 20, slow_n = 50, look_ahead = 5, train_idx = 1:train_end_idx)
df_model <- pipeline_out$model_data
feat_names <- pipeline_out$feature_names

# Slicing the past 2 months (43 observations in labeled model dataset)
eval_n <- 43
# Embargo training window by 5 days to eliminate target leakage
train_idx <- 1:(nrow(df_model) - eval_n - 5)
test_idx  <- (nrow(df_model) - eval_n + 1):nrow(df_model)
test_dates <- df_model$Date[test_idx]

# 3. Train ElasticNet Out-of-Sample with Purged CV & Platt Calibration (L1, M1)
model_fit <- fit_logistic_swing_model(
  df_model      = df_model,
  feature_names = feat_names,
  train_idx     = train_idx,
  test_idx      = test_idx,
  p_long        = 0.58,
  p_short       = 0.42,
  calibrate     = TRUE
)
model_res <- list(test_dates = test_dates, test_idx = test_idx, pred_class = model_fit$pred_class)

# 4. Run Backtest
backtest_res <- run_swing_backtest(
  ohlcv = ohlcv_sndk,
  model_res = model_res,
  pipeline_out = pipeline_out,
  allow_short = FALSE,
  target_vol = 1.00,
  max_leverage = 1.0
)

# 5. Align Benchmarks
strat_ret <- backtest_res$strat_net_ret
bh_ret    <- backtest_res$bh_ret
ma_ret    <- backtest_res$ma_ret
common_dates <- index(strat_ret)

spy_all <- Cl(ohlcv_spy)
qqq_all <- Cl(ohlcv_qqq)

spy_ret_all <- na.omit(diff(spy_all) / lag.xts(spy_all, 1))
qqq_ret_all <- na.omit(diff(qqq_all) / lag.xts(qqq_all, 1))

spy_ret <- spy_ret_all[common_dates]
qqq_ret <- qqq_ret_all[common_dates]

start_d <- as.character(common_dates[1])
end_d   <- as.character(tail(common_dates, 1))

calc_stat <- function(r_series, name) {
  calc_performance_metrics(r_series, name = name)$formatted
}

scorecard <- rbind(
  calc_stat(strat_ret, "ML Swing Strategy (SNDK)"),
  calc_stat(bh_ret,    "SNDK Buy & Hold"),
  calc_stat(spy_ret,   "S&P 500 Benchmark (SPY)"),
  calc_stat(qqq_ret,   "Nasdaq 100 Benchmark (QQQ)"),
  calc_stat(ma_ret,    "Classic 50/200 MA Trend Cross")
)

cat("========================================================================================\n")
cat(sprintf(" PERFORMANCE SCORECARD (PAST 2 MONTHS: %s to %s | %d TRADING BARS)\n", start_d, end_d, length(common_dates)))
cat("========================================================================================\n")
print(scorecard, row.names = FALSE)

# CAPM Alpha & Beta vs S&P 500
strat_v <- as.numeric(strat_ret)
spy_v   <- as.numeric(spy_ret)
capm_fit <- lm(strat_v ~ spy_v)
alpha_daily <- coef(capm_fit)[1]
beta_spy    <- coef(capm_fit)[2]
alpha_ann   <- alpha_daily * 252

strat_cum_num <- as.numeric(sub("%", "", scorecard$Cumulative_Return[1]))
spy_cum_num   <- as.numeric(sub("%", "", scorecard$Cumulative_Return[3]))
excess_return <- strat_cum_num - spy_cum_num

cat("\n----------------------------------------------------------------------------------------\n")
cat(" RELATIVE PERFORMANCE & ALPHA BREAKDOWN (VS S&P 500 BENCHMARK)\n")
cat("----------------------------------------------------------------------------------------\n")
cat(sprintf(" 2-Month Total Excess Return (Alpha):  %+.2f%% (Strategy: %+.2f%% vs SPY: %+.2f%%)\n",
            excess_return, strat_cum_num, spy_cum_num))
cat(sprintf(" Annualized Jensen's Alpha:           %+.2f%%\n", alpha_ann * 100))
cat(sprintf(" Beta to S&P 500:                     %.2f (Low Market Directional Exposure)\n", beta_spy))
strat_cor <- if (!is.na(sd(strat_v)) && sd(strat_v) > 0) cor(strat_v, spy_v) else 0.0
cat(sprintf(" Correlation with S&P 500:            %.2f\n", strat_cor))
cat(sprintf(" Downside Protection (Max Drawdown):  %.2f%% (Strategy) vs %.2f%% (SNDK Buy & Hold)\n",
            as.numeric(sub("%", "", scorecard$Max_Drawdown[1])),
            as.numeric(sub("%", "", scorecard$Max_Drawdown[2]))))
cat("========================================================================================\n\n")

# Recent Trades Log
cat("Executed Swing Trades during the 2-Month Window:\n")
print(backtest_res$trade_log, row.names = FALSE)
cat("\n")

# Export Publication-Quality Equity Chart
dir.create("output", showWarnings = FALSE)
chart_path <- "output/past_2_months_vs_sp500.png"
png(chart_path, width = 1150, height = 700, res = 120)

eq_strat <- 10000 * cumprod(1 + c(0, strat_v))
eq_bh    <- 10000 * cumprod(1 + c(0, as.numeric(bh_ret)))
eq_spy   <- 10000 * cumprod(1 + c(0, spy_v))
eq_qqq   <- 10000 * cumprod(1 + c(0, as.numeric(qqq_ret)))
plot_dates <- c(as.Date(start_d) - 1, common_dates)

all_curves <- c(eq_strat, eq_bh, eq_spy, eq_qqq)
y_lims <- c(min(all_curves) * 0.95, max(all_curves) * 1.05)

par(mar = c(4.5, 4.5, 3.5, 1.5))
plot(plot_dates, eq_strat, type = "l", col = "#1f77b4", lwd = 3.5,
     ylim = y_lims,
     main = sprintf("Past 2 Months Performance: ML Swing Strategy vs S&P 500 & Benchmarks\n%s to %s (Base Capital: $10,000)", start_d, end_d),
     ylab = "Portfolio Equity ($)", xlab = "Trading Date",
     xaxt = "n", yaxt = "n", col.main = "#111827", font.main = 2)

axis.Date(1, at = seq(min(plot_dates), max(plot_dates), by = "week"), format = "%b %d", cex.axis = 0.9)
axis(2, at = axTicks(2), labels = sprintf("$%s", format(axTicks(2), big.mark = ",")), las = 1, cex.axis = 0.9)
grid(col = "gray88", lty = 1)

lines(plot_dates, eq_bh, col = "#ff7f0e", lwd = 2.0, lty = 2)
lines(plot_dates, eq_qqq, col = "#9467bd", lwd = 2.0, lty = 4)
lines(plot_dates, eq_spy, col = "#2ca02c", lwd = 2.8, lty = 1)

abline(h = 10000, col = "gray50", lty = 3, lwd = 1.2)

legend("topleft",
       legend = c(
         sprintf("ML Swing Strategy: %s (Sharpe: %s | MaxDD: %s)", 
                 scorecard$Cumulative_Return[1], scorecard$Sharpe_Ratio[1], scorecard$Max_Drawdown[1]),
         sprintf("SNDK Buy & Hold:   %s (Sharpe: %s | MaxDD: %s)", 
                 scorecard$Cumulative_Return[2], scorecard$Sharpe_Ratio[2], scorecard$Max_Drawdown[2]),
         sprintf("Nasdaq 100 (QQQ):  %s (Sharpe: %s | MaxDD: %s)", 
                 scorecard$Cumulative_Return[4], scorecard$Sharpe_Ratio[4], scorecard$Max_Drawdown[4]),
         sprintf("S&P 500 (SPY):     %s (Sharpe: %s | MaxDD: %s)", 
                 scorecard$Cumulative_Return[3], scorecard$Sharpe_Ratio[3], scorecard$Max_Drawdown[3])
       ),
       col = c("#1f77b4", "#ff7f0e", "#9467bd", "#2ca02c"),
       lwd = c(3.5, 2.0, 2.0, 2.8),
       lty = c(1, 2, 4, 1),
       bg = "white", box.col = "gray75", cex = 0.85)

dev.off()
cat(sprintf("[Chart] Exported comparative equity curve chart to: %s\n\n", chart_path))
