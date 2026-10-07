#!/usr/bin/env Rscript
# ==============================================================================
# Performance Comparison: symbols_broad.txt & symbols_all2.txt vs VTI & QQQ
# Horizons: 12 Months, 24 Months, and 60 Months (5 Years)
# ==============================================================================

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(TTR)
})

source("R/08_metrics.R")

OUTPUT_DIR   <- "output"
ARTIFACT_DIR <- "/Users/cmejo/.gemini/antigravity/brain/785859a9-21ba-4446-b7f5-980fd7d1dd48"
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

# 1. Load Benchmarks
vti_ohlcv <- readRDS("data/data_vti.rds")
qqq_ohlcv <- readRDS("data/data_qqq.rds")
vti_cl <- Cl(vti_ohlcv)
qqq_cl <- Cl(qqq_ohlcv)

vti_ret_all <- na.omit(vti_cl / lag.xts(vti_cl, 1) - 1)
qqq_ret_all <- na.omit(qqq_cl / lag.xts(qqq_cl, 1) - 1)

# 2. Load Simulation Evaluation Data
data_obj <- readRDS("output/comprehensive_5universe_data.rds")
evals <- data_obj$evals

horizons <- c("12 Months", "24 Months", "60 Months")
scorecard_rows <- list()
plot_data <- list()

for (h in horizons) {
  ev_broad <- NULL
  ev_all2  <- NULL
  
  for (ev in evals) {
    if (ev$mode == "ENHANCED" && ev$horizon == h) {
      if (grepl("Broad", ev$universe)) ev_broad <- ev
      if (grepl("All2",  ev$universe)) ev_all2  <- ev
    }
  }
  
  # Align dates strictly across all 4 series
  common_dates <- sort(intersect(ev_broad$dates, ev_all2$dates))
  common_dates <- sort(intersect(common_dates, index(vti_ret_all)))
  common_dates <- sort(intersect(common_dates, index(qqq_ret_all)))
  n_b <- length(common_dates)
  
  idx_b <- match(common_dates, ev_broad$dates)
  idx_a <- match(common_dates, ev_all2$dates)
  idx_v <- match(common_dates, index(vti_ret_all))
  idx_q <- match(common_dates, index(qqq_ret_all))
  
  ret_broad <- as.numeric(ev_broad$daily_returns)[idx_b]
  ret_all2  <- as.numeric(ev_all2$daily_returns)[idx_a]
  vti_sub   <- as.numeric(vti_ret_all)[idx_v]
  qqq_sub   <- as.numeric(qqq_ret_all)[idx_q]
  
  eval_dates <- common_dates
  
  # Metrics for all 4 assets
  m_broad <- calc_performance_metrics(ret_broad, name = "Broad (symbols_broad.txt)")$raw
  m_all2  <- calc_performance_metrics(ret_all2,  name = "All2 (symbols_all2.txt)")$raw
  m_vti   <- calc_performance_metrics(vti_sub,   name = "VTI Benchmark")$raw
  m_qqq   <- calc_performance_metrics(qqq_sub,   name = "QQQ Benchmark")$raw
  
  # CAPM & Relative Metrics vs VTI
  cov_b_vti <- cov(ret_broad, vti_sub)
  var_vti   <- var(vti_sub)
  beta_b_vti <- ifelse(var_vti > 0, cov_b_vti / var_vti, 1.0)
  alpha_b_vti <- (m_broad$ann_ret - (0.04 + beta_b_vti * (m_vti$ann_ret - 0.04))) * 100
  corr_b_vti  <- cor(ret_broad, vti_sub)
  
  cov_a_vti <- cov(ret_all2, vti_sub)
  beta_a_vti <- ifelse(var_vti > 0, cov_a_vti / var_vti, 1.0)
  alpha_a_vti <- (m_all2$ann_ret - (0.04 + beta_a_vti * (m_vti$ann_ret - 0.04))) * 100
  corr_a_vti  <- cor(ret_all2, vti_sub)
  
  # Correlation vs QQQ
  corr_b_qqq <- cor(ret_broad, qqq_sub)
  corr_a_qqq <- cor(ret_all2,  qqq_sub)
  
  # Store for plotting
  plot_data[[h]] <- list(
    dates     = eval_dates,
    ret_broad = ret_broad,
    ret_all2  = ret_all2,
    ret_vti   = vti_sub,
    ret_qqq   = qqq_sub,
    m_broad   = m_broad,
    m_all2    = m_all2,
    m_vti     = m_vti,
    m_qqq     = m_qqq
  )
  
  # Add rows to scorecard
  assets <- list(
    list(name = "Broad (symbols_broad.txt)", m = m_broad, beta_v = beta_b_vti, alpha_v = alpha_b_vti, corr_v = corr_b_vti, corr_q = corr_b_qqq),
    list(name = "All2 (symbols_all2.txt)",   m = m_all2,  beta_v = beta_a_vti, alpha_v = alpha_a_vti, corr_v = corr_a_vti, corr_q = corr_a_qqq),
    list(name = "VTI (Total US Market)",     m = m_vti,   beta_v = 1.00,       alpha_v = 0.00,        corr_v = 1.00,       corr_q = cor(vti_sub, qqq_sub)),
    list(name = "QQQ (Nasdaq 100)",          m = m_qqq,   beta_v = cov(qqq_sub, vti_sub) / var_vti, alpha_v = 0.00, corr_v = cor(qqq_sub, vti_sub), corr_q = 1.00)
  )
  
  for (ast in assets) {
    scorecard_rows[[length(scorecard_rows) + 1]] <- data.frame(
      Horizon       = h,
      Trading_Days  = n_b,
      Asset         = ast$name,
      Cum_Return    = sprintf("%+.2f%%", ast$m$cum_ret * 100),
      Ann_Return    = sprintf("%+.2f%%", ast$m$ann_ret * 100),
      Ann_Vol       = sprintf("%.2f%%",  ast$m$ann_vol * 100),
      Sharpe_Ratio  = sprintf("%.2f",    ast$m$sharpe),
      Sortino_Ratio = sprintf("%.2f",    ast$m$sortino),
      Max_Drawdown  = sprintf("%.2f%%",  ast$m$max_dd * 100),
      Win_Rate      = sprintf("%.2f%%",  ast$m$win_rate * 100),
      Profit_Factor = sprintf("%.2f",    ast$m$profit_factor),
      Beta_vs_VTI   = sprintf("%.2f",    ast$beta_v),
      Alpha_vs_VTI  = sprintf("%+.2f%%", ast$alpha_v),
      Corr_vs_VTI   = sprintf("%.2f",    ast$corr_v),
      Corr_vs_QQQ   = sprintf("%.2f",    ast$corr_q),
      stringsAsFactors = FALSE
    )
  }
}

scorecard_df <- do.call(rbind, scorecard_rows)

# Save Scorecard
csv_path <- file.path(OUTPUT_DIR, "broad_all2_vs_vti_qqq_scorecard.csv")
write.csv(scorecard_df, csv_path, row.names = FALSE)
cat(sprintf("[Saved] Comparison scorecard saved to: %s\n", csv_path))

# 3. Generate 4-Panel Comparison Figure
plot_png <- file.path(OUTPUT_DIR, "broad_all2_vs_vti_qqq_chart.png")
png(plot_png, width = 2000, height = 1400, res = 150)
par(mfrow = c(2, 2), mar = c(4.5, 4.8, 3.2, 1), oma = c(1, 1, 3.5, 1))

# Colors
col_broad <- "#2b83ba" # Strong Blue
col_all2  <- "#d7191c" # Vibrant Red
col_vti   <- "#1a9641" # Forest Green
col_qqq   <- "#7b3294" # Deep Purple

# Panel 1: 12-Month Equity Curves
d12 <- plot_data[["12 Months"]]
eq_b_12 <- cumprod(1 + d12$ret_broad)
eq_a_12 <- cumprod(1 + d12$ret_all2)
eq_v_12 <- cumprod(1 + d12$ret_vti)
eq_q_12 <- cumprod(1 + d12$ret_qqq)

y_min12 <- min(eq_b_12, eq_a_12, eq_v_12, eq_q_12) * 0.95
y_max12 <- max(eq_b_12, eq_a_12, eq_v_12, eq_q_12) * 1.05

plot(eq_b_12, type = "l", col = col_broad, lwd = 2.8, ylim = c(y_min12, y_max12),
     main = "12-Month Horizon (252 Trading Days)", xlab = "Trading Days", ylab = "Portfolio Equity (Base 1.0)",
     cex.main = 1.25, cex.axis = 0.95, font.main = 2)
lines(eq_a_12, col = col_all2, lwd = 2.5, lty = 2)
lines(eq_q_12, col = col_qqq,  lwd = 2.0, lty = 4)
lines(eq_v_12, col = col_vti,  lwd = 2.0, lty = 3)
grid(col = "gray85")
legend("topleft",
       legend = c(
         sprintf("Broad (symbols_broad.txt) [%+.1f%% | Sharpe %.2f]", (tail(eq_b_12, 1) - 1) * 100, d12$m_broad$sharpe),
         sprintf("All2 (symbols_all2.txt)   [%+.1f%% | Sharpe %.2f]", (tail(eq_a_12, 1) - 1) * 100, d12$m_all2$sharpe),
         sprintf("QQQ (Nasdaq 100)          [%+.1f%% | Sharpe %.2f]", (tail(eq_q_12, 1) - 1) * 100, d12$m_qqq$sharpe),
         sprintf("VTI (Total US Market)     [%+.1f%% | Sharpe %.2f]", (tail(eq_v_12, 1) - 1) * 100, d12$m_vti$sharpe)
       ),
       col = c(col_broad, col_all2, col_qqq, col_vti),
       lwd = c(2.8, 2.5, 2.0, 2.0), lty = c(1, 2, 4, 3),
       cex = 0.76, bg = rgb(1, 1, 1, 0.92), box.col = "gray75")

# Panel 2: 24-Month Equity Curves
d24 <- plot_data[["24 Months"]]
eq_b_24 <- cumprod(1 + d24$ret_broad)
eq_a_24 <- cumprod(1 + d24$ret_all2)
eq_v_24 <- cumprod(1 + d24$ret_vti)
eq_q_24 <- cumprod(1 + d24$ret_qqq)

y_min24 <- min(eq_b_24, eq_a_24, eq_v_24, eq_q_24) * 0.95
y_max24 <- max(eq_b_24, eq_a_24, eq_v_24, eq_q_24) * 1.05

plot(eq_b_24, type = "l", col = col_broad, lwd = 2.8, ylim = c(y_min24, y_max24),
     main = "24-Month Horizon (504 Trading Days)", xlab = "Trading Days", ylab = "Portfolio Equity (Base 1.0)",
     cex.main = 1.25, cex.axis = 0.95, font.main = 2)
lines(eq_a_24, col = col_all2, lwd = 2.5, lty = 2)
lines(eq_q_24, col = col_qqq,  lwd = 2.0, lty = 4)
lines(eq_v_24, col = col_vti,  lwd = 2.0, lty = 3)
grid(col = "gray85")
legend("topleft",
       legend = c(
         sprintf("Broad (symbols_broad.txt) [%+.1f%% | Sharpe %.2f]", (tail(eq_b_24, 1) - 1) * 100, d24$m_broad$sharpe),
         sprintf("All2 (symbols_all2.txt)   [%+.1f%% | Sharpe %.2f]", (tail(eq_a_24, 1) - 1) * 100, d24$m_all2$sharpe),
         sprintf("QQQ (Nasdaq 100)          [%+.1f%% | Sharpe %.2f]", (tail(eq_q_24, 1) - 1) * 100, d24$m_qqq$sharpe),
         sprintf("VTI (Total US Market)     [%+.1f%% | Sharpe %.2f]", (tail(eq_v_24, 1) - 1) * 100, d24$m_vti$sharpe)
       ),
       col = c(col_broad, col_all2, col_qqq, col_vti),
       lwd = c(2.8, 2.5, 2.0, 2.0), lty = c(1, 2, 4, 3),
       cex = 0.76, bg = rgb(1, 1, 1, 0.92), box.col = "gray75")

# Panel 3: 60-Month (5 Years) Equity Curves (Log Scale for clear multi-thousand % view)
d60 <- plot_data[["60 Months"]]
eq_b_60 <- cumprod(1 + d60$ret_broad)
eq_a_60 <- cumprod(1 + d60$ret_all2)
eq_v_60 <- cumprod(1 + d60$ret_vti)
eq_q_60 <- cumprod(1 + d60$ret_qqq)

y_min60 <- min(eq_b_60, eq_a_60, eq_v_60, eq_q_60) * 0.90
y_max60 <- max(eq_b_60, eq_a_60, eq_v_60, eq_q_60) * 1.15

plot(eq_b_60, type = "l", col = col_broad, lwd = 2.8, ylim = c(y_min60, y_max60), log = "y",
     main = "60-Month Horizon (5 Years / 1,260 Days) [Log Scale]", xlab = "Trading Days", ylab = "Portfolio Equity (Log Base 1.0)",
     cex.main = 1.25, cex.axis = 0.95, font.main = 2)
lines(eq_a_60, col = col_all2, lwd = 2.5, lty = 2)
lines(eq_q_60, col = col_qqq,  lwd = 2.0, lty = 4)
lines(eq_v_60, col = col_vti,  lwd = 2.0, lty = 3)
grid(col = "gray85")
legend("topleft",
       legend = c(
         sprintf("Broad (symbols_broad.txt) [%+.1f%% | Sharpe %.2f]", (tail(eq_b_60, 1) - 1) * 100, d60$m_broad$sharpe),
         sprintf("All2 (symbols_all2.txt)   [%+.1f%% | Sharpe %.2f]", (tail(eq_a_60, 1) - 1) * 100, d60$m_all2$sharpe),
         sprintf("QQQ (Nasdaq 100)          [%+.1f%% | Sharpe %.2f]", (tail(eq_q_60, 1) - 1) * 100, d60$m_qqq$sharpe),
         sprintf("VTI (Total US Market)     [%+.1f%% | Sharpe %.2f]", (tail(eq_v_60, 1) - 1) * 100, d60$m_vti$sharpe)
       ),
       col = c(col_broad, col_all2, col_qqq, col_vti),
       lwd = c(2.8, 2.5, 2.0, 2.0), lty = c(1, 2, 4, 3),
       cex = 0.76, bg = rgb(1, 1, 1, 0.92), box.col = "gray75")

# Panel 4: Risk-Adjusted Sharpe Ratio vs Max Drawdown Matrix
sharpe_matrix <- matrix(c(
  d12$m_broad$sharpe, d12$m_all2$sharpe, d12$m_qqq$sharpe, d12$m_vti$sharpe,
  d24$m_broad$sharpe, d24$m_all2$sharpe, d24$m_qqq$sharpe, d24$m_vti$sharpe,
  d60$m_broad$sharpe, d60$m_all2$sharpe, d60$m_qqq$sharpe, d60$m_vti$sharpe
), nrow = 4, ncol = 3)
colnames(sharpe_matrix) <- c("12 Months", "24 Months", "60 Months")
rownames(sharpe_matrix) <- c("Broad", "All2", "QQQ", "VTI")

barplot(sharpe_matrix, beside = TRUE, col = c(col_broad, col_all2, col_qqq, col_vti),
        main = "Risk-Adjusted Efficiency (Sharpe Ratio Comparison)",
        ylab = "Annualized Sharpe Ratio", ylim = c(0, max(sharpe_matrix) * 1.25),
        cex.main = 1.25, cex.axis = 0.95, font.main = 2)
grid(col = "gray85")
legend("topright", legend = c("Broad (symbols_broad.txt)", "All2 (symbols_all2.txt)", "QQQ (Nasdaq 100)", "VTI (Total US Market)"),
       fill = c(col_broad, col_all2, col_qqq, col_vti), cex = 0.78, bg = rgb(1, 1, 1, 0.92), box.col = "gray75")

mtext("Comprehensive Backtest: symbols_broad.txt & symbols_all2.txt vs VTI & QQQ",
      outer = TRUE, cex = 1.45, font = 2, col = "#111111")
dev.off()
cat(sprintf("[Saved] Comparison chart saved to: %s\n", plot_png))

# Copy to artifact directory
if (dir.exists(ARTIFACT_DIR)) {
  file.copy(plot_png, file.path(ARTIFACT_DIR, "broad_all2_vs_vti_qqq_chart.png"), overwrite = TRUE)
  cat(sprintf("[Saved] Chart copied to artifact directory: %s\n", file.path(ARTIFACT_DIR, "broad_all2_vs_vti_qqq_chart.png")))
}

cat("\nAnalysis completed successfully!\n")
