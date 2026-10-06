suppressPackageStartupMessages({
  library(xts)
  library(quantmod)
})

source("R/01_data_loader.R")

spy_ohlcv <- load_stock_data("SPY")
qqq_ohlcv <- load_stock_data("QQQ")
spy_cl    <- Cl(spy_ohlcv)
qqq_cl    <- Cl(qqq_ohlcv)

spy_ret_all <- na.omit(spy_cl / lag.xts(spy_cl, 1) - 1)
qqq_ret_all <- na.omit(qqq_cl / lag.xts(qqq_cl, 1) - 1)

data_obj <- readRDS("output/universe_comparison_data.rds")
evals <- data_obj$evals

png("output/universe_comparison_equity_curves.png", width = 1800, height = 1200, res = 150)
par(mfrow = c(3, 3), mar = c(4.5, 4.5, 3, 1), oma = c(1, 1, 3.5, 1))

universes <- c("Focus (symbols.txt)", "Broad (symbols_broad.txt)", "Institutional (symbols_300.txt)")
horizons  <- c("2 Months", "12 Months", "24 Months")

palette_colors <- c("ENHANCED" = "#1b9e77", "BASELINE" = "#d95f02", "SPY" = "#7570b3", "QQQ" = "#e7298a")

for (u in universes) {
  u_label <- gsub(" \\(.*\\)", "", u)
  for (h in horizons) {
    e_base <- NULL
    e_enh  <- NULL
    for (ev in evals) {
      if (ev$universe == u && ev$horizon == h) {
        if (ev$mode == "BASELINE") e_base <- ev
        if (ev$mode == "ENHANCED") e_enh  <- ev
      }
    }
    
    n_b <- e_enh$bars
    # Align SPY and QQQ to last n_b bars
    spy_tail <- tail(as.numeric(spy_ret_all), n_b)
    qqq_tail <- tail(as.numeric(qqq_ret_all), n_b)
    
    eq_enh  <- cumprod(1 + e_enh$daily_returns)
    eq_base <- cumprod(1 + e_base$daily_returns)
    eq_spy  <- cumprod(1 + spy_tail)
    eq_qqq  <- cumprod(1 + qqq_tail)
    
    ymin <- min(eq_enh, eq_base, eq_spy, eq_qqq) * 0.95
    ymax <- max(eq_enh, eq_base, eq_spy, eq_qqq) * 1.05
    
    plot(eq_enh, type = "l", col = "#1b9e77", lwd = 2.5, ylim = c(ymin, ymax),
         main = sprintf("%s | %s", u_label, h),
         xlab = "Trading Days", ylab = "Equity (Base 1.0)", cex.main = 1.05, cex.axis = 0.85)
    lines(eq_base, col = "#d95f02", lwd = 1.8, lty = 2)
    lines(eq_spy,  col = "#7570b3", lwd = 1.8)
    lines(eq_qqq,  col = "#e7298a", lwd = 1.8, lty = 3)
    grid(col = "gray85")
    
    legend("topleft", 
           legend = c(
             sprintf("Enhanced (%+.1f%%)", (tail(eq_enh, 1) - 1) * 100),
             sprintf("Baseline (%+.1f%%)", (tail(eq_base, 1) - 1) * 100),
             sprintf("SPY (%+.1f%%)", (tail(eq_spy, 1) - 1) * 100),
             sprintf("QQQ (%+.1f%%)", (tail(eq_qqq, 1) - 1) * 100)
           ),
           col = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a"),
           lty = c(1, 2, 1, 3), lwd = c(2.5, 1.8, 1.8, 1.8),
           cex = 0.72, bg = rgb(1, 1, 1, 0.85), box.col = "gray70")
  }
}

mtext("Multi-Universe & Multi-Horizon Backtest Comparison: Baseline vs. 4 Enhancements vs. SPY/QQQ",
      outer = TRUE, cex = 1.3, font = 2)

dev.off()
cat("Comparison chart saved to output/universe_comparison_equity_curves.png\n")
