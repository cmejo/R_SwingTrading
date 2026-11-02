#!/usr/bin/env Rscript
# ==============================================================================
# Generate 10-Year Chart Artifact: 100% Broad Stock vs 30% QQQ / 70% Stock
# ==============================================================================
src <- readLines("scratch/compare_symbol_lists.R")
idx <- grep("^lists <- c", src)
eval(parse(text = paste(src[1:(idx - 1)], collapse = "\n")))

S10 <- as.Date("2016-10-03")
E10 <- as.Date("2026-10-06")
ev10 <- which(cal >= S10 & cal <= E10)
d <- cal[ev10]

set_universe(read_list("symbols_broad.txt"))

# Load simulation runner from test_100pct_broad_10yr.R
source("scratch/test_100pct_broad_10yr.R")

ART <- "/Users/cmejo/.gemini/antigravity/brain/64019830-0cac-4bb1-904e-d4560e71468c"
out_png <- file.path(ART, "backtest_10yr_100pct_broad_vs_baseline.png")

png(out_png, width = 1800, height = 1100, res = 130)
layout(matrix(c(1, 2), nrow = 2, byrow = TRUE), heights = c(1.3, 0.9))
par(mar = c(3.5, 5.0, 3.0, 2.0))

# Colors and linetypes
plot_series <- list(
  "Benchmark: QQQ Buy & Hold (21.3% CAGR)" = rq[ev10],
  "Baseline: 30% QQQ / 70% Stock (1.0x, Top 5) (61.0% CAGR)" = res[["Baseline: 30% QQQ / 70% Stock (1.0x, Top 5)"]],
  "100% Broad Stock (1.0x Cash, Top 5) (77.6% CAGR)" = res[["100% Broad Stock (1.0x Cash, Top 5)"]],
  "100% Broad Stock (1.5x Margin, Top 5) (111.1% CAGR)" = res[["100% Broad Stock (1.5x Margin, Top 5)"]],
  "Active Opt6: 30% QLD / 70% Stock (1.5x, Top 5) (93.9% CAGR)" = res[["Active Opt6: 30% QLD / 70% Stock (1.5x, Top 5)"]]
)

cols <- c("#7f8c8d", "#2980b9", "#27ae60", "#e74c3c", "#8e44ad")
ltys <- c(2, 1, 1, 1, 1)
lwds <- c(2, 2.5, 2.5, 2.5, 2.5)

# 1. Equity Curves
eq_mat <- sapply(plot_series, function(r) cumprod(1 + r) * 10000)
matplot(d, eq_mat, type = "l", lty = ltys, lwd = lwds, col = cols, log = "y",
        yaxt = "n", xlab = "", ylab = "Portfolio Equity ($ - Log Scale)",
        main = "10-Year Backtest (2016-2026): 100% Stock vs 30% QQQ / 70% Stock ($10,000 Initial)")

axis(2, at = c(1e4, 3e4, 1e5, 3e5, 1e6, 3e6, 1e7, 3e7),
     labels = c("$10k", "$30k", "$100k", "$300k", "$1M", "$3M", "$10M", "$30M"), las = 2)
grid(col = "gray85", lty = 1)
legend("topleft", names(plot_series), col = cols, lty = ltys, lwd = lwds, bty = "n", cex = 0.85)

# 2. Drawdowns
dd_mat <- sapply(plot_series, function(r) {
  c_r <- cumprod(1 + r)
  (c_r / cummax(c_r) - 1) * 100
})

matplot(d, dd_mat, type = "l", lty = ltys, lwd = 1.8, col = cols,
        ylab = "Drawdown (%)", xlab = "", main = "Underwater Drawdown Profiles (2016 - 2026)",
        ylim = c(-60, 5))
grid(col = "gray85", lty = 1)
abline(h = -20, col = "gray60", lty = 3)
abline(h = -35, col = "gray60", lty = 3)
legend("bottomleft", c("QQQ", "30% QQQ / 70% Stock (1.0x)", "100% Stock (1.0x)", "100% Stock (1.5x Margin)", "Active Opt6: 30% QLD / 70% Stock"),
       col = cols, lty = ltys, lwd = 1.8, bty = "n", cex = 0.8)

dev.off()
cat("Chart successfully created at:", out_png, "\n")
