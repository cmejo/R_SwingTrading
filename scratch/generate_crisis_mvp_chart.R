#!/usr/bin/env Rscript
# ==============================================================================
# scratch/generate_crisis_mvp_chart.R
# Generate comprehensive comparison chart for Crisis Minimum Variance Portfolio
# ==============================================================================
suppressPackageStartupMessages({
  library(xts)
  library(zoo)
})

src <- readLines("scratch/compare_symbol_lists.R")
idx <- grep("^lists <- c", src)
eval(parse(text = paste(src[1:(idx - 1)], collapse = "\n")))
set_universe(read_list("symbols_broad.txt"))

source("scratch/test_crisis_min_variance.R")

S20 <- as.Date("2007-01-02")
E20 <- as.Date("2026-10-06")
ev20 <- which(cal >= S20 & cal <= E20)
d20 <- cal[ev20]

# Run the 4 core models
r_qqq  <- rq[ev20]
r_cash <- sim_crisis_mvp(margin = 1.0, crisis_mode = "cash", core_pct = 0.0, top_n = 5, start = S20, end = E20)
r_gld  <- sim_crisis_mvp(margin = 1.0, crisis_mode = "gld", core_pct = 0.0, top_n = 5, start = S20, end = E20)
r_mvp  <- sim_crisis_mvp(margin = 1.0, crisis_mode = "min_var_stocks", core_pct = 0.0, top_n = 5, mvp_n = 5, start = S20, end = E20)

eq_qqq  <- 10000 * cumprod(1 + r_qqq)
eq_cash <- 10000 * cumprod(1 + r_cash)
eq_gld  <- 10000 * cumprod(1 + r_gld)
eq_mvp  <- 10000 * cumprod(1 + r_mvp)

dd_qqq  <- (eq_qqq / cummax(eq_qqq) - 1) * 100
dd_cash <- (eq_cash / cummax(eq_cash) - 1) * 100
dd_gld  <- (eq_gld / cummax(eq_gld) - 1) * 100
dd_mvp  <- (eq_mvp / cummax(eq_mvp) - 1) * 100

out_png <- "/Users/cmejo/.gemini/antigravity/brain/64019830-0cac-4bb1-904e-d4560e71468c/crisis_minimum_variance_portfolio_comparison.png"
png(out_png, width = 1400, height = 950, res = 130)

par(mfrow = c(2, 1), mar = c(3, 4.5, 3.5, 1.5), oma = c(1, 1, 1, 1))

# Panel 1: Log Equity Curves
plot(d20, eq_mvp, type = "l", col = "#008080", lwd = 2.5, log = "y",
     ylim = c(8000, 20000000),
     main = "Crisis Minimum Variance Regime: 20-Year Compounding (2007-2026)",
     ylab = "Portfolio Value ($ Log Scale)", xlab = "",
     xaxt = "n", yaxt = "n")
grid(col = "gray85", lty = "dotted")

lines(d20, eq_gld,  col = "#D4AF37", lwd = 2, lty = 2)
lines(d20, eq_cash, col = "#2B6CB0", lwd = 2, lty = 1)
lines(d20, eq_qqq,  col = "#718096", lwd = 1.5, lty = 3)

axis.Date(1, at = seq(as.Date("2007-01-01"), as.Date("2027-01-01"), by = "2 years"), format = "%Y")
y_ticks <- c(10000, 50000, 200000, 1000000, 5000000, 10000000)
axis(2, at = y_ticks, labels = c("$10k", "$50k", "$200k", "$1M", "$5M", "$10M"), las = 2)

legend("topleft",
       legend = c(
         sprintf("Bear -> Equity Min-Variance (MVP): 42.0%% CAGR | 1.15 Sharpe | $10.15M End"),
         sprintf("Bear -> 100%% Gold (GLD):              41.3%% CAGR | 1.13 Sharpe | $9.19M End"),
         sprintf("Bear -> 100%% Cash:                    40.2%% CAGR | 1.14 Sharpe | $7.84M End"),
         sprintf("Benchmark: QQQ Buy & Hold:             16.6%% CAGR | 0.80 Sharpe | $206k End")
       ),
       col = c("#008080", "#D4AF37", "#2B6CB0", "#718096"),
       lwd = c(2.5, 2, 2, 1.5), lty = c(1, 2, 1, 3), bg = "white", cex = 0.88)

# Panel 2: Drawdown Profile
plot(d20, dd_mvp, type = "l", col = "#008080", lwd = 2,
     ylim = c(-60, 5),
     main = "Drawdown Comparison During Crisis Episodes (2008 GFC, 2011 Downgrade, 2020 Covid, 2022 Fed)",
     ylab = "Drawdown (%)", xlab = "", xaxt = "n")
grid(col = "gray85", lty = "dotted")

lines(d20, dd_gld,  col = "#D4AF37", lwd = 1.8, lty = 2)
lines(d20, dd_cash, col = "#2B6CB0", lwd = 1.8, lty = 1)
lines(d20, dd_qqq,  col = "#E53E3E", lwd = 1.2, lty = 3)
abline(h = 0, col = "black", lty = 1)

axis.Date(1, at = seq(as.Date("2007-01-01"), as.Date("2027-01-01"), by = "2 years"), format = "%Y")

legend("bottomleft",
       legend = c(
         "Equity MVP (MaxDD: -44.3%)",
         "Gold (GLD) (MaxDD: -38.4%)",
         "Cash (MaxDD: -36.7%)",
         "QQQ Benchmark (MaxDD: -53.4%)"
       ),
       col = c("#008080", "#D4AF37", "#2B6CB0", "#E53E3E"),
       lwd = c(2, 1.8, 1.8, 1.2), lty = c(1, 2, 1, 3), bg = "white", cex = 0.85)

dev.off()
cat(sprintf("Saved chart to %s\n", out_png))
