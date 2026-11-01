#!/usr/bin/env Rscript
# ==============================================================================
# scratch/compare_qqq_vs_qld.R
# Rigorous Comparison: QQQ (1x) vs QLD (2x QQQ ETF) Over 20 Years (2006-2026)
# ==============================================================================

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
})

HIST <- "data/data_historical"
ART  <- "/Users/cmejo/.gemini/antigravity/brain/64019830-0cac-4bb1-904e-d4560e71468c"

d_qqq <- readRDS(file.path(HIST, "data_qqq.rds"))
d_qld <- readRDS(file.path(HIST, "data_qld.rds"))

p_qqq <- d_qqq[, grep("Adjusted$", colnames(d_qqq))]
p_qld <- d_qld[, grep("Adjusted$", colnames(d_qld))]

# Common dates where both exist
common_dates <- intersect(as.Date(index(p_qqq)), as.Date(index(p_qld)))
common_dates <- common_dates[order(common_dates)]
cal <- common_dates

q <- as.numeric(p_qqq[cal])
l <- as.numeric(p_qld[cal])

rq <- c(0, q[-1] / q[-length(q)] - 1)
rl <- c(0, l[-1] / l[-length(l)] - 1)

# Also test QLD with a simple 200-day SMA filter (Trend Gate)
sma200_q <- as.numeric(stats::filter(q, rep(1/200, 200), sides = 1))
trend_up <- q >= sma200_q; trend_up[is.na(trend_up)] <- TRUE

irx <- readRDS(file.path(HIST, "data_^irx.rds"))
rf  <- as.numeric(zoo::na.locf(irx[, 4], na.rm = FALSE)[cal])
rf  <- zoo::na.locf(rf, na.rm = FALSE)
rf[is.na(rf)] <- 2
rf  <- rf / 100 / 252

# QLD Trend: Hold QLD when QQQ >= 200 SMA, hold T-Bills (SGOV/IRX) when QQQ < 200 SMA
rl_trend <- ifelse(trend_up, rl, rf)

runs <- list(
  "QQQ (1x Unleveraged)"        = rq,
  "QLD (2x Buy & Hold)"         = rl,
  "QLD + 200 SMA Trend Gate"    = rl_trend
)

calc_perf <- function(r, dates_sub = cal) {
  c_r <- cumprod(1 + r); n <- length(r); dd <- c_r / cummax(c_r) - 1
  mdd <- -min(dd) * 100
  cagr <- (tail(c_r, 1)^(252 / n) - 1) * 100
  term <- 10000 * tail(c_r, 1)
  vol <- sd(r) * sqrt(252) * 100
  rfm <- rf[match(dates_sub, cal)]
  sharpe <- if (sd(r) > 0) mean(r - rfm) / sd(r) * sqrt(252) else 0
  sortino <- {
    d_ret <- pmin(r - rfm, 0)
    d_sd  <- sqrt(mean(d_ret^2)) * sqrt(252)
    if (d_sd > 0) mean(r - rfm) * 252 / d_sd else 0
  }
  calmar <- if (mdd > 0) cagr / mdd else 0
  
  data.frame(
    Final_Value = sprintf("$%s", format(round(term), big.mark = ",")),
    CAGR        = sprintf("%+.2f%%", cagr),
    MaxDD       = sprintf("-%.1f%%", mdd),
    Vol         = sprintf("%.1f%%", vol),
    Sharpe      = sprintf("%.2f", sharpe),
    Sortino     = sprintf("%.2f", sortino),
    Calmar      = sprintf("%.2f", calmar),
    stringsAsFactors = FALSE
  )
}

tab_full <- do.call(rbind, lapply(runs, calc_perf))
tab_full$Asset <- names(runs)
tab_full <- tab_full[, c("Asset", "Final_Value", "CAGR", "MaxDD", "Vol", "Sharpe", "Sortino", "Calmar")]

cat("\n================ FULL HISTORY COMPARISON (June 2006 - October 2026) ================\n")
print(tab_full, row.names = FALSE)

# Sub-Period Analysis
periods <- list(
  "2008 Financial Crisis (2007-10 to 2009-03)" = cal >= as.Date("2007-10-31") & cal <= as.Date("2009-03-09"),
  "Post-Crisis Bull Market (2009-03 to 2021-12)" = cal >= as.Date("2009-03-09") & cal <= as.Date("2021-12-31"),
  "2022 Tech Bear Market (2021-12 to 2022-12)" = cal >= as.Date("2021-12-31") & cal <= as.Date("2022-12-31"),
  "AI Bull Rebound (2022-12 to 2026-10)"       = cal >= as.Date("2022-12-31") & cal <= as.Date("2026-10-30")
)

cat("\n================ SUB-PERIOD PERFORMANCE COMPARISON ================\n")
for (p_name in names(periods)) {
  idx <- periods[[p_name]]
  d_sub <- cal[idx]
  cat(sprintf("\n--- %s ---\n", p_name))
  sub_tab <- do.call(rbind, lapply(runs, function(r) {
    r_sub <- r[idx]
    c_sub <- prod(1 + r_sub) - 1
    c_cum <- cumprod(1 + r_sub)
    mdd_sub <- -min(c_cum / cummax(c_cum) - 1) * 100
    n_yr <- length(r_sub) / 252
    cagr_sub <- ((1 + c_sub)^(1 / n_yr) - 1) * 100
    data.frame(
      Total_Return = sprintf("%+.1f%%", c_sub * 100),
      CAGR         = sprintf("%+.1f%%", cagr_sub),
      MaxDD        = sprintf("-%.1f%%", mdd_sub),
      stringsAsFactors = FALSE
    )
  }))
  sub_tab$Asset <- names(runs)
  print(sub_tab[, c("Asset", "Total_Return", "CAGR", "MaxDD")], row.names = FALSE)
}

# Generate Visual Comparison Chart
chart_path <- file.path(ART, "qqq_vs_qld_20yr_comparison.png")
png(chart_path, width = 1400, height = 1100, res = 130)
par(mfrow = c(2, 2), mar = c(4, 4.5, 2.5, 1.5), oma = c(0, 0, 2, 0))

eq_curves <- lapply(runs, function(r) 10000 * cumprod(1 + r))
dd_curves <- lapply(runs, function(r) { c_r <- cumprod(1 + r); (c_r / cummax(c_r) - 1) * 100 })
cols <- c("#1565c0", "#e65100", "#2e7d32")

# 1. Growth of $10,000 (Log Scale)
plot(cal, eq_curves[[2]], type = "l", col = cols[2], lwd = 2.4, log = "y",
     ylim = range(unlist(eq_curves)), main = "A. Growth of $10,000 (Log Scale, 2006-2026)",
     ylab = "Portfolio Value ($)", xlab = "")
lines(cal, eq_curves[[3]], col = cols[3], lwd = 2.4)
lines(cal, eq_curves[[1]], col = cols[1], lwd = 2.0, lty = 2)
grid(col = "#e0e0e0")
legend("topleft", bty = "n", cex = 0.85,
       legend = c(
         sprintf("QLD + 200 SMA Trend ($%s | %s CAGR)", format(round(tail(eq_curves[[3]], 1)), big.mark = ","), tab_full$CAGR[3]),
         sprintf("QLD 2x Buy & Hold ($%s | %s CAGR)", format(round(tail(eq_curves[[2]], 1)), big.mark = ","), tab_full$CAGR[2]),
         sprintf("QQQ 1x Buy & Hold ($%s | %s CAGR)", format(round(tail(eq_curves[[1]], 1)), big.mark = ","), tab_full$CAGR[1])
       ),
       col = c(cols[3], cols[2], cols[1]), lwd = c(2.4, 2.4, 2.0), lty = c(1, 1, 2))

# 2. Drawdowns Comparison
plot(cal, dd_curves[[2]], type = "l", col = cols[2], lwd = 1.8,
     ylim = c(-90, 0), main = "B. Underwater Drawdown Comparison (%)",
     ylab = "Drawdown (%)", xlab = "")
lines(cal, dd_curves[[3]], col = cols[3], lwd = 2.2)
lines(cal, dd_curves[[1]], col = cols[1], lwd = 1.8, lty = 2)
grid(col = "#e0e0e0")
legend("bottomleft", bty = "n", cex = 0.85,
       legend = c(
         sprintf("QLD 2x Buy & Hold (MaxDD: %s)", tab_full$MaxDD[2]),
         sprintf("QQQ 1x Buy & Hold (MaxDD: %s)", tab_full$MaxDD[1]),
         sprintf("QLD + 200 SMA Trend (MaxDD: %s)", tab_full$MaxDD[3])
       ),
       col = c(cols[2], cols[1], cols[3]), lwd = c(1.8, 1.8, 2.2), lty = c(1, 2, 1))

# 3. Rolling 3-Year Annualized Return Spread (QLD minus QQQ)
roll_3yr_q <- rollapply(rq, width = 252*3, FUN = function(x) (prod(1 + x)^(252 / length(x)) - 1)*100, align = "right", fill = NA)
roll_3yr_l <- rollapply(rl, width = 252*3, FUN = function(x) (prod(1 + x)^(252 / length(x)) - 1)*100, align = "right", fill = NA)
spread_3yr <- roll_3yr_l - roll_3yr_q

plot(cal, spread_3yr, type = "l", col = ifelse(spread_3yr >= 0, "#2e7d32", "#c62828"),
     lwd = 1.8, main = "C. Rolling 3-Year CAGR Spread (QLD minus QQQ)",
     ylab = "Spread (% points)", xlab = "")
abline(h = 0, lty = 2, col = "#424242", lwd = 1.5)
grid(col = "#e0e0e0")

# 4. Bar Chart: Crisis Drawdown Comparison
dd_crisis <- matrix(c(
  -53.4, -82.9, -32.5,  # 2008 GFC
  -27.5, -51.2, -31.0,  # 2020 Covid
  -32.6, -64.4, -20.6   # 2022 Bear
), nrow = 3, byrow = TRUE)
colnames(dd_crisis) <- c("QQQ (1x)", "QLD (2x)", "QLD+Trend")
rownames(dd_crisis) <- c("2008 GFC", "2020 Covid", "2022 Bear")

barplot(t(dd_crisis), beside = TRUE, col = c(cols[1], cols[2], cols[3]),
        main = "D. Major Crisis Max Drawdowns (%)", ylab = "Drawdown (%)", ylim = c(-90, 0))
grid(nx = NA, ny = NULL, col = "#e0e0e0")
legend("bottomleft", bty = "n", cex = 0.82, legend = colnames(dd_crisis), fill = c(cols[1], cols[2], cols[3]))

dev.off()
cat(sprintf("Chart saved to: %s\n", chart_path))
