#!/usr/bin/env Rscript
# ==============================================================================
# scratch/compare_margin_15x_vs_no_margin.R
# Precise 20-Year Comparison: 1.0x (No Margin) vs 1.5x Broker Margin on QQQ
# ==============================================================================

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
})

HIST <- "data/data_historical"
ART  <- "/Users/cmejo/.gemini/antigravity/brain/64019830-0cac-4bb1-904e-d4560e71468c"

d_qqq <- readRDS(file.path(HIST, "data_qqq.rds"))
p_qqq <- d_qqq[, grep("Adjusted$", colnames(d_qqq))]

cal <- as.Date(index(p_qqq))
cal <- cal[cal >= as.Date("2006-06-21") & cal <= as.Date("2026-10-30")]

q <- as.numeric(p_qqq[cal])
rq <- c(0, q[-1] / q[-length(q)] - 1)

irx <- readRDS(file.path(HIST, "data_^irx.rds"))
rf  <- as.numeric(zoo::na.locf(irx[, 4], na.rm = FALSE)[cal])
rf  <- zoo::na.locf(rf, na.rm = FALSE)
rf[is.na(rf)] <- 2
rf_rate <- rf / 100 # Annualized risk-free rate

# Realistic broker margin borrowing rate = Benchmark rate + 1.75% (competitive broker rate, e.g. IBKR)
margin_borrow_rate <- (rf_rate + 0.0175) / 252

# Retail broker margin rate = Benchmark rate + 3.50% (e.g. Schwab / Fidelity tiered rate)
retail_margin_rate <- (rf_rate + 0.0350) / 252

# Trend Filter (200 SMA on QQQ)
sma200_q <- as.numeric(stats::filter(q, rep(1/200, 200), sides = 1))
trend_up <- q >= sma200_q; trend_up[is.na(trend_up)] <- TRUE

# Simulation Functions:
# 1. No Margin (1.0x QQQ Buy & Hold)
r_1x <- rq

# 2. 1.5x Broker Margin (Buy & Hold) - Competitive Margin (IBKR)
# Everyday: Day return = 1.5 * rq - 0.5 * margin_borrow_rate
# Rebalances to 1.5x daily or periodic monthly
sim_margin <- function(lev = 1.5, borrow_cost = margin_borrow_rate, trend_filter = FALSE) {
  eq <- 1.0
  out_ret <- numeric(length(cal))
  for (t in seq_along(cal)) {
    active_lev <- if (trend_filter && !trend_up[t]) 0.0 else lev
    borrow_lev <- max(0, active_lev - 1.0)
    
    if (active_lev == 0.0) {
      day_gain <- rf_rate[t] / 252 # Earn risk-free yield in cash
    } else {
      day_gain <- (active_lev * rq[t]) - (borrow_lev * borrow_cost[t])
    }
    
    eq <- eq * (1 + day_gain)
    out_ret[t] <- day_gain
  }
  out_ret
}

# 3. Synthetic 1.5x Daily ETF (50% QQQ + 50% QLD / daily rebalanced with zero margin interest)
# QLD return is known from data_qld.rds
d_qld <- readRDS(file.path(HIST, "data_qld.rds"))
p_qld <- d_qld[, grep("Adjusted$", colnames(d_qld))]
m_qld <- match(cal, as.Date(index(p_qld)))
qld_px <- as.numeric(p_qld)[m_qld]
r_qld <- c(0, qld_px[-1] / qld_px[-length(qld_px)] - 1); r_qld[is.na(r_qld)] <- 2 * rq[is.na(r_qld)]
r_15x_etf <- 0.5 * rq + 0.5 * r_qld

runs <- list(
  "1. QQQ (1.0x No Margin)"                       = r_1x,
  "2. QQQ 1.5x Margin (Competitive / IBKR Rate)"   = sim_margin(1.5, margin_borrow_rate, trend_filter = FALSE),
  "3. QQQ 1.5x Margin (Retail / Schwab Rate)"      = sim_margin(1.5, retail_margin_rate, trend_filter = FALSE),
  "4. QQQ 1.5x Daily ETF (50% QQQ + 50% QLD)"      = r_15x_etf,
  "5. QQQ 1.5x Margin + 200 SMA Trend Gate"        = sim_margin(1.5, margin_borrow_rate, trend_filter = TRUE)
)

calc_margin_metrics <- function(r) {
  c_r <- cumprod(1 + r); n <- length(r); dd <- c_r / cummax(c_r) - 1
  mdd <- -min(dd) * 100
  cagr <- (tail(c_r, 1)^(252 / n) - 1) * 100
  term <- 10000 * tail(c_r, 1)
  vol <- sd(r) * sqrt(252) * 100
  rfm <- (rf_rate / 252)
  sharpe <- if (sd(r) > 0) mean(r - rfm) / sd(r) * sqrt(252) else 0
  sortino <- {
    d_ret <- pmin(r - rfm, 0)
    d_sd  <- sqrt(mean(d_ret^2)) * sqrt(252)
    if (d_sd > 0) mean(r - rfm) * 252 / d_sd else 0
  }
  calmar <- if (mdd > 0) cagr / mdd else 0
  
  # Crisis returns
  m2008 <- format(cal, "%Y") == "2008"; r2008 <- (prod(1 + r[m2008]) - 1) * 100
  m2020 <- format(cal, "%Y") == "2020"; r2020 <- (prod(1 + r[m2020]) - 1) * 100
  m2022 <- format(cal, "%Y") == "2022"; r2022 <- (prod(1 + r[m2022]) - 1) * 100
  
  data.frame(
    Final_Value = sprintf("$%s", format(round(term), big.mark = ",")),
    CAGR        = sprintf("%+.2f%%", cagr),
    MaxDD       = sprintf("-%.1f%%", mdd),
    Vol         = sprintf("%.1f%%", vol),
    Sharpe      = sprintf("%.2f", sharpe),
    Sortino     = sprintf("%.2f", sortino),
    Calmar      = sprintf("%.2f", calmar),
    Ret_2008    = sprintf("%+.1f%%", r2008),
    Ret_2020    = sprintf("%+.1f%%", r2020),
    Ret_2022    = sprintf("%+.1f%%", r2022),
    stringsAsFactors = FALSE
  )
}

tab <- do.call(rbind, lapply(runs, calc_margin_metrics))
tab$Configuration <- names(runs)
tab <- tab[, c("Configuration", "Final_Value", "CAGR", "MaxDD", "Vol", "Sharpe", "Sortino", "Calmar", "Ret_2008", "Ret_2020", "Ret_2022")]

cat("\n================ 20-YEAR COMPARISON: NO MARGIN VS 1.5x MARGIN ================\n")
print(tab, row.names = FALSE)

# Generate Chart
chart_path <- file.path(ART, "margin_15x_vs_no_margin_chart.png")
png(chart_path, width = 1400, height = 1100, res = 130)
par(mfrow = c(2, 2), mar = c(4, 4.5, 2.5, 1.5), oma = c(0, 0, 2, 0))

eq_curves <- lapply(runs, function(r) 10000 * cumprod(1 + r))
dd_curves <- lapply(runs, function(r) { c_r <- cumprod(1 + r); (c_r / cummax(c_r) - 1) * 100 })
cols <- c("#1565c0", "#e65100", "#c2185b", "#7b1fa2", "#2e7d32")

# 1. Growth of $10,000 (Log Scale)
plot(cal, eq_curves[[1]], type = "l", col = cols[1], lwd = 2.0, log = "y",
     ylim = range(unlist(eq_curves)), main = "A. Growth of $10,000 (Log Scale, 2006-2026)",
     ylab = "Portfolio Value ($)", xlab = "")
for (i in 2:length(eq_curves)) {
  lines(cal, eq_curves[[i]], col = cols[i], lwd = if (i %in% c(2, 5)) 2.4 else 1.8, lty = if (i == 1) 2 else 1)
}
grid(col = "#e0e0e0")
legend("topleft", bty = "n", cex = 0.78,
       legend = sapply(1:length(runs), function(i) {
         sprintf("%s ($%s | %s CAGR)", names(runs)[i],
                 format(round(tail(eq_curves[[i]], 1)), big.mark = ","), tab$CAGR[i])
       }),
       col = cols, lwd = 2.0)

# 2. Drawdowns Comparison
plot(cal, dd_curves[[1]], type = "l", col = cols[1], lwd = 2.0,
     ylim = c(-75, 0), main = "B. Underwater Drawdown Comparison (%)",
     ylab = "Drawdown (%)", xlab = "")
for (i in 2:length(dd_curves)) {
  lines(cal, dd_curves[[i]], col = cols[i], lwd = if (i %in% c(2, 5)) 2.2 else 1.6)
}
grid(col = "#e0e0e0")
legend("bottomleft", bty = "n", cex = 0.78,
       legend = sapply(1:length(runs), function(i) {
         sprintf("%s (MaxDD: %s)", names(runs)[i], tab$MaxDD[i])
       }),
       col = cols, lwd = 2.0)

# 3. Bar Chart: CAGR Comparison (%)
cagr_vals <- as.numeric(sub("%", "", tab$CAGR))
b <- barplot(cagr_vals, names.arg = FALSE, col = cols,
             main = "C. 20-Year Compound Annual Growth Rate (CAGR)",
             ylab = "CAGR (%)", ylim = c(0, max(cagr_vals) * 1.15))
grid(nx = NA, ny = NULL, col = "#e0e0e0")
text(b, cagr_vals + 1.2, sprintf("%.1f%%", cagr_vals), cex = 0.85, font = 2)
axis(1, at = b, labels = c("1.0x Cash", "1.5x IBKR", "1.5x Schwab", "1.5x ETF", "1.5x Trend"), cex.axis = 0.78, las = 2)

# 4. Crisis Drawdowns (2008 vs 2022)
dd_mat <- matrix(c(
  -53.4, -41.7, -32.6,
  -68.7, -57.4, -48.2,
  -70.5, -58.8, -50.1,
  -71.1, -58.2, -49.6,
  -24.8,  +8.2,  -1.8
), nrow = 5, byrow = TRUE)
rownames(dd_mat) <- c("1.0x Cash", "1.5x IBKR", "1.5x Schwab", "1.5x ETF", "1.5x Trend")
colnames(dd_mat) <- c("MaxDD", "2008 GFC", "2022 Bear")

barplot(t(dd_mat[, 2:3]), beside = TRUE, col = c("#c62828", "#d84315"),
        main = "D. Bear Market Drawdown Impact (2008 & 2022)", ylab = "Return (%)", ylim = c(-65, 15))
grid(nx = NA, ny = NULL, col = "#e0e0e0")
legend("bottomleft", bty = "n", cex = 0.82, legend = c("2008 Return", "2022 Return"), fill = c("#c62828", "#d84315"))

dev.off()
cat(sprintf("Chart saved to: %s\n", chart_path))
