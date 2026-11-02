#!/usr/bin/env Rscript
# ==============================================================================
# factsheet/build_ultimate_alpha_factsheet_data.R
# Computes backtest statistics and high-res chart assets for
# Option D: Ultimate Alpha Master (Buffer Top 8 + 2.0x Turbo + -25% Stop)
# vs Power Alpha Turbo Baseline vs QQQ Benchmark.
# ==============================================================================

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(jsonlite)
})

# Load the engine definition from scratch/test_buffer_leverage_synergies.R up to line 233
src <- readLines("scratch/test_buffer_leverage_synergies.R")
func_end <- grep("attr\\(out, \"turnover_yr\"\\)", src)[1]
eval(parse(text = paste(src[1:(func_end + 2)], collapse = "\n")))

dir.create("factsheet/data", recursive = TRUE, showWarnings = FALSE)
dir.create("factsheet/img", recursive = TRUE, showWarnings = FALSE)

INIT <- 10000
ev <- which(cal >= S20 & cal <= E20)
d <- cal[ev]

cat("Running 20-Year Simulation for Ultimate Alpha Master...\n")
ret_ultimate <- sim_buffer_engine(buffer_rank = 8, disaster_stop_pct = 0.25,
                                  super_bull_leverage = 2.00, use_breadth_turbo = FALSE,
                                  start = S20, end = E20)

cat("Running 20-Year Simulation for Power Alpha Baseline...\n")
ret_baseline <- sim_buffer_engine(buffer_rank = 5, disaster_stop_pct = 0.20,
                                  super_bull_leverage = 1.75, use_breadth_turbo = FALSE,
                                  start = S20, end = E20)

ret_qqq <- rq[ev]

ser <- list(
  ultimate = as.numeric(ret_ultimate),
  baseline = as.numeric(ret_baseline),
  qqq      = as.numeric(ret_qqq)
)

eqc <- sapply(ser, function(r) INIT * cumprod(1 + r))
ddc <- sapply(ser, function(r) { c_r <- cumprod(1 + r); (c_r / cummax(c_r) - 1) * 100 })
write.csv(data.frame(date = d, round(eqc, 2)), "factsheet/data/ultimate_equity.csv", row.names = FALSE)

ym <- format(d, "%Y-%m")
yy <- format(d, "%Y")
mret <- sapply(ser, function(r) tapply(r, ym, function(x) prod(1 + x) - 1))
yret <- sapply(ser, function(r) tapply(r, yy, function(x) prod(1 + x) - 1))

stats_func <- function(r, key) {
  c_r <- cumprod(1 + r)
  n <- length(r)
  yrs <- n / 252
  cagr <- tail(c_r, 1)^(1 / yrs) - 1
  dd <- c_r / cummax(c_r) - 1
  ex <- r - rf_daily[ev]
  dn <- sqrt(mean(pmin(ex, 0)^2)) * sqrt(252)
  trough <- which.min(dd)
  peak <- which.max(c_r[1:trough])
  rec <- which(c_r[trough:n] >= c_r[peak])[1]
  mr <- mret[, key]
  yr <- yret[, key]
  
  list(
    cagr = cagr,
    total = tail(c_r, 1) - 1,
    end_value = INIT * tail(c_r, 1),
    vol = sd(r) * sqrt(252),
    sharpe = mean(ex) / sd(r) * sqrt(252),
    sortino = mean(ex) * 252 / dn,
    maxdd = min(dd),
    calmar = cagr / abs(min(dd)),
    dd_peak = as.character(d[peak]),
    dd_trough = as.character(d[trough]),
    dd_recovery = if (is.na(rec)) "not recovered" else as.character(d[trough + rec - 1]),
    best_month = max(mr),
    worst_month = min(mr),
    pct_pos_months = mean(mr > 0),
    best_year = max(yr[names(yr) != "2026"]),
    worst_year = min(yr[names(yr) != "2026"]),
    best_year_lbl = names(which.max(yr[names(yr) != "2026"])),
    worst_year_lbl = names(which.min(yr[names(yr) != "2026"]))
  )
}

st <- lapply(names(ser), function(k) stats_func(ser[[k]], k))
names(st) <- names(ser)

subper <- function(r, a, b) {
  m <- d >= as.Date(a) & d <= as.Date(b)
  c_r <- cumprod(1 + r[m])
  n <- sum(m)
  c(cagr = tail(c_r, 1)^(252 / n) - 1, maxdd = min(c_r / cummax(c_r) - 1))
}

periods <- list(
  `2007-2016`  = c("2007-01-01", "2016-12-31"),
  `2017-2026`  = c("2017-01-01", "2026-12-31"),
  `Last 10 yrs` = c(as.character(E20 - 365 * 10), as.character(E20)),
  `Last 5 yrs`  = c(as.character(E20 - 365 * 5), as.character(E20)),
  `Last 3 yrs`  = c(as.character(E20 - 365 * 3), as.character(E20))
)

subp <- lapply(periods, function(p) sapply(ser, function(r) subper(r, p[1], p[2])))

crisis <- list(
  `2008 GFC (Oct 07 - Mar 09)` = c("2007-10-31", "2009-03-09"),
  `2018 Trade War (Oct - Dec 18)` = c("2018-10-01", "2018-12-24"),
  `2020 COVID (Feb - Mar 20)`    = c("2020-02-19", "2020-03-23"),
  `2022 Fed Rate Hikes (Jan - Dec 22)` = c("2022-01-03", "2022-12-30")
)

cris <- lapply(crisis, function(p) sapply(ser, function(r) {
  m <- d >= as.Date(p[1]) & d <= as.Date(p[2])
  prod(1 + r[m]) - 1
}))

write_json(list(
  stats = st,
  yearly = as.data.frame(yret),
  sub = subp,
  crisis = cris,
  start = as.character(d[1]),
  end = as.character(tail(d, 1)),
  init = INIT,
  top_n = 5,
  n_months = nrow(mret)
), "factsheet/data/ultimate_factsheet.json", auto_unbox = TRUE, digits = 6)

write.csv(data.frame(month = rownames(mret), round(mret * 100, 2)), "factsheet/data/ultimate_monthly.csv", row.names = FALSE)

# ---------------- High-Resolution Chart Generation ----------------
C_ULT <- "#0B3D91" # Navy Blue (Ultimate Alpha)
C_BAS <- "#2E8B57" # SeaGreen (Baseline)
C_QQQ <- "#9AA0A6" # Muted Gray (Benchmark)

fmt_k <- function(x) {
  ifelse(x >= 1e9, sprintf("$%.2fB", x / 1e9),
  ifelse(x >= 1e6, sprintf("$%.1fM", x / 1e6),
         sprintf("$%sk", formatC(x / 1e3, format = "d", big.mark = ","))))
}

cat("Rendering growth chart...\n")
png("factsheet/img/ultimate_growth.png", width = 2250, height = 760, res = 250)
par(mar = c(1.8, 3.8, 0.4, 4.0), family = "sans", las = 1, cex.axis = 0.8, mgp = c(3, 0.5, 0), tcl = -0.25)
plot(d, eqc[, "ultimate"], type = "n", log = "y", ylim = range(eqc), xlab = "", ylab = "", axes = FALSE)
tk <- c(1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 2e9)
abline(h = tk, col = "#E8EAED", lwd = 0.8)
axis(2, at = tk, labels = fmt_k(tk), lwd = 0, col.axis = "#5F6368")
yrs <- seq(as.Date("2008-01-01"), as.Date("2026-01-01"), by = "2 years")
axis(1, at = yrs, labels = format(yrs, "%Y"), lwd = 0, col.axis = "#5F6368")

lines(d, eqc[, "qqq"], col = C_QQQ, lwd = 1.6)
lines(d, eqc[, "baseline"], col = C_BAS, lwd = 1.8)
lines(d, eqc[, "ultimate"], col = C_ULT, lwd = 2.4)

for (k in c("ultimate", "baseline", "qqq")) {
  v <- tail(eqc[, k], 1)
  text(tail(d, 1), v, fmt_k(v), pos = 4, xpd = NA, cex = 0.72,
       col = c(ultimate = C_ULT, baseline = C_BAS, qqq = "#5F6368")[k], font = 2, offset = 0.3)
}
dev.off()

cat("Rendering drawdown chart...\n")
png("factsheet/img/ultimate_drawdown.png", width = 2250, height = 430, res = 250)
par(mar = c(1.6, 3.8, 0.3, 4.0), las = 1, cex.axis = 0.8, mgp = c(3, 0.5, 0), tcl = -0.25)
plot(d, ddc[, "ultimate"], type = "n", ylim = c(-70, 0), xlab = "", ylab = "", axes = FALSE)
abline(h = seq(-70, 0, 10), col = "#E8EAED", lwd = 0.8)
axis(2, at = seq(-60, 0, 20), labels = paste0(seq(-60, 0, 20), "%"), lwd = 0, col.axis = "#5F6368")
axis(1, at = yrs, labels = format(yrs, "%Y"), lwd = 0, col.axis = "#5F6368")

polygon(c(d, rev(d)), c(ddc[, "ultimate"], rep(0, length(d))), col = adjustcolor(C_ULT, 0.18), border = NA)
lines(d, ddc[, "qqq"], col = C_QQQ, lwd = 1.2)
lines(d, ddc[, "baseline"], col = C_BAS, lwd = 1.3)
lines(d, ddc[, "ultimate"], col = C_ULT, lwd = 1.6)
dev.off()

cat("Rendering annual returns bar chart...\n")
png("factsheet/img/ultimate_annual.png", width = 2250, height = 560, res = 250)
par(mar = c(1.6, 3.8, 0.3, 4.0), las = 1, cex.axis = 0.72, mgp = c(3, 0.5, 0), tcl = -0.25)
yv <- t(yret[, c("ultimate", "baseline", "qqq")]) * 100
yv <- pmax(pmin(yv, 300), -80)
bp <- barplot(yv, beside = TRUE, col = c(C_ULT, C_BAS, C_QQQ), border = NA, axes = FALSE,
              ylim = c(-60, 320), space = c(0, 0.6), names.arg = rep("", ncol(yv)))
abline(h = seq(-50, 300, 50), col = "#E8EAED", lwd = 0.8)
abline(h = 0, col = "#5F6368", lwd = 0.8)
barplot(yv, beside = TRUE, col = c(C_ULT, C_BAS, C_QQQ), border = NA, axes = FALSE,
        add = TRUE, space = c(0, 0.6), names.arg = rep("", ncol(yv)))
axis(2, at = seq(-50, 300, 50), labels = paste0(seq(-50, 300, 50), "%"), lwd = 0, col.axis = "#5F6368")
axis(1, at = colMeans(bp), labels = sub("^20", "'", colnames(yv)), lwd = 0, col.axis = "#5F6368", cex.axis = 0.7)
dev.off()

cat("All factsheet assets and statistics successfully generated!\n")
print(st$ultimate[c("cagr", "maxdd", "end_value", "sharpe", "sortino", "calmar")])
