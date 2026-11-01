#!/usr/bin/env Rscript
# ==============================================================================
# factsheet/build_factsheet_data.R
# Computes the survivorship-free (point-in-time Nasdaq-100) backtest series for
# the factsheet: Option 6 (1.5x margin), Option 3 (cash/IRA), QQQ benchmark.
# $10,000 initial investment, 2007-01-02 .. 2026-10-06, top 3, stops -> QLD.
# Outputs: factsheet/data/*.csv|json and factsheet/img/*.png
# ==============================================================================
src <- readLines("scratch/pit_feature_search.R"); idx <- grep("^args <- commandArgs", src)
eval(parse(text = src[1:(idx - 1)]))
suppressPackageStartupMessages(library(jsonlite))
dir.create("factsheet/data", recursive = TRUE, showWarnings = FALSE)
dir.create("factsheet/img", recursive = TRUE, showWarnings = FALSE)

INIT <- 10000; TOPN <- 3
ev <- which(cal >= EVAL_START & cal <= EVAL_END); d <- cal[ev]
ser <- list(
  opt6 = sim(margin = 1.5, top_n = TOPN, stop_to = "core"),
  opt3 = sim(margin = 1.0, top_n = TOPN, stop_to = "core"),
  qqq  = rq[ev])
eqc <- sapply(ser, function(r) INIT * cumprod(1 + r))
ddc <- sapply(ser, function(r) { c_r <- cumprod(1 + r); (c_r / cummax(c_r) - 1) * 100 })
write.csv(data.frame(date = d, round(eqc, 2)), "factsheet/data/equity.csv", row.names = FALSE)

ym <- format(d, "%Y-%m"); yy <- format(d, "%Y")
mret <- sapply(ser, function(r) tapply(r, ym, function(x) prod(1 + x) - 1))
yret <- sapply(ser, function(r) tapply(r, yy, function(x) prod(1 + x) - 1))
rf_m <- tapply(rf_daily[ev], ym, sum)

stats <- function(r, key) {
  c_r <- cumprod(1 + r); n <- length(r); yrs <- n / 252
  cagr <- tail(c_r, 1)^(1 / yrs) - 1; dd <- c_r / cummax(c_r) - 1
  ex <- r - rf_daily[ev]; dn <- sqrt(mean(pmin(ex, 0)^2)) * sqrt(252)
  trough <- which.min(dd); peak <- which.max(c_r[1:trough])
  rec <- which(c_r[trough:n] >= c_r[peak])[1]
  mr <- mret[, key]; yr <- yret[, key]
  list(cagr = cagr, total = tail(c_r, 1) - 1, end_value = INIT * tail(c_r, 1),
       vol = sd(r) * sqrt(252), sharpe = mean(ex) / sd(r) * sqrt(252), sortino = mean(ex) * 252 / dn,
       maxdd = min(dd), calmar = cagr / abs(min(dd)),
       dd_peak = as.character(d[peak]), dd_trough = as.character(d[trough]),
       dd_recovery = if (is.na(rec)) "not recovered" else as.character(d[trough + rec - 1]),
       best_month = max(mr), worst_month = min(mr), pct_pos_months = mean(mr > 0),
       best_year = max(yr[names(yr) != "2026"]), worst_year = min(yr[names(yr) != "2026"]),
       best_year_lbl = names(which.max(yr[names(yr) != "2026"])), worst_year_lbl = names(which.min(yr[names(yr) != "2026"])))
}
st <- lapply(names(ser), function(k) stats(ser[[k]], k)); names(st) <- names(ser)
subper <- function(r, a, b) { m <- d >= as.Date(a) & d <= as.Date(b); c_r <- cumprod(1 + r[m]); n <- sum(m)
  c(cagr = tail(c_r, 1)^(252 / n) - 1, maxdd = min(c_r / cummax(c_r) - 1)) }
periods <- list(`2007-2016` = c("2007-01-01", "2016-12-31"), `2017-2026` = c("2017-01-01", "2026-12-31"),
                `Last 5 yrs` = c(as.character(EVAL_END - 365 * 5), as.character(EVAL_END)),
                `Last 3 yrs` = c(as.character(EVAL_END - 365 * 3), as.character(EVAL_END)))
subp <- lapply(periods, function(p) sapply(ser, function(r) subper(r, p[1], p[2])))
crisis <- list(`2008 GFC (Oct 07 - Mar 09)` = c("2007-10-31", "2009-03-09"),
               `2020 COVID (Feb - Mar 20)` = c("2020-02-19", "2020-03-23"),
               `2022 Bear (Jan - Dec 22)` = c("2022-01-03", "2022-12-30"))
cris <- lapply(crisis, function(p) sapply(ser, function(r) { m <- d >= as.Date(p[1]) & d <= as.Date(p[2]); prod(1 + r[m]) - 1 }))

write_json(list(stats = st, yearly = as.data.frame(yret), sub = subp, crisis = cris,
                start = as.character(d[1]), end = as.character(tail(d, 1)), init = INIT, top_n = TOPN,
                n_months = nrow(mret)), "factsheet/data/factsheet.json", auto_unbox = TRUE, digits = 6)
write.csv(data.frame(month = rownames(mret), round(mret * 100, 2)), "factsheet/data/monthly.csv", row.names = FALSE)

# ---------------- Charts ----------------
C6 <- "#0B3D91"; C3 <- "#2E8B57"; CQ <- "#9AA0A6"
fmt_k <- function(x) ifelse(x >= 1e6, sprintf("$%.1fM", x / 1e6), sprintf("$%sk", formatC(x / 1e3, format = "d", big.mark = ",")))
png("factsheet/img/growth.png", width = 2250, height = 760, res = 250)
par(mar = c(1.8, 3.6, 0.4, 3.6), family = "sans", las = 1, cex.axis = 0.8, mgp = c(3, 0.5, 0), tcl = -0.25)
plot(d, eqc[, "opt6"], type = "n", log = "y", ylim = range(eqc), xlab = "", ylab = "", axes = FALSE)
tk <- c(1e4, 2e4, 5e4, 1e5, 2e5, 5e5, 1e6, 2e6, 5e6, 1e7); tk <- tk[tk >= min(eqc) * 0.9 & tk <= max(eqc) * 1.1]
abline(h = tk, col = "#E8EAED", lwd = 0.8); axis(2, at = tk, labels = fmt_k(tk), lwd = 0, col.axis = "#5F6368")
yrs <- seq(as.Date("2008-01-01"), as.Date("2026-01-01"), by = "2 years"); axis(1, at = yrs, labels = format(yrs, "%Y"), lwd = 0, col.axis = "#5F6368")
lines(d, eqc[, "qqq"], col = CQ, lwd = 1.6); lines(d, eqc[, "opt3"], col = C3, lwd = 1.8); lines(d, eqc[, "opt6"], col = C6, lwd = 2.2)
for (k in c("opt6", "opt3", "qqq")) { v <- tail(eqc[, k], 1); text(tail(d, 1), v, fmt_k(v), pos = 4, xpd = NA, cex = 0.72, col = c(opt6 = C6, opt3 = C3, qqq = "#5F6368")[k], font = 2, offset = 0.3) }
dev.off()

png("factsheet/img/drawdown.png", width = 2250, height = 430, res = 250)
par(mar = c(1.6, 3.6, 0.3, 3.6), las = 1, cex.axis = 0.8, mgp = c(3, 0.5, 0), tcl = -0.25)
plot(d, ddc[, "opt6"], type = "n", ylim = c(min(ddc) * 1.03, 0), xlab = "", ylab = "", axes = FALSE)
abline(h = seq(-70, 0, 10), col = "#E8EAED", lwd = 0.8); axis(2, at = seq(-60, 0, 20), labels = paste0(seq(-60, 0, 20), "%"), lwd = 0, col.axis = "#5F6368")
axis(1, at = yrs, labels = format(yrs, "%Y"), lwd = 0, col.axis = "#5F6368")
polygon(c(d, rev(d)), c(ddc[, "opt6"], rep(0, length(d))), col = adjustcolor(C6, 0.18), border = NA)
lines(d, ddc[, "qqq"], col = CQ, lwd = 1.2); lines(d, ddc[, "opt3"], col = C3, lwd = 1.3); lines(d, ddc[, "opt6"], col = C6, lwd = 1.5)
dev.off()

png("factsheet/img/annual.png", width = 2250, height = 560, res = 250)
par(mar = c(1.6, 3.6, 0.3, 3.6), las = 1, cex.axis = 0.72, mgp = c(3, 0.5, 0), tcl = -0.25)
yv <- t(yret[, c("opt6", "opt3", "qqq")]) * 100; yv <- pmax(pmin(yv, 300), -80)
bp <- barplot(yv, beside = TRUE, col = c(C6, C3, CQ), border = NA, axes = FALSE, ylim = c(min(yv) * 1.15, max(yv) * 1.1), space = c(0, 0.6), names.arg = rep("", ncol(yv)))
abline(h = seq(-50, 300, 50), col = "#E8EAED", lwd = 0.8); abline(h = 0, col = "#5F6368", lwd = 0.8)
barplot(yv, beside = TRUE, col = c(C6, C3, CQ), border = NA, axes = FALSE, add = TRUE, space = c(0, 0.6), names.arg = rep("", ncol(yv)))
axis(2, at = seq(-50, 300, 50), labels = paste0(seq(-50, 300, 50), "%"), lwd = 0, col.axis = "#5F6368")
axis(1, at = colMeans(bp), labels = sub("^20", "'", colnames(yv)), lwd = 0, col.axis = "#5F6368", cex.axis = 0.7)
dev.off()
cat("Factsheet data and charts written.\n"); str(st$opt6[c("cagr", "maxdd", "end_value", "sharpe")])
