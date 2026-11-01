#!/usr/bin/env Rscript
# ==============================================================================
# scratch/backtest_10yr_broad_top3.R
# 10-year backtest (2016-10 .. 2026-10) of the live configuration:
#   universe = symbols_broad.txt, top 3, dual RS (50/50 126d/63d), stops -> QLD,
#   QLD core, 1.5x margin gated by QQQ 200 SMA, 22% vol target -> GLD, -20% stop,
#   breadth gate OFF. Compared with alternatives and the PIT Nasdaq-100 reference.
# ==============================================================================
src <- readLines("scratch/compare_symbol_lists.R"); idx <- grep("^lists <- c\\(", src)
eval(parse(text = src[1:(idx - 1)]))   # data, sim(), set_universe(), PIT_G

S10 <- as.Date("2016-10-03"); E10 <- as.Date("2026-10-06")
run <- function(...) sim(..., stop_to = "core", start = S10, end = E10)
yearly <- function(r) { d <- cal[cal >= S10 & cal <= E10]; tapply(r, format(d, "%Y"), function(x) (prod(1 + x) - 1) * 100) }
met2 <- function(r) { c_r <- cumprod(1 + r); n <- length(r); dd <- c_r / cummax(c_r) - 1
  d <- cal[cal >= S10 & cal <= E10]
  c(CAGR = (tail(c_r, 1)^(252 / n) - 1) * 100, MaxDD = -min(dd) * 100,
    Vol = sd(r) * sqrt(252) * 100, Sharpe = mean(r) / sd(r) * sqrt(252),
    Calmar = ((tail(c_r, 1)^(252 / n) - 1) * 100) / (-min(dd) * 100),
    Worst_Mo = min(tapply(r, format(d, "%Y-%m"), function(x) (prod(1 + x) - 1) * 100)),
    End_8k = 8000 * tail(c_r, 1)) }

ev10 <- which(cal >= S10 & cal <= E10)
res <- list(); res[["QQQ Buy & Hold"]] <- rq[ev10]
invisible(set_universe(read_list("symbols_broad.txt")))
res[["LIVE: broad, top3, Opt6 (1.5x)"]]  <- run(margin = 1.5, top_n = 3)
res[["broad, top3, Opt3 (cash/IRA)"]]    <- run(margin = 1.0, top_n = 3)
res[["broad, top5, Opt6"]]                <- run(margin = 1.5, top_n = 5)
res[["broad, top3, Opt6 + breadth gate"]] <- { BREADTH <<- sapply(seq_along(cal), function(i) { m <- !is.na(SMA200[i, ]); if (sum(m) < 20) NA else mean(P[i, m] >= SMA200[i, m]) }); run(margin = 1.5, top_n = 3, breadth_gate = 0.5) }
for (nm in names(PIT_G)) assign(nm, PIT_G[[nm]])
res[["REF: PIT Nasdaq-100, top3, Opt6"]]  <- run(margin = 1.5, top_n = 3)
res[["REF: PIT Nasdaq-100, top3, Opt3"]]  <- run(margin = 1.0, top_n = 3)

cat("\n=== 10-YEAR BACKTEST: 2016-10-03 .. 2026-10-06 ($8,000 start) ===\n")
print(round(t(sapply(res, met2)), 2))
cat("\n=== CALENDAR-YEAR RETURNS (%) ===\n")
print(round(sapply(res[c(1, 2, 3, 6)], yearly), 1))

ART <- "/Users/cmejo/.gemini/antigravity/brain/64019830-0cac-4bb1-904e-d4560e71468c"
png(file.path(ART, "backtest_10yr_broad_top3.png"), width = 1600, height = 900, res = 120)
par(mfrow = c(2, 1), mar = c(3.5, 4.5, 2.5, 1.5)); d <- cal[ev10]; sel <- res[c(1, 2, 3, 6)]
cols <- c("gray50", "#c0392b", "#2980b9", "#27ae60"); lt <- c(2, 1, 1, 3)
matplot(d, sapply(sel, function(r) cumprod(1 + r) * 8000), type = "l", lty = lt, lwd = 2, col = cols, log = "y",
        yaxt = "n", xlab = "", ylab = "$ (log)", main = "10-Year Backtest: Live Config (symbols_broad.txt, Top 3) vs References")
axis(2, at = c(8e3, 3e4, 1e5, 3e5, 1e6, 3e6), labels = c("$8k", "$30k", "$100k", "$300k", "$1M", "$3M"), las = 2); grid()
legend("topleft", names(sel), col = cols, lty = lt, lwd = 2, bty = "n", cex = 0.8)
matplot(d, sapply(sel, function(r) { c_r <- cumprod(1 + r); (c_r / cummax(c_r) - 1) * 100 }), type = "l", lty = lt,
        lwd = 1.5, col = cols, ylab = "Drawdown %", xlab = "", main = "Drawdowns"); grid()
invisible(dev.off())
