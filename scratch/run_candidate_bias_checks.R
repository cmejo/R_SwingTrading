#!/usr/bin/env Rscript
# Bias diagnostics for candidates B and C
invisible(capture.output(source("scratch/run_three_candidates.R")))

# 1) Fair benchmark for B: same 80/20 split but the satellite = equal-weight universe
strat_B_EW <- function(s) {
  if (!month_end[s] && s != ev[1] - 1) return(NULL)
  ok <- colnames(P)[eligible(s, 252)]
  c(QQQ = 0.80, setNames(rep(0.20 / length(ok), length(ok)), ok))
}
# 2) Restrict universe to stocks already listed before the test starts (removes post-2006 IPO picks)
old <- colnames(P)[!is.na(P[ev[1] - 252, ])]
eligible_old <- function(s, lb = 252) eligible(s, lb) & colnames(P) %in% old
strat_C_old <- function(s) {
  if (!(month_end[s] || s == ev[1] - 1)) return(NULL)
  ok <- which(eligible_old(s, 252)); mom <- P[s - 21, ok] / P[s - 252, ok] - 1
  top <- names(sort(mom, decreasing = TRUE))[1:min(10, length(mom))]; setNames(rep(0.1, length(top)), top)
}
strat_EW_old <- function(s) {
  if (!month_end[s] && s != ev[1] - 1) return(NULL)
  ok <- colnames(P)[eligible_old(s, 252)]; setNames(rep(1 / length(ok), length(ok)), ok)
}
# 3) Random top-10 (same monthly turnover as C) -> what does ANY 10-stock pick in this universe do?
set.seed(7); rand_runs <- replicate(20, simulate(function(s) {
  if (!(month_end[s] || s == ev[1] - 1)) return(NULL)
  ok <- colnames(P)[eligible(s, 252)]; pk <- sample(ok, min(10, length(ok))); setNames(rep(0.1, length(pk)), pk)
}, "rand"), simplify = FALSE)

extra <- list(
  "B   80% QQQ + 20% top-5 RS"           = runs[["B.  80% QQQ + 20% top-5 RS"]],
  "B0  80% QQQ + 20% EW universe"        = simulate(strat_B_EW, "B0"),
  "C1  Top-10 momentum (all 151)"        = runs[["C1. Top-10 12-1 momentum"]],
  "EW  all 151"                          = runs[["EW universe (survivorship gauge)"]],
  "C1o Top-10 momentum (pre-2006 names)" = simulate(strat_C_old, "C1o"),
  "EWo EW pre-2006 names"                = simulate(strat_EW_old, "EWo"))
ce <- function(r, m = rep(TRUE, length(r))) (prod(1 + r[m])^(252 / sum(m)) - 1) * 100
cat(sprintf("\nPre-2006-listed names: %d of %d\n\n", length(old), ncol(P)))
tab <- data.frame(Strategy = names(extra),
  CAGR = sprintf("%+.2f%%", sapply(extra, ce)),
  CAGR_2006_23 = sprintf("%+.2f%%", sapply(extra, ce, m = dates < HOLDOUT)),
  CAGR_Holdout = sprintf("%+.2f%%", sapply(extra, ce, m = dates >= HOLDOUT)),
  MaxDD = sprintf("-%.1f%%", sapply(extra, function(r) { c_r <- cumprod(1 + r); -min(c_r / cummax(c_r) - 1) * 100 })),
  row.names = NULL)
print(tab, row.names = FALSE)
rc <- sapply(rand_runs, ce)
cat(sprintf("\nRandom 10-stock monthly picks (20 trials, all 151): median CAGR %+.2f%% | range %+.2f%% to %+.2f%%\n",
            median(rc), min(rc), max(rc)))
cat(sprintf("QQQ total return CAGR: %+.2f%%\n", ce(runs[[1]])))
pairs <- list(c(1, 2), c(3, 4), c(5, 6))
for (p in pairs) {
  ex <- extra[[p[1]]] - extra[[p[2]]]
  cat(sprintf("%-38s vs %-34s: excess %+.2f%%/yr, t = %.2f\n", names(extra)[p[1]], names(extra)[p[2]],
              mean(ex) * 252 * 100, mean(ex) / sd(ex) * sqrt(length(ex))))
}
write.csv(tab, "output/three_candidates_bias_checks.csv", row.names = FALSE)
