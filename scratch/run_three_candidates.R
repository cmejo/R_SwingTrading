#!/usr/bin/env Rscript
# ==============================================================================
# scratch/run_three_candidates.R
# Walk-forward test of three candidate strategies vs QQQ (Oct 2006 - Oct 2026)
#
#  A. QQQ + 200-day trend filter at 1.0x / 1.25x / 1.5x (T-bills when below SMA200)
#  B. Core-satellite: 80% QQQ + 20% equal-weight top-5 relative strength (126d vs QQQ), monthly
#  C. Top-10 12-1 momentum, equal weight, monthly (with and without QQQ trend filter)
#
# Execution / accounting (one engine for everything):
#   * Total-return (Adjusted) prices for every asset, including QQQ.
#   * Signal uses data through close(t-1); trades execute at close(t). Returns on day t
#     accrue to the OLD weights first; the rebalance happens at the close.
#   * 10 bps cost per unit of turnover. Weights drift between rebalances.
#   * Idle cash earns 13-week T-bill (^IRX); borrowed cash pays 7.0%/yr.
#   * Parameters are textbook defaults fixed in advance (not tuned).
# ==============================================================================
suppressPackageStartupMessages({ library(xts) })
HIST <- "data/data_historical"; OUT <- "output"
ART  <- "/Users/cmejo/.gemini/antigravity/brain/64019830-0cac-4bb1-904e-d4560e71468c"
EVAL_START <- as.Date("2006-10-02"); EVAL_END <- as.Date("2026-10-06")
COST <- 0.0010; MARGIN <- 0.070; HOLDOUT <- as.Date("2023-10-01")

adj <- function(sym) { d <- readRDS(file.path(HIST, sprintf("data_%s.rds", tolower(sym)))); a <- d[, grep("Adjusted$", colnames(d))]; a[a <= 0] <- NA; a }
qqq_p <- adj("QQQ")
cal <- index(qqq_p)[index(qqq_p) >= as.Date("2004-01-01") & index(qqq_p) <= EVAL_END]

EXCLUDE <- c("QQQ", "QLD", "TQQQ", "SPY", "VTI", "VFINX", "SMH", "SOXL", "XLK", "GLD_FULL",
             "AMUU", "NVDU", "PLTU", "SNDQ", "SMHC", "SKHY")
files <- list.files(HIST, pattern = "^data_[a-z0-9_]+[.]rds$")
syms  <- setdiff(toupper(sub("^data_", "", sub("[.]rds$", "", files))), EXCLUDE)
P <- matrix(NA_real_, length(cal), length(syms), dimnames = list(NULL, syms))
for (s in syms) {
  a <- tryCatch(adj(s), error = function(e) NULL); if (is.null(a)) next
  m <- match(cal, as.Date(index(a))); P[, s] <- as.numeric(a)[m]
}
P <- P[, colSums(!is.na(P)) > 300, drop = FALSE]
# forward-fill interior gaps only (never before first listing)
for (j in seq_len(ncol(P))) { v <- P[, j]; f <- which(!is.na(v))[1]; if (!is.na(f)) { v[f:length(v)] <- zoo::na.locf(v[f:length(v)], na.rm = FALSE) }; P[, j] <- v }
q <- as.numeric(qqq_p[cal])
R  <- rbind(NA, P[-1, , drop = FALSE] / P[-nrow(P), , drop = FALSE] - 1); R[is.na(R)] <- 0
rq <- c(0, q[-1] / q[-length(q)] - 1)

irx <- readRDS(file.path(HIST, "data_^irx.rds"))
rf <- as.numeric(zoo::na.locf(irx[, 4], na.rm = FALSE)[cal]); rf <- zoo::na.locf(rf, na.rm = FALSE); rf[is.na(rf)] <- 2
rf <- rf / 100 / 252

sma200 <- as.numeric(stats::filter(q, rep(1 / 200, 200), sides = 1))
trend_up <- q > sma200; trend_up[is.na(trend_up)] <- TRUE

ev <- which(cal >= EVAL_START)
month_end <- c(format(cal[-1], "%m") != format(cal[-length(cal)], "%m"), TRUE)
cat(sprintf("Universe: %d stocks (ETFs/leveraged excluded) | Eval: %s to %s (%d sessions)\n",
            ncol(P), cal[ev[1]], tail(cal, 1), length(ev)))

# ------------------------------------------------------------------------------
# Generic drifting-weights simulator. target_fn(s) returns a named weight vector
# (names in colnames(P) or "QQQ"); NULL means "no change". Called for signal day s,
# executed at close of s+1.
# ------------------------------------------------------------------------------
assets <- c(colnames(P), "QQQ")
RET <- cbind(R, QQQ = rq)
simulate <- function(target_fn, label) {
  w <- setNames(rep(0, length(assets)), assets); eq <- 1; out <- numeric(length(ev)); turn_tot <- 0; nreb <- 0
  for (k in seq_along(ev)) {
    t <- ev[k]
    r_t <- RET[t, ]
    cash <- 1 - sum(w)
    pr <- sum(w * r_t) + if (cash >= 0) cash * rf[t] else cash * MARGIN / 252
    eq <- eq * (1 + pr)
    w <- w * (1 + r_t) / (1 + pr)
    tgt <- target_fn(t - 1)
    if (!is.null(tgt)) {
      new_w <- setNames(rep(0, length(assets)), assets); new_w[names(tgt)] <- tgt
      to <- sum(abs(new_w - w))
      if (to > 1e-9) { eq <- eq * (1 - COST * to); turn_tot <- turn_tot + to; nreb <- nreb + 1 }
      w <- new_w
    }
    out[k] <- if (k == 1) pr else eq / prev_eq - 1
    prev_eq <- eq
  }
  attr(out, "turnover_yr") <- turn_tot / (length(ev) / 252)
  attr(out, "label") <- label
  out
}

eligible <- function(s, lb = 252) { ok <- !is.na(P[s, ]) & !is.na(P[pmax(1, s - lb), ]) & (s - lb) >= 1; ok }

# A. Trend-filtered QQQ at leverage L (re-levered monthly and on regime flips)
strat_A <- function(L, filter = TRUE) {
  last <- NA
  function(s) {
    up <- if (filter) trend_up[s] else TRUE
    flip <- is.na(last) || up != last; last <<- up
    if (flip || month_end[s]) c(QQQ = if (up) L else 0) else NULL
  }
}
# B. 80% QQQ + 20% top-5 relative strength (126d return minus QQQ 126d return)
strat_B <- function(s) {
  if (!month_end[s] && s != ev[1] - 1) return(NULL)
  ok <- which(eligible(s, 252))
  rs <- P[s, ok] / P[s - 126, ok] - 1 - (q[s] / q[s - 126] - 1)
  top <- names(sort(rs, decreasing = TRUE))[1:min(5, length(rs))]
  c(QQQ = 0.80, setNames(rep(0.20 / length(top), length(top)), top))
}
# C. Top-10 12-1 momentum, equal weight; optional QQQ trend filter
strat_C <- function(filter = FALSE) {
  last <- NA
  function(s) {
    up <- if (filter) trend_up[s] else TRUE
    flip <- filter && (is.na(last) || up != last); last <<- up
    if (!(month_end[s] || flip || s == ev[1] - 1)) return(NULL)
    if (!up) return(c(QQQ = 0))
    ok <- which(eligible(s, 252))
    mom <- P[s - 21, ok] / P[s - 252, ok] - 1
    top <- names(sort(mom, decreasing = TRUE))[1:min(10, length(mom))]
    setNames(rep(1 / length(top), length(top)), top)
  }
}
# Survivorship gauge: equal weight of every eligible universe stock, monthly
strat_EW <- function(s) {
  if (!month_end[s] && s != ev[1] - 1) return(NULL)
  ok <- colnames(P)[eligible(s, 252)]; setNames(rep(1 / length(ok), length(ok)), ok)
}

cat("Simulating...\n")
runs <- list(
  "QQQ buy & hold (total return)"     = simulate(function(s) if (s == ev[1] - 1) c(QQQ = 1) else NULL, "QQQ"),
  "A1. QQQ + 200d filter, 1.0x"        = simulate(strat_A(1.0), "A1"),
  "A2. QQQ + 200d filter, 1.25x"       = simulate(strat_A(1.25), "A2"),
  "A3. QQQ + 200d filter, 1.5x"        = simulate(strat_A(1.5), "A3"),
  "A0. QQQ 1.25x, no filter"           = simulate(strat_A(1.25, filter = FALSE), "A0"),
  "B.  80% QQQ + 20% top-5 RS"         = simulate(strat_B, "B"),
  "C1. Top-10 12-1 momentum"           = simulate(strat_C(FALSE), "C1"),
  "C2. Top-10 momentum + 200d filter"  = simulate(strat_C(TRUE), "C2"),
  "EW universe (survivorship gauge)"   = simulate(strat_EW, "EW")
)

dates <- cal[ev]; rf_e <- rf[ev]; rq_e <- runs[[1]]
stats_fn <- function(r, mask = rep(TRUE, length(r))) {
  r <- r[mask]; c_r <- cumprod(1 + r); n <- length(r); dd <- c_r / cummax(c_r) - 1
  ex <- r - rq_e[mask]
  list(term = 10000 * tail(c_r, 1), cagr = (tail(c_r, 1)^(252 / n) - 1) * 100, mdd = -min(dd) * 100,
       vol = sd(r) * sqrt(252) * 100, sharpe = mean(r - rf_e[mask]) / sd(r) * sqrt(252),
       ex_ann = mean(ex) * 252 * 100, ex_t = if (sd(ex) > 0) mean(ex) / sd(ex) * sqrt(n) else NA,
       equity = 10000 * c_r, dd = dd * 100)
}
# After-tax (simplified): strategies taxed yearly on net gains at 40.8% (37% + 3.8% NIIT, all short-term,
# loss carryforward) -- conservative for A, whose trades are infrequent.
# QQQ buy & hold taxed once at the end at 23.8% LTCG.
after_tax <- function(r, buy_hold = FALSE) {
  yrs <- format(dates, "%Y"); w <- 10000; cf <- 0
  if (buy_hold) { g <- 10000 * prod(1 + r) - 10000; w <- 10000 + g * (1 - 0.238) }
  else for (y in unique(yrs)) { g <- w * (prod(1 + r[yrs == y]) - 1); tx <- g - cf
    if (tx > 0) { w <- w + g - tx * 0.408; cf <- 0 } else { w <- w + g; cf <- -tx } }
  ((w / 10000)^(252 / length(r)) - 1) * 100
}

full <- lapply(runs, stats_fn)
pre  <- lapply(runs, stats_fn, mask = dates <  HOLDOUT)
hold <- lapply(runs, stats_fn, mask = dates >= HOLDOUT)
score <- data.frame(
  Strategy = names(runs),
  Terminal = sapply(full, function(s) sprintf("$%s", format(round(s$term), big.mark = ","))),
  CAGR = sprintf("%+.2f%%", sapply(full, `[[`, "cagr")),
  AfterTax_CAGR = sprintf("%+.2f%%", mapply(after_tax, runs, names(runs) == names(runs)[1])),
  MaxDD = sprintf("-%.1f%%", sapply(full, `[[`, "mdd")),
  Vol = sprintf("%.1f%%", sapply(full, `[[`, "vol")),
  Sharpe = sprintf("%.2f", sapply(full, `[[`, "sharpe")),
  Excess_vs_QQQ = sprintf("%+.2f%%", sapply(full, `[[`, "ex_ann")),
  t_stat = sprintf("%.2f", sapply(full, `[[`, "ex_t")),
  Turnover_yr = sprintf("%.1fx", sapply(runs, attr, "turnover_yr")),
  CAGR_2006_2023 = sprintf("%+.2f%%", sapply(pre, `[[`, "cagr")),
  CAGR_Holdout_2023_26 = sprintf("%+.2f%%", sapply(hold, `[[`, "cagr")),
  row.names = NULL)
score$Excess_vs_QQQ[1] <- "-"; score$t_stat[1] <- "-"
options(width = 250)
cat("\n=== THREE CANDIDATES vs QQQ (Oct 2006 - Oct 2026, total return, $10,000) ===\n"); print(score, row.names = FALSE)
write.csv(score, file.path(OUT, "three_candidates_scorecard.csv"), row.names = FALSE)

yrs <- unique(format(dates, "%Y"))
yr <- function(r) sapply(yrs, function(y) (prod(1 + r[format(dates, "%Y") == y]) - 1) * 100)
yr_df <- data.frame(Year = yrs, sapply(runs, function(r) sprintf("%+.1f", yr(r))), check.names = FALSE)
colnames(yr_df) <- c("Year", "QQQ", "A1", "A2", "A3", "A0", "B", "C1", "C2", "EW")
cat("\n=== CALENDAR RETURNS (%) ===\n"); print(yr_df, row.names = FALSE)
write.csv(yr_df, file.path(OUT, "three_candidates_calendar.csv"), row.names = FALSE)

# Charts
png(file.path(OUT, "three_candidates_chart.png"), width = 1600, height = 1350, res = 140)
layout(matrix(c(1, 1, 2, 3), ncol = 2, byrow = TRUE), heights = c(1.35, 1))
par(mar = c(3, 5, 3, 1.5))
cols <- c("#c62828", "#90caf9", "#1e88e5", "#0d47a1", "#9e9e9e", "#2e7d32", "#ef6c00", "#8e24aa", "#000000")
ltys <- c(1, 1, 1, 1, 3, 1, 1, 1, 2); lwds <- c(2.8, 1.6, 1.8, 2.2, 1.5, 2.0, 2.0, 2.0, 1.4)
yl <- range(unlist(lapply(full, `[[`, "equity")))
plot(dates, full[[1]]$equity, type = "n", log = "y", ylim = c(yl[1] * 0.9, yl[2] * 3),
     main = "Three Candidates vs QQQ: Growth of $10,000 (total return, log scale, Oct 2006 - Oct 2026)", ylab = "Equity ($, log)", xlab = "")
grid(col = "gray88", lty = 1); abline(v = HOLDOUT, col = "gray40", lty = 2)
text(HOLDOUT, yl[1] * 1.1, " holdout ->", adj = 0, cex = 0.75, col = "gray30")
for (i in seq_along(full)) lines(dates, full[[i]]$equity, col = cols[i], lty = ltys[i], lwd = lwds[i])
legend("topleft", bg = "white", cex = 0.72, col = cols, lty = ltys, lwd = lwds,
       legend = sprintf("%s: $%s | %+.1f%% CAGR | MaxDD -%.0f%% | Sharpe %.2f", names(full),
                        sapply(full, function(s) format(round(s$term), big.mark = ",")),
                        sapply(full, `[[`, "cagr"), sapply(full, `[[`, "mdd"), sapply(full, `[[`, "sharpe")))
plot(dates, full[[1]]$dd, type = "l", col = cols[1], lwd = 1.8, ylim = c(-75, 0),
     main = "Drawdown (%)", ylab = "%", xlab = "")
grid(col = "gray88", lty = 1)
for (i in c(3, 4, 6, 8)) lines(dates, full[[i]]$dd, col = cols[i], lwd = 1.4)
legend("bottomleft", bg = "white", cex = 0.7, col = cols[c(1, 3, 4, 6, 8)], lwd = 1.6,
       legend = c("QQQ", "A2 1.25x filter", "A3 1.5x filter", "B core-satellite", "C2 momentum+filter"))
ex_mat <- sapply(runs[-1], function(r) yr(r) - yr(rq_e))
ex_cagr <- sapply(full[-1], `[[`, "ex_ann")
barplot(ex_cagr, names.arg = c("A1", "A2", "A3", "A0", "B", "C1", "C2", "EW"), col = cols[-1], border = NA,
        main = "Average Annual Excess Return vs QQQ (%)", ylab = "% per year")
abline(h = 0)
dev.off()
file.copy(file.path(OUT, "three_candidates_chart.png"), file.path(ART, "three_candidates_chart.png"), overwrite = TRUE)
saveRDS(list(runs = runs, dates = dates, score = score, yr = yr_df), file.path(OUT, "three_candidates_results.rds"))
cat("\nChart saved: output/three_candidates_chart.png\n")
