#!/usr/bin/env Rscript
# ==============================================================================
# scratch/verify_20yr_backtest.R
# Independent verification + robustness audit of the Option 6 / Option 3
# Dual-Lookback RS backtest (2006-2026).
#   A. Reproduce headline numbers
#   B. Sub-period stability (2006-2016 vs 2016-2026)
#   C. Lookback-weight sensitivity grid (is 50/50 a plateau or a spike?)
#   D. Survivorship / hindsight-universe stress tests
#   E. Attribution: replace stock picks with QQQ (no selection skill)
# ==============================================================================
suppressPackageStartupMessages({ library(xts); library(zoo) })

HIST <- "data/data_historical"
EVAL_START <- as.Date("2006-10-02"); EVAL_END <- as.Date("2026-10-06")
COST <- 0.0010; MARGIN_SPREAD <- 0.0175

adj <- function(sym) {
  p <- file.path(HIST, sprintf("data_%s.rds", tolower(sym)))
  if (!file.exists(p)) return(NULL)
  d <- readRDS(p); a <- d[, grep("Adjusted$", colnames(d))]; a[a <= 0] <- NA; a
}
qqq_p <- adj("QQQ"); qld_p <- adj("QLD"); gld_p <- adj("GLD_FULL")
cal <- index(qqq_p)[index(qqq_p) >= as.Date("2004-01-01") & index(qqq_p) <= EVAL_END]
q  <- as.numeric(qqq_p[cal]); rq <- c(0, q[-1] / q[-length(q)] - 1)
sma200_q <- as.numeric(stats::filter(q, rep(1/200, 200), sides = 1))
trend_up <- q >= sma200_q; trend_up[is.na(trend_up)] <- TRUE

qld_px <- zoo::na.locf(as.numeric(qld_p)[match(cal, as.Date(index(qld_p)))], na.rm = FALSE)
r_qld <- c(0, qld_px[-1] / qld_px[-length(qld_px)] - 1); r_qld[is.na(r_qld)] <- 2 * rq[is.na(r_qld)]
gld_px <- zoo::na.locf(as.numeric(gld_p)[match(cal, as.Date(index(gld_p)))], na.rm = FALSE)
r_gld <- c(0, gld_px[-1] / gld_px[-length(gld_px)] - 1); r_gld[is.na(r_gld)] <- 0

irx <- readRDS(file.path(HIST, "data_^irx.rds"))
rf <- zoo::na.locf(as.numeric(zoo::na.locf(irx[, 4], na.rm = FALSE)[cal]), na.rm = FALSE); rf[is.na(rf)] <- 2
rf_daily <- rf / 100 / 252; borrow_daily <- (rf / 100 + MARGIN_SPREAD) / 252

EXCLUDE <- c("QQQ","QLD","TQQQ","SQQQ","SPY","VTI","VFINX","SMH","SOXL","XLK","GLD_FULL",
             "AMUU","NVDU","PLTU","SNDQ","SMHC","SKHY","^VIX","^IRX","^GSPC","^NDX")
syms <- setdiff(toupper(sub("^data_", "", sub("[.]rds$", "", list.files(HIST, "^data_.*[.]rds$")))), EXCLUDE)
P <- matrix(NA_real_, length(cal), length(syms), dimnames = list(NULL, syms))
for (s in syms) { a <- tryCatch(adj(s), error = function(e) NULL); if (!is.null(a)) P[, s] <- as.numeric(a)[match(cal, as.Date(index(a)))] }
P <- P[, colSums(!is.na(P)) > 300, drop = FALSE]
for (j in seq_len(ncol(P))) { v <- P[, j]; f <- which(!is.na(v))[1]
  if (!is.na(f)) v[f:length(v)] <- zoo::na.locf(v[f:length(v)], na.rm = FALSE); P[, j] <- v }
R <- rbind(NA, P[-1, , drop = FALSE] / P[-nrow(P), , drop = FALSE] - 1); R[is.na(R)] <- 0
vol20 <- rollapply(rq, 20, function(x) sd(x) * sqrt(252), align = "right", fill = 0.20)
month_end <- c(format(cal[-1], "%m") != format(cal[-length(cal)], "%m"), TRUE)
SMA200 <- apply(P, 2, function(v) as.numeric(stats::filter(v, rep(1/200, 200), sides = 1)))

sim <- function(w_long = 0.5, margin = 1.5, universe = colnames(P), start = EVAL_START, end = EVAL_END,
                sat_mode = c("stocks", "qqq"), top_n = 5, core_pct = 0.30, vt = 0.22, stop_pct = 0.20) {
  sat_mode <- match.arg(sat_mode)
  ev <- which(cal >= start & cal <= end); U <- universe; sat_pct <- 1 - core_pct
  cw <- 0; sw <- setNames(rep(0, length(U)), U); dw <- 0; qw <- 0
  ep <- setNames(rep(NA_real_, length(U)), U); eq <- 1; out <- numeric(length(ev))
  for (k in seq_along(ev)) {
    t <- ev[k]; rs_t <- R[t, U]; prev <- eq
    gross <- cw + sum(sw) + dw + qw
    g <- cw * r_qld[t] + sum(sw * rs_t) + dw * r_gld[t] + qw * rq[t] -
         max(0, gross - 1) * borrow_daily[t] + max(0, 1 - gross) * rf_daily[t]
    eq <- eq * (1 + g)
    cw <- cw * (1 + r_qld[t]) / (1 + g); sw <- sw * (1 + rs_t) / (1 + g)
    dw <- dw * (1 + r_gld[t]) / (1 + g); qw <- qw * (1 + rq[t]) / (1 + g)
    for (s in names(sw)[sw > 0]) if (!is.na(ep[s]) && P[t, s] / ep[s] - 1 <= -stop_pct) {
      eq <- eq * (1 - COST * sw[s]); sw[s] <- 0 }
    s0 <- t - 1
    if (month_end[s0] || k == 1) {
      sc <- min(1, vt / max(0.05, vol20[s0])); bull <- isTRUE(trend_up[s0])
      am <- if (margin > 1 && !bull) 1 else margin
      tc <- if (bull) core_pct * sc * am else 0
      ts <- setNames(rep(0, length(U)), U); tq <- 0
      if (sat_mode == "qqq") { tq <- sat_pct * sc * am } else {
        ok <- U[!is.na(P[s0, U]) & !is.na(P[max(1, s0 - 252), U])]
        if (length(ok) > 0) {
          r126 <- P[s0, ok] / P[s0 - 126, ok] - 1 - (q[s0] / q[s0 - 126] - 1)
          r63  <- P[s0, ok] / P[s0 - 63, ok]  - 1 - (q[s0] / q[s0 - 63] - 1)
          rs <- w_long * r126 + (1 - w_long) * r63
          okf <- ok[!is.na(SMA200[s0, ok]) & P[s0, ok] >= SMA200[s0, ok]]
          if (length(okf) >= top_n) { ok <- okf; rs <- rs[ok] }
          top <- names(sort(rs, decreasing = TRUE))[1:min(top_n, length(rs))]
          ts[top] <- sat_pct * sc * am / length(top)
          for (ne in setdiff(top, names(sw)[sw > 0])) ep[ne] <- P[t, ne]
        }
      }
      td <- max(0, 1 - ((if (bull) core_pct else 0) * sc + sat_pct * sc))
      to <- abs(tc - cw) + sum(abs(ts - sw)) + abs(td - dw) + abs(tq - qw)
      eq <- eq * (1 - COST * to); cw <- tc; sw <- ts; dw <- td; qw <- tq
    }
    out[k] <- eq / prev - 1
  }
  out
}
met <- function(r) { c_r <- cumprod(1 + r); n <- length(r)
  c(CAGR = (tail(c_r, 1)^(252 / n) - 1) * 100, MaxDD = -min(c_r / cummax(c_r) - 1) * 100,
    Sharpe = mean(r) / sd(r) * sqrt(252), End_8k = 8000 * tail(c_r, 1)) }
pr <- function(title, lst) { cat("\n=== ", title, " ===\n"); print(round(t(sapply(lst, met)), 2)) }

ev_all <- which(cal >= EVAL_START)
# A. Reproduce
pr("A. REPRODUCTION", list(
  "QQQ Buy&Hold"              = rq[ev_all],
  "Opt6 126d only (old)"      = sim(w_long = 1.0, margin = 1.5),
  "Opt6 Dual 50/50 (new)"     = sim(w_long = 0.5, margin = 1.5),
  "Opt3 126d only (old)"      = sim(w_long = 1.0, margin = 1.0),
  "Opt3 Dual 50/50 (new)"     = sim(w_long = 0.5, margin = 1.0)))

# B. Sub-periods
pr("B. SUB-PERIOD STABILITY (Opt6)", list(
  "2006-2016 126d"  = sim(1.0, end = as.Date("2016-09-30")),
  "2006-2016 Dual"  = sim(0.5, end = as.Date("2016-09-30")),
  "2016-2026 126d"  = sim(1.0, start = as.Date("2016-10-03")),
  "2016-2026 Dual"  = sim(0.5, start = as.Date("2016-10-03"))))

# C. Weight grid
grid <- c(1.0, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.0)
pr("C. LONG-WEIGHT SENSITIVITY (Opt6; w=1 is pure 126d, w=0 is pure 63d)",
   setNames(lapply(grid, function(w) sim(w_long = w)), sprintf("w_long=%.1f", grid)))

# D. Survivorship stress
first_valid <- apply(P, 2, function(v) which(!is.na(v))[1])
old_names <- colnames(P)[first_valid <= which(cal >= as.Date("2006-01-01"))[1]]
cat(sprintf("\nUniverse size: %d | Names with data before 2006: %d\n", ncol(P), length(old_names)))
set.seed(42)
rand_runs <- sapply(1:20, function(i) met(sim(0.5, universe = sample(colnames(P), floor(ncol(P) / 2))))["CAGR"])
pr("D. SURVIVORSHIP STRESS (Opt6 Dual)", list(
  "Full hindsight universe"      = sim(0.5),
  "Only names listed pre-2006"   = sim(0.5, universe = old_names)))
cat(sprintf("Random 50%% sub-universes (20 draws): CAGR median %.1f%% | min %.1f%% | max %.1f%%\n",
            median(rand_runs), min(rand_runs), max(rand_runs)))

# E. Attribution
pr("E. ATTRIBUTION: same engine, satellites = QQQ (no stock picking)", list(
  "Opt6 engine, QQQ satellites" = sim(margin = 1.5, sat_mode = "qqq"),
  "Opt3 engine, QQQ satellites" = sim(margin = 1.0, sat_mode = "qqq")))
