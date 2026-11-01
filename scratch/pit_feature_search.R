#!/usr/bin/env Rscript
# ==============================================================================
# scratch/pit_feature_search.R
# Feature search on the point-in-time Nasdaq-100 universe, with a
# train (2007-2016) / test (2017-2026) split to guard against overfitting.
# ==============================================================================
suppressPackageStartupMessages({ library(xts); library(zoo) })
HIST <- "data/data_historical"; PIT <- "data/n100_pit"
EVAL_START <- as.Date("2007-01-02"); EVAL_END <- as.Date("2026-10-06")
SPLIT <- as.Date("2017-01-01"); COST <- 0.0010; MARGIN_SPREAD <- 0.0175

adj <- function(sym) { d <- readRDS(file.path(HIST, sprintf("data_%s.rds", tolower(sym))))
  a <- d[, grep("Adjusted$", colnames(d))]; a[a <= 0] <- NA; a }
qqq_p <- adj("QQQ"); qld_p <- adj("QLD"); gld_p <- adj("GLD_FULL")
cal <- as.Date(index(qqq_p)[index(qqq_p) >= as.Date("2005-01-01") & index(qqq_p) <= EVAL_END])
q <- as.numeric(qqq_p[cal]); rq <- c(0, q[-1] / q[-length(q)] - 1)
smaq <- function(n) as.numeric(stats::filter(q, rep(1/n, n), sides = 1))
SMAQ <- list(`200` = smaq(200), `150` = smaq(150), `100` = smaq(100), `50` = smaq(50))
qld_px <- zoo::na.locf(as.numeric(qld_p)[match(cal, as.Date(index(qld_p)))], na.rm = FALSE)
r_qld <- c(0, qld_px[-1] / qld_px[-length(qld_px)] - 1); r_qld[is.na(r_qld)] <- 2 * rq[is.na(r_qld)]
gld_px <- zoo::na.locf(as.numeric(gld_p)[match(cal, as.Date(index(gld_p)))], na.rm = FALSE)
r_gld <- c(0, gld_px[-1] / gld_px[-length(gld_px)] - 1); r_gld[is.na(r_gld)] <- 0
irx <- readRDS(file.path(HIST, "data_^irx.rds"))
rf <- zoo::na.locf(as.numeric(zoo::na.locf(irx[, 4], na.rm = FALSE)[cal]), na.rm = FALSE); rf[is.na(rf)] <- 2
rf_daily <- rf / 100 / 252; borrow_daily <- (rf / 100 + MARGIN_SPREAD) / 252
vol20 <- rollapply(rq, 20, function(x) sd(x) * sqrt(252), align = "right", fill = 0.20)
month_end <- c(format(cal[-1], "%m") != format(cal[-length(cal)], "%m"), TRUE)
mid_month <- c(FALSE, as.integer(format(cal[-1], "%d")) >= 15 & as.integer(format(cal[-length(cal)], "%d")) < 15)

px <- read.csv(file.path(PIT, "prices_adjclose.csv"), check.names = FALSE); px$Date <- as.Date(substr(px$Date, 1, 10))
syms <- setdiff(names(px), "Date")
P <- matrix(NA_real_, length(cal), length(syms), dimnames = list(NULL, syms))
for (s in syms) { ok <- !is.na(px[[s]]); P[, s] <- px[[s]][ok][match(cal, px$Date[ok])] }
for (j in seq_len(ncol(P))) { v <- P[, j]; f <- which(!is.na(v))[1]
  if (!is.na(f)) { l <- max(which(!is.na(v))); v[f:l] <- zoo::na.locf(v[f:l], na.rm = FALSE)
    if (l < length(v)) v[(l + 1):length(v)] <- v[l] }; P[, j] <- v }
mem <- read.csv(file.path(PIT, "membership_monthly.csv"), stringsAsFactors = FALSE); mem$month_start <- as.Date(mem$month_start)
MEM <- matrix(FALSE, length(cal), ncol(P), dimnames = list(NULL, syms)); cal_month <- as.Date(format(cal, "%Y-%m-01"))
for (m in unique(mem$month_start)) { rows <- which(cal_month == m); tk <- intersect(mem$ticker[mem$month_start == m], syms)
  if (length(rows) && length(tk)) MEM[rows, tk] <- TRUE }
R <- rbind(NA, P[-1, ] / P[-nrow(P), ] - 1); R[is.na(R)] <- 0
SMA200 <- apply(P, 2, function(v) as.numeric(stats::filter(v, rep(1/200, 200), sides = 1)))
SMA50  <- apply(P, 2, function(v) as.numeric(stats::filter(v, rep(1/50, 50), sides = 1)))
VOL63  <- apply(R, 2, function(v) as.numeric(rollapply(v, 63, sd, align = "right", fill = NA)) * sqrt(252))
BREADTH <- sapply(seq_along(cal), function(i) { m <- MEM[i, ] & !is.na(SMA200[i, ]); if (sum(m) < 20) NA else mean(P[i, m] >= SMA200[i, m]) })

sim <- function(margin = 1.5, w_long = 0.5, top_n = 5, skip = 0, buffer = 0, weighting = "equal",
                gate_sma = "200", breadth_gate = NA, stop_to = "cash", exit_sma50 = FALSE,
                semi_monthly = FALSE, dd_brake = NA, rank_skip = 0, core_pct = 0.30, vt = 0.22, stop_pct = 0.20,
                start = EVAL_START, end = EVAL_END) {
  U <- syms; ev <- which(cal >= start & cal <= end); sat_pct <- 1 - core_pct; sma_g <- SMAQ[[gate_sma]]
  cw <- 0; sw <- setNames(rep(0, length(U)), U); dw <- 0; ep <- setNames(rep(NA_real_, length(U)), U)
  eq <- 1; peak <- 1; braked <- FALSE; out <- numeric(length(ev)); bull_now <- TRUE
  for (k in seq_along(ev)) {
    t <- ev[k]; prev <- eq; gross <- cw + sum(sw) + dw
    g <- cw * r_qld[t] + sum(sw * R[t, ]) + dw * r_gld[t] - max(0, gross - 1) * borrow_daily[t] + max(0, 1 - gross) * rf_daily[t]
    eq <- eq * (1 + g); peak <- max(peak, eq)
    cw <- cw * (1 + r_qld[t]) / (1 + g); sw <- sw * (1 + R[t, ]) / (1 + g); dw <- dw * (1 + r_gld[t]) / (1 + g)
    for (s in names(sw)[sw > 0]) {
      hit <- (!is.na(ep[s]) && P[t, s] / ep[s] - 1 <= -stop_pct) || (exit_sma50 && !is.na(SMA50[t, s]) && P[t, s] < SMA50[t, s])
      if (isTRUE(hit)) { w <- sw[s]; eq <- eq * (1 - COST * w); sw[s] <- 0
        if (stop_to == "core" && bull_now) { cw <- cw + w; eq <- eq * (1 - COST * w) }
        if (stop_to == "gld") { dw <- dw + w; eq <- eq * (1 - COST * w) } }
    }
    if (!is.na(dd_brake)) { dd <- eq / peak - 1; if (dd <= -dd_brake) braked <- TRUE; if (dd >= -dd_brake / 2) braked <- FALSE }
    s0 <- t - 1
    if (month_end[s0] || k == 1 || (semi_monthly && mid_month[s0])) {
      sc <- min(1, vt / max(0.05, vol20[s0])); bull <- isTRUE(q[s0] >= sma_g[s0]) || is.na(sma_g[s0]); bull_now <- bull
      am <- if (margin > 1 && !bull) 1 else margin
      if (!is.na(breadth_gate) && !is.na(BREADTH[s0]) && BREADTH[s0] < breadth_gate) am <- min(am, 1)
      if (braked) am <- min(am, 1)
      tc <- if (bull) core_pct * sc * am else 0
      ts <- setNames(rep(0, length(U)), U)
      ok <- U[MEM[s0, ] & !is.na(P[s0, ]) & !is.na(P[max(1, s0 - 252), ])]
      if (length(ok) >= top_n) {
        e <- s0 - skip
        r126 <- P[e, ok] / P[s0 - 126, ok] - 1 - (q[e] / q[s0 - 126] - 1)
        r63  <- P[e, ok] / P[s0 - 63, ok]  - 1 - (q[e] / q[s0 - 63] - 1)
        rs <- w_long * r126 + (1 - w_long) * r63
        okf <- ok[!is.na(SMA200[s0, ok]) & P[s0, ok] >= SMA200[s0, ok]]
        if (length(okf) >= top_n) { ok <- okf; rs <- rs[ok] }
        rk <- names(sort(rs, decreasing = TRUE)); if (rank_skip > 0) rk <- rk[-seq_len(rank_skip)]
        held <- intersect(names(sw)[sw > 0], rk[seq_len(min(length(rk), top_n + buffer))])
        top <- unique(c(held, rk))[1:top_n]; top <- top[!is.na(top)]
        wts <- if (weighting == "invvol") { iv <- 1 / pmax(VOL63[s0, top], 0.15); iv[is.na(iv)] <- mean(iv, na.rm = TRUE); iv / sum(iv) } else rep(1 / top_n, length(top))
        ts[top] <- sat_pct * sc * am * wts
        for (ne in setdiff(top, names(sw)[sw > 0])) ep[ne] <- P[t, ne]
      }
      td <- max(0, 1 - ((if (bull) core_pct else 0) * sc + sat_pct * sc))
      to <- abs(tc - cw) + sum(abs(ts - sw)) + abs(td - dw); eq <- eq * (1 - COST * to); cw <- tc; sw <- ts; dw <- td
    }
    out[k] <- eq / prev - 1
  }
  out
}
met <- function(r) { c_r <- cumprod(1 + r); n <- length(r)
  c(CAGR = (tail(c_r, 1)^(252 / n) - 1) * 100, MaxDD = -min(c_r / cummax(c_r) - 1) * 100, Sharpe = mean(r) / sd(r) * sqrt(252)) }
eval3 <- function(...) {
  a <- met(sim(..., end = SPLIT - 1)); b <- met(sim(..., start = SPLIT)); f <- met(sim(...))
  c(Train_CAGR = a[[1]], Train_DD = a[[2]], Test_CAGR = b[[1]], Test_DD = b[[2]], Full_CAGR = f[[1]], Full_DD = f[[2]], Full_Sharpe = f[[3]])
}
args <- commandArgs(trailingOnly = TRUE); MARGIN <- if (length(args)) as.numeric(args[1]) else 1.5
cfg <- list(
  "BASELINE (dual 50/50, top5)"     = list(),
  "Skip last month (12-1 style)"    = list(skip = 21),
  "Hold buffer: keep if rank<=8"    = list(buffer = 3),
  "Hold buffer: keep if rank<=10"   = list(buffer = 5),
  "Inverse-vol satellite weights"   = list(weighting = "invvol"),
  "Faster gate: QQQ 150 SMA"        = list(gate_sma = "150"),
  "Faster gate: QQQ 100 SMA"        = list(gate_sma = "100"),
  "Breadth gate: no margin if <50%" = list(breadth_gate = 0.5),
  "Stopped cash -> QLD core"        = list(stop_to = "core"),
  "Stopped cash -> GLD"             = list(stop_to = "gld"),
  "Exit stock below its 50 SMA"     = list(exit_sma50 = TRUE),
  "Semi-monthly rebalance"          = list(semi_monthly = TRUE),
  "DD brake: no margin if DD>25%"   = list(dd_brake = 0.25),
  "Top 7 stocks"                    = list(top_n = 7))
res <- t(sapply(cfg, function(o) do.call(eval3, c(list(margin = MARGIN), o))))
cat(sprintf("\n=== FEATURE SEARCH (margin %.1fx) | Train 2007-2016 | Test 2017-2026 ===\n", MARGIN))
print(round(res, 2))
saveRDS(res, sprintf("scratch/pit_feature_search_m%.1f.rds", MARGIN))
