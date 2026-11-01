#!/usr/bin/env Rscript
# ==============================================================================
# scratch/backtest_n100_point_in_time.R
# Survivorship-bias-free rebuild: the satellite sleeve may ONLY pick stocks that
# were actual Nasdaq-100 members at each rebalance date (jmccarrell/n100tickers).
# Window: 2007-01-02 .. 2026-10-06 (membership history starts 2007).
# ==============================================================================
suppressPackageStartupMessages({ library(xts); library(zoo) })

HIST <- "data/data_historical"; PIT <- "data/n100_pit"
ART  <- "/Users/cmejo/.gemini/antigravity/brain/64019830-0cac-4bb1-904e-d4560e71468c"
EVAL_START <- as.Date("2007-01-02"); EVAL_END <- as.Date("2026-10-06")
COST <- 0.0010; MARGIN_SPREAD <- 0.0175

adj <- function(sym) {
  p <- file.path(HIST, sprintf("data_%s.rds", tolower(sym)))
  if (!file.exists(p)) return(NULL)
  d <- readRDS(p); a <- d[, grep("Adjusted$", colnames(d))]; a[a <= 0] <- NA; a
}
qqq_p <- adj("QQQ"); qld_p <- adj("QLD"); gld_p <- adj("GLD_FULL")
cal <- as.Date(index(qqq_p)[index(qqq_p) >= as.Date("2005-01-01") & index(qqq_p) <= EVAL_END])
q  <- as.numeric(qqq_p[cal]); rq <- c(0, q[-1] / q[-length(q)] - 1)
trend_up <- q >= as.numeric(stats::filter(q, rep(1/200, 200), sides = 1)); trend_up[is.na(trend_up)] <- TRUE
qld_px <- zoo::na.locf(as.numeric(qld_p)[match(cal, as.Date(index(qld_p)))], na.rm = FALSE)
r_qld <- c(0, qld_px[-1] / qld_px[-length(qld_px)] - 1); r_qld[is.na(r_qld)] <- 2 * rq[is.na(r_qld)]
gld_px <- zoo::na.locf(as.numeric(gld_p)[match(cal, as.Date(index(gld_p)))], na.rm = FALSE)
r_gld <- c(0, gld_px[-1] / gld_px[-length(gld_px)] - 1); r_gld[is.na(r_gld)] <- 0
irx <- readRDS(file.path(HIST, "data_^irx.rds"))
rf <- zoo::na.locf(as.numeric(zoo::na.locf(irx[, 4], na.rm = FALSE)[cal]), na.rm = FALSE); rf[is.na(rf)] <- 2
rf_daily <- rf / 100 / 252; borrow_daily <- (rf / 100 + MARGIN_SPREAD) / 252
vol20 <- rollapply(rq, 20, function(x) sd(x) * sqrt(252), align = "right", fill = 0.20)
month_end <- c(format(cal[-1], "%m") != format(cal[-length(cal)], "%m"), TRUE)

# ---- Build price matrices ----------------------------------------------------
build_matrix <- function(price_list) {
  syms <- names(price_list)
  P <- matrix(NA_real_, length(cal), length(syms), dimnames = list(NULL, syms))
  for (s in syms) P[, s] <- price_list[[s]][match(cal, price_list[[s]]$d), "p"]
  for (j in seq_len(ncol(P))) { v <- P[, j]; f <- which(!is.na(v))[1]
    if (!is.na(f)) { l <- max(which(!is.na(v)))
      v[f:l] <- zoo::na.locf(v[f:l], na.rm = FALSE)   # fill gaps inside listing life
      if (l < length(v)) v[(l + 1):length(v)] <- v[l] # delisted: hold last price (0% return)
    }; P[, j] <- v }
  P
}
# PIT Nasdaq-100 universe
px <- read.csv(file.path(PIT, "prices_adjclose.csv"), check.names = FALSE)
px$Date <- as.Date(substr(px$Date, 1, 10))
pit_list <- lapply(setdiff(names(px), "Date"), function(s) {
  ok <- !is.na(px[[s]]); data.frame(d = px$Date[ok], p = px[[s]][ok]) })
names(pit_list) <- setdiff(names(px), "Date")
P_pit <- build_matrix(pit_list)

mem <- read.csv(file.path(PIT, "membership_monthly.csv"), stringsAsFactors = FALSE)
mem$month_start <- as.Date(mem$month_start)
MEM <- matrix(FALSE, length(cal), ncol(P_pit), dimnames = list(NULL, colnames(P_pit)))
cal_month <- as.Date(format(cal, "%Y-%m-01"))
for (m in unique(mem$month_start)) {
  rows <- which(cal_month == m); tk <- intersect(mem$ticker[mem$month_start == m], colnames(P_pit))
  if (length(rows) && length(tk)) MEM[rows, tk] <- TRUE
}

# Hindsight universe (the original 2026-selected list)
EXCLUDE <- c("QQQ","QLD","TQQQ","SQQQ","SPY","VTI","VFINX","SMH","SOXL","XLK","GLD_FULL",
             "AMUU","NVDU","PLTU","SNDQ","SMHC","SKHY","^VIX","^IRX","^GSPC","^NDX")
hs <- setdiff(toupper(sub("^data_", "", sub("[.]rds$", "", list.files(HIST, "^data_.*[.]rds$")))), EXCLUDE)
hs_list <- list()
for (s in hs) { a <- tryCatch(adj(s), error = function(e) NULL)
  if (!is.null(a)) hs_list[[s]] <- data.frame(d = as.Date(index(a)), p = as.numeric(a)) }
P_hs <- build_matrix(hs_list); P_hs <- P_hs[, colSums(!is.na(P_hs)) > 300, drop = FALSE]
MEM_hs <- matrix(TRUE, length(cal), ncol(P_hs), dimnames = list(NULL, colnames(P_hs)))

# ---- Engine ------------------------------------------------------------------
sim <- function(P, MEMB, w_long = 0.5, margin = 1.5, sat_mode = "stocks",
                start = EVAL_START, end = EVAL_END, top_n = 5, core_pct = 0.30, vt = 0.22, stop_pct = 0.20) {
  R <- rbind(NA, P[-1, , drop = FALSE] / P[-nrow(P), , drop = FALSE] - 1); R[is.na(R)] <- 0
  SMA <- apply(P, 2, function(v) as.numeric(stats::filter(v, rep(1/200, 200), sides = 1)))
  U <- colnames(P); ev <- which(cal >= start & cal <= end); sat_pct <- 1 - core_pct
  cw <- 0; sw <- setNames(rep(0, length(U)), U); dw <- 0; qw <- 0
  ep <- setNames(rep(NA_real_, length(U)), U); eq <- 1; out <- numeric(length(ev)); picks <- list()
  for (k in seq_along(ev)) {
    t <- ev[k]; rs_t <- R[t, ]; prev <- eq; gross <- cw + sum(sw) + dw + qw
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
      if (sat_mode == "qqq") tq <- sat_pct * sc * am else {
        ok <- U[MEMB[s0, ] & !is.na(P[s0, ]) & !is.na(P[max(1, s0 - 252), ])]
        if (length(ok) >= top_n) {
          r126 <- P[s0, ok] / P[s0 - 126, ok] - 1 - (q[s0] / q[s0 - 126] - 1)
          r63  <- P[s0, ok] / P[s0 - 63, ok]  - 1 - (q[s0] / q[s0 - 63] - 1)
          rs <- w_long * r126 + (1 - w_long) * r63
          okf <- ok[!is.na(SMA[s0, ok]) & P[s0, ok] >= SMA[s0, ok]]
          if (length(okf) >= top_n) { ok <- okf; rs <- rs[ok] }
          top <- names(sort(rs, decreasing = TRUE))[1:top_n]
          ts[top] <- sat_pct * sc * am / top_n
          for (ne in setdiff(top, names(sw)[sw > 0])) ep[ne] <- P[t, ne]
          picks[[as.character(cal[t])]] <- top
        }
      }
      td <- max(0, 1 - ((if (bull) core_pct else 0) * sc + sat_pct * sc))
      to <- abs(tc - cw) + sum(abs(ts - sw)) + abs(td - dw) + abs(tq - qw)
      eq <- eq * (1 - COST * to); cw <- tc; sw <- ts; dw <- td; qw <- tq
    }
    out[k] <- eq / prev - 1
  }
  attr(out, "picks") <- picks; out
}
met <- function(r) { c_r <- cumprod(1 + r); n <- length(r)
  c(CAGR = (tail(c_r, 1)^(252 / n) - 1) * 100, MaxDD = -min(c_r / cummax(c_r) - 1) * 100,
    Sharpe = mean(r) / sd(r) * sqrt(252), End_8k = 8000 * tail(c_r, 1)) }
pr <- function(title, lst) { cat("\n=== ", title, " ===\n"); print(round(t(sapply(lst, met)), 2)) }

# ---- Coverage diagnostics ----------------------------------------------------
rep <- read.csv(file.path(PIT, "download_report.csv"), stringsAsFactors = FALSE)
cov <- sapply(sort(unique(mem$month_start)), function(m) {
  tk <- mem$ticker[mem$month_start == m]; mean(tk %in% rep$ticker[rep$status == "OK"]) })
names(cov) <- format(sort(unique(mem$month_start)), "%Y")
cat(sprintf("Ever-members: %d | price history found: %d | missing: %d\n",
            nrow(rep), sum(rep$status == "OK"), sum(rep$status != "OK")))
cat("Price coverage of index members (avg by year):\n"); print(round(tapply(cov, names(cov), mean) * 100, 1))

ev <- which(cal >= EVAL_START & cal <= EVAL_END)
# ---- Main results ------------------------------------------------------------
res <- list(
  "QQQ Buy&Hold"                         = rq[ev],
  "Opt6 engine, QQQ satellites (floor)"  = sim(P_pit, MEM, margin = 1.5, sat_mode = "qqq"),
  "Opt6 Dual  - HINDSIGHT list"          = sim(P_hs, MEM_hs, 0.5, 1.5),
  "Opt6 126d  - POINT-IN-TIME N100"      = sim(P_pit, MEM, 1.0, 1.5),
  "Opt6 Dual  - POINT-IN-TIME N100"      = sim(P_pit, MEM, 0.5, 1.5),
  "Opt3 engine, QQQ satellites (floor)"  = sim(P_pit, MEM, margin = 1.0, sat_mode = "qqq"),
  "Opt3 Dual  - HINDSIGHT list"          = sim(P_hs, MEM_hs, 0.5, 1.0),
  "Opt3 126d  - POINT-IN-TIME N100"      = sim(P_pit, MEM, 1.0, 1.0),
  "Opt3 Dual  - POINT-IN-TIME N100"      = sim(P_pit, MEM, 0.5, 1.0))
pr("2007-2026: HINDSIGHT vs POINT-IN-TIME NASDAQ-100", res)

pr("SUB-PERIODS (Opt6 Dual, PIT)", list(
  "2007-2016" = sim(P_pit, MEM, 0.5, 1.5, end = as.Date("2016-12-30")),
  "2017-2026" = sim(P_pit, MEM, 0.5, 1.5, start = as.Date("2017-01-03"))))

grid <- c(1.0, 0.8, 0.6, 0.5, 0.4, 0.2, 0.0)
pr("LOOKBACK WEIGHT GRID (Opt6, PIT)", setNames(lapply(grid, function(w) sim(P_pit, MEM, w, 1.5)), sprintf("w_long=%.1f", grid)))
pr("TOP-N SENSITIVITY (Opt6 Dual, PIT)", setNames(lapply(c(3, 5, 8, 10), function(n) sim(P_pit, MEM, 0.5, 1.5, top_n = n)),
                                                  paste0("top_", c(3, 5, 8, 10))))

# Most frequently held names (sanity check)
pk <- attr(res[["Opt6 Dual  - POINT-IN-TIME N100"]], "picks")
cat("\nMost-held PIT picks (months in top 5):\n"); print(head(sort(table(unlist(pk)), decreasing = TRUE), 20))

# Chart
png(file.path(ART, "n100_point_in_time_backtest.png"), width = 1600, height = 900, res = 120)
par(mfrow = c(2, 1), mar = c(3.5, 4.5, 2.5, 1.5))
sel <- res[c(1, 2, 3, 5, 7, 9)]; d <- cal[ev]
cm <- sapply(sel, function(r) cumprod(1 + r) * 8000); cols <- c("gray50", "#2980b9", "#e74c3c", "#27ae60", "#f39c12", "#8e44ad")
matplot(d, cm, type = "l", lty = c(2, 1, 3, 1, 3, 1), lwd = 2, col = cols, log = "y", yaxt = "n", xlab = "", ylab = "$ (log)",
        main = "Hindsight list vs Point-in-Time Nasdaq-100 universe (2007-2026, $8k start)")
axis(2, at = c(8e3, 1e5, 1e6, 1e7, 5e7), labels = c("$8k", "$100k", "$1M", "$10M", "$50M"), las = 2); grid()
legend("topleft", names(sel), col = cols, lty = c(2, 1, 3, 1, 3, 1), lwd = 2, bty = "n", cex = 0.8)
dd <- sapply(sel, function(r) { c_r <- cumprod(1 + r); (c_r / cummax(c_r) - 1) * 100 })
matplot(d, dd, type = "l", lty = c(2, 1, 3, 1, 3, 1), lwd = 1.5, col = cols, ylab = "Drawdown %", main = "Drawdowns"); grid()
invisible(dev.off())
