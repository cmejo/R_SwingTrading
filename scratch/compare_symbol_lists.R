#!/usr/bin/env Rscript
# ==============================================================================
# scratch/compare_symbol_lists.R
# 2007-2026 backtest of Option 6 / Option 3 (dual RS, stops->QLD) using each
# static symbol list as the universe, vs the point-in-time Nasdaq-100 reference.
# NOTE: every static list is a 2026 snapshot -> survivorship/hindsight biased.
# ==============================================================================
src <- readLines("scratch/pit_feature_search.R"); idx <- grep("^args <- commandArgs", src)
eval(parse(text = src[1:(idx - 1)]))   # loads data, PIT matrices, sim(), met(), eval3()

# Keep PIT globals for the reference run
PIT_G <- list(P = P, MEM = MEM, syms = syms, R = R, SMA200 = SMA200, SMA50 = SMA50, VOL63 = VOL63, BREADTH = BREADTH)

EXCL <- c("QQQ","QLD","TQQQ","SQQQ","SPY","VTI","SOXL","SOXS","FNGD","FNGU","SGOV","BIL","GLD","SMH","XLK",
          "AMUU","NVDU","PLTU","SNDQ","SMHC","SKHY","^VIX","^IRX")
up <- read.csv("data/universe_prices_adjclose.csv", check.names = FALSE); up$Date <- as.Date(substr(up$Date, 1, 10))
read_list <- function(fn) { l <- trimws(sub("#.*$", "", readLines(fn))); l <- l[nzchar(l)]
  setdiff(unique(toupper(unlist(strsplit(trimws(l), "[, \t]+")))), c("", EXCL)) }

set_universe <- function(tickers) {
  tk <- intersect(tickers, names(up))
  Pm <- matrix(NA_real_, length(cal), length(tk), dimnames = list(NULL, tk))
  for (s in tk) { v0 <- up[[s]]; v0[!is.na(v0) & v0 <= 0] <- NA; ok <- !is.na(v0); Pm[, s] <- v0[ok][match(cal, up$Date[ok])] }
  for (j in seq_len(ncol(Pm))) { v <- Pm[, j]; f <- which(!is.na(v))[1]
    if (!is.na(f)) { l <- max(which(!is.na(v))); v[f:l] <- zoo::na.locf(v[f:l], na.rm = FALSE)
      if (l < length(v)) v[(l + 1):length(v)] <- v[l] }; Pm[, j] <- v }
  Pm <- Pm[, colSums(!is.na(Pm)) > 300, drop = FALSE]
  P <<- Pm; syms <<- colnames(Pm); MEM <<- matrix(TRUE, length(cal), ncol(Pm), dimnames = list(NULL, syms))
  R <<- rbind(NA, Pm[-1, , drop = FALSE] / Pm[-nrow(Pm), , drop = FALSE] - 1); R[is.na(R)] <<- 0
  SMA200 <<- apply(Pm, 2, function(v) as.numeric(stats::filter(v, rep(1/200, 200), sides = 1)))
  SMA50 <<- SMA200; VOL63 <<- SMA200; BREADTH <<- rep(NA_real_, length(cal))
  c(listed = length(tickers), with_data = ncol(Pm),
    pre2007 = sum(apply(Pm, 2, function(v) { f <- which(!is.na(v))[1]; !is.na(f) && cal[f] <= as.Date("2006-01-31") })))
}

lists <- c("symbols.txt", "symbols_n100.txt", "symbols_broad.txt", "symbols_300.txt", "symbols_all2.txt")
out <- list(); info <- list()
for (fn in lists) {
  info[[fn]] <- set_universe(read_list(fn)); cat("Running", fn, "\n")
  for (M in c(1.5, 1.0)) out[[sprintf("%-18s %s", fn, if (M > 1) "Opt6" else "Opt3")]] <-
      eval3(margin = M, stop_to = "core")
}
for (nm in names(PIT_G)) assign(nm, PIT_G[[nm]])
for (M in c(1.5, 1.0)) out[[sprintf("%-18s %s", "PIT Nasdaq-100 ref", if (M > 1) "Opt6" else "Opt3")]] <-
    eval3(margin = M, stop_to = "core")

cat("\n=== UNIVERSE SIZE ===\n"); print(do.call(rbind, info))
res <- do.call(rbind, out)
cat("\n=== 2007-2026 | Option 6 (1.5x margin) ===\n"); print(round(res[grepl("Opt6", rownames(res)), ], 2))
cat("\n=== 2007-2026 | Option 3 (cash / IRA) ===\n"); print(round(res[grepl("Opt3", rownames(res)), ], 2))
