#!/usr/bin/env Rscript
# ==============================================================================
# scratch/compare_top3_vs_top5.R
# 20-year (2007-2026) Top-3 vs Top-5 across every universe used so far,
# plus diagnostics explaining WHY the result differs:
#   (1) Top-N curve N = 1..8
#   (2) "Ranks 1-3 only" vs "Ranks 4-5 only" sleeves
#   (3) Forward 1-month excess return by RS rank (mean / median / stdev / hit rate)
# ==============================================================================
src <- readLines("scratch/compare_symbol_lists.R"); idx <- grep("^lists <- c\\(", src)
eval(parse(text = src[1:(idx - 1)]))

# Original hindsight universe (data/data_historical) - merge its prices into `up`
hs <- setdiff(toupper(sub("^data_", "", sub("[.]rds$", "", list.files(HIST, "^data_.*[.]rds$")))),
              c(EXCL, "VFINX", "GLD_FULL", "^GSPC", "^NDX", "SPY", "VTI"))
for (s in setdiff(hs, names(up))) { a <- tryCatch(adj(s), error = function(e) NULL)
  if (!is.null(a)) up[[s]] <- as.numeric(a)[match(up$Date, as.Date(index(a)))] }

universes <- list(
  "symbols.txt"           = read_list("symbols.txt"),
  "symbols_n100 (static)" = read_list("symbols_n100.txt"),
  "symbols_broad (new)"   = read_list("symbols_broad.txt"),
  "symbols_300"           = read_list("symbols_300.txt"),
  "symbols_all2"          = read_list("symbols_all2.txt"),
  "data_historical (old)" = hs,
  "PIT Nasdaq-100"        = NULL)

ev <- which(cal >= EVAL_START & cal <= EVAL_END)
me_idx <- ev[month_end[ev]]            # month-end decision dates in the window

rank_fwd <- function(max_rank = 10) {  # forward 1-month excess return vs QQQ, by rank
  out <- matrix(NA_real_, length(me_idx), max_rank)
  for (i in seq_along(me_idx)) {
    s0 <- me_idx[i]; nx <- if (i < length(me_idx)) me_idx[i + 1] else NA
    if (is.na(nx)) next
    ok <- syms[MEM[s0, ] & !is.na(P[s0, ]) & !is.na(P[max(1, s0 - 252), ])]
    if (length(ok) < max_rank) next
    rs <- 0.5 * (P[s0, ok] / P[s0 - 126, ok] - 1 - (q[s0] / q[s0 - 126] - 1)) +
          0.5 * (P[s0, ok] / P[s0 - 63, ok]  - 1 - (q[s0] / q[s0 - 63] - 1))
    okf <- ok[!is.na(SMA200[s0, ok]) & P[s0, ok] >= SMA200[s0, ok]]
    if (length(okf) >= max_rank) { ok <- okf; rs <- rs[ok] }
    top <- names(sort(rs, decreasing = TRUE))[1:max_rank]
    out[i, ] <- (P[nx, top] / P[s0, top] - 1) - (q[nx] / q[s0] - 1)
  }
  out
}

curve <- list(); t35 <- list(); slv <- list(); rk <- list()
for (u in names(universes)) {
  cat("Universe:", u, "\n")
  if (is.null(universes[[u]])) { for (nm in names(PIT_G)) assign(nm, PIT_G[[nm]]) } else invisible(set_universe(universes[[u]]))
  curve[[u]] <- sapply(1:8, function(n) met(sim(margin = 1.5, top_n = n, stop_to = "core",
                                                start = EVAL_START, end = EVAL_END))[["CAGR"]])
  for (n in c(3, 5)) for (M in c(1.5, 1.0))
    t35[[sprintf("%-22s Top%d %s", u, n, if (M > 1) "Opt6" else "Opt3")]] <- eval3(margin = M, top_n = n, stop_to = "core")
  slv[[u]] <- c(`Ranks 1-3` = met(sim(margin = 1.5, top_n = 3, stop_to = "core"))[["CAGR"]],
                `Ranks 4-5` = met(sim(margin = 1.5, top_n = 2, rank_skip = 3, stop_to = "core"))[["CAGR"]],
                `Ranks 1-3 vol` = sd(sim(margin = 1.5, top_n = 3, stop_to = "core")) * sqrt(252) * 100,
                `Ranks 1-5 vol` = sd(sim(margin = 1.5, top_n = 5, stop_to = "core")) * sqrt(252) * 100)
  rk[[u]] <- rank_fwd(10)
}

cat("\n=== (A) 20-YEAR TOP-3 vs TOP-5 (2007-2026; Train 2007-16 / Test 2017-26) ===\n")
print(round(do.call(rbind, t35), 2))

cat("\n=== (B) OPTION 6 FULL-PERIOD CAGR BY NUMBER OF STOCKS HELD (Top-N) ===\n")
cm <- do.call(rbind, curve); colnames(cm) <- paste0("N=", 1:8); print(round(cm, 1))

cat("\n=== (C) SLEEVE DECOMPOSITION (Option 6): ranks 1-3 only vs ranks 4-5 only ===\n")
print(round(do.call(rbind, slv), 1))

cat("\n=== (D) FORWARD 1-MONTH EXCESS RETURN vs QQQ BY RS RANK (%, pooled over all universes' months) ===\n")
for (u in names(rk)) {
  m <- rk[[u]]; m <- m[complete.cases(m), , drop = FALSE]
  tbl <- rbind(Mean = colMeans(m) * 100, Median = apply(m, 2, median) * 100, StDev = apply(m, 2, sd) * 100,
               `Worst %` = apply(m, 2, function(x) quantile(x, 0.05)) * 100)
  colnames(tbl) <- paste0("R", 1:10)
  cat("\n--", u, sprintf("(%d months)", nrow(m)), "--\n"); print(round(tbl, 2))
}
