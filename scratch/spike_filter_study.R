#!/usr/bin/env Rscript
# ==============================================================================
# scratch/spike_filter_study.R  <universe-name>
# Tests "speculative spike" filters on top-5 Option 6 (and Option 3 for the best),
# 2007-2026 with Train 2007-16 / Test 2017-26.
# ==============================================================================
src <- readLines("scratch/compare_symbol_lists.R"); idx <- grep("^lists <- c\\(", src)
eval(parse(text = src[1:(idx - 1)]))
u <- commandArgs(trailingOnly = TRUE)[1]
hs <- setdiff(toupper(sub("^data_", "", sub("[.]rds$", "", list.files(HIST, "^data_.*[.]rds$")))),
              c(EXCL, "VFINX", "GLD_FULL", "^GSPC", "^NDX", "SPY", "VTI"))
if (u == "data_historical") for (s in setdiff(hs, names(up))) { a <- tryCatch(adj(s), error = function(e) NULL)
  if (!is.null(a)) up[[s]] <- as.numeric(a)[match(up$Date, as.Date(index(a)))] }
if (u == "pit_n100") { for (nm in names(PIT_G)) assign(nm, PIT_G[[nm]]) } else
  invisible(set_universe(switch(u, broad = read_list("symbols_broad.txt"), s300 = read_list("symbols_300.txt"),
                                all2 = read_list("symbols_all2.txt"), data_historical = hs)))
cfg <- list(
  "Baseline (top 5)"            = list(),
  "Skip rank #1"                = list(rank_skip = 1),
  "Vol cap 60% (63d)"           = list(vol_cap = 0.60),
  "Vol cap 80% (63d)"           = list(vol_cap = 0.80),
  "Vol cap 100% (63d)"          = list(vol_cap = 1.00),
  "1-mo jump cap +30%"          = list(spike_cap = 0.30),
  "1-mo jump cap +50%"          = list(spike_cap = 0.50),
  "Extension cap 50% > 200SMA"  = list(ext_cap = 0.50),
  "Extension cap 100% > 200SMA" = list(ext_cap = 1.00),
  "Risk-adjusted RS (RS / vol)" = list(riskadj = TRUE))
res <- t(sapply(cfg, function(o) do.call(eval3, c(list(margin = 1.5, top_n = 5, stop_to = "core"), o))))
saveRDS(res, sprintf("scratch/spike_%s.rds", u))
cat("\n=== ", u, " ===\n"); print(round(res, 2))
