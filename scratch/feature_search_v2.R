#!/usr/bin/env Rscript
# scratch/feature_search_v2.R <universe> - new candidate features vs current live config
src <- readLines("scratch/compare_symbol_lists.R"); idx <- grep("^lists <- c\\(", src)
eval(parse(text = src[1:(idx - 1)]))
u <- commandArgs(trailingOnly = TRUE)[1]
if (u == "pit_n100") { for (nm in names(PIT_G)) assign(nm, PIT_G[[nm]]) } else
  invisible(set_universe(switch(u, broad = read_list("symbols_broad.txt"), all2 = read_list("symbols_all2.txt"), s300 = read_list("symbols_300.txt"))))
base <- list(margin = 1.5, top_n = 5, stop_to = "core", vol_cap = 1.0)
cfg <- list(
  "LIVE BASELINE"                 = list(),
  "Smooth: top 50% pos-day frac"  = list(smooth = 0.50),
  "Smooth: top 67% pos-day frac"  = list(smooth = 0.33),
  "Near 52w high (within 15%)"    = list(hi52 = 0.15),
  "Near 52w high (within 25%)"    = list(hi52 = 0.25),
  "Corr cap 0.80"                 = list(corr_cap = 0.80),
  "Corr cap 0.70"                 = list(corr_cap = 0.70),
  "Stop -15%"                     = list(stop_pct = 0.15),
  "Stop -25%"                     = list(stop_pct = 0.25),
  "No stop"                       = list(stop_pct = 5),
  "Vol target 25%"                = list(vt = 0.25),
  "Vol target 30%"                = list(vt = 0.30),
  "Core 20 / Sat 80"              = list(core_pct = 0.20),
  "Core 40 / Sat 60"              = list(core_pct = 0.40))
res <- t(sapply(cfg, function(o) do.call(eval3, modifyList(base, o))))
saveRDS(res, sprintf("scratch/fs2_%s.rds", u))
cat("\n=== ", u, " ===\n"); print(round(res, 2))
