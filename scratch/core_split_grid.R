src <- readLines("scratch/compare_symbol_lists.R"); idx <- grep("^lists <- c\\(", src)
eval(parse(text = src[1:(idx - 1)]))
u <- commandArgs(trailingOnly = TRUE)[1]
if (u == "pit_n100") { for (nm in names(PIT_G)) assign(nm, PIT_G[[nm]]) } else
  invisible(set_universe(switch(u, broad = read_list("symbols_broad.txt"), all2 = read_list("symbols_all2.txt"), s300 = read_list("symbols_300.txt"))))
base <- list(margin = 1.5, top_n = 5, stop_to = "core", vol_cap = 1.0)
cfg <- list("Core 30/70 (live)" = list(), "Core 25/75" = list(core_pct = 0.25), "Core 20/80" = list(core_pct = 0.20),
            "Core 15/85" = list(core_pct = 0.15), "Core 10/90" = list(core_pct = 0.10),
            "Core 20/80 + stop -25%" = list(core_pct = 0.20, stop_pct = 0.25))
res <- t(sapply(cfg, function(o) do.call(eval3, modifyList(base, o))))
o3 <- t(sapply(list(list(), list(core_pct = 0.2)), function(o) met(do.call(sim, modifyList(modifyList(base, list(margin = 1.0)), o)))))
rownames(o3) <- c("Opt3 30/70", "Opt3 20/80")
cat("\n=== ", u, " ===\n"); print(round(res[, c(1, 3, 5, 6, 7)], 2)); print(round(o3, 2))
