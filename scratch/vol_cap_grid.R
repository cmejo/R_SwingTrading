src <- readLines("scratch/compare_symbol_lists.R"); idx <- grep("^lists <- c\\(", src)
eval(parse(text = src[1:(idx - 1)]))
u <- commandArgs(trailingOnly = TRUE)[1]
if (u == "pit_n100") { for (nm in names(PIT_G)) assign(nm, PIT_G[[nm]]) } else
  invisible(set_universe(switch(u, broad = read_list("symbols_broad.txt"), all2 = read_list("symbols_all2.txt"), s300 = read_list("symbols_300.txt"))))
caps <- c(NA, 0.9, 1.0, 1.1, 1.25, 1.5)
res <- t(sapply(caps, function(v) eval3(margin = 1.5, top_n = 5, stop_to = "core", vol_cap = v)))
rownames(res) <- ifelse(is.na(caps), "no cap", sprintf("vol cap %d%%", round(caps * 100)))
o3 <- t(sapply(c(NA, 1.0), function(v) met(sim(margin = 1.0, top_n = 5, stop_to = "core", vol_cap = v))))
rownames(o3) <- c("Opt3 no cap", "Opt3 vol cap 100%")
cat("\n=== ", u, " (Option 6, top 5) ===\n"); print(round(res[, c(1, 3, 5, 6, 7)], 2)); print(round(o3, 2))
