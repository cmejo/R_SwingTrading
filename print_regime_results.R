suppressPackageStartupMessages({
  library(xts)
})
source("run_regime_breakdown.R")

cat("\n*** DETAILED NUMERICAL RESULTS ***\n")
res_list <- list(
  "Static Pos 2 / Cap 2 (Vince)" = p2_vince,
  "Static Pos 2 / Cap 2 (Equal)" = p2_equal,
  "Static Pos 5 / Cap 5 (Vince)" = p5_vince,
  "Static Pos 5 / Cap 5 (Equal)" = p5_equal,
  "Switch 5->2 (Vince)"          = sw_fwd_vince,
  "Switch 5->2 (Equal)"          = sw_fwd_equal,
  "Switch 2->5 (Vince)"          = sw_rec_vince,
  "Switch 2->5 (Equal)"          = sw_rec_equal
)

for (nm in names(res_list)) {
  r <- res_list[[nm]]
  cat(sprintf("%-30s | Wealth: $%12.2f | Ret: %+9.1f%% | Sharpe: %5.2f | MaxDD: %5.1f%%\n",
              nm, r$terminal_wealth, r$cum_ret * 100, r$sharpe, r$max_dd * 100))
}

cat("\n*** PERIOD BREAKDOWN ***\n")
cat(sprintf("Year 1 Bear (2021-2022):\n"))
cat(sprintf("  Pos 2: Ret=%+.2f%%, DD=%.2f%%\n", p2_vince$ret_y1 * 100, p2_vince$dd_y1 * 100))
cat(sprintf("  Pos 5: Ret=%+.2f%%, DD=%.2f%%\n", p5_vince$ret_y1 * 100, p5_vince$dd_y1 * 100))

cat(sprintf("Years 2-3 (2022-2024):\n"))
cat(sprintf("  Pos 2: Ret=%+.2f%%\n", p2_vince$ret_y23 * 100))
cat(sprintf("  Pos 5: Ret=%+.2f%%\n", p5_vince$ret_y23 * 100))

cat(sprintf("Recent 24M (2024-2026):\n"))
cat(sprintf("  Pos 2: Ret=%+.2f%%, Sharpe=%.2f, DD=%.2f%%\n", p2_vince$ret_rec24 * 100, p2_vince$sharpe_rec24, p2_vince$dd_rec24 * 100))
cat(sprintf("  Pos 5: Ret=%+.2f%%, Sharpe=%.2f, DD=%.2f%%\n", p5_vince$ret_rec24 * 100, p5_vince$sharpe_rec24, p5_vince$dd_rec24 * 100))
