#!/usr/bin/env Rscript
# ==============================================================================
# scratch/compare_20_vs_22_vol_target_gld.R
# Precise Comparison: 20% Vol Target into GLD vs 22% Vol Target into GLD
# ==============================================================================

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
})

source("scratch/test_defensive_sleeve_alternatives.R")

cat("\n================ SIMULATING 20% VS 22% VOL TARGET INTO GLD ================\n")

r_gld_20 <- sim_defensive_alternative(core_pct = 0.30, vol_target = 0.20, defensive_asset = "gld", label = "30/70 + 20% VT -> GLD")
r_gld_22 <- sim_defensive_alternative(core_pct = 0.30, vol_target = 0.22, defensive_asset = "gld", label = "30/70 + 22% VT -> GLD")
r_gld_16 <- sim_defensive_alternative(core_pct = 0.30, vol_target = 0.16, defensive_asset = "gld", label = "30/70 + 16% VT -> GLD")

comp_runs <- list(
  "QQQ Buy & Hold (Benchmark)" = rq[ev],
  "1. 30/70 + 16% VT -> GLD"   = r_gld_16,
  "2. 30/70 + 20% VT -> GLD"   = r_gld_20,
  "3. 30/70 + 22% VT -> GLD"   = r_gld_22
)

tab <- do.call(rbind, lapply(comp_runs, calc_metrics))
tab$Strategy <- names(comp_runs)
tab <- tab[, c("Strategy", "Final_Value", "CAGR", "MaxDD", "Vol", "Sharpe", "Sortino", "Calmar", "Ret_2008", "Ret_2020", "Ret_2022", "Turnover_yr")]

print(tab, row.names = FALSE)

# Compute Current Asset Allocation at 22.9% Realized Volatility
cur_vol <- 0.229
scale_20 <- min(1.0, 0.20 / cur_vol)
scale_22 <- min(1.0, 0.22 / cur_vol)

cat("\n================ CURRENT ALLOCATION COMPARISON (Realized Vol = 22.9%) ================\n")
cat(sprintf(" 20%% Vol Target:\n"))
cat(sprintf("   - Active Equities Exposure: %.1f%% ($%.2f on $8,000)\n", scale_20 * 100, 8000 * scale_20))
cat(sprintf("     * Core QQQ (30%%):        %.1f%% ($%.2f)\n", 30 * scale_20, 8000 * 0.30 * scale_20))
cat(sprintf("     * Satellite Top 5 (70%%):  %.1f%% ($%.2f - $%.2f/slot)\n", 70 * scale_20, 8000 * 0.70 * scale_20, (8000 * 0.70 * scale_20)/5))
cat(sprintf("   - Defensive GLD Sleeve:     %.1f%% ($%.2f)\n\n", (1 - scale_20) * 100, 8000 * (1 - scale_20)))

cat(sprintf(" 22%% Vol Target:\n"))
cat(sprintf("   - Active Equities Exposure: %.1f%% ($%.2f on $8,000)\n", scale_22 * 100, 8000 * scale_22))
cat(sprintf("     * Core QQQ (30%%):        %.1f%% ($%.2f)\n", 30 * scale_22, 8000 * 0.30 * scale_22))
cat(sprintf("     * Satellite Top 5 (70%%):  %.1f%% ($%.2f - $%.2f/slot)\n", 70 * scale_22, 8000 * 0.70 * scale_22, (8000 * 0.70 * scale_22)/5))
cat(sprintf("   - Defensive GLD Sleeve:     %.1f%% ($%.2f)\n", (1 - scale_22) * 100, 8000 * (1 - scale_22)))
cat("======================================================================================\n")
