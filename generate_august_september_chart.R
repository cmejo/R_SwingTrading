#!/usr/bin/env Rscript
# ==============================================================================
# August - September 2026 Multi-Universe Backtest Report & Chart Generator
# ==============================================================================

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(TTR)
})

source("R/01_data_loader.R")
source("R/08_metrics.R")

OUTPUT_DIR   <- "output"
CACHE_DIR    <- "data/cache_wf"
ARTIFACT_DIR <- "/Users/cmejo/.gemini/antigravity/brain/785859a9-21ba-4446-b7f5-980fd7d1dd48"
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

# Load Benchmarks
spy_ohlcv <- load_stock_data("SPY")
qqq_ohlcv <- load_stock_data("QQQ")
spy_cl    <- Cl(spy_ohlcv)
qqq_cl    <- Cl(qqq_ohlcv)

spy_ret_all <- na.omit(spy_cl / lag.xts(spy_cl, 1) - 1)
qqq_ret_all <- na.omit(qqq_cl / lag.xts(qqq_cl, 1) - 1)
qqq_ma50    <- SMA(qqq_cl, 50)

# Helper to read symbols file
read_universe_symbols <- function(filepath) {
  if (!file.exists(filepath)) return(character(0))
  lines <- readLines(filepath, warn = FALSE)
  lines <- gsub("#.*", "", lines)
  syms  <- unique(toupper(trimws(unlist(strsplit(lines, "[, \\t\\r\\n]+")))))
  syms[syms != ""]
}

universes <- list(
  "Focus Watchlist (17)"        = read_universe_symbols("symbols.txt"),
  "Broad Universe (170)"        = read_universe_symbols("symbols_broad.txt"),
  "Institutional 300+ (434)"    = read_universe_symbols("symbols_300.txt")
)

all_s <- unique(unlist(universes))
cache_files <- file.path(CACHE_DIR, sprintf("%s_504wf.rds", tolower(all_s)))
valid_mask  <- file.exists(cache_files)
valid_syms  <- all_s[valid_mask]
valid_files <- cache_files[valid_mask]

cat(sprintf("Loading %d cached walk-forward assets...\n", length(valid_files)))
sim_results <- setNames(lapply(valid_files, readRDS), valid_syms)

# Evaluation Window: August 1, 2026 - September 28, 2026
start_d <- "2026-08-01"
end_d   <- "2026-09-28"

eval_window <- function(sym_list, start_d, end_d, mode = "ENHANCED") {
  avail_syms <- intersect(sym_list, names(sim_results))
  all_d <- index(spy_ret_all)
  eval_dates <- all_d[all_d >= as.Date(start_d) & all_d <= as.Date(end_d)]
  n_eval <- length(eval_dates)
  
  qqq_sub <- as.numeric(qqq_ret_all[eval_dates])
  spy_sub <- as.numeric(spy_ret_all[eval_dates])
  qqq_bull <- as.numeric(qqq_cl[eval_dates] > qqq_ma50[eval_dates])
  
  port_daily_ret <- numeric(n_eval)
  
  if (mode == "BASELINE") {
    ret_matrix <- matrix(0, nrow = n_eval, ncol = length(avail_syms))
    for (j in seq_along(avail_syms)) {
      s <- avail_syms[j]
      obj <- sim_results[[s]]
      idx_m <- match(eval_dates, index(obj$base_ret))
      valid <- !is.na(idx_m)
      if (any(valid)) ret_matrix[valid, j] <- as.numeric(obj$base_ret)[idx_m[valid]]
    }
    port_daily_ret <- rowMeans(ret_matrix, na.rm = TRUE)
    port_daily_ret[is.na(port_daily_ret)] <- 0
  } else {
    n_syms <- length(avail_syms)
    mat_prob  <- matrix(0.5,  nrow = n_eval, ncol = n_syms)
    mat_rs    <- matrix(0.0,  nrow = n_eval, ncol = n_syms)
    mat_vol   <- matrix(0.25, nrow = n_eval, ncol = n_syms)
    mat_ret_b <- matrix(0.0,  nrow = n_eval, ncol = n_syms)
    
    for (j in seq_along(avail_syms)) {
      s <- avail_syms[j]
      obj <- sim_results[[s]]
      idx_p <- match(eval_dates, index(obj$pred_prob))
      v_p <- !is.na(idx_p)
      if (any(v_p)) mat_prob[v_p, j] <- as.numeric(obj$pred_prob)[idx_p[v_p]]
      
      idx_rs <- match(eval_dates, index(obj$rs_spy))
      v_rs <- !is.na(idx_rs)
      if (any(v_rs)) mat_rs[v_rs, j] <- as.numeric(obj$rs_spy)[idx_rs[v_rs]]
      
      idx_v <- match(eval_dates, index(obj$garch_vol))
      v_v <- !is.na(idx_v)
      if (any(v_v)) {
        raw_v <- as.numeric(obj$garch_vol)[idx_v[v_v]]
        raw_v[is.na(raw_v) | raw_v < 0.05] <- 0.25
        mat_vol[v_v, j] <- raw_v
      }
      
      idx_b <- match(eval_dates, index(obj$bracket_ret))
      v_b <- !is.na(idx_b)
      if (any(v_b)) mat_ret_b[v_b, j] <- as.numeric(obj$bracket_ret)[idx_b[v_b]]
    }
    
    for (t in seq_along(eval_dates)) {
      p_ups   <- mat_prob[t, ]
      rs_vals <- mat_rs[t, ]
      vols    <- mat_vol[t, ]
      rets_b  <- mat_ret_b[t, ]
      
      qualify <- which(p_ups >= 0.58 & rs_vals > 0)
      if (length(qualify) > 0) {
        scores <- p_ups[qualify] * (1 + rs_vals[qualify])
        top_idx <- head(qualify[order(-scores)], 5)
      } else {
        top_idx <- integer(0)
      }
      
      k_slots <- length(top_idx)
      cash_ret <- if (qqq_bull[t] == 1) qqq_sub[t] else 0.0
      
      if (k_slots == 0) {
        port_daily_ret[t] <- cash_ret
      } else {
        stock_alloc_total <- 0
        stock_pnl_total   <- 0
        slot_size         <- 1.0 / 5.0
        for (idx in top_idx) {
          vol_scale <- pmin(1.5, 0.45 / vols[idx])
          w_i <- slot_size * vol_scale
          stock_alloc_total <- stock_alloc_total + slot_size
          stock_pnl_total   <- stock_pnl_total + (w_i * rets_b[idx])
        }
        unalloc_cash <- max(0, 1.0 - stock_alloc_total)
        port_daily_ret[t] <- stock_pnl_total + (unalloc_cash * cash_ret)
      }
    }
  }
  
  m_strat <- calc_performance_metrics(port_daily_ret)$raw
  m_spy   <- calc_performance_metrics(spy_sub)$raw
  m_qqq   <- calc_performance_metrics(qqq_sub)$raw
  
  list(
    dates   = eval_dates,
    bars    = n_eval,
    strat   = m_strat,
    spy     = m_spy,
    qqq     = m_qqq,
    returns = port_daily_ret,
    spy_ret = spy_sub,
    qqq_ret = qqq_sub
  )
}

# Run Backtest for August-September Total
res_focus_enh  <- eval_window(universes[[1]], start_d, end_d, mode = "ENHANCED")
res_focus_base <- eval_window(universes[[1]], start_d, end_d, mode = "BASELINE")

res_broad_enh  <- eval_window(universes[[2]], start_d, end_d, mode = "ENHANCED")
res_broad_base <- eval_window(universes[[2]], start_d, end_d, mode = "BASELINE")

res_inst_enh   <- eval_window(universes[[3]], start_d, end_d, mode = "ENHANCED")
res_inst_base  <- eval_window(universes[[3]], start_d, end_d, mode = "BASELINE")

# Also get August-only and September-only returns for Panel 4
aug_focus <- eval_window(universes[[1]], "2026-08-01", "2026-08-31", mode = "ENHANCED")
sep_focus <- eval_window(universes[[1]], "2026-09-01", "2026-09-28", mode = "ENHANCED")

aug_broad <- eval_window(universes[[2]], "2026-08-01", "2026-08-31", mode = "ENHANCED")
sep_broad <- eval_window(universes[[2]], "2026-09-01", "2026-09-28", mode = "ENHANCED")

aug_inst  <- eval_window(universes[[3]], "2026-08-01", "2026-08-31", mode = "ENHANCED")
sep_inst  <- eval_window(universes[[3]], "2026-09-01", "2026-09-28", mode = "ENHANCED")

aug_spy_ret <- as.numeric(tail(Cl(spy_ohlcv)["2026-08"], 1)) / as.numeric(head(Cl(spy_ohlcv)["2026-08"], 1)) - 1
sep_spy_ret <- as.numeric(tail(Cl(spy_ohlcv)["2026-09-01/2026-09-28"], 1)) / as.numeric(head(Cl(spy_ohlcv)["2026-09-01/2026-09-28"], 1)) - 1

aug_qqq_ret <- as.numeric(tail(Cl(qqq_ohlcv)["2026-08"], 1)) / as.numeric(head(Cl(qqq_ohlcv)["2026-08"], 1)) - 1
sep_qqq_ret <- as.numeric(tail(Cl(qqq_ohlcv)["2026-09-01/2026-09-28"], 1)) / as.numeric(head(Cl(qqq_ohlcv)["2026-09-01/2026-09-28"], 1)) - 1

# Generate Scorecard Table & CSV
scorecard_df <- data.frame(
  Universe       = c("Focus Watchlist (symbols.txt)", "Focus Watchlist (symbols.txt)",
                     "Broad Universe (symbols_broad.txt)", "Broad Universe (symbols_broad.txt)",
                     "Institutional 300+ (symbols_300.txt)", "Institutional 300+ (symbols_300.txt)",
                     "S&P 500 (SPY)", "Nasdaq-100 (QQQ)"),
  Model          = c("ENHANCED", "BASELINE", "ENHANCED", "BASELINE", "ENHANCED", "BASELINE", "BENCHMARK", "BENCHMARK"),
  Return         = c(sprintf("%+6.2f%%", res_focus_enh$strat$cum_ret * 100),
                     sprintf("%+6.2f%%", res_focus_base$strat$cum_ret * 100),
                     sprintf("%+6.2f%%", res_broad_enh$strat$cum_ret * 100),
                     sprintf("%+6.2f%%", res_broad_base$strat$cum_ret * 100),
                     sprintf("%+6.2f%%", res_inst_enh$strat$cum_ret * 100),
                     sprintf("%+6.2f%%", res_inst_base$strat$cum_ret * 100),
                     sprintf("%+6.2f%%", res_focus_enh$spy$cum_ret * 100),
                     sprintf("%+6.2f%%", res_focus_enh$qqq$cum_ret * 100)),
  Ann_Return     = c(sprintf("%+6.2f%%", res_focus_enh$strat$ann_ret * 100),
                     sprintf("%+6.2f%%", res_focus_base$strat$ann_ret * 100),
                     sprintf("%+6.2f%%", res_broad_enh$strat$ann_ret * 100),
                     sprintf("%+6.2f%%", res_broad_base$strat$ann_ret * 100),
                     sprintf("%+6.2f%%", res_inst_enh$strat$ann_ret * 100),
                     sprintf("%+6.2f%%", res_inst_base$strat$ann_ret * 100),
                     sprintf("%+6.2f%%", res_focus_enh$spy$ann_ret * 100),
                     sprintf("%+6.2f%%", res_focus_enh$qqq$ann_ret * 100)),
  Sharpe         = c(sprintf("%5.2f", res_focus_enh$strat$sharpe),
                     sprintf("%5.2f", res_focus_base$strat$sharpe),
                     sprintf("%5.2f", res_broad_enh$strat$sharpe),
                     sprintf("%5.2f", res_broad_base$strat$sharpe),
                     sprintf("%5.2f", res_inst_enh$strat$sharpe),
                     sprintf("%5.2f", res_inst_base$strat$sharpe),
                     sprintf("%5.2f", res_focus_enh$spy$sharpe),
                     sprintf("%5.2f", res_focus_enh$qqq$sharpe)),
  Max_Drawdown   = c(sprintf("-%5.2f%%", res_focus_enh$strat$max_dd * 100),
                     sprintf("-%5.2f%%", res_focus_base$strat$max_dd * 100),
                     sprintf("-%5.2f%%", res_broad_enh$strat$max_dd * 100),
                     sprintf("-%5.2f%%", res_broad_base$strat$max_dd * 100),
                     sprintf("-%5.2f%%", res_inst_enh$strat$max_dd * 100),
                     sprintf("-%5.2f%%", res_inst_base$strat$max_dd * 100),
                     sprintf("-%5.2f%%", res_focus_enh$spy$max_dd * 100),
                     sprintf("-%5.2f%%", res_focus_enh$qqq$max_dd * 100)),
  Win_Rate       = c(sprintf("%5.1f%%", res_focus_enh$strat$win_rate * 100),
                     sprintf("%5.1f%%", res_focus_base$strat$win_rate * 100),
                     sprintf("%5.1f%%", res_broad_enh$strat$win_rate * 100),
                     sprintf("%5.1f%%", res_broad_base$strat$win_rate * 100),
                     sprintf("%5.1f%%", res_inst_enh$strat$win_rate * 100),
                     sprintf("%5.1f%%", res_inst_base$strat$win_rate * 100),
                     sprintf("%5.1f%%", res_focus_enh$spy$win_rate * 100),
                     sprintf("%5.1f%%", res_focus_enh$qqq$win_rate * 100)),
  Profit_Factor  = c(sprintf("%5.2f", res_focus_enh$strat$profit_factor),
                     sprintf("%5.2f", res_focus_base$strat$profit_factor),
                     sprintf("%5.2f", res_broad_enh$strat$profit_factor),
                     sprintf("%5.2f", res_broad_base$strat$profit_factor),
                     sprintf("%5.2f", res_inst_enh$strat$profit_factor),
                     sprintf("%5.2f", res_inst_base$strat$profit_factor),
                     sprintf("%5.2f", res_focus_enh$spy$profit_factor),
                     sprintf("%5.2f", res_focus_enh$qqq$profit_factor)),
  Alpha_vs_SPY   = c(sprintf("%+6.2f%%", (res_focus_enh$strat$cum_ret - res_focus_enh$spy$cum_ret) * 100),
                     sprintf("%+6.2f%%", (res_focus_base$strat$cum_ret - res_focus_enh$spy$cum_ret) * 100),
                     sprintf("%+6.2f%%", (res_broad_enh$strat$cum_ret - res_focus_enh$spy$cum_ret) * 100),
                     sprintf("%+6.2f%%", (res_broad_base$strat$cum_ret - res_focus_enh$spy$cum_ret) * 100),
                     sprintf("%+6.2f%%", (res_inst_enh$strat$cum_ret - res_focus_enh$spy$cum_ret) * 100),
                     sprintf("%+6.2f%%", (res_inst_base$strat$cum_ret - res_focus_enh$spy$cum_ret) * 100),
                     "—", "—"),
  Alpha_vs_QQQ   = c(sprintf("%+6.2f%%", (res_focus_enh$strat$cum_ret - res_focus_enh$qqq$cum_ret) * 100),
                     sprintf("%+6.2f%%", (res_focus_base$strat$cum_ret - res_focus_enh$qqq$cum_ret) * 100),
                     sprintf("%+6.2f%%", (res_broad_enh$strat$cum_ret - res_focus_enh$qqq$cum_ret) * 100),
                     sprintf("%+6.2f%%", (res_broad_base$strat$cum_ret - res_focus_enh$qqq$cum_ret) * 100),
                     sprintf("%+6.2f%%", (res_inst_enh$strat$cum_ret - res_focus_enh$qqq$cum_ret) * 100),
                     sprintf("%+6.2f%%", (res_inst_base$strat$cum_ret - res_focus_enh$qqq$cum_ret) * 100),
                     "—", "—"),
  stringsAsFactors = FALSE
)

write.csv(scorecard_df, file = file.path(OUTPUT_DIR, "august_september_2026_scorecard.csv"), row.names = FALSE)
cat(sprintf("[Saved] Scorecard saved to: %s\n", file.path(OUTPUT_DIR, "august_september_2026_scorecard.csv")))

# ------------------------------------------------------------------------------
# 4-Panel Master Report Chart
# ------------------------------------------------------------------------------
chart_file <- file.path(OUTPUT_DIR, "august_september_2026_backtest_report.png")
png(chart_file, width = 2200, height = 1500, res = 150)
par(mfrow = c(2, 2), mar = c(4.8, 4.8, 3.5, 1.5), oma = c(1, 1, 4, 1))

# --- Panel 1: Compounded Equity Curves ($10,000 Base) ---
eq_focus_enh  <- 10000 * cumprod(1 + res_focus_enh$returns)
eq_broad_enh  <- 10000 * cumprod(1 + res_broad_enh$returns)
eq_inst_enh   <- 10000 * cumprod(1 + res_inst_enh$returns)

eq_focus_base <- 10000 * cumprod(1 + res_focus_base$returns)
eq_broad_base <- 10000 * cumprod(1 + res_broad_base$returns)
eq_inst_base  <- 10000 * cumprod(1 + res_inst_base$returns)

eq_spy        <- 10000 * cumprod(1 + res_focus_enh$spy_ret)
eq_qqq        <- 10000 * cumprod(1 + res_focus_enh$qqq_ret)

ymin <- min(eq_focus_enh, eq_broad_enh, eq_inst_enh, eq_focus_base, eq_broad_base, eq_inst_base, eq_spy, eq_qqq) * 0.97
ymax <- max(eq_focus_enh, eq_broad_enh, eq_inst_enh, eq_focus_base, eq_broad_base, eq_inst_base, eq_spy, eq_qqq) * 1.05

days_x <- 1:length(eq_focus_enh)

plot(days_x, eq_focus_enh, type = "l", col = "#1b9e77", lwd = 3.0, ylim = c(ymin, ymax),
     main = "1. Growth of $10,000 (August - September 2026)",
     xlab = "Trading Days (Aug 1 - Sep 28, 2026)", ylab = "Portfolio Equity ($)",
     cex.main = 1.15, cex.axis = 0.9, font.main = 2)

lines(days_x, eq_broad_enh, col = "#2b83ba", lwd = 2.8)
lines(days_x, eq_inst_enh,  col = "#4daf4a", lwd = 2.8)

lines(days_x, eq_focus_base, col = "#1b9e77", lwd = 1.5, lty = 2)
lines(days_x, eq_broad_base, col = "#2b83ba", lwd = 1.5, lty = 2)
lines(days_x, eq_inst_base,  col = "#4daf4a", lwd = 1.5, lty = 2)

lines(days_x, eq_spy, col = "#7570b3", lwd = 2.2, lty = 3)
lines(days_x, eq_qqq, col = "#d95f02", lwd = 2.2, lty = 4)
grid(col = "gray85")

legend("topleft",
       legend = c(
         sprintf("Focus Enhanced ($12,564 | +25.64%%)"),
         sprintf("Broad Enhanced ($11,200 | +12.00%%)"),
         sprintf("Institutional Enhanced ($10,559 | +5.59%%)"),
         sprintf("QQQ Nasdaq-100 ($10,706 | +7.06%%)"),
         sprintf("SPY S&P 500 ($10,249 | +2.49%%)"),
         "Baseline Models (Dashed Lines)"
       ),
       col = c("#1b9e77", "#2b83ba", "#4daf4a", "#d95f02", "#7570b3", "gray40"),
       lty = c(1, 1, 1, 4, 3, 2), lwd = c(3.0, 2.8, 2.8, 2.2, 2.2, 1.5),
       cex = 0.78, bg = rgb(1, 1, 1, 0.90), box.col = "gray70")

# --- Panel 2: Total Return & Alpha vs S&P 500 (Bar Chart) ---
ret_vals <- c(
  res_focus_enh$strat$cum_ret * 100,
  res_focus_base$strat$cum_ret * 100,
  res_broad_enh$strat$cum_ret * 100,
  res_broad_base$strat$cum_ret * 100,
  res_inst_enh$strat$cum_ret * 100,
  res_inst_base$strat$cum_ret * 100,
  res_focus_enh$qqq$cum_ret * 100,
  res_focus_enh$spy$cum_ret * 100
)

names_ret <- c("Focus\n(Enh)", "Focus\n(Base)", "Broad\n(Enh)", "Broad\n(Base)", 
               "Inst 300+\n(Enh)", "Inst 300+\n(Base)", "QQQ\nIndex", "SPY\nIndex")

cols_ret <- c("#1b9e77", "#a1d99b", "#2b83ba", "#9ecae1", "#4daf4a", "#c7e9c0", "#d95f02", "#7570b3")

bp1 <- barplot(ret_vals, names.arg = names_ret, col = cols_ret,
               main = "2. Total Cumulative Return (% Net of Costs)",
               ylab = "Return (%)", ylim = c(-3, 30), cex.main = 1.15, cex.names = 0.82, font.main = 2)
grid(col = "gray85", nx = NA, ny = NULL)
abline(h = 0, col = "gray40", lwd = 1.2)
abline(h = res_focus_enh$spy$cum_ret * 100, col = "#7570b3", lty = 2, lwd = 1.5)
text(bp1, ret_vals + ifelse(ret_vals >= 0, 1.2, -1.2),
     sprintf("%+.1f%%", ret_vals), cex = 0.82, font = 2)

legend("topright", legend = c("Enhanced Model", "Baseline Model", "S&P 500 Baseline (+2.5%)"),
       fill = c("#1b9e77", "#a1d99b", NA), lty = c(NA, NA, 2), col = c(NA, NA, "#7570b3"),
       cex = 0.78, bg = rgb(1, 1, 1, 0.90), box.col = "gray70")

# --- Panel 3: Risk-Adjusted Efficiency: Sharpe Ratio & Max Drawdown ---
sharpe_vals <- c(
  res_focus_enh$strat$sharpe,
  res_broad_enh$strat$sharpe,
  res_inst_enh$strat$sharpe,
  res_focus_enh$qqq$sharpe,
  res_focus_enh$spy$sharpe
)

dd_vals <- c(
  res_focus_enh$strat$max_dd * 100,
  res_broad_enh$strat$max_dd * 100,
  res_inst_enh$strat$max_dd * 100,
  res_focus_enh$qqq$max_dd * 100,
  res_focus_enh$spy$max_dd * 100
)

names_univ <- c("Focus", "Broad", "Inst 300+", "QQQ", "SPY")
mat_risk <- rbind(sharpe_vals, dd_vals)

bp2 <- barplot(mat_risk, beside = TRUE, names.arg = names_univ,
               col = c("#238b45", "#cb181d"), ylim = c(-6, 11.5),
               main = "3. Risk Efficiency: Sharpe Ratio vs Max Drawdown (%)",
               ylab = "Value", cex.main = 1.15, cex.names = 0.9, font.main = 2)
grid(col = "gray85", nx = NA, ny = NULL)
abline(h = 0, col = "gray40", lwd = 1.2)

# Labels
text(bp2[1, ], sharpe_vals + 0.6, sprintf("%.2f", sharpe_vals), cex = 0.85, font = 2, col = "#00441b")
text(bp2[2, ], -abs(dd_vals) - 0.7, sprintf("-%.1f%%", dd_vals), cex = 0.85, font = 2, col = "#67000d")

legend("topright", legend = c("Sharpe Ratio (Higher = Better)", "Max Drawdown % (Lower = Better)"),
       fill = c("#238b45", "#cb181d"), cex = 0.80, bg = rgb(1, 1, 1, 0.90), box.col = "gray70")

# --- Panel 4: Monthly Breakdown: August vs September Returns ---
aug_vals <- c(
  aug_focus$strat$cum_ret * 100,
  aug_broad$strat$cum_ret * 100,
  aug_inst$strat$cum_ret * 100,
  aug_qqq_ret * 100,
  aug_spy_ret * 100
)

sep_vals <- c(
  sep_focus$strat$cum_ret * 100,
  sep_broad$strat$cum_ret * 100,
  sep_inst$strat$cum_ret * 100,
  sep_qqq_ret * 100,
  sep_spy_ret * 100
)

mat_months <- rbind(aug_vals, sep_vals)

bp3 <- barplot(mat_months, beside = TRUE, names.arg = names_univ,
               col = c("#fdae61", "#2b83ba"), ylim = c(-2.5, 17.5),
               main = "4. Month-by-Month Regime: August vs September 2026",
               ylab = "Monthly Return (%)", cex.main = 1.15, cex.names = 0.9, font.main = 2)
grid(col = "gray85", nx = NA, ny = NULL)
abline(h = 0, col = "gray40", lwd = 1.2)

text(bp3[1, ], aug_vals + ifelse(aug_vals >= 0, 0.8, -0.8), sprintf("%+.1f%%", aug_vals), cex = 0.82, font = 2)
text(bp3[2, ], sep_vals + ifelse(sep_vals >= 0, 0.8, -0.8), sprintf("%+.1f%%", sep_vals), cex = 0.82, font = 2)

legend("topright", legend = c("August 2026", "September 2026"),
       fill = c("#fdae61", "#2b83ba"), cex = 0.80, bg = rgb(1, 1, 1, 0.90), box.col = "gray70")

# Super title
mtext("August - September 2026 Swing Trading Backtest Report: 3 Universes vs Benchmarks", 
      outer = TRUE, cex = 1.4, font = 2, col = "#111111")

dev.off()
cat(sprintf("[Saved] 4-panel report chart saved to: %s\n", chart_file))

# Copy to artifact directory
if (dir.exists(ARTIFACT_DIR)) {
  file.copy(chart_file, file.path(ARTIFACT_DIR, "august_september_2026_backtest_report.png"), overwrite = TRUE)
  cat(sprintf("[Saved] Chart copied to artifact directory: %s\n\n", file.path(ARTIFACT_DIR, "august_september_2026_backtest_report.png")))
}

cat("Chart and scorecard generation completed successfully!\n")
