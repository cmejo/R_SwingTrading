#!/usr/bin/env Rscript
# ==============================================================================
# June through September 2026 Multi-Universe Backtest Engine
# Compares symbols.txt, symbols_broad.txt, and symbols_300.txt across:
#   1. June 2026 (21 Trading Days)
#   2. July 2026 (22 Trading Days)
#   3. August 2026 (21 Trading Days)
#   4. September 2026 (19 Trading Days)
#   5. Full 4-Month Period: June - September 2026 (83 Trading Days)
#
# Evaluates:
#   - BASELINE Model (Equal-weight basket, Target Vol = 0.30, 0% Cash Yield)
#   - ENHANCED Model (Top-5 Cross-Sectional Ranking, Target Vol = 0.45, Macro-Gated QQQ Cash Yield, 25% Chandelier Runner)
#   - Benchmarks: SPY & QQQ
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

cat("\n========================================================================================\n")
cat(" JUNE THROUGH SEPTEMBER 2026 MULTI-UNIVERSE BACKTEST ENGINE\n")
cat("========================================================================================\n\n")

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

syms_focus <- read_universe_symbols("symbols.txt")
syms_broad <- read_universe_symbols("symbols_broad.txt")
syms_300   <- read_universe_symbols("symbols_300.txt")

universes <- list(
  "Focus (symbols.txt)"             = syms_focus,
  "Broad (symbols_broad.txt)"       = syms_broad,
  "Institutional (symbols_300.txt)" = syms_300
)

all_unique_syms <- unique(c(syms_focus, syms_broad, syms_300))
cache_files <- file.path(CACHE_DIR, sprintf("%s_504wf.rds", tolower(all_unique_syms)))
valid_mask  <- file.exists(cache_files)
valid_syms  <- all_unique_syms[valid_mask]
valid_files <- cache_files[valid_mask]

cat(sprintf("Loaded Universes: Focus Watchlist (%d), Broad (%d), Institutional 300+ (%d)\n",
            length(syms_focus), length(syms_broad), length(syms_300)))
cat(sprintf("Loading %d cached walk-forward assets from %s...\n", length(valid_files), CACHE_DIR))

t_start <- Sys.time()
sim_results <- setNames(lapply(valid_files, readRDS), valid_syms)
t_end   <- Sys.time()
cat(sprintf("Assets loaded successfully in %.2f seconds!\n\n", as.numeric(difftime(t_end, t_start, units = "secs"))))

# Define Backtest Periods
periods <- list(
  "June 2026"               = list(start = "2026-06-01", end = "2026-06-30"),
  "July 2026"               = list(start = "2026-07-01", end = "2026-07-31"),
  "August 2026"             = list(start = "2026-08-01", end = "2026-08-31"),
  "September 2026"          = list(start = "2026-09-01", end = "2026-09-28"),
  "4-Month (Jun-Sep 2026)"  = list(start = "2026-06-01", end = "2026-09-28")
)

# Portfolio Evaluation Function
evaluate_portfolio_window <- function(sym_list, start_d, end_d, mode = c("BASELINE", "ENHANCED"), universe_name = "Focus", period_name = "June 2026") {
  mode <- match.arg(mode)
  avail_syms <- intersect(sym_list, names(sim_results))
  if (length(avail_syms) == 0) return(NULL)
  
  all_d <- index(spy_ret_all)
  eval_dates <- all_d[all_d >= as.Date(start_d) & all_d <= as.Date(end_d)]
  n_eval <- length(eval_dates)
  if (n_eval == 0) return(NULL)
  
  spy_sub  <- as.numeric(spy_ret_all[eval_dates])
  qqq_sub  <- as.numeric(qqq_ret_all[eval_dates])
  qqq_bull <- as.numeric(qqq_cl[eval_dates] > qqq_ma50[eval_dates])
  
  port_daily_ret <- numeric(n_eval)
  
  if (mode == "BASELINE") {
    # BASELINE: Equal-weight all assets, target_vol = 0.30, 0% cash yield
    ret_matrix <- matrix(0, nrow = n_eval, ncol = length(avail_syms))
    for (j in seq_along(avail_syms)) {
      s <- avail_syms[j]
      obj <- sim_results[[s]]
      idx_m <- match(eval_dates, index(obj$base_ret))
      valid <- !is.na(idx_m)
      if (any(valid)) {
        ret_matrix[valid, j] <- as.numeric(obj$base_ret)[idx_m[valid]]
      }
    }
    port_daily_ret <- rowMeans(ret_matrix, na.rm = TRUE)
    port_daily_ret[is.na(port_daily_ret)] <- 0
    
  } else if (mode == "ENHANCED") {
    # ENHANCED:
    # 1. Top-5 ranking by P(Up) * (1 + RS) (P(Up) >= 0.58, RS > 0)
    # 2. Target vol = 0.45 (up to 1.5x margin)
    # 3. Macro-gated Core-Satellite QQQ cash parking (only park in QQQ when QQQ > SMA50, otherwise cash)
    # 4. 25% Chandelier runner lot
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
        raw_vol <- as.numeric(obj$garch_vol)[idx_v[v_v]]
        raw_vol[is.na(raw_vol) | raw_vol < 0.05] <- 0.25
        mat_vol[v_v, j] <- raw_vol
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
      # Macro gate: if QQQ is bullish, earn QQQ return, else 0% cash
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
        cash_yield_pnl <- unalloc_cash * cash_ret
        port_daily_ret[t] <- stock_pnl_total + cash_yield_pnl
      }
    }
  }
  
  strat_name <- sprintf("%s | %s (%s)", universe_name, mode, period_name)
  m_strat <- calc_performance_metrics(port_daily_ret, name = strat_name)$raw
  m_spy   <- calc_performance_metrics(spy_sub, name = "SPY")$raw
  m_qqq   <- calc_performance_metrics(qqq_sub, name = "QQQ")$raw
  
  list(
    universe      = universe_name,
    period        = period_name,
    dates         = eval_dates,
    bars          = n_eval,
    mode          = mode,
    strat_ret     = m_strat$cum_ret,
    strat_ann_ret = m_strat$ann_ret,
    strat_vol     = m_strat$ann_vol,
    strat_sharpe  = m_strat$sharpe,
    strat_max_dd  = m_strat$max_dd,
    strat_win_r   = m_strat$win_rate,
    strat_pf      = m_strat$profit_factor,
    spy_ret       = m_spy$cum_ret,
    spy_sharpe    = m_spy$sharpe,
    qqq_ret       = m_qqq$cum_ret,
    qqq_sharpe    = m_qqq$sharpe,
    alpha_spy     = m_strat$cum_ret - m_spy$cum_ret,
    alpha_qqq     = m_strat$cum_ret - m_qqq$cum_ret,
    daily_returns = port_daily_ret,
    spy_daily     = spy_sub,
    qqq_daily     = qqq_sub
  )
}

# Run Grid Across Periods & Universes
all_evals <- list()

for (p_name in names(periods)) {
  p_info <- periods[[p_name]]
  for (u_name in names(universes)) {
    u_syms <- universes[[u_name]]
    
    # Baseline
    res_base <- evaluate_portfolio_window(u_syms, p_info$start, p_info$end, mode = "BASELINE", universe_name = u_name, period_name = p_name)
    if (!is.null(res_base)) all_evals[[length(all_evals) + 1]] <- res_base
    
    # Enhanced
    res_enh  <- evaluate_portfolio_window(u_syms, p_info$start, p_info$end, mode = "ENHANCED", universe_name = u_name, period_name = p_name)
    if (!is.null(res_enh)) all_evals[[length(all_evals) + 1]] <- res_enh
  }
}

# Build Summary Table
results_df <- do.call(rbind, lapply(all_evals, function(e) {
  data.frame(
    Period         = e$period,
    Universe       = e$universe,
    Strategy_Mode  = e$mode,
    Bars           = e$bars,
    Cumulative_Ret = sprintf("%+6.2f%%", e$strat_ret * 100),
    Annualized_Ret = sprintf("%+6.2f%%", e$strat_ann_ret * 100),
    Annualized_Vol = sprintf("%5.2f%%",  e$strat_vol * 100),
    Sharpe_Ratio   = sprintf("%5.2f",    e$strat_sharpe),
    Max_Drawdown   = sprintf("-%5.2f%%", e$strat_max_dd * 100),
    Win_Rate       = sprintf("%5.1f%%",  e$strat_win_r * 100),
    Profit_Factor  = sprintf("%5.2f",    ifelse(is.na(e$strat_pf), 0, e$strat_pf)),
    SP500_Ret      = sprintf("%+6.2f%%", e$spy_ret * 100),
    SP500_Sharpe   = sprintf("%5.2f",    e$spy_sharpe),
    Nasdaq_Ret     = sprintf("%+6.2f%%", e$qqq_ret * 100),
    Alpha_vs_SPY   = sprintf("%+6.2f%%", e$alpha_spy * 100),
    Alpha_vs_QQQ   = sprintf("%+6.2f%%", e$alpha_qqq * 100),
    stringsAsFactors = FALSE
  )
}))

cat("\n========================================================================================\n")
cat("            JUNE THROUGH SEPTEMBER 2026 BACKTEST SCORECARD                              \n")
cat("========================================================================================\n\n")
print(results_df, row.names = FALSE)

# Save to CSV and RDS
scorecard_csv <- file.path(OUTPUT_DIR, "june_to_september_2026_scorecard.csv")
write.csv(results_df, file = scorecard_csv, row.names = FALSE)
cat(sprintf("\n[Saved] Scorecard saved to: %s\n", scorecard_csv))

data_rds <- file.path(OUTPUT_DIR, "june_to_september_2026_data.rds")
saveRDS(list(evals = all_evals, table = results_df), file = data_rds)
cat(sprintf("[Saved] Raw data saved to: %s\n", data_rds))

# ------------------------------------------------------------------------------
# Generate 5-Panel Visualization Plot (2 rows x 3 cols, panel 6 as summary legend)
# ------------------------------------------------------------------------------
plot_png <- file.path(OUTPUT_DIR, "june_to_september_2026_equity_curves.png")
png(plot_png, width = 2100, height = 1400, res = 150)
layout(matrix(c(1, 2, 3, 4, 5, 5), nrow = 2, ncol = 3, byrow = TRUE))
par(mar = c(4.5, 4.5, 3, 1), oma = c(1, 1, 3.5, 1))

period_keys <- c("June 2026", "July 2026", "August 2026", "September 2026", "4-Month (Jun-Sep 2026)")

for (p in period_keys) {
  e_focus <- NULL
  e_broad <- NULL
  e_inst  <- NULL
  
  for (ev in all_evals) {
    if (ev$period == p && ev$mode == "ENHANCED") {
      if (grepl("Focus", ev$universe)) e_focus <- ev
      if (grepl("Broad", ev$universe)) e_broad <- ev
      if (grepl("Institutional", ev$universe)) e_inst <- ev
    }
  }
  
  eq_focus <- cumprod(1 + e_focus$daily_returns)
  eq_broad <- cumprod(1 + e_broad$daily_returns)
  eq_inst  <- cumprod(1 + e_inst$daily_returns)
  eq_spy   <- cumprod(1 + e_focus$spy_daily)
  eq_qqq   <- cumprod(1 + e_focus$qqq_daily)
  
  ymin <- min(eq_focus, eq_broad, eq_inst, eq_spy, eq_qqq) * 0.96
  ymax <- max(eq_focus, eq_broad, eq_inst, eq_spy, eq_qqq) * 1.04
  
  plot(eq_focus, type = "l", col = "#1b9e77", lwd = 2.6, ylim = c(ymin, ymax),
       main = sprintf("%s (Daily Equity)", p),
       xlab = "Trading Days", ylab = "Equity (Base 1.0)", cex.main = 1.15, cex.axis = 0.9)
  lines(eq_broad, col = "#377eb8", lwd = 2.6, lty = 1)
  lines(eq_inst,  col = "#4daf4a", lwd = 2.6, lty = 1)
  lines(eq_spy,   col = "#7570b3", lwd = 2.0, lty = 2)
  lines(eq_qqq,   col = "#e7298a", lwd = 2.0, lty = 3)
  grid(col = "gray85")
  
  legend("topleft",
         legend = c(
           sprintf("Focus Watchlist (%+.2f%%)",    (tail(eq_focus, 1) - 1) * 100),
           sprintf("Institutional 300+ (%+.2f%%)", (tail(eq_inst, 1) - 1) * 100),
           sprintf("Broad Universe (%+.2f%%)",     (tail(eq_broad, 1) - 1) * 100),
           sprintf("SPY Benchmark (%+.2f%%)",      (tail(eq_spy, 1) - 1) * 100),
           sprintf("QQQ Benchmark (%+.2f%%)",      (tail(eq_qqq, 1) - 1) * 100)
         ),
         col = c("#1b9e77", "#4daf4a", "#377eb8", "#7570b3", "#e7298a"),
         lty = c(1, 1, 1, 2, 3), lwd = c(2.6, 2.6, 2.6, 2.0, 2.0),
         cex = 0.82, bg = rgb(1, 1, 1, 0.90), box.col = "gray70")
}

mtext("June through September 2026 Backtest Comparison: 3 Universes vs SPY & QQQ", 
      outer = TRUE, cex = 1.4, font = 2, col = "#111111")
dev.off()
cat(sprintf("[Saved] Multi-panel equity curves chart saved to: %s\n", plot_png))

# Copy plot to artifact dir if exists
if (dir.exists(ARTIFACT_DIR)) {
  file.copy(plot_png, file.path(ARTIFACT_DIR, "june_to_september_2026_equity_curves.png"), overwrite = TRUE)
  cat(sprintf("[Saved] Chart copied to artifact directory: %s\n\n", file.path(ARTIFACT_DIR, "june_to_september_2026_equity_curves.png")))
}

cat("Execution completed successfully!\n")
