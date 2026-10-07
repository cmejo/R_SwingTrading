suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(TTR)
})

source("R/08_metrics.R")

CACHE_DIR_10Y <- "data/cache_10yr"
DATA_DIR_10Y  <- "data/data_10yr"
OUTPUT_DIR    <- "output"
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

# 1. Load Benchmarks
qqq_ohlcv <- readRDS(file.path(DATA_DIR_10Y, "data_qqq.rds"))
spy_ohlcv <- readRDS(file.path(DATA_DIR_10Y, "data_spy.rds"))
qqq_cl    <- Cl(qqq_ohlcv)
spy_cl    <- Cl(spy_ohlcv)
qqq_ret   <- na.omit(qqq_cl / lag.xts(qqq_cl, 1) - 1)
spy_ret   <- na.omit(spy_cl / lag.xts(spy_cl, 1) - 1)

# 2. Load Sector Map
SECTOR_MAP <- if (file.exists("sector_map.json")) jsonlite::fromJSON("sector_map.json") else list()
get_sym_sector <- function(s) {
  if (!is.null(SECTOR_MAP[[s]])) return(SECTOR_MAP[[s]])
  return("General_Tech")
}

# 3. Load all cached 10-year symbol objects
cache_files <- list.files(CACHE_DIR_10Y, pattern = "_10yr_wf\\.rds$", full.names = TRUE)
cat(sprintf("[10Yr-Sim] Loading %d cached symbol files...\n", length(cache_files)))

sim_data <- list()
for (cf in cache_files) {
  obj <- readRDS(cf)
  sim_data[[obj$symbol]] <- obj
}
avail_syms <- names(sim_data)

# 4. Find valid 10-year evaluation dates
common_bench_dates <- intersect(index(qqq_ret), index(spy_ret))
eval_dates <- sort(common_bench_dates)
if (length(eval_dates) > 2520) eval_dates <- tail(eval_dates, 2520)
n_eval <- length(eval_dates)

cat(sprintf("[10Yr-Sim] 10-Year window: %d trading sessions (%s to %s)\n",
            n_eval, as.character(eval_dates[1]), as.character(tail(eval_dates, 1))))

qqq_idx <- match(eval_dates, index(qqq_ret))
qqq_sub <- as.numeric(qqq_ret)[qqq_idx]
qqq_sub[is.na(qqq_sub)] <- 0

spy_idx <- match(eval_dates, index(spy_ret))
spy_sub <- as.numeric(spy_ret)[spy_idx]
spy_sub[is.na(spy_sub)] <- 0

# 5. Build dense precomputed matrices
n_syms <- length(avail_syms)
mat_prob  <- matrix(0.5,  nrow = n_eval, ncol = n_syms)
mat_rs    <- matrix(0.0,  nrow = n_eval, ncol = n_syms)
mat_vol   <- matrix(0.25, nrow = n_eval, ncol = n_syms)
mat_ret_b <- matrix(0.0,  nrow = n_eval, ncol = n_syms)
mat_valid <- matrix(FALSE, nrow = n_eval, ncol = n_syms)

for (j in seq_along(avail_syms)) {
  s <- avail_syms[j]
  obj <- sim_data[[s]]
  
  idx_p <- match(eval_dates, index(obj$pred_prob))
  v_p   <- !is.na(idx_p)
  if (any(v_p)) mat_prob[v_p, j] <- as.numeric(obj$pred_prob)[idx_p[v_p]]
  
  idx_rs <- match(eval_dates, index(obj$rs_spy))
  v_rs   <- !is.na(idx_rs)
  if (any(v_rs)) mat_rs[v_rs, j] <- as.numeric(obj$rs_spy)[idx_rs[v_rs]]
  
  idx_v <- match(eval_dates, index(obj$garch_vol))
  v_v   <- !is.na(idx_v)
  if (any(v_v)) {
    raw_vol <- as.numeric(obj$garch_vol)[idx_v[v_v]]
    raw_vol[is.na(raw_vol) | raw_vol < 0.05] <- 0.25
    mat_vol[v_v, j] <- raw_vol
  }
  
  idx_b <- match(eval_dates, index(obj$bracket_ret))
  v_b   <- !is.na(idx_b)
  if (any(v_b)) {
    mat_ret_b[v_b, j] <- as.numeric(obj$bracket_ret)[idx_b[v_b]]
    mat_valid[v_b, j] <- TRUE
  }
}

sym_sectors <- sapply(avail_syms, get_sym_sector)

simulate_portfolio <- function(max_pos, max_per_sector, sizing_mode = "vince") {
  port_daily_ret <- numeric(n_eval)
  for (t in 1:n_eval) {
    p_ups   <- mat_prob[t, ]
    rs_vals <- mat_rs[t, ]
    vols    <- mat_vol[t, ]
    rets_b  <- mat_ret_b[t, ]
    val_flg <- mat_valid[t, ]
    
    # Candidate selection
    qualify <- which(val_flg & p_ups >= 0.58 & rs_vals > 0)
    top_idx <- integer(0)
    
    if (length(qualify) > 0) {
      scores <- p_ups[qualify] * (1 + rs_vals[qualify])
      ranked_candidates <- qualify[order(-scores)]
      
      sec_counts <- list()
      for (cand in ranked_candidates) {
        c_sec <- sym_sectors[cand]
        cnt <- if (is.null(sec_counts[[c_sec]])) 0 else sec_counts[[c_sec]]
        if (cnt < max_per_sector) {
          top_idx <- c(top_idx, cand)
          sec_counts[[c_sec]] <- cnt + 1
          if (length(top_idx) >= max_pos) break
        }
      }
    }
    
    n_sel <- length(top_idx)
    if (n_sel > 0) {
      if (sizing_mode == "equal") {
        weights <- rep(1 / n_sel, n_sel)
      } else {
        inv_vols <- 1 / vols[top_idx]
        weights  <- inv_vols / sum(inv_vols)
      }
      swing_exposure <- min(1.0, n_sel / max_pos)
      swing_ret <- sum(weights * rets_b[top_idx])
      cash_ret  <- qqq_sub[t]
      port_daily_ret[t] <- swing_exposure * swing_ret + (1 - swing_exposure) * cash_ret
    } else {
      port_daily_ret[t] <- qqq_sub[t]
    }
  }
  
  port_xts <- xts(port_daily_ret, order.by = eval_dates)
  m_strat  <- calc_performance_metrics(port_xts)$raw
  term_wealth <- 10000 * prod(1 + port_daily_ret)
  
  list(
    equity_curve    = cumprod(1 + port_daily_ret) * 10000,
    daily_returns   = port_daily_ret,
    terminal_wealth = term_wealth,
    cum_return      = (term_wealth / 10000 - 1) * 100,
    ann_return      = m_strat$ann_ret * 100,
    sharpe          = m_strat$sharpe,
    sortino         = m_strat$sortino,
    max_dd          = m_strat$max_dd * 100,
    win_rate        = m_strat$win_rate * 100,
    profit_factor   = m_strat$profit_factor
  )
}

# Run all test configurations
configs_to_test <- list(
  list(name = "MAX_POS=2, CAP=2 (Vince)", mp = 2, cap = 2, sm = "vince", col = "#E63946", lty = 1, lwd = 2.5),
  list(name = "MAX_POS=2, CAP=2 (Equal)", mp = 2, cap = 2, sm = "equal", col = "#D62828", lty = 2, lwd = 2.0),
  list(name = "MAX_POS=3, CAP=2 (Vince)", mp = 3, cap = 2, sm = "vince", col = "#F77F00", lty = 1, lwd = 2.5),
  list(name = "MAX_POS=3, CAP=2 (Equal)", mp = 3, cap = 2, sm = "equal", col = "#FCBF49", lty = 2, lwd = 2.0),
  list(name = "MAX_POS=3, CAP=3 (Vince)", mp = 3, cap = 3, sm = "vince", col = "#E76F51", lty = 1, lwd = 2.0),
  list(name = "MAX_POS=4, CAP=3 (Vince)", mp = 4, cap = 3, sm = "vince", col = "#2A9D8F", lty = 1, lwd = 2.0),
  list(name = "MAX_POS=4, CAP=4 (Vince)", mp = 4, cap = 4, sm = "vince", col = "#264653", lty = 1, lwd = 2.0),
  list(name = "MAX_POS=5, CAP=4 (Vince)", mp = 5, cap = 4, sm = "vince", col = "#0077B6", lty = 1, lwd = 2.5),
  list(name = "MAX_POS=5, CAP=5 (Vince)", mp = 5, cap = 5, sm = "vince", col = "#023E8A", lty = 1, lwd = 2.5),
  list(name = "MAX_POS=5, CAP=5 (Equal)", mp = 5, cap = 5, sm = "equal", col = "#48CAE4", lty = 2, lwd = 2.0)
)

scorecard_rows <- list()
curves_list <- list()

for (cfg in configs_to_test) {
  cat(sprintf("Simulating 10-Year: %s...\n", cfg$name))
  res <- simulate_portfolio(cfg$mp, cfg$cap, cfg$sm)
  curves_list[[cfg$name]] <- res$equity_curve
  
  scorecard_rows[[length(scorecard_rows) + 1]] <- data.frame(
    Configuration   = cfg$name,
    Max_Positions   = cfg$mp,
    Sector_Cap      = cfg$cap,
    Sizing_Mode     = cfg$sm,
    Terminal_Wealth = sprintf("$%.2f", res$terminal_wealth),
    Cum_Return      = sprintf("%+.2f%%", res$cum_return),
    Ann_Return      = sprintf("%+.2f%%", res$ann_return),
    Sharpe_Ratio    = sprintf("%.2f", res$sharpe),
    Sortino_Ratio   = sprintf("%.2f", res$sortino),
    Max_Drawdown    = sprintf("%.2f%%", res$max_dd),
    Win_Rate        = sprintf("%.2f%%", res$win_rate),
    Profit_Factor   = sprintf("%.2f", res$profit_factor),
    Wealth_Num      = res$terminal_wealth,
    Sharpe_Num      = res$sharpe,
    Max_DD_Num      = res$max_dd,
    stringsAsFactors = FALSE
  )
}

df_scorecard <- do.call(rbind, scorecard_rows)
write.csv(df_scorecard, file.path(OUTPUT_DIR, "ten_year_backtest_scorecard.csv"), row.names = FALSE)
cat("[10Yr-Sim] Scorecard saved to output/ten_year_backtest_scorecard.csv\n")
print(df_scorecard[, c("Configuration", "Terminal_Wealth", "Sharpe_Ratio", "Max_Drawdown")])

# Benchmark curves
qqq_curve <- cumprod(1 + qqq_sub) * 10000
spy_curve <- cumprod(1 + spy_sub) * 10000

# 6. Generate Unified High-Resolution 10-Year Chart
chart_file <- file.path(OUTPUT_DIR, "ten_year_backtest_chart.png")
png(chart_file, width = 1750, height = 980, res = 130)

par(mar = c(5, 5, 4, 22), bg = "#F8F9FA")

y_min <- 8000
y_max <- max(sapply(curves_list, max), max(qqq_curve)) * 1.3

plot(eval_dates, qqq_curve, type = "n", log = "y", ylim = c(y_min, y_max),
     xlab = "Year (10-Year Horizon: 2016 - 2026)", ylab = "Portfolio Value (Log Scale, Starting $10,000)",
     main = "10-Year Multi-Configuration Swing Trading Backtest (symbols_broad.txt | Starting $10,000)",
     cex.main = 1.3, font.main = 2, col.main = "#1F2937",
     cex.lab = 1.1, col.lab = "#374151", las = 1)

grid(nx = NULL, ny = NULL, col = "#E5E7EB", lty = 1, lwd = 1.2)

# Benchmarks
lines(eval_dates, spy_curve, col = "#9CA3AF", lwd = 2.0, lty = 3)
lines(eval_dates, qqq_curve, col = "#4B5563", lwd = 2.2, lty = 2)

# Strategy curves
for (cfg in configs_to_test) {
  lines(eval_dates, curves_list[[cfg$name]], col = cfg$col, lty = cfg$lty, lwd = cfg$lwd)
}

# Legend in outer margin
par(xpd = TRUE)
legend_labels <- c(
  sapply(configs_to_test, function(x) {
    row <- df_scorecard[df_scorecard$Configuration == x$name, ]
    sprintf("%s: %s (DD: %s)", x$name, row$Terminal_Wealth, row$Max_Drawdown)
  }),
  sprintf("QQQ Benchmark: $%.0f", tail(qqq_curve, 1)),
  sprintf("SPY Benchmark: $%.0f", tail(spy_curve, 1))
)
legend_cols <- c(sapply(configs_to_test, function(x) x$col), "#4B5563", "#9CA3AF")
legend_ltys <- c(sapply(configs_to_test, function(x) x$lty), 2, 3)
legend_lwds <- c(sapply(configs_to_test, function(x) x$lwd), 2.2, 2.0)

legend(max(eval_dates) + 40, y_max,
       legend = legend_labels,
       col = legend_cols,
       lty = legend_ltys,
       lwd = legend_lwds,
       bty = "n", cex = 0.80, y.intersp = 1.25)
par(xpd = FALSE)

dev.off()
cat(sprintf("[10Yr-Sim] Chart successfully saved to %s\n", chart_file))
