suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(TTR)
})

source("R/08_metrics.R")

CACHE_DIR_ETF <- "data/cache_etf"
DATA_DIR_ETF  <- "data/data_etf"
OUTPUT_DIR    <- "output"
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

user_etfs <- c("IYC", "IYK", "IYE", "IYF", "IYH", "IYJ", "IYM", "IYW", "IYZ", "IDU", "EFA", "EEM", "IEF")

# Map ETF sectors
ETF_SECTOR_MAP <- list(
  "IYC" = "Consumer_Discretionary",
  "IYK" = "Consumer_Staples",
  "IYE" = "Energy",
  "IYF" = "Financials",
  "IYH" = "Healthcare",
  "IYJ" = "Industrials",
  "IYM" = "Materials",
  "IYW" = "Technology",
  "IYZ" = "Telecommunications",
  "IDU" = "Utilities",
  "EFA" = "International_Dev",
  "EEM" = "Emerging_Markets",
  "IEF" = "Treasury_Bonds"
)

get_etf_sector <- function(s) {
  if (!is.null(ETF_SECTOR_MAP[[s]])) return(ETF_SECTOR_MAP[[s]])
  return("General_ETF")
}

# 1. Load Benchmarks
qqq_ohlcv <- readRDS(file.path(DATA_DIR_ETF, "data_qqq.rds"))
spy_ohlcv <- readRDS(file.path(DATA_DIR_ETF, "data_spy.rds"))
qqq_cl    <- Cl(qqq_ohlcv)
spy_cl    <- Cl(spy_ohlcv)
qqq_ret   <- na.omit(qqq_cl / lag.xts(qqq_cl, 1) - 1)
spy_ret   <- na.omit(spy_cl / lag.xts(spy_cl, 1) - 1)

# 2. Load cached ETF objects
sim_data <- list()
for (s in user_etfs) {
  cf <- file.path(CACHE_DIR_ETF, sprintf("%s_etf_wf.rds", tolower(s)))
  sim_data[[s]] <- readRDS(cf)
}

# 3. Find common date range as far back as possible (when all 13 ETFs are available)
common_dates <- index(sim_data[[user_etfs[1]]]$pred_prob)
for (s in user_etfs[-1]) {
  common_dates <- intersect(common_dates, index(sim_data[[s]]$pred_prob))
}
common_dates <- sort(intersect(common_dates, index(qqq_ret)))

eval_dates <- as.Date(common_dates)
n_eval <- length(eval_dates)
start_date <- eval_dates[1]
end_date   <- tail(eval_dates, 1)
num_years  <- round(as.numeric(end_date - start_date) / 365.25, 1)

cat(sprintf("[ETF-Sim] Maximum Available Backtest Window across all 13 ETFs: %d bars (%.1f years: %s to %s)\n",
            n_eval, num_years, as.character(start_date), as.character(end_date)))

qqq_idx <- match(eval_dates, index(qqq_ret))
qqq_sub <- as.numeric(qqq_ret)[qqq_idx]
qqq_sub[is.na(qqq_sub)] <- 0

spy_idx <- match(eval_dates, index(spy_ret))
spy_sub <- as.numeric(spy_ret)[spy_idx]
spy_sub[is.na(spy_sub)] <- 0

# 4. Dense Matrices
n_syms <- length(user_etfs)
mat_prob  <- matrix(0.5,  nrow = n_eval, ncol = n_syms)
mat_rs    <- matrix(0.0,  nrow = n_eval, ncol = n_syms)
mat_vol   <- matrix(0.20, nrow = n_eval, ncol = n_syms)
mat_ret_b <- matrix(0.0,  nrow = n_eval, ncol = n_syms)

for (j in seq_along(user_etfs)) {
  s <- user_etfs[j]
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
    raw_vol[is.na(raw_vol) | raw_vol < 0.04] <- 0.15
    mat_vol[v_v, j] <- raw_vol
  }
  
  idx_b <- match(eval_dates, index(obj$bracket_ret))
  v_b   <- !is.na(idx_b)
  if (any(v_b)) mat_ret_b[v_b, j] <- as.numeric(obj$bracket_ret)[idx_b[v_b]]
}

sym_sectors <- sapply(user_etfs, get_etf_sector)

simulate_portfolio <- function(max_pos, max_per_sector, sizing_mode = "vince") {
  port_daily_ret <- numeric(n_eval)
  for (t in 1:n_eval) {
    p_ups   <- mat_prob[t, ]
    rs_vals <- mat_rs[t, ]
    vols    <- mat_vol[t, ]
    rets_b  <- mat_ret_b[t, ]
    
    # Candidate selection
    qualify <- which(p_ups >= 0.55 & rs_vals > 0)
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

# Run matrix combinations for user:
# max_pos: 2, 3, 4, 5
# cap: 2, 3, 4, 5
# sizing: vince, equal

max_pos_vals <- c(2, 3, 4, 5)
sector_caps  <- c(2, 3, 4, 5)
sizing_modes <- c("vince", "equal")

scorecard_rows <- list()
curves_list <- list()

for (mp in max_pos_vals) {
  valid_caps <- unique(pmin(mp, sector_caps))
  for (cap in valid_caps) {
    for (sm in sizing_modes) {
      cfg_name <- sprintf("MAX_POS=%d, CAP=%d (%s)", mp, cap, ifelse(sm == "vince", "Vince", "Equal"))
      cat(sprintf("Simulating ETF Universe: %s...\n", cfg_name))
      res <- simulate_portfolio(mp, cap, sm)
      curves_list[[cfg_name]] <- res$equity_curve
      
      scorecard_rows[[length(scorecard_rows) + 1]] <- data.frame(
        Configuration   = cfg_name,
        Max_Positions   = mp,
        Sector_Cap      = cap,
        Sizing_Mode     = sm,
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
  }
}

df_scorecard <- do.call(rbind, scorecard_rows)
write.csv(df_scorecard, file.path(OUTPUT_DIR, "etf_universe_backtest_scorecard.csv"), row.names = FALSE)
cat("[ETF-Sim] Scorecard saved to output/etf_universe_backtest_scorecard.csv\n")
print(df_scorecard[, c("Configuration", "Terminal_Wealth", "Sharpe_Ratio", "Max_Drawdown")])

# Benchmark curves
qqq_curve <- cumprod(1 + qqq_sub) * 10000
spy_curve <- cumprod(1 + spy_sub) * 10000

# 5. Generate Unified ETF Chart
chart_file <- file.path(OUTPUT_DIR, "etf_universe_backtest_chart.png")
png(chart_file, width = 1800, height = 1000, res = 130)

par(mar = c(5, 5, 4, 23), bg = "#F8F9FA")

y_min <- 7000
y_max <- max(sapply(curves_list, max), max(qqq_curve)) * 1.3

plot(eval_dates, qqq_curve, type = "n", log = "y", ylim = c(y_min, y_max),
     xlab = sprintf("Year (23-Year Horizon: %s - %s)", format(start_date, "%Y"), format(end_date, "%Y")),
     ylab = "Portfolio Value (Log Scale, Starting $10,000)",
     main = sprintf("Multi-Decade Sector & Asset Class ETF Swing Backtest (13 ETFs | 2003-2026 | Starting $10,000)"),
     cex.main = 1.25, font.main = 2, col.main = "#1F2937",
     cex.lab = 1.1, col.lab = "#374151", las = 1)

grid(nx = NULL, ny = NULL, col = "#E5E7EB", lty = 1, lwd = 1.2)

# Benchmarks
lines(eval_dates, spy_curve, col = "#9CA3AF", lwd = 2.0, lty = 3)
lines(eval_dates, qqq_curve, col = "#4B5563", lwd = 2.2, lty = 2)

# Color palette for curves
cols <- c(
  "#E63946", "#D62828", # Pos 2
  "#F77F00", "#FCBF49", # Pos 3 Cap 2
  "#D97706", "#B45309", # Pos 3 Cap 3
  "#059669", "#10B981", # Pos 4 Cap 2
  "#047857", "#065F46", # Pos 4 Cap 3
  "#0E7490", "#06B6D4", # Pos 4 Cap 4
  "#2563EB", "#3B82F6", # Pos 5 Cap 2
  "#1D4ED8", "#60A5FA", # Pos 5 Cap 3
  "#4338CA", "#6366F1", # Pos 5 Cap 4
  "#023E8A", "#0077B6"  # Pos 5 Cap 5
)

for (i in seq_along(names(curves_list))) {
  cname <- names(curves_list)[i]
  is_vince <- grepl("Vince", cname)
  lines(eval_dates, curves_list[[cname]], col = cols[(i - 1) %% length(cols) + 1], lty = if (is_vince) 1 else 2, lwd = if (is_vince) 2.2 else 1.8)
}

# Legend in outer margin
par(xpd = TRUE)
legend_labels <- c(
  sapply(seq_along(names(curves_list)), function(i) {
    cname <- names(curves_list)[i]
    row <- df_scorecard[df_scorecard$Configuration == cname, ]
    sprintf("%s: %s (DD: %s)", cname, row$Terminal_Wealth, row$Max_Drawdown)
  }),
  sprintf("QQQ Benchmark: $%.0f", tail(qqq_curve, 1)),
  sprintf("SPY Benchmark: $%.0f", tail(spy_curve, 1))
)
legend_cols <- c(cols[1:length(names(curves_list))], "#4B5563", "#9CA3AF")
legend_ltys <- c(ifelse(grepl("Vince", names(curves_list)), 1, 2), 2, 3)
legend_lwds <- c(rep(2.0, length(names(curves_list))), 2.2, 2.0)

legend(max(eval_dates) + 30, y_max,
       legend = legend_labels,
       col = legend_cols,
       lty = legend_ltys,
       lwd = legend_lwds,
       bty = "n", cex = 0.76, y.intersp = 1.22)
par(xpd = FALSE)

dev.off()
cat(sprintf("[ETF-Sim] Chart successfully saved to %s\n", chart_file))
