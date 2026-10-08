#!/usr/bin/env Rscript
# ==============================================================================
# Comprehensive Multi-Horizon, Multi-Configuration Backtest with Tax & Leverage
# Initial Capital: $10,000
# Leverage: 1.5x (with 7.0% annual margin interest deducted on borrowed 0.5x)
# Short-Term Capital Gains Tax Rate: 37% (applied annually at tax year-end)
# Sizing Mode: Vince (LSPM risk-weighted) & Equal-weight benchmark comparison
# Max Positions: 2, 3, 4, 5
# Sector Caps: 2, 3, 4, 5 (where sector_cap <= max_positions)
# Horizons: 2M (42b), 6M (126b), 12M (252b), 24M (504b), 60M (1260b), 84M (1764b), 120M (2520b)
# ==============================================================================

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(TTR)
  library(jsonlite)
})

source("R/08_metrics.R")

CACHE_DIR_10Y <- "data/cache_10yr"
DATA_DIR_10Y  <- "data/data_10yr"
OUTPUT_DIR    <- "output"
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

STARTING_CAPITAL <- 10000
LEVERAGE         <- 1.5
BORROWED_MARGIN  <- LEVERAGE - 1.0 # 0.5x
MARGIN_RATE_ANN  <- 0.070          # 7.0% broker margin interest rate
DAILY_MARGIN_FEE <- (MARGIN_RATE_ANN / 252) * BORROWED_MARGIN
TAX_RATE         <- 0.37           # 37% short-term capital gains tax rate

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
cat(sprintf("[Setup] Loading %d cached 10-year symbol files...\n", length(cache_files)))

sim_data <- list()
for (cf in cache_files) {
  obj <- readRDS(cf)
  sim_data[[obj$symbol]] <- obj
}
avail_syms <- names(sim_data)

# 4. Find valid shared trading dates
all_dates <- do.call(c, lapply(avail_syms, function(s) index(sim_data[[s]]$pred_prob)))
date_counts <- table(all_dates)
shared_dates <- as.Date(names(date_counts)[date_counts >= 30])
shared_dates <- sort(intersect(shared_dates, intersect(index(qqq_ret), index(spy_ret))))
max_avail_bars <- length(shared_dates)

cat(sprintf("[Setup] Shared evaluation window: %d trading sessions (%s to %s)\n",
            max_avail_bars, as.character(shared_dates[1]), as.character(tail(shared_dates, 1))))

# 5. Precompute dense matrices across all shared dates
n_syms <- length(avail_syms)
full_mat_prob  <- matrix(0.5,  nrow = max_avail_bars, ncol = n_syms)
full_mat_rs    <- matrix(0.0,  nrow = max_avail_bars, ncol = n_syms)
full_mat_vol   <- matrix(0.25, nrow = max_avail_bars, ncol = n_syms)
full_mat_ret_b <- matrix(0.0,  nrow = max_avail_bars, ncol = n_syms)
full_mat_valid <- matrix(FALSE, nrow = max_avail_bars, ncol = n_syms)

for (j in seq_along(avail_syms)) {
  s <- avail_syms[j]
  obj <- sim_data[[s]]
  
  idx_p <- match(shared_dates, index(obj$pred_prob))
  v_p   <- !is.na(idx_p)
  if (any(v_p)) full_mat_prob[v_p, j] <- as.numeric(obj$pred_prob)[idx_p[v_p]]
  
  idx_rs <- match(shared_dates, index(obj$rs_spy))
  v_rs   <- !is.na(idx_rs)
  if (any(v_rs)) full_mat_rs[v_rs, j] <- as.numeric(obj$rs_spy)[idx_rs[v_rs]]
  
  idx_v <- match(shared_dates, index(obj$garch_vol))
  v_v   <- !is.na(idx_v)
  if (any(v_v)) {
    raw_vol <- as.numeric(obj$garch_vol)[idx_v[v_v]]
    raw_vol[is.na(raw_vol) | raw_vol < 0.05] <- 0.25
    full_mat_vol[v_v, j] <- raw_vol
  }
  
  idx_b <- match(shared_dates, index(obj$bracket_ret))
  v_b   <- !is.na(idx_b)
  if (any(v_b)) {
    full_mat_ret_b[v_b, j] <- as.numeric(obj$bracket_ret)[idx_b[v_b]]
    full_mat_valid[v_b, j] <- TRUE
  }
}

sym_sectors <- sapply(avail_syms, get_sym_sector)

qqq_full <- as.numeric(qqq_ret)[match(shared_dates, index(qqq_ret))]
spy_full <- as.numeric(spy_ret)[match(shared_dates, index(spy_ret))]
qqq_full[is.na(qqq_full)] <- 0
spy_full[is.na(spy_full)] <- 0

# Helper function to simulate a specific horizon and configuration
simulate_horizon_config <- function(h_bars, max_pos, max_per_sector, sizing_mode = "vince") {
  act_bars <- min(h_bars, max_avail_bars)
  start_idx <- max_avail_bars - act_bars + 1
  sub_dates <- shared_dates[start_idx:max_avail_bars]
  
  sub_prob  <- full_mat_prob[start_idx:max_avail_bars, , drop = FALSE]
  sub_rs    <- full_mat_rs[start_idx:max_avail_bars, , drop = FALSE]
  sub_vol   <- full_mat_vol[start_idx:max_avail_bars, , drop = FALSE]
  sub_ret_b <- full_mat_ret_b[start_idx:max_avail_bars, , drop = FALSE]
  sub_valid <- full_mat_valid[start_idx:max_avail_bars, , drop = FALSE]
  sub_qqq   <- qqq_full[start_idx:max_avail_bars]
  
  # Step 1: Base unleveraged daily strategy returns
  base_daily_ret <- numeric(act_bars)
  for (t in 1:act_bars) {
    p_ups   <- sub_prob[t, ]
    rs_vals <- sub_rs[t, ]
    vols    <- sub_vol[t, ]
    rets_b  <- sub_ret_b[t, ]
    val_flg <- sub_valid[t, ]
    
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
      cash_ret  <- sub_qqq[t]
      base_daily_ret[t] <- swing_exposure * swing_ret + (1 - swing_exposure) * cash_ret
    } else {
      base_daily_ret[t] <- sub_qqq[t]
    }
  }
  
  # Step 2: Apply 1.5x Leverage and Margin Financing Cost
  # Daily leveraged return = 1.5 * r_base - DAILY_MARGIN_FEE
  lev_daily_ret <- base_daily_ret * LEVERAGE - DAILY_MARGIN_FEE
  
  # Step 3: Compute Pre-Tax Compounding
  pre_tax_wealth <- STARTING_CAPITAL * prod(1 + lev_daily_ret)
  pre_tax_curve  <- STARTING_CAPITAL * cumprod(1 + lev_daily_ret)
  
  # Step 4: Authentic Realized Short-Term Capital Gains Tax Accounting (37% Tax Rate)
  # In swing trading with 7-day average holding periods, 100% of realized gains are short-term.
  # Taxes are settled annually at the close of each tax year (December 31st) or horizon end.
  # Any year with net positive gains incurs a 37% tax liability deducted from capital.
  # Loss carryforwards are tracked to offset future net gains.
  years_vec <- as.numeric(format(sub_dates, "%Y"))
  unique_years <- unique(years_vec)
  
  cap_after_tax <- STARTING_CAPITAL
  loss_carryforward <- 0
  after_tax_curve <- numeric(act_bars)
  daily_after_tax_ret <- numeric(act_bars)
  
  idx_counter <- 1
  for (yr in unique_years) {
    yr_mask <- which(years_vec == yr)
    yr_rets <- lev_daily_ret[yr_mask]
    yr_cap_start <- cap_after_tax
    
    # Intrayear wealth path
    yr_cumprod <- cumprod(1 + yr_rets)
    for (step_i in seq_along(yr_rets)) {
      after_tax_curve[idx_counter] <- yr_cap_start * yr_cumprod[step_i]
      idx_counter <- idx_counter + 1
    }
    
    # Year-end close
    yr_cap_end_pre_tax <- yr_cap_start * prod(1 + yr_rets)
    net_gain <- yr_cap_end_pre_tax - yr_cap_start
    
    if (net_gain > 0) {
      taxable_gain <- max(0, net_gain - loss_carryforward)
      loss_carryforward <- max(0, loss_carryforward - net_gain)
      tax_liability <- taxable_gain * TAX_RATE
      cap_after_tax <- yr_cap_end_pre_tax - tax_liability
    } else {
      # Net loss added to loss carryforward
      loss_carryforward <- loss_carryforward + abs(net_gain)
      cap_after_tax <- yr_cap_end_pre_tax
    }
    # Update curve endpoint for year-end settlement
    after_tax_curve[idx_counter - 1] <- cap_after_tax
  }
  
  # Effective daily returns after tax for metric calculations
  daily_after_tax_ret[1] <- (after_tax_curve[1] / STARTING_CAPITAL) - 1
  if (act_bars > 1) {
    daily_after_tax_ret[2:act_bars] <- (after_tax_curve[2:act_bars] / after_tax_curve[1:(act_bars - 1)]) - 1
  }
  
  after_tax_wealth <- cap_after_tax
  
  # Compute performance metrics on after-tax series
  port_xts_post <- xts(daily_after_tax_ret, order.by = sub_dates)
  m_post <- calc_performance_metrics(port_xts_post)$raw
  
  # Compute performance metrics on pre-tax series for comparison
  port_xts_pre <- xts(lev_daily_ret, order.by = sub_dates)
  m_pre <- calc_performance_metrics(port_xts_pre)$raw
  
  list(
    trading_days       = act_bars,
    pre_tax_wealth     = pre_tax_wealth,
    after_tax_wealth   = after_tax_wealth,
    pre_tax_cum_ret    = (pre_tax_wealth / STARTING_CAPITAL - 1) * 100,
    after_tax_cum_ret  = (after_tax_wealth / STARTING_CAPITAL - 1) * 100,
    pre_tax_ann_ret    = m_pre$ann_ret * 100,
    after_tax_ann_ret  = m_post$ann_ret * 100,
    sharpe             = m_post$sharpe,
    sortino            = m_post$sortino,
    max_dd             = m_post$max_dd * 100,
    win_rate           = m_post$win_rate * 100,
    profit_factor      = m_post$profit_factor,
    curve_pre_tax      = pre_tax_curve,
    curve_after_tax    = after_tax_curve,
    dates              = sub_dates
  )
}

# Horizons defined by user
horizons <- list(
  "2 Months"   = 42,
  "6 Months"   = 126,
  "12 Months"  = 252,
  "24 Months"  = 504,
  "60 Months"  = 1260,
  "84 Months"  = 1764,
  "120 Months" = 2520
)

# Test matrix:
# MAX_POS: 2, 3, 4, 5
# MAX_PER_SECTOR: 2, 3, 4, 5 (sector_cap <= max_pos)
# Sizing: Vince (primary) + Equal for comparison
all_results <- list()

cat("\n========================================================================================\n")
cat(" RUNNING TAX-ADJUSTED (37%) & 1.5x LEVERAGE BACKTEST MATRIX ACROSS ALL 7 HORIZONS       \n")
cat("========================================================================================\n")

pos_cap_combos <- list(
  list(mp = 2, cap = 2, sm = "vince"),
  list(mp = 3, cap = 2, sm = "vince"),
  list(mp = 3, cap = 3, sm = "vince"),
  list(mp = 4, cap = 2, sm = "vince"),
  list(mp = 4, cap = 3, sm = "vince"),
  list(mp = 4, cap = 4, sm = "vince"),
  list(mp = 5, cap = 2, sm = "vince"),
  list(mp = 5, cap = 3, sm = "vince"),
  list(mp = 5, cap = 4, sm = "vince"),
  list(mp = 5, cap = 5, sm = "vince"),
  # Equal-weight benchmark comparison configs
  list(mp = 3, cap = 3, sm = "equal"),
  list(mp = 4, cap = 4, sm = "equal"),
  list(mp = 5, cap = 5, sm = "equal")
)

# Progress loop
for (h_name in names(horizons)) {
  h_bars <- horizons[[h_name]]
  cat(sprintf("\n>>> Processing Horizon: %s (%d trading days)...\n", h_name, h_bars))
  for (combo in pos_cap_combos) {
    mp  <- combo$mp
    cap <- combo$cap
    sm  <- combo$sm
    cfg_name <- sprintf("MAX_POS=%d, CAP=%d (%s)", mp, cap, ifelse(sm == "vince", "Vince", "Equal"))
    
    res <- simulate_horizon_config(h_bars, max_pos = mp, max_per_sector = cap, sizing_mode = sm)
    
    all_results[[length(all_results) + 1]] <- data.frame(
      Horizon             = h_name,
      Trading_Days        = res$trading_days,
      Configuration       = cfg_name,
      Max_Positions       = mp,
      Sector_Cap          = cap,
      Sizing_Mode         = sm,
      Starting_Capital    = STARTING_CAPITAL,
      PreTax_Wealth       = sprintf("$%.2f", res$pre_tax_wealth),
      AfterTax_Wealth     = sprintf("$%.2f", res$after_tax_wealth),
      PreTax_Wealth_Num   = res$pre_tax_wealth,
      AfterTax_Wealth_Num = res$after_tax_wealth,
      PreTax_AnnRet       = sprintf("%+.2f%%", res$pre_tax_ann_ret),
      AfterTax_AnnRet     = sprintf("%+.2f%%", res$after_tax_ann_ret),
      Sharpe_Ratio        = sprintf("%.2f", res$sharpe),
      Sortino_Ratio       = sprintf("%.2f", res$sortino),
      Max_Drawdown        = sprintf("%.2f%%", res$max_dd),
      Win_Rate            = sprintf("%.2f%%", res$win_rate),
      Profit_Factor       = sprintf("%.2f", res$profit_factor),
      AfterTax_AnnRet_Num = res$after_tax_ann_ret,
      Sharpe_Num          = res$sharpe,
      Max_DD_Num          = res$max_dd,
      stringsAsFactors    = FALSE
    )
  }
}

df_all <- do.call(rbind, all_results)
csv_out <- file.path(OUTPUT_DIR, "tax_adjusted_1.5x_matrix_backtest.csv")
write.csv(df_all, csv_out, row.names = FALSE)
cat(sprintf("\n[Complete] Full scorecard successfully written to %s (%d total scenario rows)\n", csv_out, nrow(df_all)))

# Also generate 10-Year and 7-Year comparative charts with after-tax curves
chart_10y_file <- file.path(OUTPUT_DIR, "ten_year_tax_adjusted_chart.png")
png(chart_10y_file, width = 1750, height = 980, res = 130)
par(mar = c(5, 5, 4, 22), bg = "#F8F9FA")

# Run representative 10-year configurations for the chart
plot_configs <- list(
  list(name = "MAX_POS=2, CAP=2 (Vince)", mp = 2, cap = 2, sm = "vince", col = "#E63946", lty = 1, lwd = 2.5),
  list(name = "MAX_POS=3, CAP=3 (Vince)", mp = 3, cap = 3, sm = "vince", col = "#F77F00", lty = 1, lwd = 2.5),
  list(name = "MAX_POS=4, CAP=4 (Vince)", mp = 4, cap = 4, sm = "vince", col = "#2A9D8F", lty = 1, lwd = 2.2),
  list(name = "MAX_POS=5, CAP=5 (Vince)", mp = 5, cap = 5, sm = "vince", col = "#0077B6", lty = 1, lwd = 2.2),
  list(name = "MAX_POS=3, CAP=3 (Equal)", mp = 3, cap = 3, sm = "equal", col = "#FCBF49", lty = 2, lwd = 1.8),
  list(name = "MAX_POS=4, CAP=4 (Equal)", mp = 4, cap = 4, sm = "equal", col = "#48CAE4", lty = 2, lwd = 1.8)
)

sim_10y_curves <- list()
sim_10y_post_w <- list()
for (pc in plot_configs) {
  res_c <- simulate_horizon_config(2520, max_pos = pc$mp, max_per_sector = pc$cap, sizing_mode = pc$sm)
  sim_10y_curves[[pc$name]] <- res_c$curve_after_tax
  sim_10y_post_w[[pc$name]] <- res_c$after_tax_wealth
}

c_dates <- tail(shared_dates, 2520)
qqq_10y <- cumprod(1 + tail(qqq_full, 2520)) * 10000
spy_10y <- cumprod(1 + tail(spy_full, 2520)) * 10000

y_min <- 6000
y_max <- max(sapply(sim_10y_curves, max), max(qqq_10y)) * 1.3

plot(c_dates, qqq_10y, type = "n", log = "y", ylim = c(y_min, y_max),
     xlab = "Year (10-Year Horizon: 2016 - 2026)",
     ylab = "After-Tax Portfolio Value (Log Scale, Starting $10,000, 37% Tax)",
     main = "10-Year Swing Strategy Backtest with 1.5x Leverage & 37% Short-Term Capital Gains Tax",
     cex.main = 1.25, font.main = 2, col.main = "#1F2937",
     cex.lab = 1.05, col.lab = "#374151", las = 1)

grid(nx = NULL, ny = NULL, col = "#E5E7EB", lty = 1, lwd = 1.2)
lines(c_dates, spy_10y, col = "#9CA3AF", lwd = 2.0, lty = 3)
lines(c_dates, qqq_10y, col = "#4B5563", lwd = 2.2, lty = 2)

for (pc in plot_configs) {
  lines(c_dates, sim_10y_curves[[pc$name]], col = pc$col, lty = pc$lty, lwd = pc$lwd)
}

par(xpd = TRUE)
legend_labels <- c(
  sapply(plot_configs, function(x) {
    w <- sim_10y_post_w[[x$name]]
    sprintf("%s: $%.0f", x$name, w)
  }),
  sprintf("QQQ Benchmark: $%.0f", tail(qqq_10y, 1)),
  sprintf("SPY Benchmark: $%.0f", tail(spy_10y, 1))
)
legend_cols <- c(sapply(plot_configs, function(x) x$col), "#4B5563", "#9CA3AF")
legend_ltys <- c(sapply(plot_configs, function(x) x$lty), 2, 3)
legend_lwds <- c(sapply(plot_configs, function(x) x$lwd), 2.2, 2.0)

legend(max(c_dates) + 40, y_max,
       legend = legend_labels,
       col = legend_cols,
       lty = legend_ltys,
       lwd = legend_lwds,
       bty = "n", cex = 0.82, y.intersp = 1.3)
par(xpd = FALSE)
dev.off()
cat(sprintf("[Chart] Saved 10-Year tax-adjusted chart to %s\n", chart_10y_file))

