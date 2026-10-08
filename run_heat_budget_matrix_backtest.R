#!/usr/bin/env Rscript
# ==============================================================================
# Comprehensive Multi-Horizon Backtest Comparing Portfolio Heat Budgets:
# Heat Budgets: 5.0% (Current) vs 8.0% vs 10.0%
# Horizons: 2M (42b), 6M (126b), 12M (252b), 24M (504b), 60M (1260b), 84M (1764b), 120M (2520b)
# Max Positions: 2, 3, 4, 5
# Sector Caps: 2, 3, 4, 5 (cap <= max_pos)
# Sizing: Ralph Vince Leverage Space Model
# Gross Leverage: 1.5x (with 7.0% margin interest deducted daily on borrowed 0.5x)
# Short-Term Capital Gains Tax Rate: 37% (annual settlement with loss carryforward)
# Starting Capital: $10,000
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

# 1. Load Benchmarks and Cash Asset Data
qqq_ohlcv <- readRDS(file.path(DATA_DIR_10Y, "data_qqq.rds"))
spy_ohlcv <- readRDS(file.path(DATA_DIR_10Y, "data_spy.rds"))
qqq_cl <- Cl(qqq_ohlcv)
spy_cl <- Cl(spy_ohlcv)
qqq_ret <- na.omit(qqq_cl / lag.xts(qqq_cl, 1) - 1)
spy_ret <- na.omit(spy_cl / lag.xts(spy_cl, 1) - 1)

# 2. Load Sector Map
SECTOR_MAP <- if (file.exists("sector_map.json")) jsonlite::fromJSON("sector_map.json") else list()
get_sym_sector <- function(s) {
  if (!is.null(SECTOR_MAP[[s]])) return(SECTOR_MAP[[s]])
  return("General_Tech")
}

# 3. Load all cached 10-year symbol objects
cache_files <- list.files(CACHE_DIR_10Y, pattern = "_10yr_wf\\.rds$", full.names = TRUE)
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

# 5. Precompute dense matrices
n_syms <- length(avail_syms)
full_mat_prob  <- matrix(0.5,  nrow = max_avail_bars, ncol = n_syms)
full_mat_rs    <- matrix(0.0,  nrow = max_avail_bars, ncol = n_syms)
full_mat_vol   <- matrix(0.25, nrow = max_avail_bars, ncol = n_syms)
full_mat_ret_b <- matrix(0.0,  nrow = max_avail_bars, ncol = n_syms)
full_mat_valid <- matrix(FALSE, nrow = max_avail_bars, ncol = n_syms)
full_mat_sl_pct <- matrix(0.10, nrow = max_avail_bars, ncol = n_syms) # Downside risk pct to stop loss (approx 2.0x daily vol)

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
    # Downside stop risk distance pct = 2.0 * daily_vol = 2.0 * (ann_vol / sqrt(252))
    full_mat_sl_pct[v_v, j] <- pmax(0.04, pmin(0.25, 2.0 * (raw_vol / sqrt(252)) * 1.5))
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
qqq_full[is.na(qqq_full)] <- 0

# Core simulation function incorporating explicit heat budget cap
simulate_heat_config <- function(h_bars, max_pos, max_per_sector, heat_cap_pct = 0.05) {
  act_bars <- min(h_bars, max_avail_bars)
  start_idx <- max_avail_bars - act_bars + 1
  sub_dates <- shared_dates[start_idx:max_avail_bars]
  
  sub_prob  <- full_mat_prob[start_idx:max_avail_bars, , drop = FALSE]
  sub_rs    <- full_mat_rs[start_idx:max_avail_bars, , drop = FALSE]
  sub_vol   <- full_mat_vol[start_idx:max_avail_bars, , drop = FALSE]
  sub_ret_b <- full_mat_ret_b[start_idx:max_avail_bars, , drop = FALSE]
  sub_valid <- full_mat_valid[start_idx:max_avail_bars, , drop = FALSE]
  sub_sl    <- full_mat_sl_pct[start_idx:max_avail_bars, , drop = FALSE]
  sub_qqq   <- qqq_full[start_idx:max_avail_bars]
  
  base_daily_ret <- numeric(act_bars)
  
  for (t in 1:act_bars) {
    p_ups   <- sub_prob[t, ]
    rs_vals <- sub_rs[t, ]
    vols    <- sub_vol[t, ]
    rets_b  <- sub_ret_b[t, ]
    sl_pcts <- sub_sl[t, ]
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
      inv_vols <- 1 / vols[top_idx]
      weights  <- inv_vols / sum(inv_vols)
      
      # Vince slot allocation out of full portfolio equity
      # Each slot normally gets (1.0 / max_pos) of portfolio
      slot_allocations <- weights * (n_sel / max_pos)
      
      # Downside risk for each position = allocation * sl_pct
      cand_risks <- slot_allocations * sl_pcts[top_idx]
      total_proposed_risk <- sum(cand_risks)
      
      # Apply Heat Defense Scaling
      heat_scale <- if (total_proposed_risk > heat_cap_pct && total_proposed_risk > 0) {
        heat_cap_pct / total_proposed_risk
      } else {
        1.0
      }
      
      effective_weights <- weights * heat_scale
      effective_swing_exposure <- min(1.0, (n_sel / max_pos) * heat_scale)
      
      swing_ret <- sum(effective_weights * rets_b[top_idx])
      cash_ret  <- sub_qqq[t]
      base_daily_ret[t] <- effective_swing_exposure * swing_ret + (1 - effective_swing_exposure) * cash_ret
    } else {
      base_daily_ret[t] <- sub_qqq[t]
    }
  }
  
  # 1.5x Margin financing
  lev_daily_ret <- base_daily_ret * LEVERAGE - DAILY_MARGIN_FEE
  pre_tax_wealth <- STARTING_CAPITAL * prod(1 + lev_daily_ret)
  
  # 37% Short-Term Capital Gains Tax Accounting
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
    
    yr_cumprod <- cumprod(1 + yr_rets)
    for (step_i in seq_along(yr_rets)) {
      after_tax_curve[idx_counter] <- yr_cap_start * yr_cumprod[step_i]
      idx_counter <- idx_counter + 1
    }
    
    yr_cap_end_pre_tax <- yr_cap_start * prod(1 + yr_rets)
    net_gain <- yr_cap_end_pre_tax - yr_cap_start
    
    if (net_gain > 0) {
      taxable_gain <- max(0, net_gain - loss_carryforward)
      loss_carryforward <- max(0, loss_carryforward - net_gain)
      tax_liability <- taxable_gain * TAX_RATE
      cap_after_tax <- yr_cap_end_pre_tax - tax_liability
    } else {
      loss_carryforward <- loss_carryforward + abs(net_gain)
      cap_after_tax <- yr_cap_end_pre_tax
    }
    after_tax_curve[idx_counter - 1] <- cap_after_tax
  }
  
  daily_after_tax_ret[1] <- (after_tax_curve[1] / STARTING_CAPITAL) - 1
  if (act_bars > 1) {
    daily_after_tax_ret[2:act_bars] <- (after_tax_curve[2:act_bars] / after_tax_curve[1:(act_bars - 1)]) - 1
  }
  
  port_xts_post <- xts(daily_after_tax_ret, order.by = sub_dates)
  m_post <- calc_performance_metrics(port_xts_post)$raw
  
  port_xts_pre <- xts(lev_daily_ret, order.by = sub_dates)
  m_pre <- calc_performance_metrics(port_xts_pre)$raw
  
  list(
    trading_days      = act_bars,
    pre_tax_wealth    = pre_tax_wealth,
    after_tax_wealth  = cap_after_tax,
    pre_tax_ann_ret   = m_pre$ann_ret * 100,
    after_tax_ann_ret = m_post$ann_ret * 100,
    sharpe            = m_post$sharpe,
    sortino           = m_post$sortino,
    max_dd            = m_post$max_dd * 100,
    win_rate          = m_post$win_rate * 100,
    profit_factor     = m_post$profit_factor,
    curve_after_tax   = after_tax_curve,
    dates             = sub_dates
  )
}

horizons <- list(
  "2 Months"   = 42,
  "6 Months"   = 126,
  "12 Months"  = 252,
  "24 Months"  = 504,
  "60 Months"  = 1260,
  "84 Months"  = 1764,
  "120 Months" = 2520
)

heat_budgets <- c(0.05, 0.08, 0.10)

combos <- list(
  list(mp = 2, cap = 2),
  list(mp = 3, cap = 2),
  list(mp = 3, cap = 3),
  list(mp = 4, cap = 2),
  list(mp = 4, cap = 3),
  list(mp = 4, cap = 4),
  list(mp = 5, cap = 2),
  list(mp = 5, cap = 3),
  list(mp = 5, cap = 4),
  list(mp = 5, cap = 5)
)

results_list <- list()

cat("\n========================================================================================\n")
cat(" RUNNING HEAT BUDGET MATRIX (5% vs 8% vs 10%) ACROSS ALL 7 HORIZONS                      \n")
cat("========================================================================================\n")

for (h_name in names(horizons)) {
  h_bars <- horizons[[h_name]]
  cat(sprintf("\n>>> Simulating Horizon: %s (%d trading days)...\n", h_name, h_bars))
  for (cb in combos) {
    mp  <- cb$mp
    cap <- cb$cap
    
    for (heat in heat_budgets) {
      res <- simulate_heat_config(h_bars, mp, cap, heat_cap_pct = heat)
      cfg_name <- sprintf("MAX_POS=%d, CAP=%d", mp, cap)
      heat_lbl <- sprintf("%.0f%%", heat * 100)
      
      results_list[[length(results_list) + 1]] <- data.frame(
        Horizon          = h_name,
        Trading_Days     = h_bars,
        Configuration    = cfg_name,
        Max_Positions    = mp,
        Sector_Cap       = cap,
        Heat_Budget      = heat_lbl,
        Heat_Num         = heat,
        PreTax_Wealth    = sprintf("$%.2f", res$pre_tax_wealth),
        AfterTax_Wealth  = sprintf("$%.2f", res$after_tax_wealth),
        PreTax_AnnRet    = sprintf("%+.2f%%", res$pre_tax_ann_ret),
        AfterTax_AnnRet  = sprintf("%+.2f%%", res$after_tax_ann_ret),
        Sharpe_Ratio     = sprintf("%.2f", res$sharpe),
        Sortino_Ratio    = sprintf("%.2f", res$sortino),
        Max_Drawdown     = sprintf("%.2f%%", res$max_dd),
        Win_Rate         = sprintf("%.2f%%", res$win_rate),
        Profit_Factor    = sprintf("%.2f", res$profit_factor),
        PreTax_Wealth_Num = res$pre_tax_wealth,
        AfterTax_Wealth_Num = res$after_tax_wealth,
        Sharpe_Num       = res$sharpe,
        Max_DD_Num       = res$max_dd,
        stringsAsFactors = FALSE
      )
    }
  }
}

df_heat <- do.call(rbind, results_list)
csv_out <- file.path(OUTPUT_DIR, "heat_budget_matrix_backtest.csv")
write.csv(df_heat, csv_out, row.names = FALSE)
cat(sprintf("\n[Complete] Heat Budget scorecard written to %s (%d total scenario rows)\n", csv_out, nrow(df_heat)))

# Generate High-Res Chart comparing 5% vs 8% vs 10% Heat for MAX_POS=3, CAP=3 over 10 Years
chart_file <- file.path(OUTPUT_DIR, "heat_budget_comparison_chart.png")
png(chart_file, width = 1750, height = 980, res = 130)
par(mar = c(5, 5, 4, 24), bg = "#F8F9FA")

c_dates <- tail(shared_dates, 2520)
res_h05 <- simulate_heat_config(2520, 3, 3, 0.05)
res_h08 <- simulate_heat_config(2520, 3, 3, 0.08)
res_h10 <- simulate_heat_config(2520, 3, 3, 0.10)

qqq_curve_10y <- cumprod(1 + tail(qqq_full, 2520)) * 10000
spy_curve_10y <- cumprod(1 + as.numeric(spy_ret)[match(c_dates, index(spy_ret))]) * 10000

y_min <- 6000
y_max <- max(res_h10$curve_after_tax, res_h08$curve_after_tax, res_h05$curve_after_tax) * 1.3

plot(c_dates, qqq_curve_10y, type = "n", log = "y", ylim = c(y_min, y_max),
     xlab = "Year (10-Year Horizon: 2016 - 2026)",
     ylab = "After-Tax Portfolio Value (Log Scale, Starting $10,000, 37% Tax)",
     main = "10-Year Heat Budget Comparison: 5% vs 8% vs 10% Max Risk (MAX_POS=3, CAP=3)",
     cex.main = 1.25, font.main = 2, col.main = "#1F2937",
     cex.lab = 1.05, col.lab = "#374151", las = 1)

grid(nx = NULL, ny = NULL, col = "#E5E7EB", lty = 1, lwd = 1.2)

# Benchmarks
lines(c_dates, spy_curve_10y, col = "#9CA3AF", lwd = 1.8, lty = 3)
lines(c_dates, qqq_curve_10y, col = "#4B5563", lwd = 2.0, lty = 2)

# Strategy curves
lines(c_dates, res_h10$curve_after_tax, col = "#D90429", lwd = 2.6, lty = 1)
lines(c_dates, res_h08$curve_after_tax, col = "#F77F00", lwd = 2.4, lty = 1)
lines(c_dates, res_h05$curve_after_tax, col = "#0077B6", lwd = 2.2, lty = 1)

par(xpd = TRUE)
labels <- c(
  sprintf("10%% Heat Budget: $%.0f (Sharpe: %.2f | DD: %.1f%%)", res_h10$after_tax_wealth, res_h10$sharpe, res_h10$max_dd),
  sprintf("8%% Heat Budget:  $%.0f (Sharpe: %.2f | DD: %.1f%%)", res_h08$after_tax_wealth, res_h08$sharpe, res_h08$max_dd),
  sprintf("5%% Heat Budget:  $%.0f (Sharpe: %.2f | DD: %.1f%%)", res_h05$after_tax_wealth, res_h05$sharpe, res_h05$max_dd),
  sprintf("QQQ Benchmark:  $%.0f (DD: 35.1%%)", tail(qqq_curve_10y, 1)),
  sprintf("SPY Benchmark:  $%.0f (DD: 23.9%%)", tail(spy_curve_10y, 1))
)
cols <- c("#D90429", "#F77F00", "#0077B6", "#4B5563", "#9CA3AF")
ltys <- c(1, 1, 1, 2, 3)
lwds <- c(2.6, 2.4, 2.2, 2.0, 1.8)

legend(max(c_dates) + 40, y_max,
       legend = labels, col = cols, lty = ltys, lwd = lwds,
       bty = "n", cex = 0.82, y.intersp = 1.35)
par(xpd = FALSE)
dev.off()
cat(sprintf("[Chart] Saved Heat Budget comparison chart to %s\n", chart_file))
