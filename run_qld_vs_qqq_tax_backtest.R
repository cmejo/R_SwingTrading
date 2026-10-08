#!/usr/bin/env Rscript
# ==============================================================================
# Comprehensive Comparison: Excess Cash parked in QLD (2x QQQ) vs QQQ (1x)
# Initial Capital: $10,000
# Gross Leverage: 1.5x (with 7.0% APR margin financing cost on borrowed 0.5x)
# Short-Term Capital Gains Tax Rate: 37% (annual tax accounting with loss carryforward)
# Max Positions: 2, 3, 4, 5
# Sector Caps: 2, 3, 4, 5 (cap <= max_pos)
# Sizing: Vince (primary) + Equal comparison
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

# 1. Load Benchmarks and Cash Asset Data
qqq_ohlcv <- readRDS(file.path(DATA_DIR_10Y, "data_qqq.rds"))
spy_ohlcv <- readRDS(file.path(DATA_DIR_10Y, "data_spy.rds"))
qld_ohlcv <- readRDS(file.path(DATA_DIR_10Y, "data_qld.rds"))

qqq_cl <- Cl(qqq_ohlcv)
spy_cl <- Cl(spy_ohlcv)
qld_cl <- Cl(qld_ohlcv)

qqq_ret <- na.omit(qqq_cl / lag.xts(qqq_cl, 1) - 1)
spy_ret <- na.omit(spy_cl / lag.xts(spy_cl, 1) - 1)
qld_ret <- na.omit(qld_cl / lag.xts(qld_cl, 1) - 1)

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
shared_dates <- sort(intersect(shared_dates, intersect(index(qqq_ret), intersect(index(spy_ret), index(qld_ret)))))
max_avail_bars <- length(shared_dates)

cat(sprintf("[Setup] Shared dates across universe, QQQ, SPY, and QLD: %d trading sessions (%s to %s)\n",
            max_avail_bars, as.character(shared_dates[1]), as.character(tail(shared_dates, 1))))

# 5. Precompute dense matrices
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
qld_full <- as.numeric(qld_ret)[match(shared_dates, index(qld_ret))]
spy_full <- as.numeric(spy_ret)[match(shared_dates, index(spy_ret))]
qqq_full[is.na(qqq_full)] <- 0
qld_full[is.na(qld_full)] <- 0
spy_full[is.na(spy_full)] <- 0

# Core simulation function with cash_asset ("QQQ" vs "QLD")
simulate_config <- function(h_bars, max_pos, max_per_sector, sizing_mode = "vince", cash_asset = "QQQ") {
  act_bars <- min(h_bars, max_avail_bars)
  start_idx <- max_avail_bars - act_bars + 1
  sub_dates <- shared_dates[start_idx:max_avail_bars]
  
  sub_prob  <- full_mat_prob[start_idx:max_avail_bars, , drop = FALSE]
  sub_rs    <- full_mat_rs[start_idx:max_avail_bars, , drop = FALSE]
  sub_vol   <- full_mat_vol[start_idx:max_avail_bars, , drop = FALSE]
  sub_ret_b <- full_mat_ret_b[start_idx:max_avail_bars, , drop = FALSE]
  sub_valid <- full_mat_valid[start_idx:max_avail_bars, , drop = FALSE]
  
  sub_cash_ret <- if (cash_asset == "QLD") qld_full[start_idx:max_avail_bars] else qqq_full[start_idx:max_avail_bars]
  
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
      cash_ret  <- sub_cash_ret[t]
      base_daily_ret[t] <- swing_exposure * swing_ret + (1 - swing_exposure) * cash_ret
    } else {
      base_daily_ret[t] <- sub_cash_ret[t]
    }
  }
  
  # 1.5x Margin financing
  lev_daily_ret <- base_daily_ret * LEVERAGE - DAILY_MARGIN_FEE
  pre_tax_wealth <- STARTING_CAPITAL * prod(1 + lev_daily_ret)
  
  # 37% Short-Term Capital Gains Tax Accounting (Annual Settlement with Loss Carryforward)
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

combos <- list(
  list(mp = 2, cap = 2, sm = "vince"),
  list(mp = 3, cap = 2, sm = "vince"),
  list(mp = 3, cap = 3, sm = "vince"),
  list(mp = 4, cap = 2, sm = "vince"),
  list(mp = 4, cap = 3, sm = "vince"),
  list(mp = 4, cap = 4, sm = "vince"),
  list(mp = 5, cap = 2, sm = "vince"),
  list(mp = 5, cap = 3, sm = "vince"),
  list(mp = 5, cap = 4, sm = "vince"),
  list(mp = 5, cap = 5, sm = "vince")
)

all_comp_results <- list()

cat("\n========================================================================================\n")
cat(" RUNNING FULL MATRIX: EXCESS CASH IN QLD (2x QQQ) VS QQQ (1x) (1.5x LEV & 37% TAX)      \n")
cat("========================================================================================\n")

for (h_name in names(horizons)) {
  h_bars <- horizons[[h_name]]
  cat(sprintf("\n>>> Simulating Horizon: %s (%d trading days)...\n", h_name, h_bars))
  for (cb in combos) {
    mp  <- cb$mp
    cap <- cb$cap
    sm  <- cb$sm
    
    # Run with QQQ
    res_qqq <- simulate_config(h_bars, mp, cap, sm, cash_asset = "QQQ")
    # Run with QLD
    res_qld <- simulate_config(h_bars, mp, cap, sm, cash_asset = "QLD")
    
    cfg_name <- sprintf("MAX_POS=%d, CAP=%d", mp, cap)
    
    all_comp_results[[length(all_comp_results) + 1]] <- data.frame(
      Horizon              = h_name,
      Trading_Days         = h_bars,
      Configuration        = cfg_name,
      Max_Positions        = mp,
      Sector_Cap           = cap,
      Sizing_Mode          = sm,
      # QQQ Cash Metrics
      QQQ_PreTax_Wealth    = sprintf("$%.2f", res_qqq$pre_tax_wealth),
      QQQ_AfterTax_Wealth  = sprintf("$%.2f", res_qqq$after_tax_wealth),
      QQQ_AfterTax_AnnRet  = sprintf("%+.2f%%", res_qqq$after_tax_ann_ret),
      QQQ_Sharpe           = sprintf("%.2f", res_qqq$sharpe),
      QQQ_MaxDD            = sprintf("%.2f%%", res_qqq$max_dd),
      # QLD Cash Metrics
      QLD_PreTax_Wealth    = sprintf("$%.2f", res_qld$pre_tax_wealth),
      QLD_AfterTax_Wealth  = sprintf("$%.2f", res_qld$after_tax_wealth),
      QLD_AfterTax_AnnRet  = sprintf("%+.2f%%", res_qld$after_tax_ann_ret),
      QLD_Sharpe           = sprintf("%.2f", res_qld$sharpe),
      QLD_MaxDD            = sprintf("%.2f%%", res_qld$max_dd),
      # Numeric Deltas
      Delta_Wealth_Num     = res_qld$after_tax_wealth - res_qqq$after_tax_wealth,
      Delta_Sharpe         = res_qld$sharpe - res_qqq$sharpe,
      Delta_MaxDD          = res_qld$max_dd - res_qqq$max_dd,
      QQQ_Wealth_Num       = res_qqq$after_tax_wealth,
      QLD_Wealth_Num       = res_qld$after_tax_wealth,
      stringsAsFactors     = FALSE
    )
  }
}

df_comp <- do.call(rbind, all_comp_results)
csv_out <- file.path(OUTPUT_DIR, "qld_vs_qqq_cash_backtest_scorecard.csv")
write.csv(df_comp, csv_out, row.names = FALSE)
cat(sprintf("\n[Complete] Scorecard saved to %s (%d rows)\n", csv_out, nrow(df_comp)))

# Generate High-Res 10-Year Chart comparing QLD vs QQQ cash for MAX_POS=3, CAP=3 and MAX_POS=4, CAP=4
chart_file <- file.path(OUTPUT_DIR, "qld_vs_qqq_cash_10year_chart.png")
png(chart_file, width = 1750, height = 980, res = 130)
par(mar = c(5, 5, 4, 24), bg = "#F8F9FA")

c_dates <- tail(shared_dates, 2520)
res_qqq_p3 <- simulate_config(2520, 3, 3, "vince", "QQQ")
res_qld_p3 <- simulate_config(2520, 3, 3, "vince", "QLD")
res_qqq_p4 <- simulate_config(2520, 4, 4, "vince", "QQQ")
res_qld_p4 <- simulate_config(2520, 4, 4, "vince", "QLD")
res_qqq_p5 <- simulate_config(2520, 5, 5, "vince", "QQQ")
res_qld_p5 <- simulate_config(2520, 5, 5, "vince", "QLD")

qqq_curve_10y <- cumprod(1 + tail(qqq_full, 2520)) * 10000
qld_curve_10y <- cumprod(1 + tail(qld_full, 2520)) * 10000
spy_curve_10y <- cumprod(1 + tail(spy_full, 2520)) * 10000

y_min <- 5000
y_max <- max(res_qld_p3$curve_after_tax, res_qqq_p3$curve_after_tax) * 1.3

plot(c_dates, qqq_curve_10y, type = "n", log = "y", ylim = c(y_min, y_max),
     xlab = "Year (10-Year Horizon: 2016 - 2026)",
     ylab = "After-Tax Portfolio Value (Log Scale, Starting $10,000, 37% Tax)",
     main = "10-Year Backtest: Excess Cash in 2x QLD vs 1x QQQ (1.5x Margin & 37% Tax)",
     cex.main = 1.25, font.main = 2, col.main = "#1F2937",
     cex.lab = 1.05, col.lab = "#374151", las = 1)

grid(nx = NULL, ny = NULL, col = "#E5E7EB", lty = 1, lwd = 1.2)

# Benchmarks
lines(c_dates, spy_curve_10y, col = "#9CA3AF", lwd = 1.8, lty = 3)
lines(c_dates, qqq_curve_10y, col = "#6B7280", lwd = 2.0, lty = 2)
lines(c_dates, qld_curve_10y, col = "#4B5563", lwd = 2.0, lty = 4)

# Strategy lines
lines(c_dates, res_qld_p3$curve_after_tax, col = "#D90429", lwd = 2.6, lty = 1)
lines(c_dates, res_qqq_p3$curve_after_tax, col = "#F77F00", lwd = 2.2, lty = 2)

lines(c_dates, res_qld_p4$curve_after_tax, col = "#0077B6", lwd = 2.4, lty = 1)
lines(c_dates, res_qqq_p4$curve_after_tax, col = "#2A9D8F", lwd = 2.0, lty = 2)

lines(c_dates, res_qld_p5$curve_after_tax, col = "#7209B7", lwd = 2.2, lty = 1)
lines(c_dates, res_qqq_p5$curve_after_tax, col = "#B5179E", lwd = 1.8, lty = 2)

par(xpd = TRUE)
labels <- c(
  sprintf("MAX_POS=3 (QLD Cash): $%.0f (DD: %.1f%%)", res_qld_p3$after_tax_wealth, res_qld_p3$max_dd),
  sprintf("MAX_POS=3 (QQQ Cash): $%.0f (DD: %.1f%%)", res_qqq_p3$after_tax_wealth, res_qqq_p3$max_dd),
  sprintf("MAX_POS=4 (QLD Cash): $%.0f (DD: %.1f%%)", res_qld_p4$after_tax_wealth, res_qld_p4$max_dd),
  sprintf("MAX_POS=4 (QQQ Cash): $%.0f (DD: %.1f%%)", res_qqq_p4$after_tax_wealth, res_qqq_p4$max_dd),
  sprintf("MAX_POS=5 (QLD Cash): $%.0f (DD: %.1f%%)", res_qld_p5$after_tax_wealth, res_qld_p5$max_dd),
  sprintf("MAX_POS=5 (QQQ Cash): $%.0f (DD: %.1f%%)", res_qqq_p5$after_tax_wealth, res_qqq_p5$max_dd),
  sprintf("QLD 2x Buy & Hold: $%.0f (DD: 63.5%%)", tail(qld_curve_10y, 1)),
  sprintf("QQQ 1x Buy & Hold: $%.0f (DD: 35.1%%)", tail(qqq_curve_10y, 1)),
  sprintf("SPY 1x Buy & Hold: $%.0f (DD: 23.9%%)", tail(spy_curve_10y, 1))
)
cols <- c("#D90429", "#F77F00", "#0077B6", "#2A9D8F", "#7209B7", "#B5179E", "#4B5563", "#6B7280", "#9CA3AF")
ltys <- c(1, 2, 1, 2, 1, 2, 4, 2, 3)
lwds <- c(2.6, 2.2, 2.4, 2.0, 2.2, 1.8, 2.0, 2.0, 1.8)

legend(max(c_dates) + 40, y_max,
       legend = labels, col = cols, lty = ltys, lwd = lwds,
       bty = "n", cex = 0.80, y.intersp = 1.3)
par(xpd = FALSE)
dev.off()
cat(sprintf("[Chart] Saved QLD vs QQQ chart to %s\n", chart_file))
