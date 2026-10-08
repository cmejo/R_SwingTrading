# Detailed Backtest with exact live daily_signal.R Heat Scaling logic across all 7 horizons
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

STARTING_CAPITAL <- 10000
LEVERAGE         <- 1.5
BORROWED_MARGIN  <- 0.5
MARGIN_RATE_ANN  <- 0.070
DAILY_MARGIN_FEE <- (MARGIN_RATE_ANN / 252) * BORROWED_MARGIN
TAX_RATE         <- 0.37

qqq_ohlcv <- readRDS(file.path(DATA_DIR_10Y, "data_qqq.rds"))
qqq_cl    <- Cl(qqq_ohlcv)
qqq_ret   <- na.omit(qqq_cl / lag.xts(qqq_cl, 1) - 1)

cache_files <- list.files(CACHE_DIR_10Y, pattern = "_10yr_wf\\.rds$", full.names = TRUE)
sim_data <- list()
for (cf in cache_files) {
  obj <- readRDS(cf)
  sim_data[[obj$symbol]] <- obj
}
avail_syms <- names(sim_data)

all_dates <- do.call(c, lapply(avail_syms, function(s) index(sim_data[[s]]$pred_prob)))
date_counts <- table(all_dates)
shared_dates <- as.Date(names(date_counts)[date_counts >= 30])
shared_dates <- sort(intersect(shared_dates, index(qqq_ret)))
max_avail_bars <- length(shared_dates)

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

SECTOR_MAP <- if (file.exists("sector_map.json")) jsonlite::fromJSON("sector_map.json") else list()
get_sym_sector <- function(s) {
  if (!is.null(SECTOR_MAP[[s]])) return(SECTOR_MAP[[s]])
  return("General_Tech")
}
sym_sectors <- sapply(avail_syms, get_sym_sector)
qqq_full <- as.numeric(qqq_ret)[match(shared_dates, index(qqq_ret))]
qqq_full[is.na(qqq_full)] <- 0

# True Heat Scaling Simulation
# In daily_signal.R, each position has a 2.0x ATR Stop.
# The distance to stop is approximately 2.0 * daily_vol = 2.0 * (garch_vol / sqrt(252)).
# If total risk of the positions > heat_budget, shares are scaled down by (heat_budget / total_risk).
sim_heat_exact <- function(h_bars, max_pos = 3, max_per_sector = 3, heat_budget = 0.05) {
  act_bars <- min(h_bars, max_avail_bars)
  start_idx <- max_avail_bars - act_bars + 1
  sub_dates <- shared_dates[start_idx:max_avail_bars]
  
  sub_prob  <- full_mat_prob[start_idx:max_avail_bars, , drop = FALSE]
  sub_rs    <- full_mat_rs[start_idx:max_avail_bars, , drop = FALSE]
  sub_vol   <- full_mat_vol[start_idx:max_avail_bars, , drop = FALSE]
  sub_ret_b <- full_mat_ret_b[start_idx:max_avail_bars, , drop = FALSE]
  sub_valid <- full_mat_valid[start_idx:max_avail_bars, , drop = FALSE]
  sub_qqq   <- qqq_full[start_idx:max_avail_bars]
  
  base_daily_ret <- numeric(act_bars)
  heat_scale_history <- numeric(act_bars)
  
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
      inv_vols <- 1 / vols[top_idx]
      weights  <- inv_vols / sum(inv_vols)
      
      # Stop distance = 2.0 * (ann_vol / sqrt(252))
      stop_dist_pcts <- 2.0 * (vols[top_idx] / sqrt(252))
      
      # Nominal position dollar allocation as fraction of equity
      # Full allocation across slots = LEVERAGE (1.5x)
      # Position i dollar fraction = LEVERAGE * weights[i]
      pos_fracs <- LEVERAGE * weights
      
      # Dollar risk fraction = sum(pos_fracs * stop_dist_pcts)
      total_proposed_risk <- sum(pos_fracs * stop_dist_pcts)
      
      # Heat Defense: If total proposed risk > heat_budget, scale positions down
      heat_scale <- if (total_proposed_risk > heat_budget && total_proposed_risk > 0) {
        heat_budget / total_proposed_risk
      } else {
        1.0
      }
      heat_scale_history[t] <- heat_scale
      
      # Scaled swing exposure
      scaled_weights <- weights * heat_scale
      # Invested swing fraction of total equity
      invested_equity_frac <- min(1.0, (n_sel / max_pos) * heat_scale)
      
      swing_ret <- sum(scaled_weights * rets_b[top_idx])
      cash_ret  <- sub_qqq[t]
      
      # Leveraged return directly
      daily_r <- (sum(pos_fracs * heat_scale * rets_b[top_idx])) + 
                 ((1 - min(1.0, sum(pos_fracs * heat_scale) / LEVERAGE)) * cash_ret) - 
                 DAILY_MARGIN_FEE
      base_daily_ret[t] <- daily_r
    } else {
      base_daily_ret[t] <- sub_qqq[t] * LEVERAGE - DAILY_MARGIN_FEE
      heat_scale_history[t] <- 1.0
    }
  }
  
  lev_daily_ret <- base_daily_ret
  pre_tax_wealth <- STARTING_CAPITAL * prod(1 + lev_daily_ret)
  
  # After-tax 37%
  years_vec <- as.numeric(format(sub_dates, "%Y"))
  cap_after_tax <- STARTING_CAPITAL
  loss_carryforward <- 0
  after_tax_curve <- numeric(act_bars)
  daily_after_tax_ret <- numeric(act_bars)
  
  idx_counter <- 1
  for (yr in unique(years_vec)) {
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
  
  list(
    pre_tax_wealth    = pre_tax_wealth,
    after_tax_wealth  = cap_after_tax,
    sharpe            = m_post$sharpe,
    max_dd            = m_post$max_dd * 100,
    ann_ret           = m_post$ann_ret * 100,
    avg_heat_scale    = mean(heat_scale_history),
    curve_after_tax   = after_tax_curve
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

cat("\n--- DETAILED HEAT COMPARISON (MAX_POS=3, CAP=3) ---\n")
heat_levels <- c(0.05, 0.08, 0.10)
heat_results <- list()

for (h_name in names(horizons)) {
  h_bars <- horizons[[h_name]]
  for (hl in heat_levels) {
    res <- sim_heat_exact(h_bars, max_pos = 3, max_per_sector = 3, heat_budget = hl)
    heat_results[[length(heat_results) + 1]] <- data.frame(
      Horizon         = h_name,
      Heat_Budget     = sprintf("%.0f%%", hl * 100),
      PreTax_Wealth   = sprintf("$%.2f", res$pre_tax_wealth),
      AfterTax_Wealth = sprintf("$%.2f", res$after_tax_wealth),
      Ann_Return      = sprintf("%+.2f%%", res$ann_ret),
      Sharpe          = sprintf("%.2f", res$sharpe),
      Max_DD          = sprintf("%.2f%%", res$max_dd),
      Avg_Heat_Scale  = sprintf("%.1f%%", res$avg_heat_scale * 100),
      stringsAsFactors = FALSE
    )
  }
}

df_hr <- do.call(rbind, heat_results)
print(df_hr, row.names = FALSE)
write.csv(df_hr, file.path(OUTPUT_DIR, "heat_budget_detailed_comparison.csv"), row.names = FALSE)
