#!/usr/bin/env Rscript
# ==============================================================================
# 40-Year Full Continuous Backtest (1985 to 2026 / Present)
# Starting Capital: $8,230.00
# Constituent Lifespan: Option A (Dynamic activation at >= 500 bars; benchmark proxying)
# Strategy: Authentic Swing Momentum with 2-Tier ATR Brackets + 25% Runner Lot
# Sizing: Top-3 positions, Sector Cap = 2, 1.5x Leverage, 7.0% Margin Interest
# Benchmarks: 100% QQQ, 100% VTI, 70% VTI / 30% QQQ Blend
# Tax Regimes:
#   1. Gross Pre-Tax / Puerto Rico Act 60 (0.0%)
#   2. NYC TTS + S-Corp + Cash Balance Pension (28.4%)
#   3. NYC Individual Resident (51.2%)
# ==============================================================================

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(TTR)
  library(jsonlite)
})

source("R/08_metrics.R")

HIST_DIR   <- "data/data_historical"
OUTPUT_DIR <- "output"
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

STARTING_CAPITAL <- 8230.00
LEVERAGE         <- 1.5
BORROWED_MARGIN  <- LEVERAGE - 1.0  # 0.5x
MAX_POSITIONS    <- 3
SECTOR_CAP       <- 2
MIN_HISTORY_BARS <- 500

# Sector mapping
SECTOR_MAP <- if (file.exists("sector_map.json")) jsonlite::fromJSON("sector_map.json") else list()
get_sym_sector <- function(s) {
  if (!is.null(SECTOR_MAP[[s]])) return(SECTOR_MAP[[s]])
  if (s %in% c("NVDA", "AMD", "TSM", "AVGO", "QCOM", "MU", "AMAT", "LRCX", "ARM", "SOXL", "SMH", "INTC", "ADI", "MRVL")) return("Semiconductors")
  if (s %in% c("MSFT", "AAPL", "AMZN", "GOOGL", "META", "TSLA", "NFLX")) return("MegaCap_Tech")
  if (s %in% c("CRM", "NOW", "ADBE", "INTU", "ORCL", "SNOW", "WDAY", "SHOP", "PLTR")) return("Enterprise_Software")
  if (s %in% c("PANW", "CRWD", "FTNT", "NET", "ANET", "CSCO")) return("Cybersecurity")
  if (s %in% c("V", "MA", "PYPL", "SQ", "COIN", "MSTR", "WFC", "GS", "CFG")) return("Financials")
  if (s %in% c("CEG", "VST", "GE", "COP", "GLNG")) return("Energy_Industrials")
  return("General_Tech")
}

# 1. Load Benchmarks
cat("[40Yr-Engine] Loading and splicing benchmarks from 1985 to present...\n")
ndx_raw   <- readRDS(file.path(HIST_DIR, "data_^ndx.rds"))
qqq_raw   <- readRDS(file.path(HIST_DIR, "data_qqq.rds"))
vfinx_raw <- readRDS(file.path(HIST_DIR, "data_vfinx.rds"))
vti_raw   <- readRDS(file.path(HIST_DIR, "data_vti.rds"))
gspc_raw  <- readRDS(file.path(HIST_DIR, "data_^gspc.rds"))
spy_raw   <- readRDS(file.path(HIST_DIR, "data_spy.rds"))
irx_raw   <- readRDS(file.path(HIST_DIR, "data_^irx.rds"))

# Spliced QQQ: QQQ after 1999-03-10, ^NDX minus 0.20% expense ratio prior
ndx_cl <- Cl(ndx_raw); qqq_cl <- Cl(qqq_raw)
ndx_ret <- na.omit(ndx_cl / lag.xts(ndx_cl, 1) - 1) - (0.0020 / 252)
qqq_ret <- na.omit(qqq_cl / lag.xts(qqq_cl, 1) - 1)
qqq_start <- index(qqq_ret)[1]
qqq_spliced <- rbind(ndx_ret[index(ndx_ret) < qqq_start], qqq_ret)

# Spliced VTI: VTI after 2001-06-15, VFINX minus 0.03% expense ratio prior
vfinx_cl <- Cl(vfinx_raw); vti_cl <- Cl(vti_raw)
vfinx_ret <- na.omit(vfinx_cl / lag.xts(vfinx_cl, 1) - 1) - (0.0003 / 252)
vti_ret   <- na.omit(vti_cl / lag.xts(vti_cl, 1) - 1)
vti_start <- index(vti_ret)[1]
vti_spliced <- rbind(vfinx_ret[index(vfinx_ret) < vti_start], vti_ret)

# Spliced SPY for Relative Strength calculation (SPY after 1993-01-29, ^GSPC prior)
gspc_cl <- Cl(gspc_raw); spy_cl <- Cl(spy_raw)
gspc_ret <- na.omit(gspc_cl / lag.xts(gspc_cl, 1) - 1)
spy_ret  <- na.omit(spy_cl / lag.xts(spy_cl, 1) - 1)
spy_start <- index(spy_ret)[1]
spy_spliced <- rbind(gspc_ret[index(gspc_ret) < spy_start], spy_ret)
spy_level   <- cumprod(1 + spy_spliced) * 100

# Risk-free / Cash Park daily rate from ^IRX
irx_yield <- na.locf(irx_raw[, 4], na.rm = FALSE)
irx_yield[is.na(irx_yield)] <- 2.0
rf_daily <- xts((as.numeric(irx_yield) / 100) / 252, order.by = index(irx_raw))

# 2. Precompute authentic walkforward brackets across entire stock history
stock_files <- list.files(HIST_DIR, pattern = "^data_[a-z0-9_]+\\.rds$", full.names = TRUE)
stock_files <- stock_files[!grepl("data_(\\^|vti|qqq|spy|vfinx|gspc|ndx|irx)", stock_files)]

cat(sprintf("[40Yr-Engine] Precomputing authentic walk-forward brackets for %d stocks...\n", length(stock_files)))

precomputed_stocks <- list()
for (f in stock_files) {
  sym <- toupper(sub("^data_", "", sub("\\.rds$", "", basename(f))))
  d <- tryCatch(readRDS(f), error = function(e) NULL)
  if (is.null(d) || nrow(d) < 150) next
  
  df_sub <- tryCatch(na.locf(na.locf(d, na.rm = FALSE), fromLast = TRUE), error = function(e) NULL)
  if (is.null(df_sub) || nrow(df_sub) < 100) next
  
  all_idx <- index(df_sub)
  cl_xts  <- Cl(df_sub)
  op_xts  <- Op(df_sub)
  hi_xts  <- Hi(df_sub)
  lo_xts  <- Lo(df_sub)
  
  # 63-day RS vs SPY
  spy_aligned <- spy_level[all_idx]
  rs_xts <- (cl_xts / lag.xts(cl_xts, 63)) - (spy_aligned / lag.xts(spy_aligned, 63))
  
  # 20-day momentum
  mom20_xts <- cl_xts / lag.xts(cl_xts, 20) - 1
  
  # 50-day SMA
  sma50_xts <- tryCatch(SMA(cl_xts, n = 50), error = function(e) rep(NA_real_, nrow(df_sub)))
  
  # ATR and Volatility
  atr_res <- tryCatch(ATR(HLC(df_sub), n = 14)$atr, error = function(e) cl_xts * 0.02)
  atr_abs <- as.numeric(atr_res)
  c_sub   <- as.numeric(cl_xts)
  atr_abs[is.na(atr_abs) | atr_abs <= 0] <- c_sub[is.na(atr_abs) | atr_abs <= 0] * 0.02
  
  vol_ann <- (atr_abs / c_sub) * sqrt(252)
  vol_ann[is.na(vol_ann) | vol_ann < 0.10] <- 0.25
  
  # Multi-factor signal proxy: Uptrend (Cl > SMA50), RS > 0, Mom > 0
  p_cl  <- as.numeric(lag.xts(cl_xts, 1))
  p_sma <- as.numeric(lag.xts(sma50_xts, 1))
  p_rs  <- as.numeric(lag.xts(rs_xts, 1))
  p_mom <- as.numeric(lag.xts(mom20_xts, 1))
  
  sig_trigger <- ifelse(!is.na(p_cl) & !is.na(p_sma) & p_cl > p_sma & 
                        !is.na(p_rs) & p_rs > 0.01 & 
                        !is.na(p_mom) & p_mom > 0.01, 1, 0)
  sig_trigger[is.na(sig_trigger)] <- 0
  
  clean_rs  <- ifelse(is.na(p_rs), 0, p_rs)
  clean_mom <- ifelse(is.na(p_mom), 0, p_mom)
  prob_proxy <- 0.50 + 0.15 * pmin(1.0, pmax(0.0, clean_rs * 3)) + 0.10 * pmin(1.0, pmax(0.0, clean_mom * 2))
  prob_proxy[sig_trigger == 0] <- 0.50
  
  # Authentic Bracket Return Simulation
  nb <- length(c_sub)
  o_sub <- as.numeric(op_xts)
  h_sub <- as.numeric(hi_xts)
  l_sub <- as.numeric(lo_xts)
  
  bracket_ret <- numeric(nb)
  in_pos <- FALSE
  entry_p <- 0; stop_p <- 0; t1_p <- 0; t2_p <- 0; high_p <- 0; days_h <- 0
  t1_hit <- FALSE; t2_hit <- FALSE; pos_shares_pct <- 0
  
  for (i in 2:nb) {
    cur_sig <- sig_trigger[i]
    cur_o   <- o_sub[i]; cur_h <- h_sub[i]; cur_l <- l_sub[i]; cur_c <- c_sub[i]
    prev_c  <- c_sub[i - 1]; cur_atr <- atr_abs[i]
    
    # Entry
    if (!in_pos && cur_sig == 1) {
      in_pos  <- TRUE
      entry_p <- ifelse(!is.na(cur_o) && cur_o > 0, cur_o, cur_c)
      high_p  <- entry_p
      days_h  <- 0
      t1_hit  <- FALSE
      t2_hit  <- FALSE
      pos_shares_pct <- 1.0
      
      risk_1r <- 2.0 * cur_atr
      stop_p  <- entry_p - risk_1r
      t1_p    <- entry_p + 1.5 * risk_1r
      t2_p    <- entry_p + 3.0 * risk_1r
      
      bracket_ret[i] <- (cur_c - entry_p) / entry_p - (10 / 10000)
      next
    }
    
    if (in_pos) {
      days_h <- days_h + 1
      high_p <- max(high_p, cur_h)
      chand_stop <- high_p - 2.5 * cur_atr
      
      # Stop loss hit
      if (cur_l <= stop_p) {
        exit_p <- min(cur_o, stop_p)
        bar_r  <- (exit_p - prev_c) / prev_c
        bracket_ret[i] <- pos_shares_pct * bar_r - (10 / 10000)
        in_pos <- FALSE
        pos_shares_pct <- 0
        next
      }
      
      # Tier 1 (+1.5R): sell 50%
      if (!t1_hit && cur_h >= t1_p) {
        t1_hit <- TRUE
        pos_shares_pct <- 0.50
        stop_p <- max(entry_p, chand_stop)
      }
      
      # Tier 2 (+3.0R): sell 25%, 25% runner
      if (t1_hit && !t2_hit && cur_h >= t2_p) {
        t2_hit <- TRUE
        pos_shares_pct <- 0.25
        stop_p <- max(stop_p, chand_stop)
      }
      
      if (t1_hit) stop_p <- max(stop_p, chand_stop)
      
      # Max hold 7 days for non-runners
      if (!t2_hit && days_h >= 7) {
        bar_r <- (cur_c - prev_c) / prev_c
        bracket_ret[i] <- pos_shares_pct * bar_r - (10 / 10000)
        in_pos <- FALSE
        pos_shares_pct <- 0
        next
      }
      
      bar_r <- (cur_c - prev_c) / prev_c
      bracket_ret[i] <- pos_shares_pct * bar_r
    }
  }
  
  precomputed_stocks[[sym]] <- list(
    dates       = all_idx,
    prob        = prob_proxy,
    rs          = as.numeric(rs_xts),
    vol         = vol_ann,
    bracket_ret = bracket_ret,
    bar_num     = seq_len(nb),
    sector      = get_sym_sector(sym)
  )
}

cat(sprintf("[40Yr-Engine] Successfully indexed %d stocks.\n", length(precomputed_stocks)))

# 3. Align 40-Year Continuous Date Range (1985-10-01 to Present)
bench_dates <- intersect(index(qqq_spliced), index(vti_spliced))
bench_dates <- intersect(bench_dates, index(spy_spliced))
full_dates  <- sort(bench_dates)
n_total     <- length(full_dates)

start_date <- full_dates[1]
end_date   <- tail(full_dates, 1)

cat(sprintf("\n======================================================================\n"))
cat(sprintf("40-YEAR CONTINUOUS BACKTEST (%s to %s | %d Trading Days)\n", 
            as.character(start_date), as.character(end_date), n_total))
cat(sprintf("======================================================================\n"))

qqq_full  <- as.numeric(qqq_spliced[match(full_dates, index(qqq_spliced))])
vti_full  <- as.numeric(vti_spliced[match(full_dates, index(vti_spliced))])
spy_full  <- as.numeric(spy_spliced[match(full_dates, index(spy_spliced))])
rf_full   <- as.numeric(rf_daily[match(full_dates, index(rf_daily))])
rf_full[is.na(rf_full)] <- 0.02 / 252
b70_30_full <- 0.70 * vti_full + 0.30 * qqq_full

spy_full_series <- as.numeric(spy_level)
spy_sma200_vals <- as.numeric(SMA(xts(spy_full_series, order.by = index(spy_level)), n = 200))
spy_all_idx     <- match(full_dates, index(spy_level))
spy_sma200_full <- spy_sma200_vals[spy_all_idx]

# Pre-populate dense matrix across 40 years
avail_syms <- names(precomputed_stocks)
mat_prob   <- matrix(0.5, nrow = n_total, ncol = length(avail_syms))
mat_rs     <- matrix(0,   nrow = n_total, ncol = length(avail_syms))
mat_vol    <- matrix(0.25, nrow = n_total, ncol = length(avail_syms))
mat_ret_b  <- matrix(0,   nrow = n_total, ncol = length(avail_syms))
mat_valid  <- matrix(FALSE, nrow = n_total, ncol = length(avail_syms))
sym_sectors <- sapply(avail_syms, function(s) precomputed_stocks[[s]]$sector)

for (j in seq_along(avail_syms)) {
  s <- avail_syms[j]
  obj <- precomputed_stocks[[s]]
  m_idx <- match(full_dates, obj$dates)
  v <- !is.na(m_idx)
  if (any(v)) {
    v_ep_idx <- which(v)
    v_stk_idx <- m_idx[v]
    
    # Option A: bar_num >= 500
    is_eligible <- obj$bar_num[v_stk_idx] >= MIN_HISTORY_BARS
    mat_valid[v_ep_idx[is_eligible], j] <- TRUE
    
    mat_prob[v_ep_idx, j]  <- obj$prob[v_stk_idx]
    mat_rs[v_ep_idx, j]    <- obj$rs[v_stk_idx]
    mat_vol[v_ep_idx, j]   <- obj$vol[v_stk_idx]
    mat_ret_b[v_ep_idx, j] <- obj$bracket_ret[v_stk_idx]
  }
}

cat(sprintf("[Option A] Dynamic constituent tracking active across %d trading sessions.\n", n_total))

# 4. Continuous Portfolio Simulation
port_daily_ret <- numeric(n_total)
margin_fee <- (0.070 / 252) * BORROWED_MARGIN
slot_w <- 1.0 / MAX_POSITIONS

for (t in 1:n_total) {
  macro_bull <- !is.na(spy_sma200_full[t]) && (spy_full_series[spy_all_idx[t]] >= spy_sma200_full[t])
  
  p_ups     <- mat_prob[t, ]
  rs_vals   <- mat_rs[t, ]
  vols      <- mat_vol[t, ]
  rets_b    <- mat_ret_b[t, ]
  val_flg   <- mat_valid[t, ]
  
  # Candidate qualification
  qualify <- which(val_flg & p_ups >= 0.58 & rs_vals > 0)
  top_idx <- integer(0)
  
  if (length(qualify) > 0) {
    scores <- p_ups[qualify] * (1 + rs_vals[qualify])
    ranked <- qualify[order(-scores)]
    
    sec_counts <- list()
    for (cand in ranked) {
      c_sec <- sym_sectors[cand]
      cnt <- if (is.null(sec_counts[[c_sec]])) 0 else sec_counts[[c_sec]]
      if (cnt < SECTOR_CAP) {
        top_idx <- c(top_idx, cand)
        sec_counts[[c_sec]] <- cnt + 1
        if (length(top_idx) >= MAX_POSITIONS) break
      }
    }
  }
  
  n_sel <- length(top_idx)
  cash_park_r <- if (macro_bull) qqq_full[t] else rf_full[t]
  
  if (n_sel == 0) {
    port_daily_ret[t] <- cash_park_r * LEVERAGE - margin_fee
  } else {
    stock_alloc_total <- 0
    stock_pnl_total   <- 0
    
    for (idx in top_idx) {
      vol_scale <- pmin(1.5, 0.45 / vols[idx])
      w_i <- slot_w * vol_scale
      stock_alloc_total <- stock_alloc_total + slot_w
      stock_pnl_total   <- stock_pnl_total + (w_i * rets_b[idx])
    }
    
    unalloc_cash <- max(0, 1.0 - stock_alloc_total)
    cash_yield_pnl <- unalloc_cash * cash_park_r
    
    port_daily_ret[t] <- (stock_pnl_total + cash_yield_pnl) * LEVERAGE - margin_fee
  }
}

# 5. Tax Simulation Across 40 Years
calc_tax_series <- function(daily_ret_vec, dates_vec, tax_rate, starting_cap = STARTING_CAPITAL) {
  n <- length(daily_ret_vec)
  equity_curve <- numeric(n)
  years <- as.numeric(format(dates_vec, "%Y"))
  unique_years <- unique(years)
  
  cap_curr <- starting_cap
  loss_carryforward <- 0
  
  for (yr in unique_years) {
    yr_mask <- (years == yr)
    yr_indices <- which(yr_mask)
    if (length(yr_indices) == 0) next
    
    yr_cap_start <- cap_curr
    for (i in yr_indices) {
      cap_curr <- cap_curr * (1 + daily_ret_vec[i])
      equity_curve[i] <- cap_curr
    }
    yr_cap_end <- cap_curr
    net_gain <- yr_cap_end - yr_cap_start
    
    if (tax_rate > 0) {
      if (net_gain > 0) {
        taxable_gain <- max(0, net_gain - loss_carryforward)
        loss_carryforward <- max(0, loss_carryforward - net_gain)
        tax_due <- taxable_gain * tax_rate
        cap_curr <- cap_curr - tax_due
      } else {
        loss_carryforward <- loss_carryforward + abs(net_gain)
      }
    }
    equity_curve[tail(yr_indices, 1)] <- cap_curr
  }
  
  effective_ret <- numeric(n)
  effective_ret[1] <- (equity_curve[1] / starting_cap) - 1
  if (n > 1) {
    effective_ret[2:n] <- (equity_curve[2:n] / equity_curve[1:(n - 1)]) - 1
  }
  
  list(
    terminal_wealth = cap_curr,
    equity_curve    = equity_curve,
    effective_ret   = effective_ret
  )
}

calc_benchmark_tax <- function(bench_ret_vec, dates_vec, tax_rate_ltcg, div_yield_ann = 0.015, tax_rate_div = 0.341, starting_cap = STARTING_CAPITAL) {
  n <- length(bench_ret_vec)
  equity_curve <- numeric(n)
  cap_curr <- starting_cap
  daily_div_tax <- (div_yield_ann * tax_rate_div) / 252
  
  for (i in 1:n) {
    net_r <- bench_ret_vec[i] - daily_div_tax
    cap_curr <- cap_curr * (1 + net_r)
    equity_curve[i] <- cap_curr
  }
  
  if (tax_rate_ltcg > 0) {
    total_gain <- max(0, cap_curr - starting_cap)
    ltcg_tax <- total_gain * tax_rate_ltcg
    cap_curr <- cap_curr - ltcg_tax
    equity_curve[n] <- cap_curr
  }
  
  effective_ret <- numeric(n)
  effective_ret[1] <- (equity_curve[1] / starting_cap) - 1
  if (n > 1) {
    effective_ret[2:n] <- (equity_curve[2:n] / equity_curve[1:(n - 1)]) - 1
  }
  
  list(
    terminal_wealth = cap_curr,
    equity_curve    = equity_curve,
    effective_ret   = effective_ret
  )
}

cat("[Tax-Engine] Calculating 40-year multi-tax compounding...\n")

# Strategy Tax series
strat_gross <- calc_tax_series(port_daily_ret, full_dates, tax_rate = 0.0)
strat_tts   <- calc_tax_series(port_daily_ret, full_dates, tax_rate = 0.284)
strat_nyc   <- calc_tax_series(port_daily_ret, full_dates, tax_rate = 0.512)

# Benchmarks
qqq_gross <- calc_tax_series(qqq_full, full_dates, tax_rate = 0.0)
qqq_nyc   <- calc_benchmark_tax(qqq_full, full_dates, tax_rate_ltcg = 0.341, div_yield_ann = 0.007, tax_rate_div = 0.341)

vti_gross <- calc_tax_series(vti_full, full_dates, tax_rate = 0.0)
vti_nyc   <- calc_benchmark_tax(vti_full, full_dates, tax_rate_ltcg = 0.341, div_yield_ann = 0.015, tax_rate_div = 0.341)

b7030_gross <- calc_tax_series(b70_30_full, full_dates, tax_rate = 0.0)
b7030_nyc   <- calc_benchmark_tax(b70_30_full, full_dates, tax_rate_ltcg = 0.341, div_yield_ann = 0.0125, tax_rate_div = 0.341)

get_perf <- function(ret_vec, name) {
  m <- calc_performance_metrics(xts(ret_vec, order.by = full_dates), name = name)$raw
  list(
    cagr     = m$ann_ret * 100,
    cum_ret  = m$cum_ret * 100,
    max_dd   = m$max_dd * 100,
    sharpe   = m$sharpe,
    sortino  = m$sortino,
    ann_vol  = m$ann_vol * 100
  )
}

p_strat_gross <- get_perf(strat_gross$effective_ret, "Momentum Strategy (Gross)")
p_strat_tts   <- get_perf(strat_tts$effective_ret,   "Momentum Strategy (TTS)")
p_strat_nyc   <- get_perf(strat_nyc$effective_ret,   "Momentum Strategy (NYC)")

p_qqq_gross   <- get_perf(qqq_gross$effective_ret,   "QQQ Benchmark (Gross)")
p_qqq_nyc     <- get_perf(qqq_nyc$effective_ret,     "QQQ Benchmark (NYC)")

p_vti_gross   <- get_perf(vti_gross$effective_ret,   "VTI Benchmark (Gross)")
p_vti_nyc     <- get_perf(vti_nyc$effective_ret,     "VTI Benchmark (NYC)")

p_b7030_gross <- get_perf(b7030_gross$effective_ret, "70/30 VTI/QQQ (Gross)")
p_b7030_nyc   <- get_perf(b7030_nyc$effective_ret,   "70/30 VTI/QQQ (NYC)")

cat(sprintf("\n======================================================================\n"))
cat(sprintf("40-YEAR CONTINUOUS PERFORMANCE SUMMARY (1985 to 2026)\n"))
cat(sprintf("======================================================================\n"))
cat(sprintf("Strategy Gross (0.0%% / PR):   $%.2f (CAGR: %+.2f%% | MaxDD: -%.2f%% | Sharpe: %.2f)\n", 
            strat_gross$terminal_wealth, p_strat_gross$cagr, p_strat_gross$max_dd, p_strat_gross$sharpe))
cat(sprintf("Strategy NYC TTS+Pension:     $%.2f (CAGR: %+.2f%% | MaxDD: -%.2f%% | Sharpe: %.2f)\n", 
            strat_tts$terminal_wealth, p_strat_tts$cagr, p_strat_tts$max_dd, p_strat_tts$sharpe))
cat(sprintf("Strategy NYC Individual:      $%.2f (CAGR: %+.2f%% | MaxDD: -%.2f%% | Sharpe: %.2f)\n", 
            strat_nyc$terminal_wealth, p_strat_nyc$cagr, p_strat_nyc$max_dd, p_strat_nyc$sharpe))
cat(sprintf("QQQ Benchmark Gross:          $%.2f (CAGR: %+.2f%% | MaxDD: -%.2f%% | Sharpe: %.2f)\n", 
            qqq_gross$terminal_wealth, p_qqq_gross$cagr, p_qqq_gross$max_dd, p_qqq_gross$sharpe))
cat(sprintf("QQQ Benchmark NYC Net:        $%.2f (CAGR: %+.2f%%)\n", qqq_nyc$terminal_wealth, p_qqq_nyc$cagr))
cat(sprintf("VTI Benchmark Gross:          $%.2f (CAGR: %+.2f%% | MaxDD: -%.2f%% | Sharpe: %.2f)\n", 
            vti_gross$terminal_wealth, p_vti_gross$cagr, p_vti_gross$max_dd, p_vti_gross$sharpe))
cat(sprintf("VTI Benchmark NYC Net:        $%.2f (CAGR: %+.2f%%)\n", vti_nyc$terminal_wealth, p_vti_nyc$cagr))
cat(sprintf("70/30 VTI/QQQ Blend Gross:    $%.2f (CAGR: %+.2f%% | MaxDD: -%.2f%% | Sharpe: %.2f)\n", 
            b7030_gross$terminal_wealth, p_b7030_gross$cagr, p_b7030_gross$max_dd, p_b7030_gross$sharpe))
cat(sprintf("70/30 VTI/QQQ Blend NYC Net:  $%.2f (CAGR: %+.2f%%)\n", b7030_nyc$terminal_wealth, p_b7030_nyc$cagr))

# 6. Decade-by-Decade Breakdown
years <- as.numeric(format(full_dates, "%Y"))
decades <- list(
  "1985-1994 (Decade 1: Pre-Tech Dawn)"         = c(1985, 1994),
  "1995-2004 (Decade 2: Dot-Com Boom & Bust)"   = c(1995, 2004),
  "2005-2014 (Decade 3: GFC & Recovery)"        = c(2005, 2014),
  "2015-2024 (Decade 4: Tech Hegemony & COVID)" = c(2015, 2024),
  "2025-2026 (Recent 2 Years: Rate Shock)"      = c(2025, 2026)
)

decade_rows <- list()
for (d_name in names(decades)) {
  yr_rng <- decades[[d_name]]
  d_mask <- (years >= yr_rng[1] & years <= yr_rng[2])
  d_dates <- full_dates[d_mask]
  if (length(d_dates) < 5) next
  
  d_strat <- port_daily_ret[d_mask]
  d_qqq   <- qqq_full[d_mask]
  d_vti   <- vti_full[d_mask]
  
  m_s <- calc_performance_metrics(xts(d_strat, order.by = d_dates))$raw
  m_q <- calc_performance_metrics(xts(d_qqq,   order.by = d_dates))$raw
  m_v <- calc_performance_metrics(xts(d_vti,   order.by = d_dates))$raw
  
  decade_rows[[length(decade_rows) + 1]] <- data.frame(
    Period        = d_name,
    Strategy_CAGR = sprintf("%+.2f%%", m_s$ann_ret * 100),
    Strategy_DD   = sprintf("-%.2f%%", m_s$max_dd * 100),
    Strategy_SR   = sprintf("%.2f", m_s$sharpe),
    QQQ_CAGR      = sprintf("%+.2f%%", m_q$ann_ret * 100),
    QQQ_DD        = sprintf("-%.2f%%", m_q$max_dd * 100),
    QQQ_SR        = sprintf("%.2f", m_q$sharpe),
    VTI_CAGR      = sprintf("%+.2f%%", m_v$ann_ret * 100),
    VTI_DD        = sprintf("-%.2f%%", m_v$max_dd * 100),
    stringsAsFactors = FALSE
  )
}
decade_df <- do.call(rbind, decade_rows)

# 7. Save outputs
scorecard_40y <- data.frame(
  Metric = c("CAGR", "Cumulative Return", "Max Drawdown", "Sharpe Ratio", "Sortino Ratio", "Annual Volatility", "Terminal Wealth ($8,230 start)"),
  Strategy_Gross   = c(sprintf("%+.2f%%", p_strat_gross$cagr), sprintf("%+.2f%%", p_strat_gross$cum_ret), sprintf("-%.2f%%", p_strat_gross$max_dd), sprintf("%.2f", p_strat_gross$sharpe), sprintf("%.2f", p_strat_gross$sortino), sprintf("%.2f%%", p_strat_gross$ann_vol), sprintf("$%.2f", strat_gross$terminal_wealth)),
  Strategy_TTS     = c(sprintf("%+.2f%%", p_strat_tts$cagr), sprintf("%+.2f%%", p_strat_tts$cum_ret), sprintf("-%.2f%%", p_strat_tts$max_dd), sprintf("%.2f", p_strat_tts$sharpe), sprintf("%.2f", p_strat_tts$sortino), sprintf("%.2f%%", p_strat_tts$ann_vol), sprintf("$%.2f", strat_tts$terminal_wealth)),
  Strategy_NYC_Ind = c(sprintf("%+.2f%%", p_strat_nyc$cagr), sprintf("%+.2f%%", p_strat_nyc$cum_ret), sprintf("-%.2f%%", p_strat_nyc$max_dd), sprintf("%.2f", p_strat_nyc$sharpe), sprintf("%.2f", p_strat_nyc$sortino), sprintf("%.2f%%", p_strat_nyc$ann_vol), sprintf("$%.2f", strat_nyc$terminal_wealth)),
  QQQ_Gross        = c(sprintf("%+.2f%%", p_qqq_gross$cagr), sprintf("%+.2f%%", p_qqq_gross$cum_ret), sprintf("-%.2f%%", p_qqq_gross$max_dd), sprintf("%.2f", p_qqq_gross$sharpe), sprintf("%.2f", p_qqq_gross$sortino), sprintf("%.2f%%", p_qqq_gross$ann_vol), sprintf("$%.2f", qqq_gross$terminal_wealth)),
  QQQ_NYC_Net      = c(sprintf("%+.2f%%", p_qqq_nyc$cagr), "N/A", "N/A", "N/A", "N/A", "N/A", sprintf("$%.2f", qqq_nyc$terminal_wealth)),
  VTI_Gross        = c(sprintf("%+.2f%%", p_vti_gross$cagr), sprintf("%+.2f%%", p_vti_gross$cum_ret), sprintf("-%.2f%%", p_vti_gross$max_dd), sprintf("%.2f", p_vti_gross$sharpe), sprintf("%.2f", p_vti_gross$sortino), sprintf("%.2f%%", p_vti_gross$ann_vol), sprintf("$%.2f", vti_gross$terminal_wealth)),
  VTI_NYC_Net      = c(sprintf("%+.2f%%", p_vti_nyc$cagr), "N/A", "N/A", "N/A", "N/A", "N/A", sprintf("$%.2f", vti_nyc$terminal_wealth)),
  Blend7030_Gross  = c(sprintf("%+.2f%%", p_b7030_gross$cagr), sprintf("%+.2f%%", p_b7030_gross$cum_ret), sprintf("-%.2f%%", p_b7030_gross$max_dd), sprintf("%.2f", p_b7030_gross$sharpe), sprintf("%.2f", p_b7030_gross$sortino), sprintf("%.2f%%", p_b7030_gross$ann_vol), sprintf("$%.2f", b7030_gross$terminal_wealth)),
  Blend7030_NYC_Net= c(sprintf("%+.2f%%", p_b7030_nyc$cagr), "N/A", "N/A", "N/A", "N/A", "N/A", sprintf("$%.2f", b7030_nyc$terminal_wealth)),
  stringsAsFactors = FALSE
)

write.csv(scorecard_40y, file.path(OUTPUT_DIR, "full_40yr_continuous_scorecard.csv"), row.names = FALSE)
write.csv(decade_df, file.path(OUTPUT_DIR, "full_40yr_decade_breakdown.csv"), row.names = FALSE)

saveRDS(list(
  dates         = full_dates,
  port_ret      = port_daily_ret,
  strat_gross   = strat_gross,
  strat_tts     = strat_tts,
  strat_nyc     = strat_nyc,
  qqq_full      = qqq_full,
  vti_full      = vti_full,
  b7030_full    = b70_30_full,
  scorecard     = scorecard_40y,
  decade_df     = decade_df
), file.path(OUTPUT_DIR, "full_40yr_continuous_results.rds"))

cat(sprintf("\n[40Yr-Engine] Successfully saved 40-year scorecard to: %s\n", file.path(OUTPUT_DIR, "full_40yr_continuous_scorecard.csv")))
cat(sprintf("[40Yr-Engine] Successfully saved decade breakdown to: %s\n", file.path(OUTPUT_DIR, "full_40yr_decade_breakdown.csv")))
