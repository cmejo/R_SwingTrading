suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(TTR)
  library(jsonlite)
})

source("R/08_metrics.R")

HIST_DIR <- "data/data_historical"
STARTING_CAPITAL <- 8230.00
LEVERAGE <- 1.5
BORROWED_MARGIN <- 0.5
MAX_POSITIONS <- 3
SECTOR_CAP <- 2
MIN_HISTORY_BARS <- 500

# Benchmark
ndx_raw   <- readRDS(file.path(HIST_DIR, "data_^ndx.rds"))
qqq_raw   <- readRDS(file.path(HIST_DIR, "data_qqq.rds"))
vfinx_raw <- readRDS(file.path(HIST_DIR, "data_vfinx.rds"))
vti_raw   <- readRDS(file.path(HIST_DIR, "data_vti.rds"))
gspc_raw  <- readRDS(file.path(HIST_DIR, "data_^gspc.rds"))
spy_raw   <- readRDS(file.path(HIST_DIR, "data_spy.rds"))
irx_raw   <- readRDS(file.path(HIST_DIR, "data_^irx.rds"))

ndx_cl <- Cl(ndx_raw); qqq_cl <- Cl(qqq_raw)
ndx_ret <- na.omit(ndx_cl / lag.xts(ndx_cl, 1) - 1) - (0.0020 / 252)
qqq_ret <- na.omit(qqq_cl / lag.xts(qqq_cl, 1) - 1)
qqq_start <- index(qqq_ret)[1]
qqq_spliced <- rbind(ndx_ret[index(ndx_ret) < qqq_start], qqq_ret)

gspc_cl <- Cl(gspc_raw); spy_cl <- Cl(spy_raw)
gspc_ret <- na.omit(gspc_cl / lag.xts(gspc_cl, 1) - 1)
spy_ret  <- na.omit(spy_cl / lag.xts(spy_cl, 1) - 1)
spy_start <- index(spy_ret)[1]
spy_spliced <- rbind(gspc_ret[index(gspc_ret) < spy_start], spy_ret)
spy_level   <- cumprod(1 + spy_spliced) * 100

irx_yield <- na.locf(irx_raw[, 4], na.rm = FALSE)
irx_yield[is.na(irx_yield)] <- 2.0
rf_daily <- xts((as.numeric(irx_yield) / 100) / 252, order.by = index(irx_raw))

stock_files <- list.files(HIST_DIR, pattern = "^data_[a-z0-9_]+\\.rds$", full.names = TRUE)
stock_files <- stock_files[!grepl("data_(\\^|vti|qqq|spy|vfinx|gspc|ndx|irx)", stock_files)]

stocks_data <- list()
for (f in stock_files) {
  sym <- toupper(sub("^data_", "", sub("\\.rds$", "", basename(f))))
  d <- tryCatch(readRDS(f), error = function(e) NULL)
  if (!is.null(d) && nrow(d) >= 100) stocks_data[[sym]] <- d
}

# Test Epoch 4: 2012-01-01 to 2021-12-31
ep_start <- as.Date("2012-01-01")
ep_end   <- as.Date("2021-12-31")

bench_dates <- intersect(index(qqq_spliced), index(spy_spliced))
ep_dates    <- sort(bench_dates[bench_dates >= ep_start & bench_dates <= ep_end])
n_days      <- length(ep_dates)

qqq_ep <- as.numeric(qqq_spliced[match(ep_dates, index(qqq_spliced))])
spy_ep <- as.numeric(spy_spliced[match(ep_dates, index(spy_spliced))])
rf_ep  <- as.numeric(rf_daily[match(ep_dates, index(rf_daily))])
rf_ep[is.na(rf_ep)] <- 0.02 / 252

spy_full_series <- as.numeric(spy_level)
spy_sma200_vals <- as.numeric(SMA(xts(spy_full_series, order.by = index(spy_level)), n = 200))
spy_all_ep_idx  <- match(ep_dates, index(spy_level))
spy_sma200_ep   <- spy_sma200_vals[spy_all_ep_idx]

# Precompute indicators aligned to ep_dates for each stock
stock_syms <- names(stocks_data)
aligned_stocks <- list()

for (s in stock_syms) {
  df_raw <- stocks_data[[s]]
  df <- tryCatch(na.locf(na.locf(df_raw, na.rm = FALSE), fromLast = TRUE), error = function(e) NULL)
  if (is.null(df) || nrow(df) < 100) next
  c_idx <- intersect(index(df), ep_dates)
  if (length(c_idx) < 50) next
  
  all_idx   <- index(df)
  cl_xts    <- Cl(df)
  sma50_xts <- tryCatch(SMA(cl_xts, n = 50), error = function(e) rep(NA_real_, nrow(df)))
  atr_res   <- tryCatch(ATR(HLC(df), n = 14)$atr, error = function(e) cl_xts * 0.02)
  atr14_xts <- if (!is.null(atr_res)) atr_res else cl_xts * 0.02
  
  spy_sub      <- spy_level[all_idx]
  stock_rs_xts <- (cl_xts / lag.xts(cl_xts, 63)) - (spy_sub / lag.xts(spy_sub, 63))
  mom20_xts    <- cl_xts / lag.xts(cl_xts, 20) - 1
  
  match_d <- match(ep_dates, all_idx)
  
  aligned_stocks[[s]] <- list(
    cl    = as.numeric(cl_xts)[match_d],
    op    = as.numeric(Op(df))[match_d],
    hi    = as.numeric(Hi(df))[match_d],
    lo    = as.numeric(Lo(df))[match_d],
    sma50 = as.numeric(sma50_xts)[match_d],
    atr14 = as.numeric(atr14_xts)[match_d],
    rs    = as.numeric(stock_rs_xts)[match_d],
    mom20 = as.numeric(mom20_xts)[match_d],
    bar_num = match_d # bar index in full history
  )
}

avail_syms <- names(aligned_stocks)
cat(sprintf("Loaded %d aligned stocks for simulation.\n", length(avail_syms)))

# Run Discrete Event Simulation
open_positions <- list()
port_daily_ret <- numeric(n_days)
slot_w <- 1.0 / MAX_POSITIONS

for (d in 2:n_days) {
  macro_bull <- !is.na(spy_sma200_ep[d]) && (spy_full_series[spy_all_ep_idx[d]] >= spy_sma200_ep[d])
  margin_fee <- (0.070 / 252) * BORROWED_MARGIN
  
  pos_returns <- numeric()
  surviving_positions <- list()
  
  # 1. Update Existing Open Positions
  if (length(open_positions) > 0) {
    for (p in open_positions) {
      s <- p$symbol
      stk <- aligned_stocks[[s]]
      cur_o <- stk$op[d]; cur_h <- stk$hi[d]; cur_l <- stk$lo[d]; cur_c <- stk$cl[d]
      cur_atr <- stk$atr14[d]
      if (is.na(cur_atr) || cur_atr <= 0) cur_atr <- cur_c * 0.02
      
      prev_c <- p$prev_c
      if (is.na(cur_c) || is.na(prev_c)) next
      
      days_h <- p$days_held + 1
      high_p <- max(p$high_p, cur_h)
      chand_stop <- high_p - 2.5 * cur_atr
      
      t1_hit <- p$t1_hit
      t2_hit <- p$t2_hit
      stop_p <- p$stop_p
      shares_pct <- p$shares_pct
      
      # Check stop loss first
      if (cur_l <= stop_p) {
        exit_p <- min(cur_o, stop_p)
        r <- shares_pct * ((exit_p - prev_c) / prev_c) - 0.0010
        pos_returns <- c(pos_returns, r)
        next # fully exited
      }
      
      # Realized gain tracking for day d
      day_ret <- 0
      
      # Tier 1 (+1.5R) hit
      if (!t1_hit && cur_h >= p$t1_p) {
        t1_hit <- TRUE
        # Sell 50% at t1_p
        realized_r1 <- 0.50 * ((p$t1_p - prev_c) / prev_c) - 0.0005
        day_ret <- day_ret + realized_r1
        shares_pct <- 0.50
        stop_p <- max(p$entry_p, chand_stop)
      }
      
      # Tier 2 (+3.0R) hit
      if (t1_hit && !t2_hit && cur_h >= p$t2_p) {
        t2_hit <- TRUE
        # Sell 25% at t2_p
        realized_r2 <- 0.25 * ((p$t2_p - prev_c) / prev_c) - 0.0005
        day_ret <- day_ret + realized_r2
        shares_pct <- 0.25
        stop_p <- max(stop_p, chand_stop)
      }
      
      if (t1_hit) stop_p <- max(stop_p, chand_stop)
      
      # Time exit (7 trading days for non-runners)
      if (!t2_hit && days_h >= 7) {
        # Exit remaining shares at market close
        exit_r <- shares_pct * ((cur_c - prev_c) / prev_c) - 0.0010
        day_ret <- day_ret + exit_r
        pos_returns <- c(pos_returns, day_ret)
        next # fully exited
      }
      
      # Unrealized holding on remaining shares
      rem_r <- shares_pct * ((cur_c - prev_c) / prev_c)
      day_ret <- day_ret + rem_r
      pos_returns <- c(pos_returns, day_ret)
      
      p$prev_c <- cur_c
      p$high_p <- high_p
      p$stop_p <- stop_p
      p$days_held <- days_h
      p$t1_hit <- t1_hit
      p$t2_hit <- t2_hit
      p$shares_pct <- shares_pct
      surviving_positions[[length(surviving_positions) + 1]] <- p
    }
  }
  
  open_positions <- surviving_positions
  
  # 2. Open New Positions if Slots Available
  empty_slots <- MAX_POSITIONS - length(open_positions)
  if (empty_slots > 0) {
    held_syms <- sapply(open_positions, function(x) x$symbol)
    cands <- c()
    ranks <- c()
    
    for (s in avail_syms) {
      if (s %in% held_syms) next
      stk <- aligned_stocks[[s]]
      bar_idx <- stk$bar_num[d - 1]
      if (is.na(bar_idx) || bar_idx < MIN_HISTORY_BARS) next
      
      p_cl  <- stk$cl[d - 1]
      p_sma <- stk$sma50[d - 1]
      p_rs  <- stk$rs[d - 1]
      p_mom <- stk$mom20[d - 1]
      
      if (!is.na(p_cl) && !is.na(p_sma) && p_cl > p_sma &&
          !is.na(p_rs) && p_rs > 0.01 &&
          !is.na(p_mom) && p_mom > 0.02) {
        cands <- c(cands, s)
        ranks <- c(ranks, p_rs + p_mom)
      }
    }
    
    if (length(cands) > 0) {
      sorted_cands <- cands[order(-ranks)]
      for (s in sorted_cands) {
        if (empty_slots <= 0) break
        stk <- aligned_stocks[[s]]
        cur_o <- stk$op[d]
        cur_c <- stk$cl[d]
        cur_atr <- stk$atr14[d]
        if (is.na(cur_atr) || cur_atr <= 0) cur_atr <- cur_c * 0.02
        if (is.na(cur_o) || cur_o <= 0) cur_o <- cur_c
        if (is.na(cur_o) || is.na(cur_c)) next
        
        entry_p <- cur_o
        risk_1r <- 2.0 * cur_atr
        
        new_pos <- list(
          symbol     = s,
          entry_p    = entry_p,
          prev_c     = cur_c,
          high_p     = max(entry_p, stk$hi[d]),
          risk_1r    = risk_1r,
          stop_p     = entry_p - risk_1r,
          t1_p       = entry_p + 1.5 * risk_1r,
          t2_p       = entry_p + 3.0 * risk_1r,
          days_held  = 1,
          t1_hit     = FALSE,
          t2_hit     = FALSE,
          shares_pct = 1.0
        )
        
        # Day 1 return
        r <- ((cur_c - entry_p) / entry_p) - 0.0010
        pos_returns <- c(pos_returns, r)
        open_positions[[length(open_positions) + 1]] <- new_pos
        empty_slots <- empty_slots - 1
      }
    }
  }
  
  # 3. Unallocated Slots deployed via Option A
  active_n <- length(pos_returns)
  remaining_unallocated <- MAX_POSITIONS - active_n
  
  stock_ret_comp <- if (active_n > 0) sum(pos_returns) * slot_w else 0
  cash_park_r    <- if (macro_bull) qqq_ep[d] else rf_ep[d]
  unallocated_comp <- remaining_unallocated * slot_w * cash_park_r
  
  port_daily_ret[d] <- (stock_ret_comp + unallocated_comp) * LEVERAGE - margin_fee
}

# Performance
perf <- calc_performance_metrics(xts(port_daily_ret, order.by = ep_dates))$raw
qqq_perf <- calc_performance_metrics(xts(qqq_ep, order.by = ep_dates))$raw
cat(sprintf("Momentum Strategy: CAGR = %+.2f%%, MaxDD = -%.2f%%, Sharpe = %.2f\n", perf$ann_ret * 100, perf$max_dd * 100, perf$sharpe))
cat(sprintf("QQQ Benchmark:     CAGR = %+.2f%%, MaxDD = -%.2f%%, Sharpe = %.2f\n", qqq_perf$ann_ret * 100, qqq_perf$max_dd * 100, qqq_perf$sharpe))
