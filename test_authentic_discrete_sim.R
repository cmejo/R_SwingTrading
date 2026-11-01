suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(TTR)
  library(jsonlite)
})

source("R/08_metrics.R")

HIST_DIR <- "data/data_historical"

# Benchmarks
ndx_raw   <- readRDS(file.path(HIST_DIR, "data_^ndx.rds"))
qqq_raw   <- readRDS(file.path(HIST_DIR, "data_qqq.rds"))
spy_raw   <- readRDS(file.path(HIST_DIR, "data_spy.rds"))
gspc_raw  <- readRDS(file.path(HIST_DIR, "data_^gspc.rds"))
irx_raw   <- readRDS(file.path(HIST_DIR, "data_^irx.rds"))

ndx_cl <- Cl(ndx_raw); qqq_cl <- Cl(qqq_raw)
ndx_ret <- na.omit(ndx_cl / lag.xts(ndx_cl, 1) - 1) - (0.0020 / 252)
qqq_ret <- na.omit(qqq_cl / lag.xts(qqq_cl, 1) - 1)
qqq_spliced <- rbind(ndx_ret[index(ndx_ret) < index(qqq_ret)[1]], qqq_ret)

gspc_cl <- Cl(gspc_raw); spy_cl <- Cl(spy_raw)
gspc_ret <- na.omit(gspc_cl / lag.xts(gspc_cl, 1) - 1)
spy_ret  <- na.omit(spy_cl / lag.xts(spy_cl, 1) - 1)
spy_spliced <- rbind(gspc_ret[index(gspc_ret) < index(spy_ret)[1]], spy_ret)
spy_level   <- cumprod(1 + spy_spliced) * 100

rf_daily <- xts((as.numeric(na.locf(irx_raw[, 4])) / 100) / 252, order.by = index(irx_raw))

# Test on Epoch 4: 2012 to 2021
ep_start <- as.Date("2012-01-01")
ep_end   <- as.Date("2021-12-31")

common_dates <- sort(intersect(index(qqq_spliced), index(spy_spliced)))
eval_dates   <- common_dates[common_dates >= ep_start & common_dates <= ep_end]
n_eval       <- length(eval_dates)

qqq_ep <- as.numeric(qqq_spliced)[match(eval_dates, index(qqq_spliced))]
spy_ep <- as.numeric(spy_spliced)[match(eval_dates, index(spy_spliced))]
rf_ep  <- as.numeric(rf_daily)[match(eval_dates, index(rf_daily))]
rf_ep[is.na(rf_ep)] <- 0.02 / 252

spy_full_series <- as.numeric(spy_level)
spy_sma200_vals <- as.numeric(SMA(xts(spy_full_series, order.by = index(spy_level)), n = 200))
spy_all_ep_idx  <- match(eval_dates, index(spy_level))
spy_sma200_ep   <- spy_sma200_vals[spy_all_ep_idx]

# Load stocks
stock_files <- list.files(HIST_DIR, pattern = "^data_[a-z0-9_]+\\.rds$", full.names = TRUE)
stock_files <- stock_files[!grepl("data_(\\^|vti|qqq|spy|vfinx|gspc|ndx|irx)", stock_files)]

stocks <- list()
for (f in stock_files) {
  sym <- toupper(sub("^data_", "", sub("\\.rds$", "", basename(f))))
  d <- tryCatch(readRDS(f), error = function(e) NULL)
  if (is.null(d) || nrow(d) < 150) next
  df_sub <- tryCatch(na.locf(na.locf(d, na.rm = FALSE), fromLast = TRUE), error = function(e) NULL)
  if (is.null(df_sub) || nrow(df_sub) < 100) next
  
  all_idx <- index(df_sub)
  cl_xts  <- Cl(df_sub)
  spy_aligned <- spy_level[all_idx]
  rs_xts  <- (cl_xts / lag.xts(cl_xts, 63)) - (spy_aligned / lag.xts(spy_aligned, 63))
  mom_xts <- cl_xts / lag.xts(cl_xts, 20) - 1
  sma_xts <- SMA(cl_xts, n = 50)
  atr_res <- tryCatch(ATR(HLC(df_sub), n = 14)$atr, error = function(e) cl_xts * 0.02)
  atr_abs <- as.numeric(atr_res)
  atr_abs[is.na(atr_abs) | atr_abs <= 0] <- as.numeric(cl_xts)[is.na(atr_abs) | atr_abs <= 0] * 0.02
  
  m_idx <- match(eval_dates, all_idx)
  stocks[[sym]] <- list(
    cl    = as.numeric(cl_xts)[m_idx],
    op    = as.numeric(Op(df_sub))[m_idx],
    hi    = as.numeric(Hi(df_sub))[m_idx],
    lo    = as.numeric(Lo(df_sub))[m_idx],
    sma50 = as.numeric(sma_xts)[m_idx],
    rs    = as.numeric(rs_xts)[m_idx],
    mom   = as.numeric(mom_xts)[m_idx],
    atr   = atr_abs[m_idx],
    bars  = m_idx
  )
}

avail_syms <- names(stocks)
MAX_POS <- 3
open_positions <- list()
port_daily_ret <- numeric(n_eval)
total_trades_count <- 0
winning_trades_count <- 0

for (t in 2:n_eval) {
  macro_bull <- !is.na(spy_sma200_ep[t]) && (spy_full_series[spy_all_ep_idx[t]] >= spy_sma200_ep[t])
  margin_fee <- (0.070 / 252) * 0.5
  
  pos_daily_returns <- numeric()
  surviving_positions <- list()
  
  # 1. Update Existing Open Positions
  if (length(open_positions) > 0) {
    for (p in open_positions) {
      stk <- stocks[[p$symbol]]
      cur_o <- stk$op[t]; cur_h <- stk$hi[t]; cur_l <- stk$lo[t]; cur_c <- stk$cl[t]
      cur_atr <- stk$atr[t]
      if (is.na(cur_atr) || cur_atr <= 0) cur_atr <- cur_c * 0.02
      prev_p <- p$prev_p
      
      if (is.na(cur_c) || is.na(prev_p)) next
      
      days_h <- p$days_held + 1
      high_p <- max(p$high_p, cur_h)
      chand_stop <- high_p - 2.5 * cur_atr
      
      t1_hit <- p$t1_hit
      t2_hit <- p$t2_hit
      stop_p <- p$stop_p
      shares_pct <- p$shares_pct
      
      # Stop loss check
      if (cur_l <= stop_p) {
        exit_p <- min(cur_o, stop_p)
        r <- shares_pct * ((exit_p - prev_p) / prev_p) - 0.0010
        pos_daily_returns <- c(pos_daily_returns, r)
        total_trades_count <<- total_trades_count + 1
        if (exit_p > p$entry_p) winning_trades_count <<- winning_trades_count + 1
        next
      }
      
      # Target realization
      day_r <- 0
      if (!t1_hit && cur_h >= p$t1_p) {
        t1_hit <- TRUE
        # Sell 50% at t1_p
        realized_r1 <- 0.50 * ((p$t1_p - prev_p) / prev_p) - 0.0005
        day_r <- day_r + realized_r1
        shares_pct <- 0.50
        stop_p <- max(p$entry_p, chand_stop)
      }
      
      if (t1_hit && !t2_hit && cur_h >= p$t2_p) {
        t2_hit <- TRUE
        # Sell 25% at t2_p
        realized_r2 <- 0.25 * ((p$t2_p - prev_p) / prev_p) - 0.0005
        day_r <- day_r + realized_r2
        shares_pct <- 0.25
        stop_p <- max(stop_p, chand_stop)
      }
      
      if (t1_hit) stop_p <- max(stop_p, chand_stop)
      
      # Max holding period exit (7 days for non-runners)
      if (!t2_hit && days_h >= 7) {
        exit_r <- shares_pct * ((cur_c - prev_p) / prev_p) - 0.0010
        day_r <- day_r + exit_r
        pos_daily_returns <- c(pos_daily_returns, day_r)
        total_trades_count <<- total_trades_count + 1
        if (cur_c > p$entry_p) winning_trades_count <<- winning_trades_count + 1
        next
      }
      
      # Still holding
      day_r <- day_r + shares_pct * ((cur_c - prev_p) / prev_p)
      pos_daily_returns <- c(pos_daily_returns, day_r)
      
      p$prev_p     <- cur_c
      p$high_p     <- high_p
      p$stop_p     <- stop_p
      p$days_held  <- days_h
      p$t1_hit     <- t1_hit
      p$t2_hit     <- t2_hit
      p$shares_pct <- shares_pct
      surviving_positions[[length(surviving_positions) + 1]] <- p
    }
  }
  
  open_positions <- surviving_positions
  
  # 2. Open new positions if vacant slots exist
  empty_slots <- MAX_POS - length(open_positions)
  if (empty_slots > 0) {
    held_syms <- sapply(open_positions, function(x) x$symbol)
    cands <- c()
    ranks <- c()
    
    for (s in avail_syms) {
      if (s %in% held_syms) next
      stk <- stocks[[s]]
      bar_idx <- stk$bars[t - 1]
      if (is.na(bar_idx) || bar_idx < 500) next # Option A: >= 500 bars
      
      p_cl  <- stk$cl[t - 1]
      p_sma <- stk$sma50[t - 1]
      p_rs  <- stk$rs[t - 1]
      p_mom <- stk$mom[t - 1]
      
      if (!is.na(p_cl) && !is.na(p_sma) && p_cl > p_sma &&
          !is.na(p_rs) && p_rs > 0.02 &&
          !is.na(p_mom) && p_mom > 0.02) {
        cands <- c(cands, s)
        ranks <- c(ranks, p_rs + p_mom)
      }
    }
    
    if (length(cands) > 0) {
      sorted_cands <- cands[order(-ranks)]
      for (s in sorted_cands) {
        if (empty_slots <= 0) break
        stk <- stocks[[s]]
        cur_o <- stk$op[t]; cur_c <- stk$cl[t]; cur_atr <- stk$atr[t]
        if (is.na(cur_atr) || cur_atr <= 0) cur_atr <- cur_c * 0.02
        if (is.na(cur_o) || cur_o <= 0) cur_o <- cur_c
        if (is.na(cur_o) || is.na(cur_c)) next
        
        entry_p <- cur_o
        risk_1r <- 2.0 * cur_atr
        
        new_pos <- list(
          symbol     = s,
          entry_p    = entry_p,
          prev_p     = cur_c,
          high_p     = max(entry_p, stk$hi[t]),
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
        pos_daily_returns <- c(pos_daily_returns, r)
        open_positions[[length(open_positions) + 1]] <- new_pos
        empty_slots <- empty_slots - 1
      }
    }
  }
  
  # 3. Unallocated cash deployment via Option A
  active_n <- length(pos_daily_returns)
  remaining_empty <- MAX_POS - active_n
  
  stock_ret_comp <- if (active_n > 0) sum(pos_daily_returns) / MAX_POS else 0
  cash_park_r    <- if (macro_bull) qqq_ep[t] else rf_ep[t]
  unalloc_comp   <- (remaining_empty / MAX_POS) * cash_park_r
  
  port_daily_ret[t] <- (stock_ret_comp + unalloc_comp) * 1.5 - margin_fee
}

m <- calc_performance_metrics(xts(port_daily_ret, order.by = eval_dates))$raw
q <- calc_performance_metrics(xts(qqq_ep, order.by = eval_dates))$raw

cat(sprintf("Authentic Swing Model (Epoch 4): CAGR = %+.2f%%, MaxDD = -%.2f%%, Sharpe = %.2f\n", 
            m$ann_ret * 100, m$max_dd * 100, m$sharpe))
cat(sprintf("Total Trades: %d, Win Rate: %.1f%%\n", total_trades_count, (winning_trades_count / max(1, total_trades_count)) * 100))
cat(sprintf("QQQ Benchmark: CAGR = %+.2f%%, MaxDD = -%.2f%%, Sharpe = %.2f\n", 
            q$ann_ret * 100, q$max_dd * 100, q$sharpe))
