#!/usr/bin/env Rscript
# ==============================================================================
# scratch/evaluate_30_70_triple_engine_trades.R
# Detailed Empirical Trade Frequency & Backtest for 30/70 Triple-Engine Model
# ==============================================================================

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
})

HIST <- "data/data_historical"
EVAL_START <- as.Date("2006-10-02")
EVAL_END   <- as.Date("2026-10-06")
COST       <- 0.0010
MARGIN     <- 0.070

adj <- function(sym) {
  p <- file.path(HIST, sprintf("data_%s.rds", tolower(sym)))
  if (!file.exists(p)) return(NULL)
  d <- readRDS(p)
  a <- d[, grep("Adjusted$", colnames(d))]
  a[a <= 0] <- NA
  a
}

qqq_p <- adj("QQQ")
cal   <- index(qqq_p)[index(qqq_p) >= as.Date("2004-01-01") & index(qqq_p) <= EVAL_END]

EXCLUDE <- c("QQQ", "QLD", "TQQQ", "SQQQ", "SPY", "VTI", "VFINX", "SMH", "SOXL", "XLK", "GLD_FULL",
             "AMUU", "NVDU", "PLTU", "SNDQ", "SMHC", "SKHY", "^VIX", "^IRX")
files   <- list.files(HIST, pattern = "^data_[a-z0-9_]+[.]rds$")
syms    <- setdiff(toupper(sub("^data_", "", sub("[.]rds$", "", files))), EXCLUDE)

P <- matrix(NA_real_, length(cal), length(syms), dimnames = list(NULL, syms))
for (s in syms) {
  a <- tryCatch(adj(s), error = function(e) NULL)
  if (is.null(a)) next
  m <- match(cal, as.Date(index(a)))
  P[, s] <- as.numeric(a)[m]
}
P <- P[, colSums(!is.na(P)) > 300, drop = FALSE]

for (j in seq_len(ncol(P))) {
  v <- P[, j]
  f <- which(!is.na(v))[1]
  if (!is.na(f)) v[f:length(v)] <- zoo::na.locf(v[f:length(v)], na.rm = FALSE)
  P[, j] <- v
}

q  <- as.numeric(qqq_p[cal])
R  <- rbind(NA, P[-1, , drop = FALSE] / P[-nrow(P), , drop = FALSE] - 1); R[is.na(R)] <- 0
rq <- c(0, q[-1] / q[-length(q)] - 1)

irx <- readRDS(file.path(HIST, "data_^irx.rds"))
rf  <- as.numeric(zoo::na.locf(irx[, 4], na.rm = FALSE)[cal])
rf  <- zoo::na.locf(rf, na.rm = FALSE)
rf[is.na(rf)] <- 2
rf  <- rf / 100 / 252

roll_vol_20  <- rollapply(rq, width = 20, FUN = function(x) sd(x) * sqrt(252), align = "right", fill = 0.20)
sma200_q     <- as.numeric(stats::filter(q, rep(1 / 200, 200), sides = 1))
qqq_trend_up <- q >= sma200_q; qqq_trend_up[is.na(qqq_trend_up)] <- TRUE

ev <- which(cal >= EVAL_START)
dates <- cal[ev]
month_end <- c(format(cal[-1], "%m") != format(cal[-length(cal)], "%m"), TRUE)

# Run discrete trade tracking simulation
sim_trade_tracking <- function(core_pct = 0.30, top_n = 5, vol_target = 0.16,
                               core_bull_leverage = 1.25, use_core_trend = TRUE,
                               disaster_stop = 0.20) {
  sat_pct <- 1 - core_pct
  
  eq <- 1.0
  peak_eq <- 1.0
  curr_core_w  <- 0.0
  curr_sat_w   <- rep(0.0, ncol(P)); names(curr_sat_w) <- colnames(P)
  entry_prices <- rep(NA_real_, ncol(P)); names(entry_prices) <- colnames(P)
  entry_dates  <- rep(as.Date(NA), ncol(P)); names(entry_dates) <- colnames(P)
  stopped_out  <- rep(FALSE, ncol(P)); names(stopped_out) <- colnames(P)
  
  # Tracking trades
  trade_log <- list()
  monthly_trades <- rep(0, length(unique(format(dates, "%Y-%m"))))
  names(monthly_trades) <- unique(format(dates, "%Y-%m"))
  
  monthly_turnover <- rep(0, length(monthly_trades))
  names(monthly_turnover) <- names(monthly_trades)
  
  holding_days_list <- numeric()
  
  prev_eq <- eq
  
  for (k in seq_along(ev)) {
    t <- ev[k]
    cur_date <- dates[k]
    ym <- format(cur_date, "%Y-%m")
    
    r_qqq <- rq[t]
    r_sat <- R[t, ]
    
    tot_sat_val  <- sum(curr_sat_w * (1 + r_sat))
    tot_core_val <- curr_core_w * (1 + r_qqq)
    curr_cash_w  <- 1 - (curr_core_w + sum(curr_sat_w))
    
    cash_gain <- if (curr_cash_w >= 0) curr_cash_w * rf[t] else curr_cash_w * MARGIN / 252
    day_gain  <- (curr_core_w * r_qqq) + sum(curr_sat_w * r_sat) + cash_gain
    
    eq <- eq * (1 + day_gain)
    if (eq > peak_eq) peak_eq <- eq
    
    curr_core_w <- tot_core_val / (1 + day_gain)
    curr_sat_w  <- (curr_sat_w * (1 + r_sat)) / (1 + day_gain)
    
    # Check disaster stops intramonth
    active_sats <- names(curr_sat_w)[curr_sat_w > 0]
    for (sym in active_sats) {
      px_now <- P[t, sym]
      if (!is.na(entry_prices[sym]) && entry_prices[sym] > 0) {
        if ((px_now / entry_prices[sym] - 1) <= -disaster_stop) {
          # Liquidate stop
          to <- curr_sat_w[sym]
          eq <- eq * (1 - COST * to)
          curr_sat_w[sym] <- 0
          stopped_out[sym] <- TRUE
          
          # Log stop trade
          h_days <- as.numeric(cur_date - entry_dates[sym])
          holding_days_list <- c(holding_days_list, h_days)
          trade_log[[length(trade_log) + 1]] <- list(
            date = cur_date, ym = ym, type = "DISASTER_STOP", symbol = sym,
            holding_days = h_days, pnl_pct = (px_now / entry_prices[sym] - 1)
          )
          monthly_trades[ym] <- monthly_trades[ym] + 1
          monthly_turnover[ym] <- monthly_turnover[ym] + to
        }
      }
    }
    
    # Monthly Rebalance Check
    s <- t - 1
    if (month_end[s] || k == 1) {
      realized_v <- roll_vol_20[s]
      scale_mult <- min(1.0, vol_target / max(0.05, realized_v))
      
      # Core trend & leverage rule
      core_active <- TRUE
      if (use_core_trend && !isTRUE(qqq_trend_up[s])) {
        core_active <- FALSE
      }
      
      core_lev <- 1.0
      if (core_active && core_bull_leverage > 1.0 && isTRUE(qqq_trend_up[s]) && realized_v <= 0.16) {
        core_lev <- core_bull_leverage
      }
      
      tgt_core_w <- if (core_active) core_pct * scale_mult * core_lev else 0.0
      
      # Satellite Selection: Top-N RS
      lb_252 <- pmax(1, s - 252); lb_126 <- pmax(1, s - 126)
      ok <- colnames(P)[!is.na(P[s, ]) & !is.na(P[lb_252, ]) & (s >= 252)]
      
      tgt_sat_w <- rep(0.0, ncol(P)); names(tgt_sat_w) <- colnames(P)
      if (length(ok) > 0) {
        ret_stock <- P[s, ok] / P[lb_126, ok] - 1
        ret_qqq   <- q[s] / q[lb_126] - 1
        rs        <- ret_stock - ret_qqq
        
        # 200 SMA trend filter on stocks
        sma200_stocks <- sapply(ok, function(sym) mean(P[(s - 199):s, sym], na.rm = TRUE))
        pass_trend <- P[s, ok] >= sma200_stocks
        ok_filtered <- ok[pass_trend]
        if (length(ok_filtered) >= top_n) {
          ok <- ok_filtered; rs <- rs[ok]
        }
        
        top_syms <- names(sort(rs, decreasing = TRUE))[1:min(top_n, length(rs))]
        if (length(top_syms) > 0) {
          tgt_sat_w[top_syms] <- (sat_pct * scale_mult) / length(top_syms)
        }
        
        # Determine rotations: exits and entries
        curr_active_syms <- names(curr_sat_w)[curr_sat_w > 0.001]
        exits <- setdiff(curr_active_syms, top_syms)
        entries <- setdiff(top_syms, curr_active_syms)
        retains <- intersect(curr_active_syms, top_syms)
        
        # Log exit trades
        for (ex in exits) {
          h_days <- as.numeric(cur_date - entry_dates[ex])
          holding_days_list <- c(holding_days_list, h_days)
          trade_log[[length(trade_log) + 1]] <- list(
            date = cur_date, ym = ym, type = "ROTATION_EXIT", symbol = ex,
            holding_days = h_days, pnl_pct = (P[t, ex] / entry_prices[ex] - 1)
          )
          monthly_trades[ym] <- monthly_trades[ym] + 1
        }
        
        # Log entry trades
        for (en in entries) {
          entry_prices[en] <- P[t, en]
          entry_dates[en]  <- cur_date
          stopped_out[en]  <- FALSE
          trade_log[[length(trade_log) + 1]] <- list(
            date = cur_date, ym = ym, type = "ROTATION_ENTRY", symbol = en,
            holding_days = 0, pnl_pct = 0
          )
          monthly_trades[ym] <- monthly_trades[ym] + 1
        }
        
        # For retained stocks, check if rebalancing occurred
        for (rt in retains) {
          drift_w <- abs(tgt_sat_w[rt] - curr_sat_w[rt])
          if (drift_w > 0.02) { # Only count as trade if rebalance drift > 2%
            trade_log[[length(trade_log) + 1]] <- list(
              date = cur_date, ym = ym, type = "REBALANCE_ADJUST", symbol = rt,
              holding_days = as.numeric(cur_date - entry_dates[rt]), pnl_pct = 0
            )
            monthly_trades[ym] <- monthly_trades[ym] + 1
          }
        }
      }
      
      # Core adjustment
      core_drift <- abs(tgt_core_w - curr_core_w)
      if (core_drift > 0.03) { # Core rebalance trade ticket
        trade_log[[length(trade_log) + 1]] <- list(
          date = cur_date, ym = ym, type = "CORE_ADJUST", symbol = "QQQ",
          holding_days = 0, pnl_pct = 0
        )
        monthly_trades[ym] <- monthly_trades[ym] + 1
      }
      
      to <- abs(tgt_core_w - curr_core_w) + sum(abs(tgt_sat_w - curr_sat_w))
      if (to > 1e-6) {
        eq <- eq * (1 - COST * to)
        monthly_turnover[ym] <- monthly_turnover[ym] + to
      }
      
      curr_core_w <- tgt_core_w
      curr_sat_w  <- tgt_sat_w
    }
  }
  
  list(
    final_equity = eq,
    cagr = (eq^(252 / length(ev)) - 1) * 100,
    monthly_trades = monthly_trades,
    trade_log = trade_log,
    holding_days = holding_days_list,
    monthly_turnover = monthly_turnover
  )
}

res <- sim_trade_tracking(
  core_pct = 0.30, top_n = 5, vol_target = 0.16,
  core_bull_leverage = 1.25, use_core_trend = TRUE, disaster_stop = 0.20
)

mt <- res$monthly_trades
cat("\n================ TRADE FREQUENCY SUMMARY (30/70 TRIPLE-ENGINE) ================\n")
cat(sprintf(" Total Months Simulated:            %d months (2006 - 2026)\n", length(mt)))
cat(sprintf(" Total Trades Executed:             %d trade tickets\n", sum(mt)))
cat(sprintf(" Average Trades Per Month:          %.2f tickets/month\n", mean(mt)))
cat(sprintf(" Median Trades Per Month:           %.1f tickets/month\n", median(mt)))
cat(sprintf(" Min Trades in a Month:             %d tickets\n", min(mt)))
cat(sprintf(" Max Trades in a Month:             %d tickets\n", max(mt)))
cat(sprintf(" Standard Deviation of Trades:      %.2f\n\n", sd(mt)))

types <- sapply(res$trade_log, function(x) x$type)
tab_types <- table(types)
cat(" Breakdown of Trade Types over 20 Years:\n")
cat("--------------------------------------------------------------------------------\n")
for (nm in names(tab_types)) {
  cat(sprintf("   %-20s: %5d tickets (%5.1f%% | %.2f / month)\n",
              nm, tab_types[nm], (tab_types[nm] / length(types)) * 100, tab_types[nm] / length(mt)))
}
cat("--------------------------------------------------------------------------------\n\n")

# Distribution of monthly trades
q_trades <- quantile(mt, probs = c(0.10, 0.25, 0.50, 0.75, 0.90, 0.99))
cat(" Monthly Trade Count Percentiles:\n")
cat(sprintf("   10th percentile:                 %.1f trades/month\n", q_trades[1]))
cat(sprintf("   25th percentile:                 %.1f trades/month\n", q_trades[2]))
cat(sprintf("   50th percentile (Median):        %.1f trades/month\n", q_trades[3]))
cat(sprintf("   75th percentile:                 %.1f trades/month\n", q_trades[4]))
cat(sprintf("   90th percentile:                 %.1f trades/month\n", q_trades[5]))
cat(sprintf("   99th percentile:                 %.1f trades/month\n\n", q_trades[6]))

# Holding duration
hd_days <- res$holding_days
hd_months <- hd_days / 30.4375
cat(" Position Holding Period Statistics (Satellite Momentum Winners):\n")
cat(sprintf("   Mean Holding Period:             %.1f days (%.1f months)\n", mean(hd_days), mean(hd_months)))
cat(sprintf("   Median Holding Period:           %.1f days (%.1f months)\n", median(hd_days), median(hd_months)))
cat(sprintf("   Max Holding Period:              %.1f days (%.1f months)\n", max(hd_days), max(hd_months)))
cat(sprintf("   Annualized Portfolio Turnover:   %.1fx per year\n", mean(res$monthly_turnover) * 12))
cat("================================================================================\n")
