# ==============================================================================
# PHASE A & B: ROBUSTNESS AUDIT & UNTESTED ALPHA / DRAWDOWN EXPERIMENTS
# ==============================================================================
# 1. Phase A2: In-Sample (2016-2022) vs. Out-of-Sample Hold-Out (2023-2026)
# 2. Phase A3: Parameter Sensitivity & Stability Plateau
# 3. Phase A4: Block Bootstrap Confidence Intervals & Deflated Sharpe Ratio
# 4. Phase A5: Execution Stress Test (25 bps slippage)
# 5. Phase B: Novel Mechanics:
#    - B3: Gap-Up Entry Guard (Skip or limit chasing gaps > 1.0x ATR)
#    - B4: Idle-Cash QQQ Sleeve in Confirmed Bull Regimes
#    - B5: QQQ-State Exposure Throttle for Drawdown Reduction
#    - B7: Sector Loss-Cluster Cooldown (2 stop-outs in 10D -> 10D sector pause)
# ==============================================================================

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(TTR)
})

cat("[1. Loading precomputed clean environment...]\n")
clean_env <- readRDS("scratch/clean_backtest_env.rds")
stocks_list <- clean_env$stocks_list
macro_data  <- clean_env$macro_data
eval_dates  <- clean_env$eval_dates
stock_names <- names(stocks_list)
n_days      <- length(eval_dates)

# Engine supporting Phase B toggles and sub-period date ranges
run_robust_sim <- function(stocks_list,
                           macro_data,
                           eval_dates,
                           initial_capital = 10000,
                           max_positions = 5,
                           max_per_sector = 2,
                           t1_r = 2.0,
                           t2_r = 4.0,
                           t1_scale_pct = 0.35,
                           t2_scale_pct = 0.35,
                           stop_atr_mult = 2.0,
                           chand_mult = 2.5,
                           friction_bps = 10,
                           # Phase B toggles:
                           use_gap_guard = FALSE,       # B3: Skip entry if Open > Close + 1.0*ATR
                           gap_guard_atr = 1.0,
                           use_bull_qqq_cash = FALSE,   # B4: Park idle cash in QQQ when SPY > 200 SMA
                           use_qqq_state_throttle = FALSE, # B5: Scale exposure down if QQQ < 50 SMA & vol elevated
                           use_sector_cooldown = FALSE, # B7: Pause sector for 10D after 2 stop-outs in 10D
                           cooldown_stops = 2,
                           cooldown_days = 10
) {
  n_sub <- length(eval_dates)
  spy_cl   <- macro_data$spy_close
  qqq_cl   <- macro_data$qqq_close
  rf_vec   <- macro_data$rf_daily
  gld_cl   <- macro_data$gld_close

  spy_ret <- (spy_cl / lag.xts(spy_cl, 1)) - 1; spy_ret[is.na(spy_ret)] <- 0
  qqq_ret <- (qqq_cl / lag.xts(qqq_cl, 1)) - 1; qqq_ret[is.na(qqq_ret)] <- 0

  spy_sub <- as.numeric(spy_ret[eval_dates]); spy_sub[is.na(spy_sub)] <- 0
  qqq_sub <- as.numeric(qqq_ret[eval_dates]); qqq_sub[is.na(qqq_sub)] <- 0
  rf_sub  <- as.numeric(rf_vec[eval_dates])

  spy_sma200 <- SMA(spy_cl, n = min(200, max(5, floor(length(spy_cl) / 2))))
  macro_bull <- as.numeric(spy_cl[eval_dates]) > as.numeric(spy_sma200[eval_dates])
  macro_bull[is.na(macro_bull)] <- TRUE
  macro_bull <- head(macro_bull, n_sub)

  qqq_sma50 <- SMA(qqq_cl, n = min(50, max(5, floor(length(qqq_cl) / 2))))
  qqq_bear  <- as.numeric(qqq_cl[eval_dates]) < as.numeric(qqq_sma50[eval_dates])
  qqq_bear[is.na(qqq_bear)] <- FALSE
  qqq_bear <- head(qqq_bear, n_sub)

  # QQQ 20-day realized volatility for B5
  qqq_vol20 <- runSD(qqq_ret, n = 20) * sqrt(252)
  qqq_vol_sub <- as.numeric(qqq_vol20[eval_dates])

  gld_bear <- rep(TRUE, n_sub)
  if (!is.null(gld_cl)) {
    gld_aligned <- na.locf(gld_cl[eval_dates], na.rm = FALSE)
    if (nrow(gld_aligned) > 0) {
      gld_sma50 <- SMA(gld_aligned, n = min(50, max(5, floor(nrow(gld_aligned) / 2))))
      g_diff <- as.numeric(gld_aligned) < as.numeric(gld_sma50)
      g_diff[is.na(g_diff)] <- TRUE
      gld_bear <- head(c(g_diff, rep(TRUE, n_sub)), n_sub)
    }
  }
  dual_bear <- (qqq_bear & gld_bear)

  inv_hedge_daily <- pmin(0.15, pmax(-0.15, -3.0 * qqq_sub - (0.0095 / 252)))

  portfolio_equity   <- numeric(n_sub)
  portfolio_cash     <- numeric(n_sub)
  daily_return_vec   <- numeric(n_sub)
  margin_interest    <- numeric(n_sub)

  current_equity <- initial_capital
  current_cash   <- initial_capital
  peak_equity    <- initial_capital

  open_positions <- list()
  trade_log      <- list()

  in_hedge_ep    <- FALSE
  hedge_day      <- 0
  hedge_cum      <- 1.0
  hedge_stopped  <- FALSE

  slot_budget <- 1.0 / max_positions
  friction_rate <- friction_bps / 10000

  portfolio_equity[1] <- current_equity
  portfolio_cash[1]   <- current_cash

  pending_buy_orders <- list()

  # B7: Sector stop-loss history
  sector_stops_history <- list() # sec -> vector of stop dates

  for (t in 2:n_sub) {
    prev_d <- eval_dates[t - 1]
    curr_d <- eval_dates[t]

    is_macro_bull <- macro_bull[t - 1]
    is_dual_bear  <- dual_bear[t - 1]

    # B5: Exposure throttle scale
    exposure_scale <- 1.0
    if (use_qqq_state_throttle) {
      is_qqq_weak <- qqq_bear[t - 1]
      is_vol_high <- !is.na(qqq_vol_sub[t - 1]) && qqq_vol_sub[t - 1] > 0.25
      if (is_qqq_weak && is_vol_high) {
        exposure_scale <- 0.50 # Cut gross allocation by half during high-vol tech downtrends
      } else if (is_qqq_weak) {
        exposure_scale <- 0.75
      }
    }

    # 1. Execute pending orders at Open
    if (length(pending_buy_orders) > 0 && length(open_positions) < max_positions) {
      for (ord in pending_buy_orders) {
        if (length(open_positions) >= max_positions) break
        stk <- stocks_list[[ord$symbol]]
        idx_t <- match(curr_d, stk$dates)
        if (is.na(idx_t)) next

        open_px <- stk$open[idx_t]
        atr_val <- stk$atr[idx_t]
        if (is.na(open_px) || open_px <= 0 || is.na(atr_val) || atr_val <= 0) next

        # B3: Gap-up entry guard
        if (use_gap_guard && !is.null(ord$signal_close)) {
          gap_up_dist <- open_px - ord$signal_close
          if (gap_up_dist > (gap_guard_atr * atr_val)) {
            next # Chasing gap past threshold: skip order to protect risk/reward
          }
        }

        # B7: Sector cooldown check
        if (use_sector_cooldown) {
          recent_sec_stops <- sector_stops_history[[ord$sector]]
          if (!is.null(recent_sec_stops)) {
            active_recent <- recent_sec_stops[as.numeric(curr_d - recent_sec_stops) <= cooldown_days]
            if (length(active_recent) >= cooldown_stops) {
              next # Sector is cooling down after clustered stop-outs
            }
          }
        }

        exec_px <- open_px * (1 + friction_rate)
        target_dollar <- current_equity * slot_budget * ord$size_mult * exposure_scale
        max_single_stock_margin <- current_equity * 0.50
        target_dollar <- min(target_dollar, max_single_stock_margin)

        n_shares <- floor(target_dollar / exec_px)
        if (n_shares <= 0) next

        actual_cost <- n_shares * exec_px
        current_cash <- current_cash - actual_cost

        stop_loss_dist <- stop_atr_mult * atr_val
        stop_loss_px   <- exec_px - stop_loss_dist
        t1_px          <- exec_px + t1_r * stop_loss_dist
        t2_px          <- exec_px + t2_r * stop_loss_dist

        open_positions[[length(open_positions) + 1]] <- list(
          symbol          = ord$symbol,
          sector          = ord$sector,
          entry_date      = curr_d,
          entry_price     = exec_px,
          shares          = n_shares,
          original_shares = n_shares,
          shares_pct      = 1.0,
          stop_price      = stop_loss_px,
          initial_stop_dist = stop_loss_dist,
          t1_price        = t1_px,
          t2_price        = t2_px,
          atr             = atr_val,
          days_held       = 1,
          t1_hit          = FALSE,
          t2_hit          = FALSE,
          is_runner       = FALSE,
          high_water_mark = exec_px
        )
      }
      pending_buy_orders <- list()
    }

    # 2. Intraday monitoring
    surviving_positions <- list()
    for (pos in open_positions) {
      stk <- stocks_list[[pos$symbol]]
      idx_t <- match(curr_d, stk$dates)
      if (is.na(idx_t)) {
        surviving_positions[[length(surviving_positions) + 1]] <- pos
        next
      }

      high_px <- stk$high[idx_t]
      low_px  <- stk$low[idx_t]
      close_px<- stk$close[idx_t]
      open_px <- stk$open[idx_t]
      atr_val <- stk$atr[idx_t]

      if (pos$entry_date != curr_d) {
        pos$days_held <- pos$days_held + 1
      }

      high_wm <- max(pos$high_water_mark, high_px)
      chand_stop <- high_wm - chand_mult * atr_val

      # Stop loss check
      if (low_px <= pos$stop_price) {
        exit_px <- min(open_px, pos$stop_price)
        proceeds <- pos$shares * exit_px * (1 - friction_rate)
        trade_pnl <- (exit_px * (1 - friction_rate) - pos$entry_price) * pos$shares
        current_cash <- current_cash + proceeds
        trade_log[[length(trade_log) + 1]] <- list(
          symbol = pos$symbol, entry_date = pos$entry_date, exit_date = curr_d,
          entry_price = pos$entry_price, exit_price = exit_px,
          shares = pos$shares, pnl = trade_pnl, reason = "STOP_LOSS",
          days_held = pos$days_held, win = (trade_pnl > 0)
        )
        # Log stop for B7 cooldown
        if (use_sector_cooldown) {
          sector_stops_history[[pos$sector]] <- c(sector_stops_history[[pos$sector]], curr_d)
        }
        next
      }

      # Tier 1 Target Hit
      if (!pos$t1_hit && high_px >= pos$t1_price) {
        pos$t1_hit <- TRUE
        shares_to_sell <- pos$original_shares * t1_scale_pct
        shares_to_sell <- min(shares_to_sell, pos$shares)
        exit_px <- pos$t1_price
        proceeds <- shares_to_sell * exit_px * (1 - friction_rate)
        trade_pnl <- (exit_px * (1 - friction_rate) - pos$entry_price) * shares_to_sell
        current_cash <- current_cash + proceeds
        pos$shares <- pos$shares - shares_to_sell
        pos$shares_pct <- pos$shares / pos$original_shares
        pos$stop_price <- max(pos$entry_price, chand_stop)
        trade_log[[length(trade_log) + 1]] <- list(
          symbol = pos$symbol, entry_date = pos$entry_date, exit_date = curr_d,
          entry_price = pos$entry_price, exit_price = exit_px,
          shares = shares_to_sell, pnl = trade_pnl, reason = "TARGET_1_SCALE",
          days_held = pos$days_held, win = TRUE
        )
      }

      # Tier 2 Target Hit
      if (pos$t1_hit && !pos$t2_hit && high_px >= pos$t2_price) {
        pos$t2_hit <- TRUE
        pos$is_runner <- TRUE
        shares_to_sell <- min(pos$original_shares * t2_scale_pct, pos$shares)
        exit_px <- pos$t2_price
        proceeds <- shares_to_sell * exit_px * (1 - friction_rate)
        trade_pnl <- (exit_px * (1 - friction_rate) - pos$entry_price) * shares_to_sell
        current_cash <- current_cash + proceeds
        pos$shares <- pos$shares - shares_to_sell
        pos$shares_pct <- pos$shares / pos$original_shares
        pos$stop_price <- max(pos$stop_price, chand_stop)
        trade_log[[length(trade_log) + 1]] <- list(
          symbol = pos$symbol, entry_date = pos$entry_date, exit_date = curr_d,
          entry_price = pos$entry_price, exit_price = exit_px,
          shares = shares_to_sell, pnl = trade_pnl, reason = "TARGET_2_SCALE",
          days_held = pos$days_held, win = TRUE
        )
      }

      if (pos$t1_hit) {
        pos$stop_price <- max(pos$stop_price, chand_stop)
      }

      # Time stop
      max_hold <- if (pos$is_runner) 20 else 7
      if (pos$days_held >= max_hold) {
        exit_px <- close_px * (1 - friction_rate)
        proceeds <- pos$shares * exit_px
        trade_pnl <- (exit_px - pos$entry_price) * pos$shares
        current_cash <- current_cash + proceeds
        trade_log[[length(trade_log) + 1]] <- list(
          symbol = pos$symbol, entry_date = pos$entry_date, exit_date = curr_d,
          entry_price = pos$entry_price, exit_price = exit_px,
          shares = pos$shares, pnl = trade_pnl, reason = "TIME_STOP",
          days_held = pos$days_held, win = (trade_pnl > 0)
        )
        next
      }

      pos$high_water_mark <- high_wm
      surviving_positions[[length(surviving_positions) + 1]] <- pos
    }
    open_positions <- surviving_positions

    # Mark to market
    total_stock_value <- 0.0
    for (pos in open_positions) {
      stk <- stocks_list[[pos$symbol]]
      idx_t <- match(curr_d, stk$dates)
      if (!is.na(idx_t)) {
        total_stock_value <- total_stock_value + pos$shares * stk$close[idx_t]
      } else {
        total_stock_value <- total_stock_value + pos$shares * pos$entry_price
      }
    }

    borrowed_debt <- max(0, -current_cash)
    daily_margin_interest <- borrowed_debt * (0.070 / 252)

    idle_cash <- max(0, current_cash)
    
    # B4: Bull market idle-cash QQQ overlay
    bull_qqq_pnl <- 0.0
    if (use_bull_qqq_cash && is_macro_bull && !is_dual_bear && idle_cash > 0) {
      # Park 80% of idle cash in QQQ to capture broad market beta when setups are scarce
      qqq_alloc <- idle_cash * 0.80
      bull_qqq_pnl <- qqq_alloc * qqq_sub[t]
      # Remaining 20% earns risk-free rate
      daily_cash_yield <- (idle_cash * 0.20) * rf_sub[t]
    } else {
      daily_cash_yield <- idle_cash * rf_sub[t]
    }

    # Hedge sleeve
    hedge_pnl <- 0.0
    if (is_dual_bear && idle_cash > 0) {
      if (!in_hedge_ep) {
        in_hedge_ep   <- TRUE
        hedge_day     <- 0
        hedge_cum     <- 1.0
        hedge_stopped <- FALSE
      }
      hedge_day <- hedge_day + 1
      if (hedge_day > 15) hedge_stopped <- TRUE

      if (!hedge_stopped) {
        h_ret <- inv_hedge_daily[t]
        hedge_pnl <- idle_cash * h_ret
        hedge_cum <- hedge_cum * (1 + h_ret)
        if ((hedge_cum - 1.0) <= -0.10) {
          hedge_stopped <- TRUE
        }
      }
    } else {
      in_hedge_ep   <- FALSE
      hedge_day     <- 0
      hedge_cum     <- 1.0
      hedge_stopped <- FALSE
    }

    current_cash   <- current_cash - daily_margin_interest + daily_cash_yield + hedge_pnl + bull_qqq_pnl
    current_equity <- current_cash + total_stock_value

    if (current_equity > peak_equity) peak_equity <- current_equity

    portfolio_equity[t] <- current_equity
    portfolio_cash[t]   <- current_cash
    margin_interest[t]  <- daily_margin_interest

    prev_eq <- portfolio_equity[t - 1]
    daily_return_vec[t] <- if (prev_eq > 0) (current_equity - prev_eq) / prev_eq else 0

    # Order generation
    empty_slots <- max_positions - length(open_positions)
    if (empty_slots > 0 && t < n_sub) {
      cands_syms   <- character(0)
      cands_scores <- numeric(0)
      cands_sector <- character(0)
      cands_sizes  <- numeric(0)
      cands_close  <- numeric(0)

      held_syms <- sapply(open_positions, function(p) p$symbol)
      eff_p_hurdle <- if (macro_bull[t]) 0.58 else 0.65

      for (sym in names(stocks_list)) {
        if (sym %in% held_syms) next
        stk <- stocks_list[[sym]]
        idx_t <- match(curr_d, stk$dates)
        if (is.na(idx_t) || idx_t < 150) next

        p_up    <- stk$prob[idx_t]
        rs_val  <- stk$rs[idx_t]
        vol_ann <- stk$vol[idx_t]
        atr_ext <- stk$atr_ext[idx_t]
        smom    <- stk$sortino[idx_t]
        eff_r   <- stk$eff_ratio[idx_t]
        nwick   <- stk$net_wick[idx_t]

        if (is.na(p_up) || is.na(rs_val) || is.na(vol_ann)) next
        if (!is.na(atr_ext) && atr_ext > 3.0) next

        if (p_up >= eff_p_hurdle && rs_val > 0) {
          sm_boost <- if (!is.na(smom)) pmax(0, (smom - 0.80) * 0.06) else 0
          er_boost <- if (!is.na(eff_r)) pmax(0, (eff_r - 0.30) * 0.20) else 0
          nw_boost <- if (!is.na(nwick)) pmax(0, nwick * 0.08) else 0

          score <- p_up * (1.0 + rs_val + sm_boost + er_boost + nw_boost)

          eff_vol <- pmax(0.15, vol_ann) / (1.0 + 0.20 * er_boost + 0.10 * sm_boost)
          base_mult <- pmin(1.25, 0.35 / eff_vol)

          cands_syms   <- c(cands_syms, sym)
          cands_scores <- c(cands_scores, score)
          cands_sector <- c(cands_sector, stk$sector)
          cands_sizes  <- c(cands_sizes, base_mult)
          cands_close  <- c(cands_close, stk$close[idx_t])
        }
      }

      if (length(cands_syms) > 0) {
        ranked_idx <- order(-cands_scores)
        top_syms   <- cands_syms[ranked_idx]
        top_secs   <- cands_sector[ranked_idx]
        top_sizes  <- cands_sizes[ranked_idx]
        top_close  <- cands_close[ranked_idx]

        orders_count <- 0
        held_sectors <- if (length(open_positions) > 0) sapply(open_positions, function(p) p$sector) else character(0)
        sec_counts   <- if (length(held_sectors) > 0) table(held_sectors) else integer(0)

        for (k in seq_along(top_syms)) {
          if (orders_count >= empty_slots) break
          sym_k <- top_syms[k]
          sec_k <- top_secs[k]
          cur_s_cnt <- if (!is.na(sec_counts[sec_k])) as.integer(sec_counts[sec_k]) else 0L
          if (cur_s_cnt >= max_per_sector) next

          pending_buy_orders[[length(pending_buy_orders) + 1]] <- list(
            symbol       = sym_k,
            sector       = sec_k,
            size_mult    = top_sizes[k],
            signal_close = top_close[k]
          )
          sec_counts[sec_k] <- cur_s_cnt + 1L
          orders_count <- orders_count + 1
        }
      }
    }
  }

  cum_ret <- cumprod(1 + daily_return_vec)
  cagr <- ((tail(cum_ret, 1))^(252 / n_sub) - 1) * 100
  cum_max <- cummax(cum_ret)
  dd <- (cum_ret - cum_max) / cum_max
  max_dd <- abs(min(dd)) * 100
  annual_vol <- sd(daily_return_vec) * sqrt(252) * 100
  rf_ann <- mean(rf_sub) * 252 * 100
  excess_ret <- daily_return_vec - rf_sub
  sharpe <- (mean(excess_ret) / sd(daily_return_vec)) * sqrt(252)
  downside_ret <- daily_return_vec[daily_return_vec < 0]
  sortino <- if (length(downside_ret) > 0) (mean(excess_ret) / sd(downside_ret)) * sqrt(252) else NA
  calmar <- if (max_dd > 0) cagr / max_dd else NA
  win_rate <- if (length(trade_log) > 0) sum(sapply(trade_log, function(x) x$win)) / length(trade_log) * 100 else NA

  list(
    terminal_wealth = tail(portfolio_equity, 1),
    cagr            = cagr,
    max_dd          = max_dd,
    sharpe          = sharpe,
    sortino         = sortino,
    calmar          = calmar,
    annual_vol      = annual_vol,
    win_rate        = win_rate,
    total_trades    = length(trade_log),
    total_margin    = sum(margin_interest),
    daily_returns   = daily_return_vec,
    equity_curve    = portfolio_equity
  )
}

# ------------------------------------------------------------------------------
# EXPERIMENT 1: PHASE A2 — IN-SAMPLE (2016-2022) vs. HOLD-OUT (2023-2026)
# ------------------------------------------------------------------------------
cat("\n========================================================================================\n")
cat("          PHASE A2: IN-SAMPLE (2016-2022) vs. UNTOUCHED HOLD-OUT (2023-2026)            \n")
cat("========================================================================================\n")

dates_is <- eval_dates[eval_dates <= as.Date("2022-12-31")]
dates_oos <- eval_dates[eval_dates >= as.Date("2023-01-01")]

cat(sprintf("In-Sample:  %d sessions (%s to %s)\n", length(dates_is), min(dates_is), max(dates_is)))
cat(sprintf("Hold-Out:   %d sessions (%s to %s)\n\n", length(dates_oos), min(dates_oos), max(dates_oos)))

res_is  <- run_robust_sim(stocks_list, macro_data, dates_is)
res_oos <- run_robust_sim(stocks_list, macro_data, dates_oos)
res_full<- run_robust_sim(stocks_list, macro_data, eval_dates)

df_split <- data.frame(
  Period          = c("Full 10-Yr (2016-2026)", "In-Sample (2016-2022)", "Untouched Hold-Out (2023-2026)"),
  Sessions        = c(length(eval_dates), length(dates_is), length(dates_oos)),
  CAGR            = c(sprintf("%+.2f%%", res_full$cagr), sprintf("%+.2f%%", res_is$cagr), sprintf("%+.2f%%", res_oos$cagr)),
  Max_Drawdown    = c(sprintf("-%.2f%%", res_full$max_dd), sprintf("-%.2f%%", res_is$max_dd), sprintf("-%.2f%%", res_oos$max_dd)),
  Sharpe_Ratio    = c(sprintf("%.2f", res_full$sharpe), sprintf("%.2f", res_is$sharpe), sprintf("%.2f", res_oos$sharpe)),
  Sortino_Ratio   = c(sprintf("%.2f", res_full$sortino), sprintf("%.2f", res_is$sortino), sprintf("%.2f", res_oos$sortino)),
  Win_Rate        = c(sprintf("%.1f%%", res_full$win_rate), sprintf("%.1f%%", res_is$win_rate), sprintf("%.1f%%", res_oos$win_rate)),
  Total_Trades    = c(res_full$total_trades, res_is$total_trades, res_oos$total_trades)
)
print(df_split, row.names = FALSE)

# ------------------------------------------------------------------------------
# EXPERIMENT 2: PHASE A3 — PARAMETER SENSITIVITY & STABILITY PLATEAU
# ------------------------------------------------------------------------------
cat("\n========================================================================================\n")
cat("          PHASE A3: PARAMETER SENSITIVITY & STABILITY PLATEAU (PERTURBATIONS)           \n")
cat("========================================================================================\n")

sens_grid <- list(
  list(name = "Baseline Flagship (T1=2.0R, T2=4.0R, Stop=2.0x, Chand=2.5x)", t1=2.0, t2=4.0, stop=2.0, ch=2.5),
  list(name = "Target Perturbation -0.25R (T1=1.75R, T2=3.75R)",              t1=1.75, t2=3.75, stop=2.0, ch=2.5),
  list(name = "Target Perturbation +0.25R (T1=2.25R, T2=4.25R)",              t1=2.25, t2=4.25, stop=2.0, ch=2.5),
  list(name = "Stop Perturbation -0.25 ATR (Stop=1.75x ATR)",                 t1=2.0, t2=4.0, stop=1.75, ch=2.5),
  list(name = "Stop Perturbation +0.25 ATR (Stop=2.25x ATR)",                 t1=2.0, t2=4.0, stop=2.25, ch=2.5),
  list(name = "Chandelier Perturbation -0.25 ATR (Chand=2.25x)",             t1=2.0, t2=4.0, stop=2.0, ch=2.25),
  list(name = "Chandelier Perturbation +0.25 ATR (Chand=2.75x)",             t1=2.0, t2=4.0, stop=2.0, ch=2.75)
)

sens_res <- list()
for (sg in sens_grid) {
  r <- run_robust_sim(stocks_list, macro_data, eval_dates, t1_r=sg$t1, t2_r=sg$t2, stop_atr_mult=sg$stop, chand_mult=sg$ch)
  sens_res[[length(sens_res) + 1]] <- data.frame(
    Perturbation = sg$name,
    Terminal     = sprintf("$%s", format(round(r$terminal_wealth, 2), big.mark = ",")),
    CAGR         = sprintf("%+.2f%%", r$cagr),
    MaxDD        = sprintf("-%.2f%%", r$max_dd),
    Sharpe       = sprintf("%.2f", r$sharpe),
    Sortino      = sprintf("%.2f", r$sortino),
    WinRate      = sprintf("%.1f%%", r$win_rate)
  )
}
df_sens <- do.call(rbind, sens_res)
print(df_sens, row.names = FALSE)

# ------------------------------------------------------------------------------
# EXPERIMENT 3: PHASE A4 — BLOCK BOOTSTRAP & DEFLATED SHARPE RATIO
# ------------------------------------------------------------------------------
cat("\n========================================================================================\n")
cat("          PHASE A4: BLOCK BOOTSTRAP (1,000 ITERATIONS) & DEFLATED SHARPE RATIO         \n")
cat("========================================================================================\n")

daily_rets <- res_full$daily_returns
n_obs <- length(daily_rets)
block_len <- 10 # 10-day block bootstrap
n_blocks <- ceiling(n_obs / block_len)

set.seed(42)
B <- 1000
boot_cagrs  <- numeric(B)
boot_maxdds <- numeric(B)
boot_sharpes<- numeric(B)

for (b in 1:B) {
  start_idx <- sample(1:(n_obs - block_len + 1), size = n_blocks, replace = TRUE)
  sampled_indices <- unlist(lapply(start_idx, function(s) s:(s + block_len - 1)))[1:n_obs]
  b_rets <- daily_rets[sampled_indices]
  
  b_cum <- cumprod(1 + b_rets)
  boot_cagrs[b] <- ((tail(b_cum, 1))^(252 / n_obs) - 1) * 100
  b_dd <- (b_cum - cummax(b_cum)) / cummax(b_cum)
  boot_maxdds[b] <- abs(min(b_dd)) * 100
  boot_sharpes[b] <- (mean(b_rets) / sd(b_rets)) * sqrt(252)
}

cagr_ci  <- quantile(boot_cagrs, probs = c(0.025, 0.50, 0.975))
maxdd_ci <- quantile(boot_maxdds, probs = c(0.025, 0.50, 0.975))
sharpe_ci<- quantile(boot_sharpes, probs = c(0.025, 0.50, 0.975))

cat(sprintf("95%% Bootstrap CI for CAGR:      [%+.2f%%, %+.2f%%] (Median: %+.2f%%)\n", cagr_ci[1], cagr_ci[3], cagr_ci[2]))
cat(sprintf("95%% Bootstrap CI for MaxDD:     [-%.2f%%, -%.2f%%] (Median: -%.2f%%)\n", maxdd_ci[3], maxdd_ci[1], maxdd_ci[2]))
cat(sprintf("95%% Bootstrap CI for Sharpe:    [%.2f, %.2f] (Median: %.2f)\n\n", sharpe_ci[1], sharpe_ci[3], sharpe_ci[2]))

# Deflated Sharpe Ratio calculation (Bailey & Lopez de Prado 2014)
# Adjusting for N ~ 50 independent trials
N_trials <- 50
var_sharpe <- 0.15 # estimated variance across trial Sharpes
gamma_skew <- mean((daily_rets - mean(daily_rets))^3) / (sd(daily_rets)^3)
gamma_kurt <- mean((daily_rets - mean(daily_rets))^4) / (sd(daily_rets)^4)
expected_max_sharpe <- sqrt(var_sharpe) * ((1 - 0.5772) * qnorm(1 - 1/N_trials) + 0.5772 * qnorm(1 - 1/(N_trials * exp(1))))
sr_standardized <- (res_full$sharpe - expected_max_sharpe) * sqrt(n_obs - 1) / sqrt(1 - gamma_skew * res_full$sharpe + ((gamma_kurt - 1) / 4) * res_full$sharpe^2)
dsr_prob <- pnorm(sr_standardized)

cat(sprintf("Deflated Sharpe Ratio (DSR) across %d trials: %.4f (Probability of True Skill: %.1f%%)\n\n", 
            N_trials, dsr_prob, dsr_prob * 100))

# ------------------------------------------------------------------------------
# EXPERIMENT 4: PHASE A5 — FRICTION & EXECUTION STRESS (25 BPS SLIPPAGE)
# ------------------------------------------------------------------------------
cat("\n========================================================================================\n")
cat("          PHASE A5: EXECUTION STRESS TEST (25 BPS SLIPPAGE vs. 10 BPS BASELINE)         \n")
cat("========================================================================================\n")

res_stress_25 <- run_robust_sim(stocks_list, macro_data, eval_dates, friction_bps = 25)
res_stress_15 <- run_robust_sim(stocks_list, macro_data, eval_dates, friction_bps = 15)

df_fric <- data.frame(
  Slippage_Friction = c("10 bps (Baseline)", "15 bps (+5 bps drag)", "25 bps (+15 bps Institutional Stress)"),
  Terminal          = c(sprintf("$%s", format(round(res_full$terminal_wealth, 2), big.mark = ",")),
                        sprintf("$%s", format(round(res_stress_15$terminal_wealth, 2), big.mark = ",")),
                        sprintf("$%s", format(round(res_stress_25$terminal_wealth, 2), big.mark = ","))),
  CAGR              = c(sprintf("%+.2f%%", res_full$cagr), sprintf("%+.2f%%", res_stress_15$cagr), sprintf("%+.2f%%", res_stress_25$cagr)),
  CAGR_Retention    = c("100.0%", sprintf("%.1f%%", (res_stress_15$cagr / res_full$cagr)*100), sprintf("%.1f%%", (res_stress_25$cagr / res_full$cagr)*100)),
  MaxDD             = c(sprintf("-%.2f%%", res_full$max_dd), sprintf("-%.2f%%", res_stress_15$max_dd), sprintf("-%.2f%%", res_stress_25$max_dd)),
  Sharpe            = c(sprintf("%.2f", res_full$sharpe), sprintf("%.2f", res_stress_15$sharpe), sprintf("%.2f", res_stress_25$sharpe))
)
print(df_fric, row.names = FALSE)

# ------------------------------------------------------------------------------
# EXPERIMENT 5: PHASE B — UNTESTED NOVEL MECHANICS
# ------------------------------------------------------------------------------
cat("\n========================================================================================\n")
cat("          PHASE B: UNTESTED ALPHA & DRAWDOWN MECHANICS (EVALUATED ON HOLD-OUT)          \n")
cat("========================================================================================\n")

b_experiments <- list(
  list(name = "0. Control Baseline (Asymmetric 35/35/30)",
       gg = FALSE, b_qqq = FALSE, q_thr = FALSE, sec_cd = FALSE),
  list(name = "B3. Gap-Up Entry Guard (Skip if Open > Close + 1.0x ATR)",
       gg = TRUE, b_qqq = FALSE, q_thr = FALSE, sec_cd = FALSE),
  list(name = "B4. Bull-Market Idle-Cash QQQ Overlay (80% idle to QQQ in Bull)",
       gg = FALSE, b_qqq = TRUE, q_thr = FALSE, sec_cd = FALSE),
  list(name = "B5. QQQ-State Exposure Throttle (Cut size if QQQ < 50 SMA & Vol > 25%)",
       gg = FALSE, b_qqq = FALSE, q_thr = TRUE, sec_cd = FALSE),
  list(name = "B7. Sector Loss-Cluster Cooldown (10D pause after 2 sector stops)",
       gg = FALSE, b_qqq = FALSE, q_thr = FALSE, sec_cd = TRUE),
  list(name = "B_Combo. Drawdown Shield: QQQ-State Throttle + Sector Cooldown",
       gg = FALSE, b_qqq = FALSE, q_thr = TRUE, sec_cd = TRUE),
  list(name = "B_Combo2. Max Alpha: Bull QQQ Overlay + Gap Guard",
       gg = TRUE, b_qqq = TRUE, q_thr = FALSE, sec_cd = FALSE)
)

b_results <- list()
for (exp in b_experiments) {
  # Full period
  rf <- run_robust_sim(stocks_list, macro_data, eval_dates,
                       use_gap_guard = exp$gg, use_bull_qqq_cash = exp$b_qqq,
                       use_qqq_state_throttle = exp$q_thr, use_sector_cooldown = exp$sec_cd)
  # Hold-out period (2023-2026)
  ro <- run_robust_sim(stocks_list, macro_data, dates_oos,
                       use_gap_guard = exp$gg, use_bull_qqq_cash = exp$b_qqq,
                       use_qqq_state_throttle = exp$q_thr, use_sector_cooldown = exp$sec_cd)
  
  b_results[[length(b_results) + 1]] <- data.frame(
    Configuration     = exp$name,
    Full_CAGR         = sprintf("%+.2f%%", rf$cagr),
    Full_MaxDD        = sprintf("-%.2f%%", rf$max_dd),
    Full_Sharpe       = sprintf("%.2f", rf$sharpe),
    HoldOut_CAGR      = sprintf("%+.2f%%", ro$cagr),
    HoldOut_MaxDD     = sprintf("-%.2f%%", ro$max_dd),
    HoldOut_Sharpe    = sprintf("%.2f", ro$sharpe),
    HoldOut_WinRate   = sprintf("%.1f%%", ro$win_rate)
  )
}
df_phase_b <- do.call(rbind, b_results)
print(df_phase_b, row.names = FALSE)

write.csv(df_phase_b, "output/phase_b_untested_mechanics_scorecard.csv", row.names = FALSE)
cat("\n[Done] All Phase A & B results computed and saved to output/phase_b_untested_mechanics_scorecard.csv\n")
