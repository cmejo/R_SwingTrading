# ==============================================================================
# NOVEL ALPHA EXPANSION & FEATURE TOURNAMENT (TARGETING > 50% CAGR)
# ==============================================================================
# Tests the 10 novel alpha strategies and features across the 10-year audited dataset
# (2016-2026, 2,520 trading sessions) on the discrete clean engine.
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

cat(sprintf("[Loaded %d stocks, %d evaluation sessions (2016-2026)]\n", length(stocks_list), n_days))

# ------------------------------------------------------------------------------
# 2. PRECOMPUTE NOVEL SIGNAL MATRICES (RVOL, VCP, WEEKLY TREND)
# ------------------------------------------------------------------------------
cat("[2. Precomputing Novel Microstructure & Trend Matrices...]\n")

rvol_matrix <- matrix(NA_real_, nrow = n_days, ncol = length(stock_names),
                      dimnames = list(as.character(eval_dates), stock_names))
vcp_matrix  <- matrix(NA_real_, nrow = n_days, ncol = length(stock_names),
                      dimnames = list(as.character(eval_dates), stock_names))
weekly_trend_matrix <- matrix(FALSE, nrow = n_days, ncol = length(stock_names),
                              dimnames = list(as.character(eval_dates), stock_names))

t0 <- Sys.time()
for (sym in stock_names) {
  raw_file <- sprintf("data/data_10yr/data_%s.rds", tolower(sym))
  if (file.exists(raw_file)) {
    d <- readRDS(raw_file)
    d_dates <- as.Date(index(d))
    
    # Extract volume, high, low, close
    v_col <- grep("Volume", colnames(d), value = TRUE)[1]
    c_col <- grep("Close", colnames(d), value = TRUE)[1]
    h_col <- grep("High", colnames(d), value = TRUE)[1]
    l_col <- grep("Low", colnames(d), value = TRUE)[1]
    
    vol_vec <- as.numeric(d[, v_col])
    cl_vec  <- as.numeric(d[, c_col])
    hi_vec  <- as.numeric(d[, h_col])
    lo_vec  <- as.numeric(d[, l_col])
    
    # 20-day SMA of volume
    vol_sma20 <- SMA(vol_vec, n = 20)
    rvol <- vol_vec / vol_sma20
    
    # VCP: ATR(5) / ATR(20)
    # Using True Range
    tr_vec <- pmax(hi_vec - lo_vec, 
                   abs(hi_vec - c(NA, cl_vec[-length(cl_vec)])), 
                   abs(lo_vec - c(NA, cl_vec[-length(cl_vec)])), na.rm = TRUE)
    atr5  <- SMA(tr_vec, n = 5)
    atr20 <- SMA(tr_vec, n = 20)
    vcp_ratio <- atr5 / atr20
    
    # Weekly trend: daily EMA(50) > EMA(150) and Close > EMA(50)
    ema50  <- EMA(cl_vec, n = 50)
    ema150 <- EMA(cl_vec, n = 150)
    w_trend <- (cl_vec > ema50) & (ema50 > ema150)
    w_trend[is.na(w_trend)] <- FALSE
    
    # Map to eval_dates
    m_idx <- match(eval_dates, d_dates)
    valid <- !is.na(m_idx)
    rvol_matrix[valid, sym] <- rvol[m_idx[valid]]
    vcp_matrix[valid, sym]  <- vcp_ratio[m_idx[valid]]
    weekly_trend_matrix[valid, sym] <- w_trend[m_idx[valid]]
  }
}
t1 <- Sys.time()
cat(sprintf("   -> Precomputed matrices across all 161 stocks in %.2f seconds.\n\n", 
            as.numeric(t1 - t0, units = "secs")))

# Fast 20/50 EMA on SPY
spy_cl <- macro_data$spy_close
spy_cl_aligned <- na.locf(spy_cl[eval_dates], na.rm = FALSE)
spy_ema20 <- EMA(spy_cl_aligned, n = 20)
spy_ema50 <- EMA(spy_cl_aligned, n = 50)
fast_spy_bull <- (spy_cl_aligned > spy_ema50) & (spy_ema20 > spy_ema50)
fast_spy_bull[is.na(fast_spy_bull)] <- FALSE

# ------------------------------------------------------------------------------
# 3. HIGH-PERFORMANCE DISCRETE CLEAN ENGINE WITH MODULAR NOVEL FEATURES
# ------------------------------------------------------------------------------
run_novel_sim <- function(stocks_list,
                          macro_data,
                          eval_dates,
                          initial_capital = 10000,
                          max_positions = 5,
                          max_per_sector = 2,
                          t1_r = 2.0,
                          t2_r = 4.0,
                          t1_scale_pct = 0.50,
                          t2_scale_pct = 0.25,
                          chand_mult = 2.5,
                          # Novel Feature Toggles
                          use_vcp_bonus = FALSE,          # Feature 1.1: Volatility Contraction Pattern bonus
                          use_rvol_bonus = FALSE,         # Feature 1.2: Relative Volume surge bonus
                          use_weekly_trend_bonus = FALSE, # Feature 1.3: Weekly Stage-2 trend bonus
                          use_stagnant_ejection = FALSE,  # Feature 2.1: Day 3 non-progression exit
                          stagnant_threshold_r = 0.0,     # If PnL <= stagnant_threshold_r on Day 3, exit on Day 4
                          use_progressive_stop = FALSE,   # Feature 2.2: Tighten stop on Day 4/5
                          use_conviction_sizing = FALSE,  # Feature 3.1: Rank-weighted Kelly slots
                          use_adaptive_targets = FALSE,   # Feature 3.2: Expand targets to +2.5R/+5.0R on clean trends
                          use_fast_macro_bull = FALSE,    # Feature 4.1: Fast 20/50 EMA macro regime
                          use_bull_margin_boost = FALSE,  # Feature 4.2: Asymmetric 1.15x margin on #1 setup
                          bull_margin_mult = 1.15
) {
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
  macro_bull_standard <- as.numeric(spy_cl[eval_dates]) > as.numeric(spy_sma200[eval_dates])
  macro_bull_standard[is.na(macro_bull_standard)] <- TRUE
  macro_bull_standard <- head(macro_bull_standard, n_days)

  macro_bull <- if (use_fast_macro_bull) {
    macro_bull_standard | as.logical(fast_spy_bull)
  } else {
    macro_bull_standard
  }

  qqq_sma50 <- SMA(qqq_cl, n = min(50, max(5, floor(length(qqq_cl) / 2))))
  qqq_bear  <- as.numeric(qqq_cl[eval_dates]) < as.numeric(qqq_sma50[eval_dates])
  qqq_bear[is.na(qqq_bear)] <- FALSE
  qqq_bear <- head(qqq_bear, n_days)

  gld_bear <- rep(TRUE, n_days)
  if (!is.null(gld_cl)) {
    gld_aligned <- na.locf(gld_cl[eval_dates], na.rm = FALSE)
    if (nrow(gld_aligned) > 0) {
      gld_sma50 <- SMA(gld_aligned, n = min(50, max(5, floor(nrow(gld_aligned) / 2))))
      g_diff <- as.numeric(gld_aligned) < as.numeric(gld_sma50)
      g_diff[is.na(g_diff)] <- TRUE
      gld_bear <- head(c(g_diff, rep(TRUE, n_days)), n_days)
    }
  }
  dual_bear <- (qqq_bear & gld_bear)

  inv_hedge_daily <- pmin(0.15, pmax(-0.15, -3.0 * qqq_sub - (0.0095 / 252)))

  portfolio_equity   <- numeric(n_days)
  portfolio_cash     <- numeric(n_days)
  daily_return_vec   <- numeric(n_days)
  margin_interest    <- numeric(n_days)

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
  friction_rate <- 10 / 10000

  portfolio_equity[1] <- current_equity
  portfolio_cash[1]   <- current_cash

  pending_buy_orders <- list()

  # Conviction sizing weights for ranks 1..5
  rank_weights <- c(1.30, 1.15, 1.00, 0.85, 0.70)

  for (t in 2:n_days) {
    prev_d <- eval_dates[t - 1]
    curr_d <- eval_dates[t]

    is_macro_bull <- macro_bull[t - 1]
    is_dual_bear  <- dual_bear[t - 1]

    # 1. Execute pending buy orders at Open
    if (length(pending_buy_orders) > 0 && length(open_positions) < max_positions) {
      for (ord in pending_buy_orders) {
        if (length(open_positions) >= max_positions) break
        stk <- stocks_list[[ord$symbol]]
        idx_t <- match(curr_d, stk$dates)
        if (is.na(idx_t)) next

        open_px <- stk$open[idx_t]
        atr_val <- stk$atr[idx_t]
        if (is.na(open_px) || open_px <= 0 || is.na(atr_val) || atr_val <= 0) next

        exec_px <- open_px * (1 + friction_rate)
        
        # Sizing calculation
        eff_slot_mult <- ord$size_mult
        if (use_conviction_sizing && !is.null(ord$rank)) {
          r_idx <- min(length(rank_weights), max(1, ord$rank))
          eff_slot_mult <- eff_slot_mult * rank_weights[r_idx]
        }
        if (use_bull_margin_boost && is_macro_bull && ord$rank == 1) {
          eff_slot_mult <- eff_slot_mult * bull_margin_mult
        }

        target_dollar <- current_equity * slot_budget * eff_slot_mult
        max_single_stock_margin <- current_equity * 0.50 * (if (use_bull_margin_boost) bull_margin_mult else 1.0)
        target_dollar <- min(target_dollar, max_single_stock_margin)

        n_shares <- floor(target_dollar / exec_px)
        if (n_shares <= 0) next

        actual_cost <- n_shares * exec_px
        current_cash <- current_cash - actual_cost

        stop_loss_dist <- 2.0 * atr_val
        stop_loss_px   <- exec_px - stop_loss_dist

        # Adaptive target multipliers
        eff_t1_r <- t1_r
        eff_t2_r <- t2_r
        if (use_adaptive_targets && !is.null(ord$eff_ratio) && !is.na(ord$eff_ratio) && ord$eff_ratio >= 0.40) {
          eff_t1_r <- 2.5
          eff_t2_r <- 5.0
        }

        t1_px <- exec_px + eff_t1_r * stop_loss_dist
        t2_px <- exec_px + eff_t2_r * stop_loss_dist

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

    # 2. Monitor intraday stops, targets & exits
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

      # Progressive stop tightening after Day 3
      if (use_progressive_stop && !pos$t1_hit && pos$days_held >= 4) {
        tight_dist <- if (pos$days_held == 4) 1.5 * atr_val else 1.0 * atr_val
        tight_stop <- high_wm - tight_dist
        pos$stop_price <- max(pos$stop_price, tight_stop)
      }

      # Stagnant Capital Ejection: Exit on Day 4 Open if trade failed to progress by Day 3
      if (use_stagnant_ejection && !pos$t1_hit && pos$days_held >= 4 && pos$entry_date != curr_d) {
        # Check performance on prior close (Day 3)
        prior_close <- stk$close[idx_t - 1]
        prior_pnl_r <- (prior_close - pos$entry_price) / pos$initial_stop_dist
        if (prior_pnl_r <= stagnant_threshold_r) {
          exit_px <- open_px * (1 - friction_rate)
          proceeds <- pos$shares * exit_px
          trade_pnl <- (exit_px - pos$entry_price) * pos$shares
          current_cash <- current_cash + proceeds
          trade_log[[length(trade_log) + 1]] <- list(
            symbol = pos$symbol, entry_date = pos$entry_date, exit_date = curr_d,
            entry_price = pos$entry_price, exit_price = exit_px,
            shares = pos$shares, pnl = trade_pnl, reason = "STAGNANT_EJECTION",
            days_held = pos$days_held, win = (trade_pnl > 0)
          )
          next
        }
      }

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

      # Time stop: runners get 20 days, non-runners get 7 days
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
    total_assets  <- current_cash + total_stock_value
    daily_margin_interest <- borrowed_debt * (0.070 / 252)

    idle_cash <- max(0, current_cash)
    daily_cash_yield <- idle_cash * rf_sub[t]

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

    current_cash   <- current_cash - daily_margin_interest + daily_cash_yield + hedge_pnl
    current_equity <- current_cash + total_stock_value

    if (current_equity > peak_equity) peak_equity <- current_equity

    portfolio_equity[t] <- current_equity
    portfolio_cash[t]   <- current_cash
    margin_interest[t]  <- daily_margin_interest

    prev_eq <- portfolio_equity[t - 1]
    daily_return_vec[t] <- if (prev_eq > 0) (current_equity - prev_eq) / prev_eq else 0

    # Order generation for tomorrow
    empty_slots <- max_positions - length(open_positions)
    if (empty_slots > 0 && t < n_days) {
      cands_syms   <- character(0)
      cands_scores <- numeric(0)
      cands_sector <- character(0)
      cands_sizes  <- numeric(0)
      cands_er     <- numeric(0)

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

          # Novel Feature 1.1: Volatility Contraction Pattern (VCP) Boost
          vcp_boost <- 0.0
          if (use_vcp_bonus) {
            vcp_val <- vcp_matrix[t, sym]
            if (!is.na(vcp_val) && vcp_val < 0.85) {
              vcp_boost <- pmin(0.20, (0.85 - vcp_val) * 0.60)
            }
          }

          # Novel Feature 1.2: Relative Volume Breakout (RVOL) Boost
          rvol_boost <- 0.0
          if (use_rvol_bonus) {
            rv_val <- rvol_matrix[t, sym]
            if (!is.na(rv_val) && rv_val >= 1.20) {
              rvol_boost <- pmin(0.20, (rv_val - 1.0) * 0.15)
            }
          }

          # Novel Feature 1.3: Weekly Trend Quality Boost
          w_boost <- 0.0
          if (use_weekly_trend_bonus) {
            is_w_up <- weekly_trend_matrix[t, sym]
            if (isTRUE(is_w_up)) w_boost <- 0.12
          }

          score <- p_up * (1.0 + rs_val + sm_boost + er_boost + nw_boost + vcp_boost + rvol_boost + w_boost)

          eff_vol <- pmax(0.15, vol_ann) / (1.0 + 0.20 * er_boost + 0.10 * sm_boost)
          base_mult <- pmin(1.25, 0.35 / eff_vol)

          cands_syms   <- c(cands_syms, sym)
          cands_scores <- c(cands_scores, score)
          cands_sector <- c(cands_sector, stk$sector)
          cands_sizes  <- c(cands_sizes, base_mult)
          cands_er     <- c(cands_er, if (!is.na(eff_r)) eff_r else 0.30)
        }
      }

      if (length(cands_syms) > 0) {
        ranked_idx <- order(-cands_scores)
        top_syms   <- cands_syms[ranked_idx]
        top_secs   <- cands_sector[ranked_idx]
        top_sizes  <- cands_sizes[ranked_idx]
        top_er     <- cands_er[ranked_idx]

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
            symbol    = sym_k,
            sector    = sec_k,
            size_mult = top_sizes[k],
            eff_ratio = top_er[k],
            rank      = orders_count + 1
          )
          sec_counts[sec_k] <- cur_s_cnt + 1L
          orders_count <- orders_count + 1
        }
      }
    }
  }

  cum_ret <- cumprod(1 + daily_return_vec)
  cagr <- ((tail(cum_ret, 1))^(252 / n_days) - 1) * 100
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
    total_margin    = sum(margin_interest)
  )
}

# ------------------------------------------------------------------------------
# 4. SYSTEMATIC EXPERIMENT TOURNAMENT
# ------------------------------------------------------------------------------
novel_experiments <- list(
  # Baselines
  list(name = "0. Flagship Baseline (50% @ +2R, 25% @ +4R, 25% Runner)",
       t1_p = 0.50, t2_p = 0.25, vcp = FALSE, rvol = FALSE, wt = FALSE,
       stag = FALSE, stag_r = 0.0, p_stop = FALSE, conv = FALSE, ad_tgt = FALSE,
       fast_b = FALSE, b_marg = FALSE),

  list(name = "1. Asymmetric Scale Baseline (35% @ +2R, 35% @ +4R, 30% Runner)",
       t1_p = 0.35, t2_p = 0.35, vcp = FALSE, rvol = FALSE, wt = FALSE,
       stag = FALSE, stag_r = 0.0, p_stop = FALSE, conv = FALSE, ad_tgt = FALSE,
       fast_b = FALSE, b_marg = FALSE),

  # Pillar 1: Signal Purity
  list(name = "2A. Feature 1.1: Volatility Contraction Pattern (VCP / Squeeze Bonus)",
       t1_p = 0.35, t2_p = 0.35, vcp = TRUE, rvol = FALSE, wt = FALSE,
       stag = FALSE, stag_r = 0.0, p_stop = FALSE, conv = FALSE, ad_tgt = FALSE,
       fast_b = FALSE, b_marg = FALSE),

  list(name = "2B. Feature 1.2: Relative Volume Breakout Surge (RVOL >= 1.20x)",
       t1_p = 0.35, t2_p = 0.35, vcp = FALSE, rvol = TRUE, wt = FALSE,
       stag = FALSE, stag_r = 0.0, p_stop = FALSE, conv = FALSE, ad_tgt = FALSE,
       fast_b = FALSE, b_marg = FALSE),

  list(name = "2C. Feature 1.3: Weekly Trend Quality Confirmation (EMA50 > EMA150)",
       t1_p = 0.35, t2_p = 0.35, vcp = FALSE, rvol = FALSE, wt = TRUE,
       stag = FALSE, stag_r = 0.0, p_stop = FALSE, conv = FALSE, ad_tgt = FALSE,
       fast_b = FALSE, b_marg = FALSE),

  # Pillar 2: Capital Velocity & Dead Money
  list(name = "3A. Feature 2.1A: Stagnant Capital Ejection (Exit if Day 3 PnL <= 0)",
       t1_p = 0.35, t2_p = 0.35, vcp = FALSE, rvol = FALSE, wt = FALSE,
       stag = TRUE, stag_r = 0.0, p_stop = FALSE, conv = FALSE, ad_tgt = FALSE,
       fast_b = FALSE, b_marg = FALSE),

  list(name = "3B. Feature 2.1B: Stagnant Capital Ejection (Exit if Day 3 Gain < +0.25R)",
       t1_p = 0.35, t2_p = 0.35, vcp = FALSE, rvol = FALSE, wt = FALSE,
       stag = TRUE, stag_r = 0.25, p_stop = FALSE, conv = FALSE, ad_tgt = FALSE,
       fast_b = FALSE, b_marg = FALSE),

  list(name = "3C. Feature 2.2: Progressive Stop Tightening (Day 4 -> 1.5x, Day 5 -> 1.0x)",
       t1_p = 0.35, t2_p = 0.35, vcp = FALSE, rvol = FALSE, wt = FALSE,
       stag = FALSE, stag_r = 0.0, p_stop = TRUE, conv = FALSE, ad_tgt = FALSE,
       fast_b = FALSE, b_marg = FALSE),

  # Pillar 3: Dynamic Sizing & Targets
  list(name = "4A. Feature 3.1: Rank-Weighted Conviction Allocation (Kelly Slots)",
       t1_p = 0.35, t2_p = 0.35, vcp = FALSE, rvol = FALSE, wt = FALSE,
       stag = FALSE, stag_r = 0.0, p_stop = FALSE, conv = TRUE, ad_tgt = FALSE,
       fast_b = FALSE, b_marg = FALSE),

  list(name = "4B. Feature 3.2: Efficiency-Adaptive Targets (+2.5R / +5.0R on clean trends)",
       t1_p = 0.35, t2_p = 0.35, vcp = FALSE, rvol = FALSE, wt = FALSE,
       stag = FALSE, stag_r = 0.0, p_stop = FALSE, conv = FALSE, ad_tgt = TRUE,
       fast_b = FALSE, b_marg = FALSE),

  # Pillar 4: Precision Regimes
  list(name = "5A. Feature 4.1: Fast 20/50 EMA Macro Regime Transition",
       t1_p = 0.35, t2_p = 0.35, vcp = FALSE, rvol = FALSE, wt = FALSE,
       stag = FALSE, stag_r = 0.0, p_stop = FALSE, conv = FALSE, ad_tgt = FALSE,
       fast_b = TRUE, b_marg = FALSE),

  list(name = "5B. Feature 4.2: Asymmetric Bull Margin Boost (1.15x on #1 Leader)",
       t1_p = 0.35, t2_p = 0.35, vcp = FALSE, rvol = FALSE, wt = FALSE,
       stag = FALSE, stag_r = 0.0, p_stop = FALSE, conv = FALSE, ad_tgt = FALSE,
       fast_b = FALSE, b_marg = TRUE),

  # Pillar 5: Synergies
  list(name = "6A. Synergy Alpha: Rank Conviction + Weekly Trend Quality",
       t1_p = 0.35, t2_p = 0.35, vcp = FALSE, rvol = FALSE, wt = TRUE,
       stag = FALSE, stag_r = 0.0, p_stop = FALSE, conv = TRUE, ad_tgt = FALSE,
       fast_b = FALSE, b_marg = FALSE),

  list(name = "6B. Synergy Alpha: Rank Conviction + Adaptive Targets (+2.5R/+5.0R)",
       t1_p = 0.35, t2_p = 0.35, vcp = FALSE, rvol = FALSE, wt = FALSE,
       stag = FALSE, stag_r = 0.0, p_stop = FALSE, conv = TRUE, ad_tgt = TRUE,
       fast_b = FALSE, b_marg = FALSE),

  list(name = "6C. Grand Synergy Master: Rank Conviction + Weekly Trend + VCP + 35/35/30",
       t1_p = 0.35, t2_p = 0.35, vcp = TRUE, rvol = FALSE, wt = TRUE,
       stag = FALSE, stag_r = 0.0, p_stop = FALSE, conv = TRUE, ad_tgt = FALSE,
       fast_b = FALSE, b_marg = FALSE),

  list(name = "6D. Grand Apex Synergy: Master + Asymmetric Bull Margin 1.15x",
       t1_p = 0.35, t2_p = 0.35, vcp = TRUE, rvol = FALSE, wt = TRUE,
       stag = FALSE, stag_r = 0.0, p_stop = FALSE, conv = TRUE, ad_tgt = FALSE,
       fast_b = FALSE, b_marg = TRUE)
)

results <- list()

for (exp in novel_experiments) {
  cat(sprintf("[Running] %s ...\n", exp$name))
  res <- run_novel_sim(
    stocks_list            = stocks_list,
    macro_data             = macro_data,
    eval_dates             = eval_dates,
    initial_capital        = 10000,
    max_positions          = 5,
    max_per_sector         = 2,
    t1_r                   = 2.0,
    t2_r                   = 4.0,
    t1_scale_pct           = exp$t1_p,
    t2_scale_pct           = exp$t2_p,
    chand_mult             = 2.5,
    use_vcp_bonus          = exp$vcp,
    use_rvol_bonus         = exp$rvol,
    use_weekly_trend_bonus = exp$wt,
    use_stagnant_ejection  = exp$stag,
    stagnant_threshold_r   = exp$stag_r,
    use_progressive_stop   = exp$p_stop,
    use_conviction_sizing  = exp$conv,
    use_adaptive_targets   = exp$ad_tgt,
    use_fast_macro_bull    = exp$fast_b,
    use_bull_margin_boost  = exp$b_marg,
    bull_margin_mult       = 1.15
  )

  results[[length(results) + 1]] <- data.frame(
    Configuration   = exp$name,
    Terminal_Wealth = sprintf("$%s", format(round(res$terminal_wealth, 2), big.mark = ",", scientific = FALSE)),
    CAGR            = sprintf("%+.2f%%", res$cagr),
    Max_Drawdown    = sprintf("-%.2f%%", res$max_dd),
    Sharpe_Ratio    = sprintf("%.2f", res$sharpe),
    Sortino_Ratio   = sprintf("%.2f", res$sortino),
    Calmar_Ratio    = if (!is.na(res$calmar)) sprintf("%.2f", res$calmar) else "N/A",
    Annual_Vol      = sprintf("%.2f%%", res$annual_vol),
    Win_Rate        = sprintf("%.1f%%", res$win_rate),
    Total_Trades    = res$total_trades,
    Margin_Paid     = sprintf("$%.2f", res$total_margin),
    stringsAsFactors = FALSE
  )
}

df_novel <- do.call(rbind, results)

cat("\n========================================================================================================\n")
cat("                  NOVEL ALPHA EXPANSION SCORECARD (AIMING FOR > 50% CAGR)                               \n")
cat("========================================================================================================\n")
print(df_novel, row.names = FALSE)
cat("========================================================================================================\n\n")

write.csv(df_novel, "output/novel_alpha_expansion_scorecard.csv", row.names = FALSE)
cat("[Done] Saved to output/novel_alpha_expansion_scorecard.csv\n")
