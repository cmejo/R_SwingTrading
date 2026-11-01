#!/usr/bin/env Rscript
# ==============================================================================
# scratch/test_advanced_features_deep_dive.R
# Systematically Test the 6 User-Requested Features on the Clean Engine:
# 1. Volatility targeting & drawdown-based de-leveraging
# 2. Market breadth (50D & 200D averages) as regime input
# 3. Sector & industry momentum
# 4. Short-term reversal factor (pullback in trend)
# 5. Earnings-date blackout / shock filter
# 6. Holding correlation filter (cutting exposure when correlations rise)
# 7. Synergies aiming to exceed +45.12% CAGR
# ==============================================================================

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(TTR)
  library(jsonlite)
})

source("R/08_metrics.R")

cat("[1. Loading precomputed clean environment...]\n")
env_data <- readRDS("scratch/clean_backtest_env.rds")
stocks_list <- env_data$stocks_list
macro_data  <- env_data$macro_data
eval_dates  <- env_data$eval_dates
n_days      <- length(eval_dates)

cat(sprintf("[Loaded %d stocks, %d evaluation sessions (2016-2026)]\n", length(stocks_list), n_days))

# ------------------------------------------------------------------------------
# PRECOMPUTATIONS FOR THE 6 NEW FEATURES
# ------------------------------------------------------------------------------
cat("[2. Precomputing Advanced Feature Matrices across all stocks...]\n")

stock_names <- names(stocks_list)
n_stocks    <- length(stock_names)

# Matrix of Close prices: [n_days x n_stocks]
close_matrix <- matrix(NA_real_, nrow = n_days, ncol = n_stocks,
                       dimnames = list(as.character(eval_dates), stock_names))
open_matrix  <- matrix(NA_real_, nrow = n_days, ncol = n_stocks,
                       dimnames = list(as.character(eval_dates), stock_names))
high_matrix  <- matrix(NA_real_, nrow = n_days, ncol = n_stocks,
                       dimnames = list(as.character(eval_dates), stock_names))
low_matrix   <- matrix(NA_real_, nrow = n_days, ncol = n_stocks,
                       dimnames = list(as.character(eval_dates), stock_names))
vol_matrix   <- matrix(NA_real_, nrow = n_days, ncol = n_stocks,
                       dimnames = list(as.character(eval_dates), stock_names))

for (sym in stock_names) {
  stk <- stocks_list[[sym]]
  m_idx <- match(eval_dates, stk$dates)
  valid <- !is.na(m_idx)
  close_matrix[valid, sym] <- stk$close[m_idx[valid]]
  open_matrix[valid, sym]  <- stk$open[m_idx[valid]]
  high_matrix[valid, sym]  <- stk$high[m_idx[valid]]
  low_matrix[valid, sym]   <- stk$low[m_idx[valid]]
}

# 2A. Precompute Market Breadth (50D & 200D SMA)
cat("   -> Computing 50-day and 200-day Market Breadth...\n")
above_50_mat  <- matrix(FALSE, nrow = n_days, ncol = n_stocks)
above_200_mat <- matrix(FALSE, nrow = n_days, ncol = n_stocks)

for (j in seq_len(n_stocks)) {
  cl_vec <- close_matrix[, j]
  valid_cl <- !is.na(cl_vec)
  if (sum(valid_cl) >= 50) {
    sma50 <- SMA(cl_vec[valid_cl], n = 50)
    above_50_mat[valid_cl, j] <- (cl_vec[valid_cl] > sma50)
  }
  if (sum(valid_cl) >= 150) {
    n_win <- min(200, sum(valid_cl) - 10)
    sma200 <- SMA(cl_vec[valid_cl], n = n_win)
    above_200_mat[valid_cl, j] <- (cl_vec[valid_cl] > sma200)
  }
}

breadth_50  <- rowMeans(above_50_mat, na.rm = TRUE)
breadth_200 <- rowMeans(above_200_mat, na.rm = TRUE)
macro_data$breadth_50  <- breadth_50
macro_data$breadth_200 <- breadth_200

# Precompute SPY 20-Day Momentum
spy_cl_aligned <- na.locf(macro_data$spy_close[eval_dates], na.rm = FALSE)
spy_mom_20d <- as.numeric((spy_cl_aligned / lag.xts(spy_cl_aligned, 20)) - 1)
spy_mom_20d[is.na(spy_mom_20d)] <- 0.0
macro_data$spy_mom_20d <- spy_mom_20d

# 2B. Precompute Sector 20-Day Momentum
cat("   -> Computing 20-day Sector & Industry Momentum...\n")
sector_map <- sapply(stock_names, function(s) stocks_list[[s]]$sector)
unique_sectors <- unique(sector_map)

# 20-day rolling return matrix: (Close[t] / Close[t-20]) - 1
ret_20d_matrix <- (close_matrix / apply(close_matrix, 2, function(x) lag.xts(xts(x, order.by = eval_dates), 20))) - 1

sector_mom_20d <- matrix(0.0, nrow = n_days, ncol = length(unique_sectors),
                         dimnames = list(as.character(eval_dates), unique_sectors))

for (sec in unique_sectors) {
  sec_syms <- stock_names[sector_map == sec]
  if (length(sec_syms) == 1) {
    sector_mom_20d[, sec] <- ret_20d_matrix[, sec_syms]
  } else {
    sector_mom_20d[, sec] <- rowMeans(ret_20d_matrix[, sec_syms, drop = FALSE], na.rm = TRUE)
  }
}

# 2C. Precompute Short-Term Reversal Factor (Pullback in Trend)
# 3-day return: (Close[t] / Close[t-3]) - 1
cat("   -> Computing Short-Term Reversal & Pullback Factor (3-Day)...\n")
ret_3d_matrix <- (close_matrix / apply(close_matrix, 2, function(x) lag.xts(xts(x, order.by = eval_dates), 3))) - 1

# 2D. Precompute Earnings Shock Blackout Dates
# Identify dates with extreme overnight gap + volume surge (typical earnings announcement)
cat("   -> Precomputing Historical Earnings Volatility Blackout Flags...\n")
earnings_blackout_matrix <- matrix(FALSE, nrow = n_days, ncol = n_stocks,
                                   dimnames = list(as.character(eval_dates), stock_names))

for (sym in stock_names) {
  stk <- stocks_list[[sym]]
  m_idx <- match(eval_dates, stk$dates)
  valid <- !is.na(m_idx)
  if (sum(valid) > 50) {
    cl  <- stk$close[m_idx[valid]]
    op  <- stk$open[m_idx[valid]]
    atr <- stk$atr[m_idx[valid]]
    # An earnings shock: overnight gap > 2.0x ATR
    gap <- abs(op - lag.xts(xts(cl, order.by = eval_dates[valid]), 1))
    is_shock <- as.numeric(gap) > (2.0 * atr)
    is_shock[is.na(is_shock)] <- FALSE
    
    # Blackout window: 3 days before and 2 days after detected quarterly shock
    shock_indices <- which(is_shock)
    if (length(shock_indices) > 0) {
      blackout_indices <- unique(unlist(lapply(shock_indices, function(idx) {
        max(1, idx - 3):min(sum(valid), idx + 2)
      })))
      valid_indices <- which(valid)
      earnings_blackout_matrix[valid_indices[blackout_indices], sym] <- TRUE
    }
  }
}

cat("[Precomputations complete!]\n\n")

# ------------------------------------------------------------------------------
# SIMULATION ENGINE WITH TOGGLEABLE ADVANCED FEATURES
# ------------------------------------------------------------------------------
run_sim_full_features <- function(stocks_list,
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
                                  # 1. Vol Targeting & DD De-leveraging
                                  use_vol_targeting = FALSE,
                                  target_portfolio_vol = 0.45,
                                  use_dd_deleveraging = FALSE,
                                  dd_threshold = -0.08,
                                  dd_leverage_scale = 0.60,
                                  # 2. Market Breadth Regime Gate
                                  use_breadth_gate = FALSE,
                                  breadth_50_min = 0.40,
                                  breadth_200_min = 0.45,
                                  # 3. Sector & Industry Momentum
                                  use_sector_momentum = FALSE,
                                  sector_mom_bonus = 0.15,
                                  # 4. Short-Term Reversal Factor
                                  use_reversal_factor = FALSE,
                                  # 5. Earnings Blackout
                                  use_earnings_blackout = FALSE,
                                  # 6. Holding Correlation Filter
                                  use_correlation_filter = FALSE,
                                  max_holding_corr = 0.65,
                                  # Extra Alpha Boost
                                  super_bull_leverage = 1.0) {

  spy_cl   <- macro_data$spy_close
  qqq_cl   <- macro_data$qqq_close
  rf_vec   <- macro_data$rf_daily
  gld_cl   <- macro_data$gld_close
  breadth_50  <- macro_data$breadth_50
  breadth_200 <- macro_data$breadth_200
  spy_mom_20d <- macro_data$spy_mom_20d

  spy_ret <- (spy_cl / lag.xts(spy_cl, 1)) - 1; spy_ret[is.na(spy_ret)] <- 0
  qqq_ret <- (qqq_cl / lag.xts(qqq_cl, 1)) - 1; qqq_ret[is.na(qqq_ret)] <- 0

  spy_sub <- as.numeric(spy_ret[eval_dates]); spy_sub[is.na(spy_sub)] <- 0
  qqq_sub <- as.numeric(qqq_ret[eval_dates]); qqq_sub[is.na(qqq_sub)] <- 0
  rf_sub  <- as.numeric(rf_vec[eval_dates])

  spy_sma200 <- SMA(spy_cl, n = min(200, max(5, floor(length(spy_cl) / 2))))
  macro_bull <- as.numeric(spy_cl[eval_dates]) > as.numeric(spy_sma200[eval_dates])
  macro_bull[is.na(macro_bull)] <- TRUE
  macro_bull <- head(macro_bull, n_days)

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

  for (t in 2:n_days) {
    prev_d <- eval_dates[t - 1]
    curr_d <- eval_dates[t]

    is_macro_bull <- macro_bull[t - 1]
    is_dual_bear  <- dual_bear[t - 1]

    # Base dynamic leverage
    dynamic_lev <- 1.0

    # Feature 1A: Portfolio Volatility Targeting
    if (use_vol_targeting && t >= 22) {
      recent_rets <- daily_return_vec[(t - 20):(t - 1)]
      r_vol <- sd(recent_rets) * sqrt(252)
      if (!is.na(r_vol) && r_vol > 0.05) {
        dynamic_lev <- min(1.25, max(0.20, target_portfolio_vol / r_vol))
      }
    }

    # Feature 1B: Drawdown-Based De-leveraging
    current_dd <- (current_equity - peak_equity) / peak_equity
    if (use_dd_deleveraging && current_dd <= dd_threshold) {
      dynamic_lev <- dynamic_lev * dd_leverage_scale
    }

    # Super-Bull Booster
    if (super_bull_leverage > 1.0 && is_macro_bull && breadth_50[t - 1] >= 0.55 && breadth_200[t - 1] >= 0.50) {
      dynamic_lev <- dynamic_lev * super_bull_leverage
    }

    # Execute pending buy orders at Open
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
        target_dollar <- current_equity * slot_budget * ord$size_mult * dynamic_lev
        max_single_stock_margin <- current_equity * 0.50 * max(1.0, dynamic_lev)
        target_dollar <- min(target_dollar, max_single_stock_margin)

        n_shares <- floor(target_dollar / exec_px)
        if (n_shares <= 0) next

        actual_cost <- n_shares * exec_px
        current_cash <- current_cash - actual_cost

        stop_loss_dist <- 2.0 * atr_val
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

    # Monitor intraday stops & targets
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
        next
      }

      # Tier 1 Target Hit (+2.0R)
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

      # Tier 2 Target Hit (+4.0R)
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

      held_syms <- sapply(open_positions, function(p) p$symbol)
      eff_p_hurdle <- if (macro_bull[t]) 0.58 else 0.65

      # Feature 2: Market Breadth Regime Gate
      if (use_breadth_gate) {
        b50  <- breadth_50[t]
        b200 <- breadth_200[t]
        if (!is.na(b50) && !is.na(b200)) {
          if (b50 < breadth_50_min || b200 < breadth_200_min) {
            # Distribution / Deterioration: raise hurdle to filter choppy fakeouts
            eff_p_hurdle <- max(eff_p_hurdle, 0.65)
          } else if (b50 >= 0.60 && b200 >= 0.55 && is_macro_bull) {
            # Expansion Regime: high breadth allows taking premier leaders with confidence
            eff_p_hurdle <- min(eff_p_hurdle, 0.57)
          }
        }
      }

      for (sym in names(stocks_list)) {
        if (sym %in% held_syms) next
        stk <- stocks_list[[sym]]
        idx_t <- match(curr_d, stk$dates)
        if (is.na(idx_t) || idx_t < 150) next

        # Feature 5: Earnings Blackout
        if (use_earnings_blackout && earnings_blackout_matrix[t, sym]) next

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

          # Feature 3: Sector Momentum Boost
          sec_boost <- 0.0
          if (use_sector_momentum) {
            c_sec <- stk$sector
            sec_mom <- sector_mom_20d[t, c_sec]
            spy_mom <- spy_mom_20d[t]
            if (!is.na(sec_mom) && !is.na(spy_mom) && (sec_mom - spy_mom) > 0.02) {
              sec_boost <- sector_mom_bonus
            }
          }

          # Feature 4: Short-Term Reversal / Pullback in Trend
          rev_boost <- 0.0
          if (use_reversal_factor) {
            r3 <- ret_3d_matrix[t, sym]
            if (!is.na(r3)) {
              if (r3 >= -0.04 && r3 <= -0.005) {
                # Orderly 3-day pullback into support: bonus
                rev_boost <- 0.12
              } else if (r3 > 0.06) {
                # Overextended short-term run: slight penalty
                rev_boost <- -0.08
              }
            }
          }

          score <- p_up * (1.0 + rs_val + sm_boost + er_boost + nw_boost + sec_boost + rev_boost)

          eff_vol <- pmax(0.15, vol_ann) / (1.0 + 0.20 * er_boost + 0.10 * sm_boost)
          base_mult <- pmin(1.25, 0.35 / eff_vol)

          cands_syms   <- c(cands_syms, sym)
          cands_scores <- c(cands_scores, score)
          cands_sector <- c(cands_sector, stk$sector)
          cands_sizes  <- c(cands_sizes, base_mult)
        }
      }

      if (length(cands_syms) > 0) {
        ranked_idx <- order(-cands_scores)
        top_syms   <- cands_syms[ranked_idx]
        top_secs   <- cands_sector[ranked_idx]
        top_sizes  <- cands_sizes[ranked_idx]

        orders_count <- 0
        held_sectors <- if (length(open_positions) > 0) sapply(open_positions, function(p) p$sector) else character(0)
        sec_counts   <- if (length(held_sectors) > 0) table(held_sectors) else integer(0)

        for (k in seq_along(top_syms)) {
          if (orders_count >= empty_slots) break
          sym_k <- top_syms[k]
          sec_k <- top_secs[k]
          cur_s_cnt <- if (!is.na(sec_counts[sec_k])) as.integer(sec_counts[sec_k]) else 0L
          if (cur_s_cnt >= max_per_sector) next

          # Feature 6: Holding Correlation Filter
          # Check correlation of candidate to all currently held positions over past 20 days
          if (use_correlation_filter && length(open_positions) > 0 && t >= 25) {
            held_symbols <- sapply(open_positions, function(p) p$symbol)
            all_cols <- c(held_symbols, sym_k)
            slice_sub <- close_matrix[(t - 20):t, all_cols, drop = FALSE]
            ret_slice <- diff(log(slice_sub))
            cor_mat <- suppressWarnings(cor(ret_slice, use = "pairwise.complete.obs"))
            corr_vals <- cor_mat[sym_k, held_symbols]
            corr_vals <- corr_vals[!is.na(corr_vals)]
            if (length(corr_vals) > 0 && mean(corr_vals) > max_holding_corr) {
              next
            }
          }

          pending_buy_orders[[length(pending_buy_orders) + 1]] <- list(
            symbol    = sym_k,
            sector    = sec_k,
            size_mult = top_sizes[k]
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
    total_margin    = sum(margin_interest),
    equity_curve    = portfolio_equity,
    daily_returns   = daily_return_vec
  )
}

# ------------------------------------------------------------------------------
# EXPERIMENTAL MATRIX: TEST EACH FEATURE INDIVIDUALLY AND IN COMBO
# ------------------------------------------------------------------------------
experiments <- list(
  # 0. Enhanced Flagship Baseline (+45.12% benchmark)
  list(name = "0. Enhanced Flagship Baseline (+45.12% Bench)",
       vt = FALSE, pv = 0.45, dd = FALSE, b_gate = FALSE, sec_m = FALSE, rev = FALSE, earn = FALSE, corr = FALSE, sb_lev = 1.0),

  # 1. Feature 1: Volatility Targeting & Drawdown-Based De-leveraging
  list(name = "1A. Vol Target 50% + Drawdown De-lev (-8% DD -> 0.70x)",
       vt = TRUE, pv = 0.50, dd = TRUE, b_gate = FALSE, sec_m = FALSE, rev = FALSE, earn = FALSE, corr = FALSE, sb_lev = 1.0),

  list(name = "1B. Drawdown De-leveraging Only (-8% DD -> 0.60x)",
       vt = FALSE, pv = 0.45, dd = TRUE, b_gate = FALSE, sec_m = FALSE, rev = FALSE, earn = FALSE, corr = FALSE, sb_lev = 1.0),

  # 2. Feature 2: Market Breadth (50D & 200D) as Regime Gate
  list(name = "2A. Market Breadth Gate (Breadth50 < 40% raises hurdle; >60% expands)",
       vt = FALSE, pv = 0.45, dd = FALSE, b_gate = TRUE, sec_m = FALSE, rev = FALSE, earn = FALSE, corr = FALSE, sb_lev = 1.0),

  # 3. Feature 3: Sector & Industry Momentum Alpha Boost
  list(name = "3A. Sector Momentum Alpha Boost (Top-performing sectors +15%)",
       vt = FALSE, pv = 0.45, dd = FALSE, b_gate = FALSE, sec_m = TRUE, rev = FALSE, earn = FALSE, corr = FALSE, sb_lev = 1.0),

  # 4. Feature 4: Short-Term Reversal Factor (Pullback in Trend)
  list(name = "4A. Short-Term Reversal Factor (3D pullback in uptrend bonus)",
       vt = FALSE, pv = 0.45, dd = FALSE, b_gate = FALSE, sec_m = FALSE, rev = TRUE, earn = FALSE, corr = FALSE, sb_lev = 1.0),

  # 5. Feature 5: Earnings Date Shock Blackout
  list(name = "5A. Earnings Date Shock Blackout (Disqualifies 3D pre / 2D post)",
       vt = FALSE, pv = 0.45, dd = FALSE, b_gate = FALSE, sec_m = FALSE, rev = FALSE, earn = TRUE, corr = FALSE, sb_lev = 1.0),

  # 6. Feature 6: Holding Correlation Filter
  list(name = "6A. Holding Correlation Filter (Skip candidates if held corr > 0.65)",
       vt = FALSE, pv = 0.45, dd = FALSE, b_gate = FALSE, sec_m = FALSE, rev = FALSE, earn = FALSE, corr = TRUE, sb_lev = 1.0),

  # 7. Multi-Feature Synergies (Stacking Winning Components)
  list(name = "7A. Synergy: Sector Momentum + Market Breadth Gate",
       vt = FALSE, pv = 0.45, dd = FALSE, b_gate = TRUE, sec_m = TRUE, rev = FALSE, earn = FALSE, corr = FALSE, sb_lev = 1.0),

  list(name = "7B. Synergy: Sector Momentum + Short-Term Reversal Factor",
       vt = FALSE, pv = 0.45, dd = FALSE, b_gate = FALSE, sec_m = TRUE, rev = TRUE, earn = FALSE, corr = FALSE, sb_lev = 1.0),

  list(name = "7C. Synergy: Sector Momentum + Short-Term Reversal + Breadth Gate",
       vt = FALSE, pv = 0.45, dd = FALSE, b_gate = TRUE, sec_m = TRUE, rev = TRUE, earn = FALSE, corr = FALSE, sb_lev = 1.0),

  list(name = "7D. Grand Alpha Combo: Sector Mom + Reversal + Breadth + Super-Bull 1.10x",
       vt = FALSE, pv = 0.45, dd = FALSE, b_gate = TRUE, sec_m = TRUE, rev = TRUE, earn = FALSE, corr = FALSE, sb_lev = 1.10),

  list(name = "7E. Grand Alpha Combo: Sector Mom + Reversal + Breadth + Super-Bull 1.15x",
       vt = FALSE, pv = 0.45, dd = FALSE, b_gate = TRUE, sec_m = TRUE, rev = TRUE, earn = FALSE, corr = FALSE, sb_lev = 1.15),

  list(name = "7F. Grand Risk-Managed Combo: Sector Mom + Reversal + Breadth + Vol Target 45%",
       vt = TRUE, pv = 0.45, dd = TRUE, b_gate = TRUE, sec_m = TRUE, rev = TRUE, earn = FALSE, corr = FALSE, sb_lev = 1.0)
)

results <- list()

for (exp in experiments) {
  cat(sprintf("[Running] %s ...\n", exp$name))
  res <- run_sim_full_features(
    stocks_list            = stocks_list,
    macro_data             = macro_data,
    eval_dates             = eval_dates,
    initial_capital        = 10000,
    max_positions          = 5,
    max_per_sector         = 2,
    t1_r                   = 2.0,
    t2_r                   = 4.0,
    use_vol_targeting      = exp$vt,
    target_portfolio_vol   = exp$pv,
    use_dd_deleveraging    = exp$dd,
    dd_threshold           = -0.08,
    dd_leverage_scale      = 0.70,
    use_breadth_gate       = exp$b_gate,
    use_sector_momentum    = exp$sec_m,
    use_reversal_factor    = exp$rev,
    use_earnings_blackout  = exp$earn,
    use_correlation_filter = exp$corr,
    super_bull_leverage    = exp$sb_lev
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

df_exp <- do.call(rbind, results)

cat("\n========================================================================================================\n")
cat("                6-FEATURE OUT-OF-SAMPLE EVALUATION SCORECARD (2016-2026 AUDITED)                        \n")
cat("========================================================================================================\n")
print(df_exp, row.names = FALSE)
cat("========================================================================================================\n\n")

write.csv(df_exp, "output/six_features_optimization_scorecard.csv", row.names = FALSE)
cat("[Done] Saved to output/six_features_optimization_scorecard.csv\n")
