#!/usr/bin/env Rscript
# ==============================================================================
# scratch/test_refined_features.R
# Refined Optimization: Raw RS + Net Wick Demand + Dynamic Vol Targeting (35% & 40%)
# ==============================================================================

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(TTR)
  library(jsonlite)
})

source("R/08_metrics.R")

cat("[Loading precomputed clean environment...]\n")
env_data <- readRDS("scratch/clean_backtest_env.rds")
stocks_list <- env_data$stocks_list
macro_data  <- env_data$macro_data
eval_dates  <- env_data$eval_dates

run_refined_sim <- function(stocks_list,
                            macro_data,
                            eval_dates,
                            initial_capital = 10000,
                            max_positions = 5,
                            max_per_sector = 2,
                            leverage = 1.0,
                            use_vol_targeting = FALSE,
                            target_portfolio_vol = 0.35,
                            use_net_wick = TRUE,
                            use_bear_hedge = TRUE,
                            p_long = 0.58) {

  n_days <- length(eval_dates)
  spy_cl   <- macro_data$spy_close
  qqq_cl   <- macro_data$qqq_close
  rf_vec   <- macro_data$rf_daily
  gld_cl   <- macro_data$gld_close

  spy_ret <- (spy_cl / lag.xts(spy_cl, 1)) - 1
  spy_ret[is.na(spy_ret)] <- 0
  qqq_ret <- (qqq_cl / lag.xts(qqq_cl, 1)) - 1
  qqq_ret[is.na(qqq_ret)] <- 0

  spy_sub <- as.numeric(spy_ret[eval_dates])
  qqq_sub <- as.numeric(qqq_ret[eval_dates])
  spy_sub[is.na(spy_sub)] <- 0
  qqq_sub[is.na(qqq_sub)] <- 0
  rf_sub  <- as.numeric(rf_vec[eval_dates])

  spy_sma_n <- min(200, max(5, floor(length(spy_cl) / 2)))
  spy_sma200 <- SMA(spy_cl, n = spy_sma_n)
  macro_bull <- as.numeric(spy_cl[eval_dates]) > as.numeric(spy_sma200[eval_dates])
  macro_bull[is.na(macro_bull)] <- TRUE
  macro_bull <- head(macro_bull, n_days)

  qqq_sma_n <- min(50, max(5, floor(length(qqq_cl) / 2)))
  qqq_sma50 <- SMA(qqq_cl, n = qqq_sma_n)
  qqq_bear  <- as.numeric(qqq_cl[eval_dates]) < as.numeric(qqq_sma50[eval_dates])
  qqq_bear[is.na(qqq_bear)] <- FALSE
  qqq_bear <- head(qqq_bear, n_days)

  gld_bear <- rep(TRUE, n_days)
  if (!is.null(gld_cl)) {
    gld_aligned <- na.locf(gld_cl[eval_dates], na.rm = FALSE)
    if (nrow(gld_aligned) > 0) {
      gld_sma_n <- min(50, max(5, floor(nrow(gld_aligned) / 2)))
      gld_sma50 <- SMA(gld_aligned, n = gld_sma_n)
      g_diff <- as.numeric(gld_aligned) < as.numeric(gld_sma50)
      g_diff[is.na(g_diff)] <- TRUE
      gld_bear <- head(c(g_diff, rep(TRUE, n_days)), n_days)
    }
  }
  dual_bear <- (qqq_bear & gld_bear)

  inv_hedge_daily <- pmin(0.15, pmax(-0.15, -3.0 * qqq_sub - (0.0095 / 252)))

  portfolio_equity   <- numeric(n_days)
  portfolio_cash     <- numeric(n_days)
  portfolio_invested <- numeric(n_days)
  portfolio_borrowed <- numeric(n_days)
  daily_return_vec   <- numeric(n_days)
  margin_interest    <- numeric(n_days)
  cash_yield_vec     <- numeric(n_days)

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

  portfolio_equity[1]   <- current_equity
  portfolio_cash[1]     <- current_cash
  portfolio_invested[1] <- 0
  portfolio_borrowed[1] <- 0

  pending_buy_orders <- list()

  for (t in 2:n_days) {
    prev_d <- eval_dates[t - 1]
    curr_d <- eval_dates[t]

    is_macro_bull <- macro_bull[t - 1]
    is_dual_bear  <- dual_bear[t - 1]

    # Dynamic Volatility Targeting
    dynamic_lev <- leverage
    if (use_vol_targeting && t >= 22) {
      recent_rets <- daily_return_vec[(t - 20):(t - 1)]
      r_vol <- sd(recent_rets) * sqrt(252)
      if (!is.na(r_vol) && r_vol > 0.05) {
        dynamic_lev <- min(leverage, max(0.25, target_portfolio_vol / r_vol))
      }
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
        max_single_stock_margin <- current_equity * 0.50 * dynamic_lev
        target_dollar <- min(target_dollar, max_single_stock_margin)

        n_shares <- floor(target_dollar / exec_px)
        if (n_shares <= 0) next

        actual_cost <- n_shares * exec_px
        current_cash <- current_cash - actual_cost

        stop_loss_px <- exec_px - 2.0 * atr_val
        t1_px        <- exec_px + 1.5 * (2.0 * atr_val)
        t2_px        <- exec_px + 3.0 * (2.0 * atr_val)

        open_positions[[length(open_positions) + 1]] <- list(
          symbol          = ord$symbol,
          sector          = ord$sector,
          entry_date      = curr_d,
          entry_price     = exec_px,
          shares          = n_shares,
          original_shares = n_shares,
          shares_pct      = 1.0,
          stop_price      = stop_loss_px,
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
    realized_pnl_today <- 0.0
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
      chand_stop <- high_wm - 2.5 * atr_val

      # Stop loss
      if (low_px <= pos$stop_price) {
        exit_px <- min(open_px, pos$stop_price)
        proceeds <- pos$shares * exit_px * (1 - friction_rate)
        trade_pnl <- (exit_px * (1 - friction_rate) - pos$entry_price) * pos$shares
        realized_pnl_today <- realized_pnl_today + trade_pnl
        current_cash <- current_cash + proceeds
        trade_log[[length(trade_log) + 1]] <- list(
          symbol = pos$symbol, entry_date = pos$entry_date, exit_date = curr_d,
          entry_price = pos$entry_price, exit_price = exit_px,
          shares = pos$shares, pnl = trade_pnl, reason = "STOP_LOSS",
          days_held = pos$days_held, win = (trade_pnl > 0)
        )
        next
      }

      # T1 Target (+1.5R)
      if (!pos$t1_hit && high_px >= pos$t1_price) {
        pos$t1_hit <- TRUE
        shares_to_sell <- pos$shares * 0.50
        exit_px <- pos$t1_price
        proceeds <- shares_to_sell * exit_px * (1 - friction_rate)
        trade_pnl <- (exit_px * (1 - friction_rate) - pos$entry_price) * shares_to_sell
        realized_pnl_today <- realized_pnl_today + trade_pnl
        current_cash <- current_cash + proceeds
        pos$shares <- pos$shares - shares_to_sell
        pos$shares_pct <- 0.50
        pos$stop_price <- max(pos$entry_price, chand_stop)
        trade_log[[length(trade_log) + 1]] <- list(
          symbol = pos$symbol, entry_date = pos$entry_date, exit_date = curr_d,
          entry_price = pos$entry_price, exit_price = exit_px,
          shares = shares_to_sell, pnl = trade_pnl, reason = "TARGET_1_SCALE",
          days_held = pos$days_held, win = TRUE
        )
      }

      # T2 Target (+3.0R)
      if (pos$t1_hit && !pos$t2_hit && high_px >= pos$t2_price) {
        pos$t2_hit <- TRUE
        pos$is_runner <- TRUE
        shares_to_sell <- min(pos$original_shares * 0.25, pos$shares)
        exit_px <- pos$t2_price
        proceeds <- shares_to_sell * exit_px * (1 - friction_rate)
        trade_pnl <- (exit_px * (1 - friction_rate) - pos$entry_price) * shares_to_sell
        realized_pnl_today <- realized_pnl_today + trade_pnl
        current_cash <- current_cash + proceeds
        pos$shares <- pos$shares - shares_to_sell
        pos$shares_pct <- 0.25
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
        realized_pnl_today <- realized_pnl_today + trade_pnl
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

    # Hedge
    hedge_pnl <- 0.0
    if (use_bear_hedge && is_dual_bear && idle_cash > 0) {
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

    new_equity <- current_equity + (total_assets - (portfolio_cash[t - 1] + portfolio_invested[t - 1])) -
                  daily_margin_interest + daily_cash_yield + hedge_pnl
    if (new_equity <= 0) new_equity <- 1.0

    daily_ret <- (new_equity / current_equity) - 1.0
    current_equity <- new_equity
    peak_equity    <- max(peak_equity, current_equity)

    portfolio_equity[t]   <- current_equity
    portfolio_cash[t]     <- current_cash
    portfolio_invested[t] <- total_stock_value
    portfolio_borrowed[t] <- borrowed_debt
    daily_return_vec[t]   <- daily_ret
    margin_interest[t]    <- daily_margin_interest
    cash_yield_vec[t]     <- daily_cash_yield

    # Order generation
    empty_slots <- max_positions - length(open_positions)
    if (empty_slots > 0 && t < n_days) {
      cands_syms   <- character(0)
      cands_scores <- numeric(0)
      cands_sector <- character(0)
      cands_sizes  <- numeric(0)

      held_syms <- sapply(open_positions, function(p) p$symbol)
      eff_p_hurdle <- if (macro_bull[t]) p_long else 0.65

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
          nw_boost <- if (use_net_wick && !is.na(nwick)) pmax(0, nwick * 0.10) else 0

          # Clean Quality Score: Raw Relative Strength + Net Wick Demand + Sortino/ER boosts
          score <- p_up * (1.0 + rs_val + sm_boost + er_boost + nw_boost)

          eff_vol <- pmax(0.15, vol_ann) / (1.0 + 0.20 * er_boost + 0.10 * sm_boost)
          size_mult <- pmin(1.25, 0.35 / eff_vol)

          cands_syms   <- c(cands_syms, sym)
          cands_scores <- c(cands_scores, score)
          cands_sector <- c(cands_sector, stk$sector)
          cands_sizes  <- c(cands_sizes, size_mult)
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

          pending_buy_orders[[length(pending_buy_orders) + 1]] <- list(
            symbol    = sym_k,
            sector    = sec_k,
            score     = cands_scores[ranked_idx[k]],
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
  sortino <- if (length(downside_ret) > 0) {
    (mean(excess_ret) / sd(downside_ret)) * sqrt(252)
  } else NA
  calmar <- if (max_dd > 0) cagr / max_dd else NA

  win_trades <- sum(sapply(trade_log, function(x) x$win))
  total_trades <- length(trade_log)
  win_rate <- if (total_trades > 0) (win_trades / total_trades) * 100 else 0

  return(list(
    terminal_wealth = tail(portfolio_equity, 1),
    cagr            = cagr,
    max_dd          = max_dd,
    sharpe          = sharpe,
    sortino         = sortino,
    calmar          = calmar,
    annual_vol      = annual_vol,
    win_rate        = win_rate,
    total_trades    = total_trades,
    total_margin    = sum(margin_interest)
  ))
}

refined_configs <- list(
  list(name = "1. 5 Slots + Dual-Bear (1.0x Cash, No Vol Target)", mp = 5, lev = 1.0,  vt = FALSE, pv = 0.35, nw = FALSE),
  list(name = "2. 5 Slots + Dual-Bear + Net Wick Demand (1.0x Cash)", mp = 5, lev = 1.0,  vt = FALSE, pv = 0.35, nw = TRUE),
  list(name = "3. 5 Slots + Dual-Bear + Net Wick + Vol Target 35%", mp = 5, lev = 1.25, vt = TRUE,  pv = 0.35, nw = TRUE),
  list(name = "4. 5 Slots + Dual-Bear + Net Wick + Vol Target 40%", mp = 5, lev = 1.25, vt = TRUE,  pv = 0.40, nw = TRUE),
  list(name = "5. 6 Slots + Dual-Bear + Net Wick (1.0x Cash)",      mp = 6, lev = 1.0,  vt = FALSE, pv = 0.35, nw = TRUE),
  list(name = "6. 6 Slots + Dual-Bear + Net Wick + Vol Target 35%", mp = 6, lev = 1.25, vt = TRUE,  pv = 0.35, nw = TRUE),
  list(name = "7. 6 Slots + Dual-Bear + Net Wick + Vol Target 40%", mp = 6, lev = 1.25, vt = TRUE,  pv = 0.40, nw = TRUE)
)

results <- list()
for (cfg in refined_configs) {
  cat(sprintf("[Running] %s ...\n", cfg$name))
  res <- run_refined_sim(
    stocks_list          = stocks_list,
    macro_data           = macro_data,
    eval_dates           = eval_dates,
    initial_capital      = 10000,
    max_positions        = cfg$mp,
    max_per_sector       = 2,
    leverage             = cfg$lev,
    use_vol_targeting    = cfg$vt,
    target_portfolio_vol = cfg$pv,
    use_net_wick         = cfg$nw,
    use_bear_hedge       = TRUE,
    p_long               = 0.58
  )
  results[[length(results) + 1]] <- data.frame(
    Configuration   = cfg$name,
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

df_res <- do.call(rbind, results)

cat("\n========================================================================================================\n")
cat("                REFINED FEATURE OPTIMIZATION SCORECARD: NET WICK & TARGET VOL                           \n")
cat("========================================================================================================\n")
print(df_res, row.names = FALSE)
cat("========================================================================================================\n\n")

write.csv(df_res, "output/refined_feature_scorecard.csv", row.names = FALSE)
cat("[Done] Saved to output/refined_feature_scorecard.csv\n")
