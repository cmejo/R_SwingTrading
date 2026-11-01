#!/usr/bin/env Rscript
# ==============================================================================
# scratch/test_moc_vs_open_and_taxes.R
# 1. Compare Day t Close (MOC) Execution vs. Day t+1 Open Execution
# 2. Complete 10-Year Tax-Adjusted Backtest across Gross, TTS (28.4%), and NYC (51.2%)
# 3. Compare with QQQ, 70% QQQ / 30% VTI, and SPY
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

# Load VTI
vti_raw <- readRDS("data/data_10yr/data_vti.rds")
vti_cl  <- Cl(vti_raw)[eval_dates]
vti_ret <- as.numeric((vti_cl / lag.xts(vti_cl, 1)) - 1)
vti_ret[is.na(vti_ret)] <- 0

qqq_cl  <- macro_data$qqq_close
qqq_ret <- as.numeric((qqq_cl / lag.xts(qqq_cl, 1)) - 1)
qqq_ret[is.na(qqq_ret)] <- 0

spy_cl  <- macro_data$spy_close
spy_ret <- as.numeric((spy_cl / lag.xts(spy_cl, 1)) - 1)
spy_ret[is.na(spy_ret)] <- 0

# 70% QQQ + 30% VTI Blend
blend_70_30_ret <- 0.70 * qqq_ret + 0.30 * vti_ret

cat(sprintf("[Loaded %d stocks, %d sessions (2016-2026)]\n", length(stocks_list), length(eval_dates)))

# Simulation supporting entry_timing: "next_open" vs "same_close_moc"
run_sim_custom <- function(stocks_list,
                           macro_data,
                           eval_dates,
                           initial_capital = 10000,
                           max_positions = 6,
                           max_per_sector = 2,
                           leverage = 1.25,
                           use_vol_targeting = TRUE,
                           target_portfolio_vol = 0.35,
                           entry_timing = "next_open") {

  n_days <- length(eval_dates)
  spy_cl   <- macro_data$spy_close
  qqq_cl   <- macro_data$qqq_close
  rf_vec   <- macro_data$rf_daily
  gld_cl   <- macro_data$gld_close

  spy_ret <- as.numeric((spy_cl / lag.xts(spy_cl, 1)) - 1); spy_ret[is.na(spy_ret)] <- 0
  qqq_ret <- as.numeric((qqq_cl / lag.xts(qqq_cl, 1)) - 1); qqq_ret[is.na(qqq_ret)] <- 0
  rf_sub  <- as.numeric(rf_vec[eval_dates])

  spy_sma200 <- SMA(spy_cl, n = min(200, max(5, floor(length(spy_cl) / 2))))
  macro_bull <- as.numeric(spy_cl[eval_dates]) > as.numeric(spy_sma200[eval_dates])
  macro_bull[is.na(macro_bull)] <- TRUE

  qqq_sma50 <- SMA(qqq_cl, n = min(50, max(5, floor(length(qqq_cl) / 2))))
  qqq_bear  <- as.numeric(qqq_cl[eval_dates]) < as.numeric(qqq_sma50[eval_dates])
  qqq_bear[is.na(qqq_bear)] <- FALSE

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

  inv_hedge_daily <- pmin(0.15, pmax(-0.15, -3.0 * qqq_ret - (0.0095 / 252)))

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
    curr_d <- eval_dates[t]
    is_dual_bear <- dual_bear[t - 1]

    dynamic_lev <- leverage
    if (use_vol_targeting && t >= 22) {
      recent_rets <- daily_return_vec[(t - 20):(t - 1)]
      r_vol <- sd(recent_rets) * sqrt(252)
      if (!is.na(r_vol) && r_vol > 0.05) {
        dynamic_lev <- min(leverage, max(0.25, target_portfolio_vol / r_vol))
      }
    }

    # STEP 1: Morning Execution of Pending Orders
    if (length(pending_buy_orders) > 0 && length(open_positions) < max_positions) {
      for (ord in pending_buy_orders) {
        if (length(open_positions) >= max_positions) break
        stk <- stocks_list[[ord$symbol]]
        idx_t <- match(curr_d, stk$dates)
        if (is.na(idx_t)) next

        fill_px <- if (entry_timing == "same_close_moc") ord$moc_price else stk$open[idx_t]
        atr_val <- stk$atr[idx_t]
        if (is.na(fill_px) || fill_px <= 0 || is.na(atr_val) || atr_val <= 0) next

        exec_px <- fill_px * (1 + friction_rate)
        target_dollar <- current_equity * slot_budget * ord$size_mult * dynamic_lev
        max_margin <- current_equity * 0.50 * dynamic_lev
        target_dollar <- min(target_dollar, max_margin)

        n_shares <- floor(target_dollar / exec_px)
        if (n_shares <= 0) next

        current_cash <- current_cash - (n_shares * exec_px)

        open_positions[[length(open_positions) + 1]] <- list(
          symbol          = ord$symbol,
          sector          = ord$sector,
          entry_date      = curr_d,
          entry_price     = exec_px,
          shares          = n_shares,
          original_shares = n_shares,
          shares_pct      = 1.0,
          stop_price      = exec_px - 2.0 * atr_val,
          t1_price        = exec_px + 3.0 * atr_val,
          t2_price        = exec_px + 6.0 * atr_val,
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

    # STEP 2: Monitor intraday stops & targets
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
        current_cash <- current_cash + proceeds
        trade_log[[length(trade_log) + 1]] <- list(win = (trade_pnl > 0))
        next
      }

      # T1 Target (+1.5R)
      if (!pos$t1_hit && high_px >= pos$t1_price) {
        pos$t1_hit <- TRUE
        s_sell <- pos$shares * 0.50
        exit_px <- pos$t1_price
        proceeds <- s_sell * exit_px * (1 - friction_rate)
        current_cash <- current_cash + proceeds
        pos$shares <- pos$shares - s_sell
        pos$shares_pct <- 0.50
        pos$stop_price <- max(pos$entry_price, chand_stop)
        trade_log[[length(trade_log) + 1]] <- list(win = TRUE)
      }

      # T2 Target (+3.0R)
      if (pos$t1_hit && !pos$t2_hit && high_px >= pos$t2_price) {
        pos$t2_hit <- TRUE
        pos$is_runner <- TRUE
        s_sell <- min(pos$original_shares * 0.25, pos$shares)
        exit_px <- pos$t2_price
        proceeds <- s_sell * exit_px * (1 - friction_rate)
        current_cash <- current_cash + proceeds
        pos$shares <- pos$shares - s_sell
        pos$shares_pct <- 0.25
        pos$stop_price <- max(pos$stop_price, chand_stop)
        trade_log[[length(trade_log) + 1]] <- list(win = TRUE)
      }

      if (pos$t1_hit) pos$stop_price <- max(pos$stop_price, chand_stop)

      # Time stop
      max_hold <- if (pos$is_runner) 20 else 7
      if (pos$days_held >= max_hold) {
        exit_px <- close_px * (1 - friction_rate)
        current_cash <- current_cash + (pos$shares * exit_px)
        trade_pnl <- (exit_px - pos$entry_price) * pos$shares
        trade_log[[length(trade_log) + 1]] <- list(win = (trade_pnl > 0))
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
        if ((hedge_cum - 1.0) <= -0.10) hedge_stopped <- TRUE
      }
    } else {
      in_hedge_ep   <- FALSE
      hedge_day     <- 0
      hedge_cum     <- 1.0
      hedge_stopped <- FALSE
    }

    new_equity <- current_equity + (total_assets - (portfolio_cash[t - 1] + (portfolio_equity[t - 1] - portfolio_cash[t - 1]))) -
                  daily_margin_interest + daily_cash_yield + hedge_pnl
    if (new_equity <= 0) new_equity <- 1.0

    daily_ret <- (new_equity / current_equity) - 1.0
    current_equity <- new_equity
    peak_equity    <- max(peak_equity, current_equity)

    portfolio_equity[t] <- current_equity
    portfolio_cash[t]   <- current_cash
    daily_return_vec[t] <- daily_ret
    margin_interest[t]  <- daily_margin_interest

    # STEP 3: Order Generation Post-Close on Day t
    empty_slots <- max_positions - length(open_positions)
    if (empty_slots > 0 && t < n_days) {
      cands_syms   <- character(0)
      cands_scores <- numeric(0)
      cands_sector <- character(0)
      cands_sizes  <- numeric(0)
      cands_mocs   <- numeric(0)

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
          nw_boost <- if (!is.na(nwick)) pmax(0, nwick * 0.10) else 0

          score <- p_up * (1.0 + rs_val + sm_boost + er_boost + nw_boost)
          eff_vol <- pmax(0.15, vol_ann) / (1.0 + 0.20 * er_boost + 0.10 * sm_boost)
          size_mult <- pmin(1.25, 0.35 / eff_vol)

          cands_syms   <- c(cands_syms, sym)
          cands_scores <- c(cands_scores, score)
          cands_sector <- c(cands_sector, stk$sector)
          cands_sizes  <- c(cands_sizes, size_mult)
          cands_mocs   <- c(cands_mocs, stk$close[idx_t])
        }
      }

      if (length(cands_syms) > 0) {
        ranked_idx <- order(-cands_scores)
        top_syms   <- cands_syms[ranked_idx]
        top_secs   <- cands_sector[ranked_idx]
        top_sizes  <- cands_sizes[ranked_idx]
        top_mocs   <- cands_mocs[ranked_idx]

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
            moc_price = top_mocs[k]
          )
          sec_counts[sec_k] <- cur_s_cnt + 1L
          orders_count <- orders_count + 1
        }
      }
    }
  }

  return(list(
    daily_returns   = daily_return_vec,
    terminal_wealth = tail(portfolio_equity, 1),
    equity_curve    = portfolio_equity,
    trade_log       = trade_log
  ))
}

# Accurate Tax Engine
calc_tax_series <- function(daily_ret_vec, dates_vec, tax_rate, starting_cap = 10000) {
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
  
  cum_ret <- (cap_curr / starting_cap) - 1
  cagr <- ((cap_curr / starting_cap)^(252 / n) - 1) * 100
  cum_max <- cummax(equity_curve)
  dd <- (equity_curve - cum_max) / cum_max
  max_dd <- abs(min(dd)) * 100
  
  effective_ret <- numeric(n)
  effective_ret[1] <- (equity_curve[1] / starting_cap) - 1
  if (n > 1) effective_ret[2:n] <- (equity_curve[2:n] / equity_curve[1:(n - 1)]) - 1
  ann_vol <- sd(effective_ret) * sqrt(252) * 100
  sharpe <- (mean(effective_ret) / sd(effective_ret)) * sqrt(252)
  downside_ret <- effective_ret[effective_ret < 0]
  sortino <- if (length(downside_ret) > 0) (mean(effective_ret) / sd(downside_ret)) * sqrt(252) else NA
  
  list(
    terminal_wealth = cap_curr,
    cagr            = cagr,
    max_dd          = max_dd,
    sharpe          = sharpe,
    sortino         = sortino,
    ann_vol         = ann_vol,
    equity_curve    = equity_curve
  )
}

calc_benchmark_tax <- function(bench_ret_vec, dates_vec, tax_rate_ltcg, div_yield_ann = 0.010, tax_rate_div = 0.341, starting_cap = 10000) {
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
  
  cum_ret <- (cap_curr / starting_cap) - 1
  cagr <- ((cap_curr / starting_cap)^(252 / n) - 1) * 100
  cum_max <- cummax(equity_curve)
  dd <- (equity_curve - cum_max) / cum_max
  max_dd <- abs(min(dd)) * 100
  
  effective_ret <- numeric(n)
  effective_ret[1] <- (equity_curve[1] / starting_cap) - 1
  if (n > 1) effective_ret[2:n] <- (equity_curve[2:n] / equity_curve[1:(n - 1)]) - 1
  ann_vol <- sd(effective_ret) * sqrt(252) * 100
  sharpe <- (mean(effective_ret) / sd(effective_ret)) * sqrt(252)
  downside_ret <- effective_ret[effective_ret < 0]
  sortino <- if (length(downside_ret) > 0) (mean(effective_ret) / sd(downside_ret)) * sqrt(252) else NA
  
  list(
    terminal_wealth = cap_curr,
    cagr            = cagr,
    max_dd          = max_dd,
    sharpe          = sharpe,
    sortino         = sortino,
    ann_vol         = ann_vol,
    equity_curve    = equity_curve
  )
}

# PART 1: TEST MOC (3:45 PM SCAN / 4:00 PM CLOSE) vs. NEXT-DAY OPEN
cat("\n========================================================================================\n")
cat("          PART 1: SCANNER TIMING TEST — DAY t CLOSE (MOC) vs. DAY t+1 OPEN              \n")
cat("========================================================================================\n")

cat("[Running] Production Robust (Next-Day Open Fills) ...\n")
sim_open <- run_sim_custom(stocks_list, macro_data, eval_dates, max_positions = 6, leverage = 1.25, use_vol_targeting = TRUE, target_portfolio_vol = 0.35, entry_timing = "next_open")

cat("[Running] Production Robust (Day t Close MOC Fills) ...\n")
sim_moc  <- run_sim_custom(stocks_list, macro_data, eval_dates, max_positions = 6, leverage = 1.25, use_vol_targeting = TRUE, target_portfolio_vol = 0.35, entry_timing = "same_close_moc")

cat("[Running] Unthrottled 5-Slot (Next-Day Open Fills) ...\n")
sim_unth_open <- run_sim_custom(stocks_list, macro_data, eval_dates, max_positions = 5, leverage = 1.00, use_vol_targeting = FALSE, entry_timing = "next_open")

cat("[Running] Unthrottled 5-Slot (Day t Close MOC Fills) ...\n")
sim_unth_moc  <- run_sim_custom(stocks_list, macro_data, eval_dates, max_positions = 5, leverage = 1.00, use_vol_targeting = FALSE, entry_timing = "same_close_moc")

# PART 2: TAX SIMULATION ACROSS STRATEGIES & BENCHMARKS
cat("\n========================================================================================\n")
cat("          PART 2: 10-YEAR AUDITED TAX-ADJUSTED BACKTEST (2016-2026)                      \n")
cat("========================================================================================\n")

strategies <- list(
  list(name = "1. Production Robust (6 Slots, Vol Target 35%, Next-Day Open)", rets = sim_open$daily_returns, is_bench = FALSE),
  list(name = "2. Production Robust (6 Slots, Vol Target 35%, Day t Close MOC)", rets = sim_moc$daily_returns, is_bench = FALSE),
  list(name = "3. Clean Diversified + Dual-Bear (Unthrottled 5 Slots, Next-Day Open)", rets = sim_unth_open$daily_returns, is_bench = FALSE),
  list(name = "4. Clean Diversified + Dual-Bear (Unthrottled 5 Slots, Day t Close MOC)", rets = sim_unth_moc$daily_returns, is_bench = FALSE),
  list(name = "5. Benchmark: Nasdaq-100 (QQQ Buy & Hold)", rets = qqq_ret, is_bench = TRUE, div = 0.007),
  list(name = "6. Benchmark: 70% QQQ / 30% VTI (Balanced Growth)", rets = blend_70_30_ret, is_bench = TRUE, div = 0.009),
  list(name = "7. Benchmark: S&P 500 (SPY Buy & Hold)", rets = spy_ret, is_bench = TRUE, div = 0.015)
)

tax_scorecard <- list()

for (st in strategies) {
  # Gross (0% Tax)
  m_gross <- calc_tax_series(st$rets, eval_dates, tax_rate = 0.0)
  
  # TTS (28.4% Tax for swing, 23.8% LTCG for benchmark)
  m_tts <- if (!st$is_bench) {
    calc_tax_series(st$rets, eval_dates, tax_rate = 0.284)
  } else {
    calc_benchmark_tax(st$rets, eval_dates, tax_rate_ltcg = 0.238, div_yield_ann = st$div, tax_rate_div = 0.238)
  }
  
  # NYC Taxable (51.2% Tax for swing, 34.1% LTCG for benchmark)
  m_nyc <- if (!st$is_bench) {
    calc_tax_series(st$rets, eval_dates, tax_rate = 0.512)
  } else {
    calc_benchmark_tax(st$rets, eval_dates, tax_rate_ltcg = 0.341, div_yield_ann = st$div, tax_rate_div = 0.341)
  }
  
  tax_scorecard[[length(tax_scorecard) + 1]] <- data.frame(
    Strategy         = st$name,
    Gross_Terminal   = sprintf("$%s", format(round(m_gross$terminal_wealth, 2), big.mark = ",", scientific = FALSE)),
    Gross_CAGR       = sprintf("%+.2f%%", m_gross$cagr),
    TTS_Terminal     = sprintf("$%s", format(round(m_tts$terminal_wealth, 2), big.mark = ",", scientific = FALSE)),
    TTS_CAGR         = sprintf("%+.2f%%", m_tts$cagr),
    NYC_Terminal     = sprintf("$%s", format(round(m_nyc$terminal_wealth, 2), big.mark = ",", scientific = FALSE)),
    NYC_CAGR         = sprintf("%+.2f%%", m_nyc$cagr),
    Gross_MaxDD      = sprintf("-%.2f%%", m_gross$max_dd),
    NYC_MaxDD        = sprintf("-%.2f%%", m_nyc$max_dd),
    Sharpe_Gross     = sprintf("%.2f", m_gross$sharpe),
    Sortino_Gross    = sprintf("%.2f", m_gross$sortino),
    stringsAsFactors = FALSE
  )
}

df_tax <- do.call(rbind, tax_scorecard)

print(df_tax, row.names = FALSE)
cat("========================================================================================\n\n")

write.csv(df_tax, "output/tax_adjusted_10yr_scorecard.csv", row.names = FALSE)
cat("[Saved to output/tax_adjusted_10yr_scorecard.csv]\n")
