#!/usr/bin/env Rscript
# ==============================================================================
# Calculate Performance for $8,230 starting on:
# 1. 1995-01-01 to Present (2026)
# 2. 1999-01-01 to Present (2026)
# 3. 2005-01-01 to Present (2026)
# 4. 2010-01-01 to Present (2026)
#
# Tracks:
# - Current Model: Gross (0% / Tax-Advantaged), TTS (28.4%), NYC (51.2%)
# - Baseline Model: Gross, TTS (28.4%), NYC (51.2%)
# - Benchmarks: QQQ Gross & NYC, VTI Gross & NYC
# ==============================================================================

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(jsonlite)
})

source("R/08_metrics.R")

res <- readRDS("output/comparison_40yr_baseline_vs_current_results.rds")
full_dates <- res$dates
ret_current <- res$ret_current
ret_baseline <- res$ret_baseline

# Benchmark data
HIST_DIR <- "data/data_historical"
ndx_raw   <- readRDS(file.path(HIST_DIR, "data_^ndx.rds"))
qqq_raw   <- readRDS(file.path(HIST_DIR, "data_qqq.rds"))
vfinx_raw <- readRDS(file.path(HIST_DIR, "data_vfinx.rds"))
vti_raw   <- readRDS(file.path(HIST_DIR, "data_vti.rds"))

ndx_cl <- Cl(ndx_raw); qqq_cl <- Cl(qqq_raw)
ndx_ret <- na.omit(ndx_cl / lag.xts(ndx_cl, 1) - 1) - (0.0020 / 252)
qqq_ret <- na.omit(qqq_cl / lag.xts(qqq_cl, 1) - 1)
qqq_start <- index(qqq_ret)[1]
qqq_spliced <- rbind(ndx_ret[index(ndx_ret) < qqq_start], qqq_ret)
qqq_full <- as.numeric(qqq_spliced[match(full_dates, index(qqq_spliced))])

vfinx_cl <- Cl(vfinx_raw); vti_cl <- Cl(vti_raw)
vfinx_ret <- na.omit(vfinx_cl / lag.xts(vfinx_cl, 1) - 1) - (0.0003 / 252)
vti_ret   <- na.omit(vti_cl / lag.xts(vti_cl, 1) - 1)
vti_start <- index(vti_ret)[1]
vti_spliced <- rbind(vfinx_ret[index(vfinx_ret) < vti_start], vti_ret)
vti_full <- as.numeric(vti_spliced[match(full_dates, index(vti_spliced))])

calc_tax_series <- function(ret_vec, dates_vec, tax_rate = 0.0, starting_cap = 8230.0) {
  n <- length(ret_vec)
  equity_curve <- numeric(n)
  cap_curr <- starting_cap
  years <- as.numeric(format(dates_vec, "%Y"))
  unique_years <- unique(years)
  loss_carryforward <- 0
  
  for (yr in unique_years) {
    yr_indices <- which(years == yr)
    yr_start_cap <- cap_curr
    for (idx in yr_indices) {
      cap_curr <- cap_curr * (1 + ret_vec[idx])
      equity_curve[idx] <- cap_curr
    }
    net_gain <- cap_curr - yr_start_cap
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
  
  list(terminal_wealth = cap_curr, equity_curve = equity_curve, effective_ret = effective_ret)
}

calc_benchmark_tax <- function(bench_ret_vec, dates_vec, tax_rate_ltcg, div_yield_ann = 0.015, tax_rate_div = 0.341, starting_cap = 8230.0) {
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
  
  list(terminal_wealth = cap_curr, equity_curve = equity_curve, effective_ret = effective_ret)
}

get_metrics <- function(ret_vec, d_vec, name) {
  m <- calc_performance_metrics(xts(ret_vec, order.by = d_vec), name = name)$raw
  list(
    cagr    = m$ann_ret * 100,
    cum_ret = m$cum_ret * 100,
    max_dd  = m$max_dd * 100,
    sharpe  = m$sharpe,
    sortino = m$sortino,
    calmar  = ifelse(!is.na(m$max_dd) && m$max_dd > 0, (m$ann_ret * 100) / (m$max_dd * 100), NA),
    ann_vol = m$ann_vol * 100,
    win_pct = m$win_rate * 100
  )
}

start_dates <- as.Date(c("1995-01-01", "1999-01-01", "2005-01-01", "2010-01-01"))

out_list <- list()

for (sd in start_dates) {
  s_date <- as.Date(sd, origin="1970-01-01")
  mask <- full_dates >= s_date
  sub_dates <- full_dates[mask]
  sub_ret_curr <- ret_current[mask]
  sub_ret_base <- ret_baseline[mask]
  sub_qqq <- qqq_full[mask]
  sub_vti <- vti_full[mask]
  
  n_days <- length(sub_dates)
  years_exact <- as.numeric(difftime(tail(sub_dates, 1), head(sub_dates, 1), units = "days")) / 365.25
  
  # Current Model tax runs
  curr_gross <- calc_tax_series(sub_ret_curr, sub_dates, tax_rate = 0.0)
  curr_tts   <- calc_tax_series(sub_ret_curr, sub_dates, tax_rate = 0.284)
  curr_nyc   <- calc_tax_series(sub_ret_curr, sub_dates, tax_rate = 0.512)
  
  # Baseline Model tax runs
  base_gross <- calc_tax_series(sub_ret_base, sub_dates, tax_rate = 0.0)
  base_tts   <- calc_tax_series(sub_ret_base, sub_dates, tax_rate = 0.284)
  base_nyc   <- calc_tax_series(sub_ret_base, sub_dates, tax_rate = 0.512)
  
  # Benchmarks
  qqq_g <- calc_tax_series(sub_qqq, sub_dates, tax_rate = 0.0)
  qqq_n <- calc_benchmark_tax(sub_qqq, sub_dates, tax_rate_ltcg = 0.341, div_yield_ann = 0.007, tax_rate_div = 0.341)
  vti_g <- calc_tax_series(sub_vti, sub_dates, tax_rate = 0.0)
  vti_n <- calc_benchmark_tax(sub_vti, sub_dates, tax_rate_ltcg = 0.341, div_yield_ann = 0.015, tax_rate_div = 0.341)
  
  # Metrics
  mc_g <- get_metrics(curr_gross$effective_ret, sub_dates, "Current Gross")
  mc_t <- get_metrics(curr_tts$effective_ret,   sub_dates, "Current TTS")
  mc_n <- get_metrics(curr_nyc$effective_ret,   sub_dates, "Current NYC")
  
  mb_g <- get_metrics(base_gross$effective_ret, sub_dates, "Baseline Gross")
  mb_t <- get_metrics(base_tts$effective_ret,   sub_dates, "Baseline TTS")
  mb_n <- get_metrics(base_nyc$effective_ret,   sub_dates, "Baseline NYC")
  
  mq_g <- get_metrics(qqq_g$effective_ret, sub_dates, "QQQ Gross")
  mq_n <- get_metrics(qqq_n$effective_ret, sub_dates, "QQQ NYC")
  mv_g <- get_metrics(vti_g$effective_ret, sub_dates, "VTI Gross")
  mv_n <- get_metrics(vti_n$effective_ret, sub_dates, "VTI NYC")
  
  out_list[[as.character(s_date)]] <- list(
    start_date = as.character(s_date),
    end_date = as.character(tail(sub_dates, 1)),
    trading_days = n_days,
    calendar_years = round(years_exact, 2),
    current = list(
      gross = list(wealth = curr_gross$terminal_wealth, m = mc_g),
      tts   = list(wealth = curr_tts$terminal_wealth,   m = mc_t),
      nyc   = list(wealth = curr_nyc$terminal_wealth,   m = mc_n)
    ),
    baseline = list(
      gross = list(wealth = base_gross$terminal_wealth, m = mb_g),
      tts   = list(wealth = base_tts$terminal_wealth,   m = mb_t),
      nyc   = list(wealth = base_nyc$terminal_wealth,   m = mb_n)
    ),
    qqq = list(
      gross = list(wealth = qqq_g$terminal_wealth, m = mq_g),
      nyc   = list(wealth = qqq_n$terminal_wealth, m = mq_n)
    ),
    vti = list(
      gross = list(wealth = vti_g$terminal_wealth, m = mv_g),
      nyc   = list(wealth = mv_n$terminal_wealth, m = mv_n)
    )
  )
}

format_wealth <- function(w) {
  if (w >= 1e18) return(sprintf("$%.2f Quintillion", w / 1e18))
  if (w >= 1e15) return(sprintf("$%.2f Quadrillion", w / 1e15))
  if (w >= 1e12) return(sprintf("$%.2f Trillion", w / 1e12))
  if (w >= 1e9)  return(sprintf("$%.2f Billion", w / 1e9))
  if (w >= 1e6)  return(sprintf("$%.2f Million", w / 1e6))
  if (w >= 1e3)  return(sprintf("$%.2f Thousand", w / 1e3))
  return(sprintf("$%.2f", w))
}

format_raw_wealth <- function(w) {
  if (w > 1e15) {
    return(sprintf("%.4e", w))
  } else {
    return(format(round(w, 2), big.mark = ",", scientific = FALSE))
  }
}

cat("\n========================================================================================\n")
cat("          STARTING $8,230 CAPITAL RUNS: 1995, 1999, 2005, 2010 TO PRESENT (2026)\n")
cat("========================================================================================\n\n")

for (s_str in names(out_list)) {
  item <- out_list[[s_str]]
  cat(sprintf("### START DATE: %s to %s (%d trading days / %.2f years)\n", 
              item$start_date, item$end_date, item$trading_days, item$calendar_years))
  
  df_disp <- data.frame(
    Tier = c(
      "Current Model — Gross (0% / Tax-Advantaged)",
      "Current Model — Trader Tax Status (TTS 28.4%)",
      "Current Model — NYC Top Bracket (51.2% Tax)",
      "Baseline Model — Gross (Pre-Upgrade)",
      "Baseline Model — TTS (28.4% Tax)",
      "Baseline Model — NYC Top Bracket (51.2% Tax)",
      "Nasdaq-100 (QQQ Buy & Hold — Gross)",
      "Nasdaq-100 (QQQ Buy & Hold — NYC Net)",
      "Total US Market (VTI Buy & Hold — Gross)"
    ),
    Terminal_Wealth = c(
      format_wealth(item$current$gross$wealth),
      format_wealth(item$current$tts$wealth),
      format_wealth(item$current$nyc$wealth),
      format_wealth(item$baseline$gross$wealth),
      format_wealth(item$baseline$tts$wealth),
      format_wealth(item$baseline$nyc$wealth),
      format_wealth(item$qqq$gross$wealth),
      format_wealth(item$qqq$nyc$wealth),
      format_wealth(item$vti$gross$wealth)
    ),
    CAGR = c(
      sprintf("%+.2f%%", item$current$gross$m$cagr),
      sprintf("%+.2f%%", item$current$tts$m$cagr),
      sprintf("%+.2f%%", item$current$nyc$m$cagr),
      sprintf("%+.2f%%", item$baseline$gross$m$cagr),
      sprintf("%+.2f%%", item$baseline$tts$m$cagr),
      sprintf("%+.2f%%", item$baseline$nyc$m$cagr),
      sprintf("%+.2f%%", item$qqq$gross$m$cagr),
      sprintf("%+.2f%%", item$qqq$nyc$m$cagr),
      sprintf("%+.2f%%", item$vti$gross$m$cagr)
    ),
    Max_DD = c(
      sprintf("-%.2f%%", item$current$gross$m$max_dd),
      sprintf("-%.2f%%", item$current$tts$m$max_dd),
      sprintf("-%.2f%%", item$current$nyc$m$max_dd),
      sprintf("-%.2f%%", item$baseline$gross$m$max_dd),
      sprintf("-%.2f%%", item$baseline$tts$m$max_dd),
      sprintf("-%.2f%%", item$baseline$nyc$m$max_dd),
      sprintf("-%.2f%%", item$qqq$gross$m$max_dd),
      sprintf("-%.2f%%", item$qqq$nyc$m$max_dd),
      sprintf("-%.2f%%", item$vti$gross$m$max_dd)
    ),
    Sharpe = c(
      sprintf("%.2f", item$current$gross$m$sharpe),
      sprintf("%.2f", item$current$tts$m$sharpe),
      sprintf("%.2f", item$current$nyc$m$sharpe),
      sprintf("%.2f", item$baseline$gross$m$sharpe),
      sprintf("%.2f", item$baseline$tts$m$sharpe),
      sprintf("%.2f", item$baseline$nyc$m$sharpe),
      sprintf("%.2f", item$qqq$gross$m$sharpe),
      sprintf("%.2f", item$qqq$nyc$m$sharpe),
      sprintf("%.2f", item$vti$gross$m$sharpe)
    ),
    Sortino = c(
      sprintf("%.2f", item$current$gross$m$sortino),
      sprintf("%.2f", item$current$tts$m$sortino),
      sprintf("%.2f", item$current$nyc$m$sortino),
      sprintf("%.2f", item$baseline$gross$m$sortino),
      sprintf("%.2f", item$baseline$tts$m$sortino),
      sprintf("%.2f", item$baseline$nyc$m$sortino),
      sprintf("%.2f", item$qqq$gross$m$sortino),
      sprintf("%.2f", item$qqq$nyc$m$sortino),
      sprintf("%.2f", item$vti$gross$m$sortino)
    ),
    Calmar = c(
      sprintf("%.2f", item$current$gross$m$calmar),
      sprintf("%.2f", item$current$tts$m$calmar),
      sprintf("%.2f", item$current$nyc$m$calmar),
      sprintf("%.2f", item$baseline$gross$m$calmar),
      sprintf("%.2f", item$baseline$tts$m$calmar),
      sprintf("%.2f", item$baseline$nyc$m$calmar),
      sprintf("%.2f", item$qqq$gross$m$calmar),
      sprintf("%.2f", item$qqq$nyc$m$calmar),
      sprintf("%.2f", item$vti$gross$m$calmar)
    ),
    Annual_Vol = c(
      sprintf("%.2f%%", item$current$gross$m$ann_vol),
      sprintf("%.2f%%", item$current$tts$m$ann_vol),
      sprintf("%.2f%%", item$current$nyc$m$ann_vol),
      sprintf("%.2f%%", item$baseline$gross$m$ann_vol),
      sprintf("%.2f%%", item$baseline$tts$m$ann_vol),
      sprintf("%.2f%%", item$baseline$nyc$m$ann_vol),
      sprintf("%.2f%%", item$qqq$gross$m$ann_vol),
      sprintf("%.2f%%", item$qqq$nyc$m$ann_vol),
      sprintf("%.2f%%", item$vti$gross$m$ann_vol)
    ),
    stringsAsFactors = FALSE
  )
  print(df_disp)
  cat("\n----------------------------------------------------------------------------------------\n\n")
}

saveRDS(out_list, "output/custom_start_dates_analysis.rds")
write_json(out_list, "output/custom_start_dates_analysis.json", pretty = TRUE, auto_unbox = TRUE)
cat("[Done] Results saved to output/custom_start_dates_analysis.rds and .json\n")
