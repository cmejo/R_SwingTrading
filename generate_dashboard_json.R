suppressPackageStartupMessages({
  library(jsonlite)
  library(xts)
})

source("R/08_metrics.R")

res_40y   <- readRDS("output/full_40yr_continuous_results.rds")
res_epoch <- readRDS("output/five_epoch_10yr_results.rds")

dates <- as.Date(res_40y$dates)
n <- length(dates)

# Helper to normalize series to $10,000 base
norm_base <- function(r_vec, base = 10000) {
  base * cumprod(1 + r_vec)
}

calc_dd <- function(eq) {
  pk <- cummax(eq)
  round((eq - pk) / pk * 100, 2)
}

# Function to extract sliced dataset
extract_slice <- function(mask, label, step = 1) {
  sub_dates <- dates[mask]
  sub_indices <- which(mask)
  
  if (step > 1) {
    sampled_idx <- sub_indices[seq(1, length(sub_indices), by = step)]
    if (tail(sampled_idx, 1) != tail(sub_indices, 1)) {
      sampled_idx <- c(sampled_idx, tail(sub_indices, 1))
    }
  } else {
    sampled_idx <- sub_indices
  }
  
  r_strat_g <- res_40y$port_ret[sub_indices]
  r_strat_t <- res_40y$strat_tts$effective_ret[sub_indices]
  r_strat_n <- res_40y$strat_nyc$effective_ret[sub_indices]
  r_qqq     <- res_40y$qqq_full[sub_indices]
  r_vti     <- res_40y$vti_full[sub_indices]
  r_b7030   <- res_40y$b7030_full[sub_indices]
  
  eq_strat_g <- norm_base(r_strat_g)
  eq_strat_t <- norm_base(r_strat_t)
  eq_strat_n <- norm_base(r_strat_n)
  eq_qqq     <- norm_base(r_qqq)
  eq_vti     <- norm_base(r_vti)
  eq_b7030   <- norm_base(r_b7030)
  
  dd_strat_g <- calc_dd(eq_strat_g)
  dd_qqq     <- calc_dd(eq_qqq)
  dd_vti     <- calc_dd(eq_vti)
  
  # Sampled points for charts
  match_samp <- match(sampled_idx, sub_indices)
  
  # Metrics
  m_sg <- calc_performance_metrics(xts(r_strat_g, order.by = sub_dates))$raw
  m_st <- calc_performance_metrics(xts(r_strat_t, order.by = sub_dates))$raw
  m_sn <- calc_performance_metrics(xts(r_strat_n, order.by = sub_dates))$raw
  m_q  <- calc_performance_metrics(xts(r_qqq,     order.by = sub_dates))$raw
  m_v  <- calc_performance_metrics(xts(r_vti,     order.by = sub_dates))$raw
  m_b  <- calc_performance_metrics(xts(r_b7030,   order.by = sub_dates))$raw
  
  list(
    label = label,
    start_date = as.character(sub_dates[1]),
    end_date   = as.character(tail(sub_dates, 1)),
    trading_days = length(sub_dates),
    chart = list(
      dates    = as.character(dates[sampled_idx]),
      strat_g  = round(eq_strat_g[match_samp], 2),
      strat_t  = round(eq_strat_t[match_samp], 2),
      strat_n  = round(eq_strat_n[match_samp], 2),
      qqq      = round(eq_qqq[match_samp], 2),
      vti      = round(eq_vti[match_samp], 2),
      b7030    = round(eq_b7030[match_samp], 2),
      strat_dd = dd_strat_g[match_samp],
      qqq_dd   = dd_qqq[match_samp],
      vti_dd   = dd_vti[match_samp]
    ),
    metrics = list(
      strategy_gross = list(
        cagr = round(m_sg$ann_ret * 100, 2),
        cum_ret = round(m_sg$cum_ret * 100, 1),
        max_dd = round(m_sg$max_dd * 100, 2),
        sharpe = round(m_sg$sharpe, 2),
        sortino = if (!is.na(m_sg$sortino)) round(m_sg$sortino, 2) else 0,
        vol = round(m_sg$ann_vol * 100, 2),
        terminal = round(tail(eq_strat_g, 1), 2)
      ),
      strategy_tts = list(
        cagr = round(m_st$ann_ret * 100, 2),
        cum_ret = round(m_st$cum_ret * 100, 1),
        max_dd = round(m_st$max_dd * 100, 2),
        sharpe = round(m_st$sharpe, 2),
        sortino = if (!is.na(m_st$sortino)) round(m_st$sortino, 2) else 0,
        vol = round(m_st$ann_vol * 100, 2),
        terminal = round(tail(eq_strat_t, 1), 2)
      ),
      strategy_nyc = list(
        cagr = round(m_sn$ann_ret * 100, 2),
        cum_ret = round(m_sn$cum_ret * 100, 1),
        max_dd = round(m_sn$max_dd * 100, 2),
        sharpe = round(m_sn$sharpe, 2),
        sortino = if (!is.na(m_sn$sortino)) round(m_sn$sortino, 2) else 0,
        vol = round(m_sn$ann_vol * 100, 2),
        terminal = round(tail(eq_strat_n, 1), 2)
      ),
      qqq = list(
        cagr = round(m_q$ann_ret * 100, 2),
        cum_ret = round(m_q$cum_ret * 100, 1),
        max_dd = round(m_q$max_dd * 100, 2),
        sharpe = round(m_q$sharpe, 2),
        vol = round(m_q$ann_vol * 100, 2),
        terminal = round(tail(eq_qqq, 1), 2)
      ),
      vti = list(
        cagr = round(m_v$ann_ret * 100, 2),
        cum_ret = round(m_v$cum_ret * 100, 1),
        max_dd = round(m_v$max_dd * 100, 2),
        sharpe = round(m_v$sharpe, 2),
        vol = round(m_v$ann_vol * 100, 2),
        terminal = round(tail(eq_vti, 1), 2)
      ),
      blend7030 = list(
        cagr = round(m_b$ann_ret * 100, 2),
        cum_ret = round(m_b$cum_ret * 100, 1),
        max_dd = round(m_b$max_dd * 100, 2),
        sharpe = round(m_b$sharpe, 2),
        vol = round(m_b$ann_vol * 100, 2),
        terminal = round(tail(eq_b7030, 1), 2)
      )
    )
  )
}

# Define Horizons
months <- substr(as.character(dates), 1, 7)
month_ends <- which(c(months[-1] != months[-length(months)], TRUE))

horizons <- list(
  "ALL_40Y"    = extract_slice(rep(TRUE, n), "40-Year Continuous (1985 - 2026)", step = 21), # ~490 monthly points
  "LAST_10Y"   = extract_slice(dates >= as.Date("2016-01-01"), "Last 10 Years (2016 - 2026)", step = 5), # weekly
  "LAST_5Y"    = extract_slice(dates >= as.Date("2021-01-01"), "Last 5 Years (2021 - 2026)", step = 2), # bi-daily
  "EPOCH_1"    = extract_slice(dates >= as.Date("1995-01-01") & dates <= as.Date("2004-12-31"), "Dot-Com Cycle (1995 - 2004)", step = 5),
  "EPOCH_2"    = extract_slice(dates >= as.Date("2000-01-01") & dates <= as.Date("2009-12-31"), "Lost Decade (2000 - 2009)", step = 5),
  "EPOCH_3"    = extract_slice(dates >= as.Date("2007-01-01") & dates <= as.Date("2016-12-31"), "GFC & Recovery (2007 - 2016)", step = 5)
)

# Decade breakdown
decades_raw <- read.csv("output/full_40yr_decade_breakdown.csv", stringsAsFactors = FALSE)

dashboard_payload <- list(
  generated_at = as.character(Sys.time()),
  starting_capital = 10000,
  horizons = horizons,
  decades = decades_raw
)

write(jsonlite::toJSON(dashboard_payload, auto_unbox = TRUE), "output/dashboard_data.json")
cat("Dashboard data successfully written to output/dashboard_data.json\n")
