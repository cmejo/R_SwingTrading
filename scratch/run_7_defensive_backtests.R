suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(TTR)
})

source("R/02_trend_lmMA.R")
source("R/08_metrics.R")

cat("========================================================================================\n")
cat(" RUNNING 7 DEFENSIVE CASH / HEDGE STRATEGY BACKTESTS ACROSS 7 TIME HORIZONS\n")
cat(" Horizons: 1M (21d), 6M (126d), 12M (252d), 24M (504d), 36M (756d), 60M (1260d), 120M (2520d)\n")
cat("========================================================================================\n\n")

# 1. Fetch live historical total return data (from 2014 to ensure 50-day warmup for 2016-10 to 2026-10)
cat("[Data] Fetching total return history for QQQ, SPY, PSQ, GLD, SGOV, BIL from Yahoo Finance...\n")
qqq_raw  <- suppressWarnings(getSymbols("QQQ",  src="yahoo", from="2014-01-01", auto.assign=FALSE))
spy_raw  <- suppressWarnings(getSymbols("SPY",  src="yahoo", from="2014-01-01", auto.assign=FALSE))
psq_raw  <- suppressWarnings(getSymbols("PSQ",  src="yahoo", from="2014-01-01", auto.assign=FALSE))
gld_raw  <- suppressWarnings(getSymbols("GLD",  src="yahoo", from="2014-01-01", auto.assign=FALSE))
bil_raw  <- suppressWarnings(getSymbols("BIL",  src="yahoo", from="2014-01-01", auto.assign=FALSE))
sgov_raw <- suppressWarnings(getSymbols("SGOV", src="yahoo", from="2020-05-26", auto.assign=FALSE))

# Extract Adjusted Close for Total Return (dividends reinvested)
qqq_ad <- Ad(qqq_raw)
spy_ad <- Ad(spy_raw)
psq_ad <- Ad(psq_raw)
gld_ad <- Ad(gld_raw)
bil_ad <- Ad(bil_raw)
sgov_ad <- Ad(sgov_raw)

# Calculate Daily Returns
qqq_ret  <- na.omit(qqq_ad / lag.xts(qqq_ad, 1) - 1)
spy_ret  <- na.omit(spy_ad / lag.xts(spy_ad, 1) - 1)
psq_ret  <- na.omit(psq_ad / lag.xts(psq_ad, 1) - 1)
gld_ret  <- na.omit(gld_ad / lag.xts(gld_ad, 1) - 1)
bil_ret  <- na.omit(bil_ad / lag.xts(bil_ad, 1) - 1)
sgov_ret <- na.omit(sgov_ad / lag.xts(sgov_ad, 1) - 1)

# Splice SGOV with BIL for dates before SGOV inception
sgov_spliced <- bil_ret
sgov_common  <- intersect(index(sgov_ret), index(sgov_spliced))
sgov_spliced[sgov_common] <- sgov_ret[sgov_common]

# Compute 50-day Trend on QQQ (Macro Gate) and GLD (Safe-Haven Gate)
cat("[Macro] Computing 50-day rolling linear regression trendlines for QQQ and GLD...\n")
qqq_lm <- lmMA(Cl(qqq_raw), n = 50)
gld_lm <- lmMA(Cl(gld_raw), n = 50)

# Align common trading dates
all_dates <- sort(intersect(intersect(intersect(index(qqq_ret), index(psq_ret)), index(gld_ret)), index(sgov_spliced)))
# Filter to dates where 50-day lmMA is valid
valid_lm_dates <- sort(intersect(index(na.omit(qqq_lm$fit)), index(na.omit(gld_lm$fit))))
eval_dates <- sort(intersect(all_dates, valid_lm_dates))

cat(sprintf("[Dates] Aligned %d trading days from %s to %s\n",
            length(eval_dates), as.character(eval_dates[1]), as.character(tail(eval_dates, 1))))

# Extract aligned vectors
qqq_v  <- as.numeric(qqq_ret[eval_dates])
psq_v  <- as.numeric(psq_ret[eval_dates])
gld_v  <- as.numeric(gld_ret[eval_dates])
sgov_v <- as.numeric(sgov_spliced[eval_dates])

# Macro Regime vectors
qqq_cl_v   <- as.numeric(Cl(qqq_raw)[eval_dates])
qqq_fit_v  <- as.numeric(qqq_lm$fit[eval_dates])
qqq_slp_v  <- as.numeric(qqq_lm$slope[eval_dates])
macro_bull <- (qqq_cl_v >= qqq_fit_v) & (qqq_slp_v > 0)

gld_cl_v   <- as.numeric(Cl(gld_raw)[eval_dates])
gld_fit_v  <- as.numeric(gld_lm$fit[eval_dates])
gld_slp_v  <- as.numeric(gld_lm$slope[eval_dates])
gld_bull   <- (gld_cl_v >= gld_fit_v) & (gld_slp_v > 0)

n_total <- length(eval_dates)
cat(sprintf("[Regime] Overall Macro Gate: %d Bullish days (%.1f%%) | %d Bearish days (%.1f%%)\n\n",
            sum(macro_bull), mean(macro_bull)*100, sum(!macro_bull), mean(!macro_bull)*100))

# 2. Load 10-year Swing Trading Walk-Forward Dataset (if available for Layer 2)
CACHE_DIR_10Y <- "data/cache_10yr"
has_swing_cache <- dir.exists(CACHE_DIR_10Y) && length(list.files(CACHE_DIR_10Y)) > 0

mat_prob  <- NULL
mat_rs    <- NULL
mat_vol   <- NULL
mat_ret_b <- NULL
mat_valid <- NULL
sym_sectors <- NULL
avail_syms <- NULL

if (has_swing_cache) {
  cat("[Swing Data] Loading 10-year walk-forward cache for portfolio integration...\n")
  SECTOR_MAP <- if (file.exists("sector_map.json")) jsonlite::fromJSON("sector_map.json") else list()
  get_sym_sector <- function(s) {
    if (!is.null(SECTOR_MAP[[s]])) return(SECTOR_MAP[[s]])
    return("General_Tech")
  }
  cache_files <- list.files(CACHE_DIR_10Y, pattern = "_10yr_wf\\.rds$", full.names = TRUE)
  sim_data <- list()
  for (cf in cache_files) {
    obj <- readRDS(cf)
    sim_data[[obj$symbol]] <- obj
  }
  avail_syms <- names(sim_data)
  n_syms <- length(avail_syms)
  mat_prob  <- matrix(0.5,  nrow = n_total, ncol = n_syms)
  mat_rs    <- matrix(0.0,  nrow = n_total, ncol = n_syms)
  mat_vol   <- matrix(0.25, nrow = n_total, ncol = n_syms)
  mat_ret_b <- matrix(0.0,  nrow = n_total, ncol = n_syms)
  mat_valid <- matrix(FALSE, nrow = n_total, ncol = n_syms)

  for (j in seq_along(avail_syms)) {
    s <- avail_syms[j]
    obj <- sim_data[[s]]
    idx_p <- match(eval_dates, index(obj$pred_prob))
    v_p   <- !is.na(idx_p)
    if (any(v_p)) mat_prob[v_p, j] <- as.numeric(obj$pred_prob)[idx_p[v_p]]
    idx_rs <- match(eval_dates, index(obj$rs_spy))
    v_rs   <- !is.na(idx_rs)
    if (any(v_rs)) mat_rs[v_rs, j] <- as.numeric(obj$rs_spy)[idx_rs[v_rs]]
    idx_v <- match(eval_dates, index(obj$garch_vol))
    v_v   <- !is.na(idx_v)
    if (any(v_v)) {
      raw_v <- as.numeric(obj$garch_vol)[idx_v[v_v]]
      raw_v[is.na(raw_v) | raw_v < 0.05] <- 0.25
      mat_vol[v_v, j] <- raw_v
    }
    idx_b <- match(eval_dates, index(obj$bracket_ret))
    v_b   <- !is.na(idx_b)
    if (any(v_b)) {
      mat_ret_b[v_b, j] <- as.numeric(obj$bracket_ret)[idx_b[v_b]]
      mat_valid[v_b, j] <- TRUE
    }
  }
  sym_sectors <- sapply(avail_syms, get_sym_sector)
  cat(sprintf("[Swing Data] Loaded %d symbols across %d sessions.\n", n_syms, n_total))
}

# 3. Define the 7 Defensive Return Functions for Day t
# Strategy 1: Without PSQ Hedge (0% Cash when Bearish)
ret_strat1 <- function(t) if (macro_bull[t]) qqq_v[t] else 0.0

# Strategy 2: 100% SGOV when Bearish
ret_strat2 <- function(t) if (macro_bull[t]) qqq_v[t] else sgov_v[t]

# Strategy 3: 100% PSQ Hedge when Bearish
ret_strat3 <- function(t) if (macro_bull[t]) qqq_v[t] else psq_v[t]

# Strategy 4: 50% SGOV / 50% PSQ when Bearish
ret_strat4 <- function(t) if (macro_bull[t]) qqq_v[t] else (0.50 * sgov_v[t] + 0.50 * psq_v[t])

# Strategy 5: 25% SGOV / 75% PSQ when Bearish
ret_strat5 <- function(t) if (macro_bull[t]) qqq_v[t] else (0.25 * sgov_v[t] + 0.75 * psq_v[t])

# Strategy 6: 75% SGOV / 25% PSQ when Bearish
ret_strat6 <- function(t) if (macro_bull[t]) qqq_v[t] else (0.75 * sgov_v[t] + 0.25 * psq_v[t])

# Strategy 7: GLD/SGOV Switch when Bearish
ret_strat7 <- function(t) {
  if (macro_bull[t]) {
    qqq_v[t]
  } else {
    if (gld_bull[t]) gld_v[t] else sgov_v[t]
  }
}

strategy_names <- c(
  "Without PSQ (0% Cash)",
  "100% SGOV (Yield)",
  "100% PSQ (Full Hedge)",
  "50% SGOV / 50% PSQ",
  "25% SGOV / 75% PSQ",
  "75% SGOV / 25% PSQ",
  "GLD / SGOV Switch"
)

strat_funcs <- list(ret_strat1, ret_strat2, ret_strat3, ret_strat4, ret_strat5, ret_strat6, ret_strat7)

# Precalculate daily cash return vectors for all 7 strategies
daily_cash_ret_matrix <- matrix(0.0, nrow = n_total, ncol = 7)
for (k in 1:7) {
  f <- strat_funcs[[k]]
  daily_cash_ret_matrix[, k] <- sapply(1:n_total, f)
}

# 4. Define Full Swing Portfolio Simulator with Unallocated Cash Allocation
# MaxPos=3, MaxPerSector=3, Vince Vol-Inverse Sizing
sim_portfolio_strategy <- function(strat_idx, sub_indices) {
  n_sub <- length(sub_indices)
  port_ret <- numeric(n_sub)
  
  if (is.null(mat_prob)) {
    # If swing cache not present, fallback to pure cash strategy
    return(daily_cash_ret_matrix[sub_indices, strat_idx])
  }
  
  for (i in seq_along(sub_indices)) {
    t <- sub_indices[i]
    is_bull <- macro_bull[t]
    eff_max_pos <- if (is_bull) 3 else 2
    eff_p_long  <- if (is_bull) 0.58 else 0.65
    
    p_ups   <- mat_prob[t, ]
    rs_vals <- mat_rs[t, ]
    vols    <- mat_vol[t, ]
    rets_b  <- mat_ret_b[t, ]
    val_flg <- mat_valid[t, ]
    
    qualify <- which(val_flg & p_ups >= eff_p_long & rs_vals > 0)
    top_idx <- integer(0)
    
    if (length(qualify) > 0) {
      scores <- p_ups[qualify] * (1 + rs_vals[qualify])
      ranked <- qualify[order(-scores)]
      sec_cnts <- list()
      for (cand in ranked) {
        c_sec <- sym_sectors[cand]
        cnt <- if (is.null(sec_cnts[[c_sec]])) 0 else sec_cnts[[c_sec]]
        if (cnt < 3) {
          top_idx <- c(top_idx, cand)
          sec_cnts[[c_sec]] <- cnt + 1
          if (length(top_idx) >= eff_max_pos) break
        }
      }
    }
    
    n_sel <- length(top_idx)
    cash_day_ret <- daily_cash_ret_matrix[t, strat_idx]
    
    if (n_sel > 0) {
      inv_v <- 1 / vols[top_idx]
      weights <- inv_v / sum(inv_v)
      swing_exposure <- min(1.0, n_sel / 3.0)
      swing_ret <- sum(weights * rets_b[top_idx])
      port_ret[i] <- swing_exposure * swing_ret + (1 - swing_exposure) * cash_day_ret
    } else {
      port_ret[i] <- cash_day_ret
    }
  }
  return(port_ret)
}

# 5. Define Horizons (bars)
horizon_defs <- list(
  "1 Month"   = 21,
  "6 Months"  = 126,
  "12 Months" = 252,
  "24 Months" = 504,
  "36 Months" = 756,
  "60 Months" = 1260,
  "120 Months"= min(2520, n_total)
)

# Helper function to compute complete metrics
evaluate_returns <- function(r_vec, dates_sub) {
  n_days <- length(r_vec)
  term_w <- 10000 * prod(1 + r_vec)
  cum_ret <- (term_w / 10000 - 1) * 100
  ann_ret <- ((term_w / 10000)^(252 / n_days) - 1) * 100
  
  # Max Drawdown
  eq <- cumprod(1 + r_vec)
  peak <- cummax(eq)
  dd <- (eq - peak) / peak
  max_dd <- min(dd) * 100
  
  # Volatility & Sharpe (Rf = 0 for standard excess calculation)
  daily_sd <- sd(r_vec)
  ann_vol <- daily_sd * sqrt(252) * 100
  sharpe <- if (daily_sd > 0) (mean(r_vec) / daily_sd) * sqrt(252) else 0.0
  
  # Sortino
  downside <- r_vec[r_vec < 0]
  down_sd <- if (length(downside) > 1) sd(downside) else 0.0
  sortino <- if (down_sd > 0) (mean(r_vec) / down_sd) * sqrt(252) else 0.0
  
  win_rate <- mean(r_vec > 0) * 100
  
  list(
    cum_ret = cum_ret,
    ann_ret = ann_ret,
    max_dd = max_dd,
    sharpe = sharpe,
    sortino = sortino,
    ann_vol = ann_vol,
    win_rate = win_rate,
    term_wealth = term_w
  )
}

# 6. Execute Simulations across all 7 horizons
portfolio_results <- list()
cash_only_results <- list()

for (h_name in names(horizon_defs)) {
  n_bars <- horizon_defs[[h_name]]
  sub_idx <- (n_total - n_bars + 1):n_total
  h_dates <- eval_dates[sub_idx]
  
  cat(sprintf("\n----------------------------------------------------------------------------------------\n"))
  cat(sprintf(" HORIZON: %s (%d Trading Days | %s to %s)\n", h_name, length(sub_idx), as.character(h_dates[1]), as.character(tail(h_dates, 1))))
  cat(sprintf("----------------------------------------------------------------------------------------\n"))
  
  # Check bearish days in this window
  h_bull <- macro_bull[sub_idx]
  cat(sprintf(" Market Regime: %d Bullish Days (%.1f%%) | %d Bearish Days (%.1f%%)\n\n",
              sum(h_bull), mean(h_bull)*100, sum(!h_bull), mean(!h_bull)*100))
  
  cat(sprintf("%-24s | %10s | %9s | %8s | %7s | %7s | %11s\n",
              "Strategy", "Cum Return", "CAGR", "Max DD", "Sharpe", "Sortino", "End Wealth"))
  cat(paste(rep("-", 88), collapse=""), "\n")
  
  for (k in 1:7) {
    s_name <- strategy_names[k]
    # Full Portfolio Return
    p_ret <- sim_portfolio_strategy(k, sub_idx)
    p_met <- evaluate_returns(p_ret, h_dates)
    
    # Store
    res_row <- data.frame(
      Horizon = h_name,
      Days = n_bars,
      Strategy = s_name,
      Cum_Return = p_met$cum_ret,
      CAGR = p_met$ann_ret,
      Max_DD = p_met$max_dd,
      Sharpe = p_met$sharpe,
      Sortino = p_met$sortino,
      Ann_Vol = p_met$ann_vol,
      Win_Rate = p_met$win_rate,
      Terminal_Wealth = p_met$term_wealth,
      stringsAsFactors = FALSE
    )
    portfolio_results[[length(portfolio_results) + 1]] <- res_row
    
    # Pure Cash/Hedge Return (Stand-alone Tactical Module)
    c_ret <- daily_cash_ret_matrix[sub_idx, k]
    c_met <- evaluate_returns(c_ret, h_dates)
    res_c_row <- data.frame(
      Horizon = h_name,
      Days = n_bars,
      Strategy = s_name,
      Cum_Return = c_met$cum_ret,
      CAGR = c_met$ann_ret,
      Max_DD = c_met$max_dd,
      Sharpe = c_met$sharpe,
      Sortino = c_met$sortino,
      Ann_Vol = c_met$ann_vol,
      Win_Rate = c_met$win_rate,
      Terminal_Wealth = c_met$term_wealth,
      stringsAsFactors = FALSE
    )
    cash_only_results[[length(cash_only_results) + 1]] <- res_c_row
    
    cat(sprintf("%-24s | %+9.2f%% | %+8.2f%% | %7.2f%% | %7.2f | %7.2f | $%10.2f\n",
                s_name, p_met$cum_ret, p_met$ann_ret, p_met$max_dd, p_met$sharpe, p_met$sortino, p_met$term_wealth))
  }
}

df_port_all <- do.call(rbind, portfolio_results)
df_cash_all <- do.call(rbind, cash_only_results)

dir.create("output", showWarnings = FALSE)
write.csv(df_port_all, "output/defensive_cash_portfolio_backtest_scorecard.csv", row.names = FALSE)
write.csv(df_cash_all, "output/defensive_cash_standalone_backtest_scorecard.csv", row.names = FALSE)
cat("\n[Saved] Output saved to output/defensive_cash_portfolio_backtest_scorecard.csv\n")
cat("[Saved] Output saved to output/defensive_cash_standalone_backtest_scorecard.csv\n")
