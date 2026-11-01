suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(TTR)
})

source("R/02_trend_lmMA.R")
source("R/08_metrics.R")

symbols <- c("QQQ", "GLD", "BIL", "SGOV", "SQQQ", "FNGD")
raw_data <- list()
for (s in symbols) {
  raw_data[[s]] <- suppressWarnings(getSymbols(s, src="yahoo", from="2010-01-01", auto.assign=FALSE))
}

ret_data <- list()
for (s in symbols) {
  ad <- Ad(raw_data[[s]])
  ret_data[[s]] <- na.omit(ad / lag.xts(ad, 1) - 1)
}

sgov_spliced <- ret_data[["BIL"]]
sgov_common  <- intersect(index(ret_data[["SGOV"]]), index(sgov_spliced))
sgov_spliced[sgov_common] <- ret_data[["SGOV"]][sgov_common]

qqq_lm <- lmMA(Cl(raw_data[["QQQ"]]), n = 50)
gld_lm <- lmMA(Cl(raw_data[["GLD"]]), n = 50)

common_10y <- sort(intersect(intersect(intersect(
  index(ret_data[["QQQ"]]), index(ret_data[["GLD"]])), index(sgov_spliced)),
  index(ret_data[["SQQQ"]])))
common_10y <- sort(intersect(common_10y, index(na.omit(qqq_lm$fit))))
common_10y <- sort(intersect(common_10y, index(na.omit(gld_lm$fit))))

eval_dates <- tail(common_10y, 2520)
n_total <- length(eval_dates)

qqq_v  <- as.numeric(ret_data[["QQQ"]][eval_dates])
gld_v  <- as.numeric(ret_data[["GLD"]][eval_dates])
sgov_v <- as.numeric(sgov_spliced[eval_dates])
sqqq_v <- as.numeric(ret_data[["SQQQ"]][eval_dates])

fngd_ret <- ret_data[["FNGD"]]
fngd_v <- numeric(n_total)
for (i in 1:n_total) {
  d <- eval_dates[i]
  if (d %in% index(fngd_ret)) {
    fngd_v[i] <- as.numeric(fngd_ret[d])
  } else {
    fngd_v[i] <- sqqq_v[i]
  }
}

qqq_cl_v   <- as.numeric(Cl(raw_data[["QQQ"]])[eval_dates])
qqq_fit_v  <- as.numeric(qqq_lm$fit[eval_dates])
qqq_slp_v  <- as.numeric(qqq_lm$slope[eval_dates])
macro_bull <- (qqq_cl_v >= qqq_fit_v) & (qqq_slp_v > 0)

gld_cl_v   <- as.numeric(Cl(raw_data[["GLD"]])[eval_dates])
gld_fit_v  <- as.numeric(gld_lm$fit[eval_dates])
gld_slp_v  <- as.numeric(gld_lm$slope[eval_dates])
gld_bull   <- (gld_cl_v >= gld_fit_v) & (gld_slp_v > 0)

# Default fallback switch: GLD / [75% FNGD + 25% SQQQ]
fb_hedge <- 0.75 * fngd_v + 0.25 * sqqq_v
r_cash_switch <- sapply(1:n_total, function(t) {
  if (macro_bull[t]) return(qqq_v[t])
  if (gld_bull[t]) return(gld_v[t])
  return(fb_hedge[t])
})

# Load full portfolio walk-forward cache
CACHE_DIR_10Y <- "data/cache_10yr"
SECTOR_MAP <- if (file.exists("sector_map.json")) jsonlite::fromJSON("sector_map.json") else list()
get_sym_sector <- function(s) {
  if (!is.null(SECTOR_MAP[[s]])) return(SECTOR_MAP[[s]])
  return("General_Tech")
}
cache_files <- list.files(CACHE_DIR_10Y, pattern = "_10yr_wf[.]rds$", full.names = TRUE)
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

# -------------------------------------------------------------
# PART 1: 10-YEAR CIRCUIT BREAKER SENSITIVITY TEST
# Thresholds: 4%, 8%, 10%, 12%, 15%, and None (Disabled)
# Circuit Breaker Mechanics:
# Tracks daily account peak equity.
# If current drawdown <= -threshold, throttle Safe f (equity exposure) by 50% for next day.
# -------------------------------------------------------------
cb_thresholds <- list(
  "Circuit Breaker @ 4%"  = 0.04,
  "Circuit Breaker @ 8%"  = 0.08,
  "Circuit Breaker @ 10%" = 0.10,
  "Circuit Breaker @ 12%" = 0.12,
  "Circuit Breaker @ 15%" = 0.15,
  "No Circuit Breaker (Disabled)" = NULL
)

sim_portfolio_cb <- function(cb_thresh = NULL, cash_series = r_cash_switch) {
  port_ret <- numeric(n_total)
  peak_equity <- 1.0
  cur_equity <- 1.0
  cb_active_days <- 0
  
  for (t in 1:n_total) {
    # Check circuit breaker status from previous day equity
    dd <- (cur_equity - peak_equity) / peak_equity
    is_cb_active <- if (!is.null(cb_thresh)) (dd <= -cb_thresh) else FALSE
    if (is_cb_active) cb_active_days <- cb_active_days + 1
    
    cb_multiplier <- if (is_cb_active) 0.50 else 1.0
    
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
    cash_day_ret <- cash_series[t]
    
    if (n_sel > 0) {
      inv_v <- 1 / vols[top_idx]
      weights <- inv_v / sum(inv_v)
      raw_swing_exp <- min(1.0, n_sel / 3.0)
      swing_exposure <- raw_swing_exp * cb_multiplier
      swing_ret <- sum(weights * rets_b[top_idx])
      port_ret[t] <- swing_exposure * swing_ret + (1 - swing_exposure) * cash_day_ret
    } else {
      port_ret[t] <- cash_day_ret
    }
    
    cur_equity <- cur_equity * (1 + port_ret[t])
    if (cur_equity > peak_equity) peak_equity <- cur_equity
  }
  
  eq <- cumprod(1 + port_ret)
  cagr <- tail(eq, 1)^(252 / n_total) - 1
  mdd <- min((eq - cummax(eq)) / cummax(eq))
  sharpe <- (mean(port_ret) * 252 - 0.04) / (sd(port_ret) * sqrt(252))
  final_10k <- 10000 * tail(eq, 1)
  
  list(CAGR = cagr, Final_10k = final_10k, MaxDD = mdd, Sharpe = sharpe, CB_Days = cb_active_days)
}

cb_results <- list()
for (nm in names(cb_thresholds)) {
  cb_results[[nm]] <- sim_portfolio_cb(cb_thresholds[[nm]], r_cash_switch)
}

df_cb <- data.frame(
  Config = names(cb_results),
  Final_10k = sapply(cb_results, function(x) x$Final_10k),
  CAGR = sapply(cb_results, function(x) x$CAGR * 100),
  MaxDD = sapply(cb_results, function(x) x$MaxDD * 100),
  Sharpe = sapply(cb_results, function(x) x$Sharpe),
  CB_Days = sapply(cb_results, function(x) x$CB_Days),
  CB_Pct_Days = sapply(cb_results, function(x) x$CB_Days / n_total * 100),
  stringsAsFactors = FALSE
)
df_cb <- df_cb[order(-df_cb$Final_10k), ]

# -------------------------------------------------------------
# PART 2: CASH PARKING IN QQQ ALWAYS VS OTHER STRATEGIES
# "when not fully invested in stocks would i be better off investing the rest of the cash in QQQ? whats the 10 year backtest result of that?"
# Compare:
# 1. Idle cash parked in 100% QQQ ALWAYS (even in bear market)
# 2. Idle cash parked in GLD / [75% FNGD + 25% SQQQ] switch (current champion)
# 3. Idle cash parked in GLD / 100% SGOV (baseline)
# 4. Idle cash parked in 100% SGOV ALWAYS (0% risk cash)
# -------------------------------------------------------------
cash_candidates <- list(
  "Surplus Cash Always in QQQ (100% QQQ Beta)" = qqq_v,
  "Surplus Cash in GLD / [75% FNGD + 25% SQQQ] Switch" = r_cash_switch,
  "Surplus Cash in GLD / SGOV Switch" = sapply(1:n_total, function(t) if (macro_bull[t]) qqq_v[t] else if (gld_bull[t]) gld_v[t] else sgov_v[t]),
  "Surplus Cash Always in SGOV (Pure Cash / Yield)" = sgov_v
)

cash_results <- list()
for (nm in names(cash_candidates)) {
  res <- sim_portfolio_cb(cb_thresh = 0.10, cash_series = cash_candidates[[nm]])
  cash_results[[nm]] <- res
}

df_cash <- data.frame(
  Cash_Strategy = names(cash_results),
  Final_10k = sapply(cash_results, function(x) x$Final_10k),
  CAGR = sapply(cash_results, function(x) x$CAGR * 100),
  MaxDD = sapply(cash_results, function(x) x$MaxDD * 100),
  Sharpe = sapply(cash_results, function(x) x$Sharpe),
  stringsAsFactors = FALSE
)
df_cash <- df_cash[order(-df_cash$Final_10k), ]

# Print Results
cat("\n========================================================================================\n")
cat(" PART 1: 10-YEAR CIRCUIT BREAKER SENSITIVITY TEST (Ranked by Returns on $10k)\n")
cat("========================================================================================\n\n")
header1 <- sprintf("%-30s | %14s | %10s | %10s | %8s | %14s", "Circuit Breaker Ceiling", "Final $ ($10k)", "10Y CAGR", "Max DD", "Sharpe", "Days Active")
cat(header1, "\n", paste(rep("-", nchar(header1)), collapse=""), "\n")
for (i in 1:nrow(df_cb)) {
  cat(sprintf("%-30s | %14s | %+9.2f%% | %9.2f%% | %7.2f | %4d (%.1f%%)\n",
              df_cb$Config[i], sprintf("$%.2fM", df_cb$Final_10k[i] / 1e6), df_cb$CAGR[i], df_cb$MaxDD[i], df_cb$Sharpe[i],
              df_cb$CB_Days[i], df_cb$CB_Pct_Days[i]))
}

cat("\n========================================================================================\n")
cat(" PART 2: IDLE CASH STRATEGY COMPARISON: QQQ ALWAYS VS ALTERNATIVES (CB @ 10%)\n")
cat("========================================================================================\n\n")
header2 <- sprintf("%-50s | %14s | %10s | %10s | %8s", "Idle Cash Parking Strategy", "Final $ ($10k)", "10Y CAGR", "Max DD", "Sharpe")
cat(header2, "\n", paste(rep("-", nchar(header2)), collapse=""), "\n")
for (i in 1:nrow(df_cash)) {
  cat(sprintf("%-50s | %14s | %+9.2f%% | %9.2f%% | %7.2f\n",
              df_cash$Cash_Strategy[i], sprintf("$%.2fM", df_cash$Final_10k[i] / 1e6), df_cash$CAGR[i], df_cash$MaxDD[i], df_cash$Sharpe[i]))
}

