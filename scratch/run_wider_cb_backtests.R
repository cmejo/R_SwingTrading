suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(TTR)
})

source("R/02_trend_lmMA.R")
source("R/08_metrics.R")

cat("========================================================================================\n")
cat(" RUNNING 10-YEAR CIRCUIT BREAKER SENSITIVITY TEST (-10%, -12%, -15%, -18%, -20%)\n")
cat(" Starting Capital: $10,000 | 2,520 Trading Days (2016 - 2026)\n")
cat("========================================================================================\n\n")

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

# Base inverse hedge blend: 75% FNGD + 25% SQQQ
fb_raw <- 0.75 * fngd_v + 0.25 * sqqq_v

r_cash_switch <- sapply(1:n_total, function(t) {
  if (macro_bull[t]) return(qqq_v[t])
  if (gld_bull[t]) return(gld_v[t])
  return(fb_raw[t])
})

# Load swing trades walk-forward cache
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

# Simulation function for circuit breaker thresholds
sim_cb_portfolio <- function(cb_thresh = NULL, cash_series = r_cash_switch) {
  port_ret <- numeric(n_total)
  peak_equity <- 1.0
  cur_equity <- 1.0
  cb_active_days <- 0
  cb_active_vec <- logical(n_total)
  
  for (t in 1:n_total) {
    dd <- (cur_equity - peak_equity) / peak_equity
    is_cb_active <- if (!is.null(cb_thresh)) (dd <= -cb_thresh) else FALSE
    cb_active_vec[t] <- is_cb_active
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
  roll_max <- cummax(eq)
  dd_curve <- (eq - roll_max) / roll_max
  mdd <- min(dd_curve)
  ann_vol <- sd(port_ret) * sqrt(252)
  sharpe <- (mean(port_ret) * 252 - 0.04) / ann_vol
  final_10k <- 10000 * tail(eq, 1)
  
  # Count episodes of circuit breaker engagement
  r_cb <- rle(cb_active_vec)
  n_episodes <- sum(r_cb$values == TRUE)
  
  list(CAGR = cagr, Final_10k = final_10k, MaxDD = mdd, Sharpe = sharpe, AnnVol = ann_vol,
       CB_Days = cb_active_days, CB_Episodes = n_episodes)
}

# The requested thresholds
cb_list <- list(
  "Circuit Breaker @ -10%" = 0.10,
  "Circuit Breaker @ -12%" = 0.12,
  "Circuit Breaker @ -15%" = 0.15,
  "Circuit Breaker @ -18%" = 0.18,
  "Circuit Breaker @ -20%" = 0.20,
  "No Circuit Breaker (Disabled)" = NULL
)

# 1. Base Strategy (Default Cash Switch)
results_base <- list()
for (nm in names(cb_list)) {
  results_base[[nm]] <- sim_cb_portfolio(cb_list[[nm]], r_cash_switch)
}

df_res <- data.frame(
  Threshold = names(results_base),
  Final_10k = sapply(results_base, function(x) x$Final_10k),
  CAGR = sapply(results_base, function(x) x$CAGR * 100),
  MaxDD = sapply(results_base, function(x) x$MaxDD * 100),
  Sharpe = sapply(results_base, function(x) x$Sharpe),
  AnnVol = sapply(results_base, function(x) x$AnnVol * 100),
  CB_Days = sapply(results_base, function(x) x$CB_Days),
  CB_Pct_Days = sapply(results_base, function(x) x$CB_Days / n_total * 100),
  CB_Episodes = sapply(results_base, function(x) x$CB_Episodes),
  stringsAsFactors = FALSE
)
df_res <- df_res[order(-df_res$Final_10k), ]

# 2. Optimized 10-day time stop cash series
cs_10_obj <- get_cash_series <- function(max_days = 10) {
  cash_ret <- numeric(n_total)
  in_ep <- FALSE
  ep_d <- 0
  for (t in 1:n_total) {
    if (macro_bull[t]) {
      cash_ret[t] <- qqq_v[t]
      in_ep <- FALSE
      ep_d <- 0
    } else if (gld_bull[t]) {
      cash_ret[t] <- gld_v[t]
      in_ep <- FALSE
      ep_d <- 0
    } else {
      if (!in_ep) { in_ep <- TRUE; ep_d <- 0 }
      ep_d <- ep_d + 1
      if (ep_d > max_days) cash_ret[t] <- sgov_v[t] else cash_ret[t] <- fb_raw[t]
    }
  }
  cash_ret
}
cash_10d <- cs_10_obj(10)

results_10d <- list()
for (nm in names(cb_list)) {
  results_10d[[nm]] <- sim_cb_portfolio(cb_list[[nm]], cash_10d)
}

df_res_10d <- data.frame(
  Threshold = names(results_10d),
  Final_10k = sapply(results_10d, function(x) x$Final_10k),
  CAGR = sapply(results_10d, function(x) x$CAGR * 100),
  MaxDD = sapply(results_10d, function(x) x$MaxDD * 100),
  Sharpe = sapply(results_10d, function(x) x$Sharpe),
  AnnVol = sapply(results_10d, function(x) x$AnnVol * 100),
  CB_Days = sapply(results_10d, function(x) x$CB_Days),
  CB_Pct_Days = sapply(results_10d, function(x) x$CB_Days / n_total * 100),
  CB_Episodes = sapply(results_10d, function(x) x$CB_Episodes),
  stringsAsFactors = FALSE
)
df_res_10d <- df_res_10d[order(-df_res_10d$Final_10k), ]

# Print formatted tables
cat("--- RESULTS: BASE CASH HEDGE (GLD / [75% FNGD + 25% SQQQ]) ---\n")
header <- sprintf("%-30s | %14s | %10s | %10s | %8s | %8s | %16s | %9s",
                  "Circuit Breaker Setting", "Final $ ($10k)", "10Y CAGR", "Max DD", "Sharpe", "Ann Vol", "Days Throttled", "Episodes")
cat(header, "\n", paste(rep("-", nchar(header)), collapse=""), "\n")
for (i in 1:nrow(df_res)) {
  cat(sprintf("%-30s | %14s | %+9.2f%% | %9.2f%% | %7.2f | %7.1f%% | %4d (%4.1f%%)   | %3d eps\n",
              df_res$Threshold[i], sprintf("$%.2fM", df_res$Final_10k[i] / 1e6), df_res$CAGR[i],
              df_res$MaxDD[i], df_res$Sharpe[i], df_res$AnnVol[i], df_res$CB_Days[i], df_res$CB_Pct_Days[i], df_res$CB_Episodes[i]))
}

cat("\n--- RESULTS: WITH 10-DAY TIME STOP CASH HEDGE (MAX 10 DAYS -> SGOV) ---\n")
cat(header, "\n", paste(rep("-", nchar(header)), collapse=""), "\n")
for (i in 1:nrow(df_res_10d)) {
  cat(sprintf("%-30s | %14s | %+9.2f%% | %9.2f%% | %7.2f | %7.1f%% | %4d (%4.1f%%)   | %3d eps\n",
              df_res_10d$Threshold[i], sprintf("$%.2fM", df_res_10d$Final_10k[i] / 1e6), df_res_10d$CAGR[i],
              df_res_10d$MaxDD[i], df_res_10d$Sharpe[i], df_res_10d$AnnVol[i], df_res_10d$CB_Days[i], df_res_10d$CB_Pct_Days[i], df_res_10d$CB_Episodes[i]))
}

