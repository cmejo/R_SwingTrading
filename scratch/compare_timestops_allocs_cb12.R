suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(TTR)
})

source("R/02_trend_lmMA.R")
source("R/08_metrics.R")

cat("========================================================================================\n")
cat(" COMPARING 10, 12, 13, 15 DAY TIME STOPS WITH CIRCUIT BREAKER @ -12%\n")
cat(" Alloc A: 56.25% FNGD + 18.75% SQQQ + 25.0% SGOV\n")
cat(" Alloc B: 50.00% FNGD + 16.70% SQQQ + 33.3% SGOV\n")
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

# Raw base return series for Alloc A and Alloc B
fb_alloc_a <- 0.5625 * fngd_v + 0.1875 * sqqq_v + 0.2500 * sgov_v
fb_alloc_b <- 0.5000 * fngd_v + 0.1670 * sqqq_v + 0.3330 * sgov_v

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

# Function to generate cash return series with specific time stop and base fallback allocation
get_timed_cash_series <- function(base_fb, max_days = NULL) {
  cash_ret <- numeric(n_total)
  in_ep <- FALSE
  ep_d <- 0
  rot_days <- 0
  rot_spells <- 0
  
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
      if (!is.null(max_days) && ep_d > max_days) {
        cash_ret[t] <- sgov_v[t]
        rot_days <- rot_days + 1
        if (ep_d == (max_days + 1)) rot_spells <- rot_spells + 1
      } else {
        cash_ret[t] <- base_fb[t]
      }
    }
  }
  list(series = cash_ret, rot_days = rot_days, rot_spells = rot_spells)
}

# Function to simulate Full Portfolio with Circuit Breaker @ -12%
sim_portfolio_cb12 <- function(cash_series, cb_thresh = 0.12) {
  port_ret <- numeric(n_total)
  peak_equity <- 1.0
  cur_equity <- 1.0
  cb_active_days <- 0
  
  for (t in 1:n_total) {
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
  ann_vol <- sd(port_ret) * sqrt(252)
  sharpe <- (mean(port_ret) * 252 - 0.04) / ann_vol
  final_10k <- 10000 * tail(eq, 1)
  
  list(CAGR = cagr, Final_10k = final_10k, MaxDD = mdd, Sharpe = sharpe, AnnVol = ann_vol, CB_Days = cb_active_days)
}

calc_standalone <- function(cash_series) {
  eq <- cumprod(1 + cash_series)
  cagr <- tail(eq, 1)^(252 / n_total) - 1
  mdd <- min((eq - cummax(eq)) / cummax(eq))
  ann_vol <- sd(cash_series) * sqrt(252)
  sharpe <- (mean(cash_series) * 252 - 0.04) / ann_vol
  final_10k <- 10000 * tail(eq, 1)
  list(CAGR = cagr, Final_10k = final_10k, MaxDD = mdd, Sharpe = sharpe, AnnVol = ann_vol)
}

time_stop_days <- list(
  "10 Days" = 10,
  "12 Days" = 12,
  "13 Days" = 13,
  "15 Days" = 15,
  "No Stop" = NULL
)

run_experiments <- function(alloc_name, base_fb) {
  rows_port <- list()
  rows_stand <- list()
  
  for (ts_name in names(time_stop_days)) {
    d_limit <- time_stop_days[[ts_name]]
    obj <- get_timed_cash_series(base_fb, d_limit)
    
    p_res <- sim_portfolio_cb12(obj$series, cb_thresh = 0.12)
    s_res <- calc_standalone(obj$series)
    
    tag <- sprintf("%s | Time Stop: %s", alloc_name, ts_name)
    
    rows_port[[ts_name]] <- data.frame(
      Allocation = alloc_name,
      Time_Stop = ts_name,
      Final_10k = p_res$Final_10k,
      CAGR = p_res$CAGR * 100,
      MaxDD = p_res$MaxDD * 100,
      Sharpe = p_res$Sharpe,
      AnnVol = p_res$AnnVol * 100,
      CB_Days = p_res$CB_Days,
      Rot_Days = obj$rot_days,
      Rot_Spells = obj$rot_spells,
      stringsAsFactors = FALSE
    )
    
    rows_stand[[ts_name]] <- data.frame(
      Allocation = alloc_name,
      Time_Stop = ts_name,
      Final_10k = s_res$Final_10k,
      CAGR = s_res$CAGR * 100,
      MaxDD = s_res$MaxDD * 100,
      Sharpe = s_res$Sharpe,
      AnnVol = s_res$AnnVol * 100,
      Rot_Days = obj$rot_days,
      Rot_Spells = obj$rot_spells,
      stringsAsFactors = FALSE
    )
  }
  list(port = do.call(rbind, rows_port), stand = do.call(rbind, rows_stand))
}

exp_a <- run_experiments("Alloc A (25% SGOV)", fb_alloc_a)
exp_b <- run_experiments("Alloc B (33% SGOV)", fb_alloc_b)

df_all_port  <- rbind(exp_a$port, exp_b$port)
df_all_stand <- rbind(exp_a$stand, exp_b$stand)

df_all_port  <- df_all_port[order(-df_all_port$Final_10k), ]
df_all_stand <- df_all_stand[order(-df_all_stand$Final_10k), ]

cat("========================================================================================\n")
cat(" RANKING: FULL INTEGRATED SWING PORTFOLIO (~140 STOCKS + CB @ -12%)\n")
cat("========================================================================================\n\n")
header1 <- sprintf("%-18s | %-10s | %14s | %10s | %10s | %8s | %8s | %15s | %12s",
                   "Allocation", "Time Stop", "Final $ ($10k)", "10Y CAGR", "Max DD", "Sharpe", "Ann Vol", "Days in SGOV", "CB Throttled")
cat(header1, "\n", paste(rep("-", nchar(header1)), collapse=""), "\n")

for (i in 1:nrow(df_all_port)) {
  cat(sprintf("%-18s | %-10s | %14s | %+9.2f%% | %9.2f%% | %7.2f | %7.1f%% | %4d d (%2d eps) | %4d d (%4.1f%%)\n",
              df_all_port$Allocation[i], df_all_port$Time_Stop[i],
              sprintf("$%.2fM", df_all_port$Final_10k[i] / 1e6), df_all_port$CAGR[i],
              df_all_port$MaxDD[i], df_all_port$Sharpe[i], df_all_port$AnnVol[i],
              df_all_port$Rot_Days[i], df_all_port$Rot_Spells[i],
              df_all_port$CB_Days[i], df_all_port$CB_Days[i] / n_total * 100))
}

cat("\n========================================================================================\n")
cat(" RANKING: STANDALONE CASH SWITCH STRATEGY (100% SWITCH ONLY, ZERO SWING TRADES)\n")
cat("========================================================================================\n\n")
header2 <- sprintf("%-18s | %-10s | %14s | %10s | %10s | %8s | %8s | %15s",
                   "Allocation", "Time Stop", "Final $ ($10k)", "10Y CAGR", "Max DD", "Sharpe", "Ann Vol", "Days in SGOV")
cat(header2, "\n", paste(rep("-", nchar(header2)), collapse=""), "\n")

for (i in 1:nrow(df_all_stand)) {
  final_s <- if (df_all_stand$Final_10k[i] >= 1e6) sprintf("$%.2fM", df_all_stand$Final_10k[i] / 1e6) else sprintf("$%.0f", df_all_stand$Final_10k[i])
  cat(sprintf("%-18s | %-10s | %14s | %+9.2f%% | %9.2f%% | %7.2f | %7.1f%% | %4d d (%2d eps)\n",
              df_all_stand$Allocation[i], df_all_stand$Time_Stop[i],
              final_s, df_all_stand$CAGR[i],
              df_all_stand$MaxDD[i], df_all_stand$Sharpe[i], df_all_stand$AnnVol[i],
              df_all_stand$Rot_Days[i], df_all_stand$Rot_Spells[i]))
}

