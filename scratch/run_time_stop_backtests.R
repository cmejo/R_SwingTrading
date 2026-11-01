suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(TTR)
})

source("R/02_trend_lmMA.R")
source("R/08_metrics.R")

cat("========================================================================================\n")
cat(" RUNNING 10-YEAR TIME STOP SENSITIVITY BACKTEST (10, 15, 20, 25, 30 DAYS -> SGOV)\n")
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

# Generate cash returns under a specific time stop
get_cash_series <- function(max_days = NULL) {
  cash_ret <- numeric(n_total)
  in_episode <- FALSE
  ep_day <- 0
  rotated_to_sgov_count <- 0
  rotated_days <- 0
  
  for (t in 1:n_total) {
    if (macro_bull[t]) {
      cash_ret[t] <- qqq_v[t]
      in_episode <- FALSE
      ep_day <- 0
    } else if (gld_bull[t]) {
      cash_ret[t] <- gld_v[t]
      in_episode <- FALSE
      ep_day <- 0
    } else {
      # In HEDGE episode
      if (!in_episode) {
        in_episode <- TRUE
        ep_day <- 0
      }
      ep_day <- ep_day + 1
      
      if (!is.null(max_days) && ep_day > max_days) {
        cash_ret[t] <- sgov_v[t]
        rotated_days <- rotated_days + 1
        if (ep_day == (max_days + 1)) rotated_to_sgov_count <- rotated_to_sgov_count + 1
      } else {
        cash_ret[t] <- fb_raw[t]
      }
    }
  }
  list(series = cash_ret, rotated_count = rotated_to_sgov_count, rotated_days = rotated_days)
}

# Simulate Full Portfolio with dynamic cash series
sim_portfolio <- function(cash_series, cb_thresh = 0.10) {
  port_ret <- numeric(n_total)
  peak_equity <- 1.0
  cur_equity <- 1.0
  
  for (t in 1:n_total) {
    dd <- (cur_equity - peak_equity) / peak_equity
    is_cb_active <- if (!is.null(cb_thresh)) (dd <= -cb_thresh) else FALSE
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
  
  list(CAGR = cagr, Final_10k = final_10k, MaxDD = mdd, Sharpe = sharpe)
}

# Standalone performance calculation
calc_standalone <- function(cash_series) {
  eq <- cumprod(1 + cash_series)
  cagr <- tail(eq, 1)^(252 / n_total) - 1
  mdd <- min((eq - cummax(eq)) / cummax(eq))
  sharpe <- (mean(cash_series) * 252 - 0.04) / (sd(cash_series) * sqrt(252))
  final_10k <- 10000 * tail(eq, 1)
  list(CAGR = cagr, Final_10k = final_10k, MaxDD = mdd, Sharpe = sharpe)
}

time_stops <- list(
  "Time Stop: Max 20 Days -> Rotate to SGOV" = 20,
  "Time Stop: Max 10 Days -> Rotate to SGOV" = 10,
  "Time Stop: Max 25 Days -> Rotate to SGOV" = 25,
  "Time Stop: Max 30 Days -> Rotate to SGOV" = 30,
  "Time Stop: Max 15 Days -> Rotate to SGOV" = 15,
  "No Time Stop (Hold Entire Hedge Spell)"   = NULL
)

port_res_list <- list()
stand_res_list <- list()
meta_list <- list()

for (nm in names(time_stops)) {
  cs_obj <- get_cash_series(time_stops[[nm]])
  port_res_list[[nm]]  <- sim_portfolio(cs_obj$series, cb_thresh = 0.10)
  stand_res_list[[nm]] <- calc_standalone(cs_obj$series)
  meta_list[[nm]] <- list(rotated_count = cs_obj$rotated_count, rotated_days = cs_obj$rotated_days)
}

# Build and sort Portfolio Data Frame
df_port <- data.frame(
  Strategy = names(port_res_list),
  Final_10k = sapply(port_res_list, function(x) x$Final_10k),
  CAGR = sapply(port_res_list, function(x) x$CAGR * 100),
  MaxDD = sapply(port_res_list, function(x) x$MaxDD * 100),
  Sharpe = sapply(port_res_list, function(x) x$Sharpe),
  Rotated_Count = sapply(meta_list, function(x) x$rotated_count),
  Rotated_Days = sapply(meta_list, function(x) x$rotated_days),
  stringsAsFactors = FALSE
)
df_port <- df_port[order(-df_port$Final_10k), ]

# Build and sort Standalone Data Frame
df_stand <- data.frame(
  Strategy = names(stand_res_list),
  Final_10k = sapply(stand_res_list, function(x) x$Final_10k),
  CAGR = sapply(stand_res_list, function(x) x$CAGR * 100),
  MaxDD = sapply(stand_res_list, function(x) x$MaxDD * 100),
  Sharpe = sapply(stand_res_list, function(x) x$Sharpe),
  Rotated_Count = sapply(meta_list, function(x) x$rotated_count),
  Rotated_Days = sapply(meta_list, function(x) x$rotated_days),
  stringsAsFactors = FALSE
)
df_stand <- df_stand[order(-df_stand$Final_10k), ]

cat("========================================================================================\n")
cat(" RANKING 1: FULL INTEGRATED SWING PORTFOLIO (~140 STOCKS + TIME STOP CASH HEDGE)\n")
cat("========================================================================================\n\n")
header1 <- sprintf("%-42s | %14s | %10s | %10s | %8s | %16s", "Time Stop Policy", "Final $ ($10k)", "10Y CAGR", "Max DD", "Sharpe", "Days in SGOV")
cat(header1, "\n", paste(rep("-", nchar(header1)), collapse=""), "\n")
for (i in 1:nrow(df_port)) {
  cat(sprintf("%-42s | %14s | %+9.2f%% | %9.2f%% | %7.2f | %4d days (%d spells)\n",
              df_port$Strategy[i], sprintf("$%.2fM", df_port$Final_10k[i] / 1e6), df_port$CAGR[i], df_port$MaxDD[i], df_port$Sharpe[i],
              df_port$Rotated_Days[i], df_port$Rotated_Count[i]))
}

cat("\n========================================================================================\n")
cat(" RANKING 2: STANDALONE CASH SWITCH STRATEGY (100% SWITCH ONLY, NO SWING TRADES)\n")
cat("========================================================================================\n\n")
header2 <- sprintf("%-42s | %14s | %10s | %10s | %8s | %16s", "Time Stop Policy", "Final $ ($10k)", "10Y CAGR", "Max DD", "Sharpe", "Days in SGOV")
cat(header2, "\n", paste(rep("-", nchar(header2)), collapse=""), "\n")
for (i in 1:nrow(df_stand)) {
  cat(sprintf("%-42s | %14s | %+9.2f%% | %9.2f%% | %7.2f | %4d days (%d spells)\n",
              df_stand$Strategy[i], sprintf("$%.2fM", df_stand$Final_10k[i] / 1e6), df_stand$CAGR[i], df_stand$MaxDD[i], df_stand$Sharpe[i],
              df_stand$Rotated_Days[i], df_stand$Rotated_Count[i]))
}

