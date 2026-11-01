suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(TTR)
})

source("R/02_trend_lmMA.R")
source("R/08_metrics.R")

cat("========================================================================================\n")
cat(" RUNNING GLD SWITCH VS GLD + PSQ/SGOV BLEND SWITCH BACKTESTS\n")
cat(" Horizons: 1M, 6M, 12M, 24M, 36M, 60M, 120M\n")
cat("========================================================================================\n\n")

# Load saved historical data or fetch
qqq_raw  <- suppressWarnings(getSymbols("QQQ",  src="yahoo", from="2014-01-01", auto.assign=FALSE))
psq_raw  <- suppressWarnings(getSymbols("PSQ",  src="yahoo", from="2014-01-01", auto.assign=FALSE))
gld_raw  <- suppressWarnings(getSymbols("GLD",  src="yahoo", from="2014-01-01", auto.assign=FALSE))
bil_raw  <- suppressWarnings(getSymbols("BIL",  src="yahoo", from="2014-01-01", auto.assign=FALSE))
sgov_raw <- suppressWarnings(getSymbols("SGOV", src="yahoo", from="2020-05-26", auto.assign=FALSE))

qqq_ret  <- na.omit(Ad(qqq_raw) / lag.xts(Ad(qqq_raw), 1) - 1)
psq_ret  <- na.omit(Ad(psq_raw) / lag.xts(Ad(psq_raw), 1) - 1)
gld_ret  <- na.omit(Ad(gld_raw) / lag.xts(Ad(gld_raw), 1) - 1)
bil_ret  <- na.omit(Ad(bil_raw) / lag.xts(Ad(bil_raw), 1) - 1)
sgov_ret <- na.omit(Ad(sgov_raw) / lag.xts(Ad(sgov_raw), 1) - 1)

sgov_spliced <- bil_ret
sgov_common  <- intersect(index(sgov_ret), index(sgov_spliced))
sgov_spliced[sgov_common] <- sgov_ret[sgov_common]

qqq_lm <- lmMA(Cl(qqq_raw), n = 50)
gld_lm <- lmMA(Cl(gld_raw), n = 50)

all_dates <- sort(intersect(intersect(intersect(index(qqq_ret), index(psq_ret)), index(gld_ret)), index(sgov_spliced)))
valid_lm_dates <- sort(intersect(index(na.omit(qqq_lm$fit)), index(na.omit(gld_lm$fit))))
eval_dates <- sort(intersect(all_dates, valid_lm_dates))

qqq_v  <- as.numeric(qqq_ret[eval_dates])
psq_v  <- as.numeric(psq_ret[eval_dates])
gld_v  <- as.numeric(gld_ret[eval_dates])
sgov_v <- as.numeric(sgov_spliced[eval_dates])

qqq_cl_v   <- as.numeric(Cl(qqq_raw)[eval_dates])
qqq_fit_v  <- as.numeric(qqq_lm$fit[eval_dates])
qqq_slp_v  <- as.numeric(qqq_lm$slope[eval_dates])
macro_bull <- (qqq_cl_v >= qqq_fit_v) & (qqq_slp_v > 0)

gld_cl_v   <- as.numeric(Cl(gld_raw)[eval_dates])
gld_fit_v  <- as.numeric(gld_lm$fit[eval_dates])
gld_slp_v  <- as.numeric(gld_lm$slope[eval_dates])
gld_bull   <- (gld_cl_v >= gld_fit_v) & (gld_slp_v > 0)

n_total <- length(eval_dates)

# Define the Strategies:
# S1: GLD / SGOV switch (GLD if GLD bull, else 100% SGOV)
s1_func <- function(t) {
  if (macro_bull[t]) return(qqq_v[t])
  if (gld_bull[t]) return(gld_v[t]) else return(sgov_v[t])
}

# S2: GLD and 75% PSQ / 25% SGOV switch (GLD if GLD bull, else 75% PSQ / 25% SGOV)
s2_func <- function(t) {
  if (macro_bull[t]) return(qqq_v[t])
  if (gld_bull[t]) return(gld_v[t]) else return(0.75 * psq_v[t] + 0.25 * sgov_v[t])
}

# S3: GLD and 50% PSQ / 50% SGOV switch (GLD if GLD bull, else 50% PSQ / 50% SGOV)
s3_func <- function(t) {
  if (macro_bull[t]) return(qqq_v[t])
  if (gld_bull[t]) return(gld_v[t]) else return(0.50 * psq_v[t] + 0.50 * sgov_v[t])
}

# S4: GLD and 25% PSQ / 75% SGOV switch (GLD if GLD bull, else 25% PSQ / 75% SGOV)
s4_func <- function(t) {
  if (macro_bull[t]) return(qqq_v[t])
  if (gld_bull[t]) return(gld_v[t]) else return(0.25 * psq_v[t] + 0.75 * sgov_v[t])
}

# Also test Simultaneous Co-Allocation (When QQQ Bearish, 50% GLD (if bull, else SGOV) + 50% PSQ/SGOV blend)
# S5: 50% GLD-Switch + 50% (75% PSQ / 25% SGOV)
s5_func <- function(t) {
  if (macro_bull[t]) return(qqq_v[t])
  gld_part <- if (gld_bull[t]) gld_v[t] else sgov_v[t]
  hedge_part <- 0.75 * psq_v[t] + 0.25 * sgov_v[t]
  return(0.50 * gld_part + 0.50 * hedge_part)
}

# S6: 50% GLD-Switch + 50% (50% PSQ / 50% SGOV)
s6_func <- function(t) {
  if (macro_bull[t]) return(qqq_v[t])
  gld_part <- if (gld_bull[t]) gld_v[t] else sgov_v[t]
  hedge_part <- 0.50 * psq_v[t] + 0.50 * sgov_v[t]
  return(0.50 * gld_part + 0.50 * hedge_part)
}

# S7: 50% GLD-Switch + 50% (25% PSQ / 75% SGOV)
s7_func <- function(t) {
  if (macro_bull[t]) return(qqq_v[t])
  gld_part <- if (gld_bull[t]) gld_v[t] else sgov_v[t]
  hedge_part <- 0.25 * psq_v[t] + 0.75 * sgov_v[t]
  return(0.50 * gld_part + 0.50 * hedge_part)
}

strat_names <- c(
  "GLD / 100% SGOV Switch",
  "GLD / (75% PSQ + 25% SGOV) Switch",
  "GLD / (50% PSQ + 50% SGOV) Switch",
  "GLD / (25% PSQ + 75% SGOV) Switch",
  "50% GLD + 50% (75% PSQ/25% SGOV)",
  "50% GLD + 50% (50% PSQ/50% SGOV)",
  "50% GLD + 50% (25% PSQ/75% SGOV)"
)

strat_funcs <- list(s1_func, s2_func, s3_func, s4_func, s5_func, s6_func, s7_func)
n_strats <- length(strat_names)

daily_mat <- matrix(0.0, nrow = n_total, ncol = n_strats)
for (k in 1:n_strats) {
  daily_mat[, k] <- sapply(1:n_total, strat_funcs[[k]])
}

# Load 10-year walk-forward cache for swing portfolio integration
CACHE_DIR_10Y <- "data/cache_10yr"
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

sim_portfolio <- function(strat_idx, sub_idx) {
  n_sub <- length(sub_idx)
  port_ret <- numeric(n_sub)
  for (i in seq_along(sub_idx)) {
    t <- sub_idx[i]
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
    cash_day_ret <- daily_mat[t, strat_idx]
    
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

horizon_defs <- list(
  "1 Month"   = 21,
  "6 Months"  = 126,
  "12 Months" = 252,
  "24 Months" = 504,
  "36 Months" = 756,
  "60 Months" = 1260,
  "120 Months"= min(2520, n_total)
)

evaluate_metrics <- function(r_vec) {
  n_days <- length(r_vec)
  term_w <- 10000 * prod(1 + r_vec)
  cum_ret <- (term_w / 10000 - 1) * 100
  ann_ret <- ((term_w / 10000)^(252 / n_days) - 1) * 100
  
  eq <- cumprod(1 + r_vec)
  peak <- cummax(eq)
  dd <- (eq - peak) / peak
  max_dd <- min(dd) * 100
  
  daily_sd <- sd(r_vec)
  ann_vol <- daily_sd * sqrt(252) * 100
  sharpe <- if (daily_sd > 0) (mean(r_vec) / daily_sd) * sqrt(252) else 0.0
  
  downside <- r_vec[r_vec < 0]
  down_sd <- if (length(downside) > 1) sd(downside) else 0.0
  sortino <- if (down_sd > 0) (mean(r_vec) / down_sd) * sqrt(252) else 0.0
  
  list(
    cum_ret = cum_ret,
    ann_ret = ann_ret,
    max_dd = max_dd,
    sharpe = sharpe,
    sortino = sortino,
    ann_vol = ann_vol,
    term_w = term_w
  )
}

port_rows <- list()
cash_rows <- list()

for (h_name in names(horizon_defs)) {
  n_bars <- horizon_defs[[h_name]]
  sub_idx <- (n_total - n_bars + 1):n_total
  
  cat(sprintf("\n========================================================================================\n"))
  cat(sprintf(" HORIZON: %s (%d Bars)\n", h_name, n_bars))
  cat(sprintf("========================================================================================\n"))
  cat(sprintf("%-36s | %10s | %9s | %8s | %7s | %7s | %11s\n",
              "Strategy", "Cum Return", "CAGR", "Max DD", "Sharpe", "Sortino", "End Wealth"))
  cat(paste(rep("-", 100), collapse=""), "\n")
  
  for (k in 1:n_strats) {
    s_name <- strat_names[k]
    # Portfolio
    p_ret <- sim_portfolio(k, sub_idx)
    pm <- evaluate_metrics(p_ret)
    port_rows[[length(port_rows) + 1]] <- data.frame(
      Horizon = h_name, Days = n_bars, Strategy = s_name,
      Cum_Return = pm$cum_ret, CAGR = pm$ann_ret, Max_DD = pm$max_dd,
      Sharpe = pm$sharpe, Sortino = pm$sortino, Ann_Vol = pm$ann_vol,
      Terminal_Wealth = pm$term_w, stringsAsFactors = FALSE
    )
    
    # Standalone Cash
    c_ret <- daily_mat[sub_idx, k]
    cm <- evaluate_metrics(c_ret)
    cash_rows[[length(cash_rows) + 1]] <- data.frame(
      Horizon = h_name, Days = n_bars, Strategy = s_name,
      Cum_Return = cm$cum_ret, CAGR = cm$ann_ret, Max_DD = cm$max_dd,
      Sharpe = cm$sharpe, Sortino = cm$sortino, Ann_Vol = cm$ann_vol,
      Terminal_Wealth = cm$term_w, stringsAsFactors = FALSE
    )
    
    cat(sprintf("%-36s | %+9.2f%% | %+8.2f%% | %7.2f%% | %7.2f | %7.2f | $%10.2f\n",
                s_name, pm$cum_ret, pm$ann_ret, pm$max_dd, pm$sharpe, pm$sortino, pm$term_w))
  }
}

df_port <- do.call(rbind, port_rows)
df_cash <- do.call(rbind, cash_rows)

write.csv(df_port, "output/gld_psq_sgov_portfolio_scorecard.csv", row.names = FALSE)
write.csv(df_cash, "output/gld_psq_sgov_standalone_scorecard.csv", row.names = FALSE)
cat("\nSaved scorecards to output/gld_psq_sgov_portfolio_scorecard.csv and output/gld_psq_sgov_standalone_scorecard.csv\n")
