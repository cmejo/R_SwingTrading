suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(TTR)
})

source("R/02_trend_lmMA.R")
source("R/08_metrics.R")

cat("========================================================================================\n")
cat(" RUNNING 14-STRATEGY COMPARATIVE BACKTEST: INVERSE VEHICLES (PSQ, QID, SQQQ, SOXS, FNGD)\n")
cat(" Horizons: 1M (21d), 6M (126d), 12M (252d), 24M (504d), 36M (756d), 60M (1260d), 120M (2520d)\n")
cat("========================================================================================\n\n")

# 1. Fetch live historical total return data
symbols <- c("QQQ", "GLD", "BIL", "SGOV", "PSQ", "QID", "SQQQ", "SOXS", "FNGD")
raw_data <- list()
for (s in symbols) {
  cat(sprintf("[Data] Fetching %s...\n", s))
  raw_data[[s]] <- suppressWarnings(getSymbols(s, src="yahoo", from="2010-01-01", auto.assign=FALSE))
}

# Adjusted close returns
ret_data <- list()
for (s in symbols) {
  ad <- Ad(raw_data[[s]])
  ret_data[[s]] <- na.omit(ad / lag.xts(ad, 1) - 1)
}

# SGOV spliced with BIL for pre-2020
sgov_spliced <- ret_data[["BIL"]]
sgov_common  <- intersect(index(ret_data[["SGOV"]]), index(sgov_spliced))
sgov_spliced[sgov_common] <- ret_data[["SGOV"]][sgov_common]

# 50-day lmMA Trendlines
qqq_lm <- lmMA(Cl(raw_data[["QQQ"]]), n = 50)
gld_lm <- lmMA(Cl(raw_data[["GLD"]]), n = 50)

# Full 10-year dates (using QQQ, GLD, PSQ, QID, SQQQ, SOXS, SGOV)
common_10y <- sort(intersect(intersect(intersect(intersect(intersect(
  index(ret_data[["QQQ"]]), index(ret_data[["GLD"]])), index(sgov_spliced)),
  index(ret_data[["PSQ"]])), index(ret_data[["QID"]])), index(ret_data[["SQQQ"]])))
common_10y <- sort(intersect(common_10y, index(ret_data[["SOXS"]])))
common_10y <- sort(intersect(common_10y, index(na.omit(qqq_lm$fit))))
common_10y <- sort(intersect(common_10y, index(na.omit(gld_lm$fit))))

cat(sprintf("[Dates] 10-Year master aligned dates: %d sessions from %s to %s\n",
            length(common_10y), as.character(common_10y[1]), as.character(tail(common_10y, 1))))

eval_dates <- common_10y
n_total <- length(eval_dates)

# Vectors for 10-year evaluation
qqq_v  <- as.numeric(ret_data[["QQQ"]][eval_dates])
gld_v  <- as.numeric(ret_data[["GLD"]][eval_dates])
sgov_v <- as.numeric(sgov_spliced[eval_dates])
psq_v  <- as.numeric(ret_data[["PSQ"]][eval_dates])
qid_v  <- as.numeric(ret_data[["QID"]][eval_dates])
sqqq_v <- as.numeric(ret_data[["SQQQ"]][eval_dates])
soxs_v <- as.numeric(ret_data[["SOXS"]][eval_dates])

# FNGD vector: starts 2018-01-23. For dates prior to FNGD inception, proxy with SQQQ (-3x tech)
fngd_ret <- ret_data[["FNGD"]]
fngd_v <- numeric(n_total)
for (i in 1:n_total) {
  d <- eval_dates[i]
  if (d %in% index(fngd_ret)) {
    fngd_v[i] <- as.numeric(fngd_ret[d])
  } else {
    # Prior to Jan 2018, proxy with SQQQ (3x tech bear)
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

# Define the 14 Strategy Fallbacks
# When Macro Gate is BULLISH -> qqq_v[t]
# When Macro Gate is DEFENSIVE:
#   If GLD Bull -> gld_v[t]
#   Else -> Fallback

strat_names <- c(
  "1. GLD / 100% SGOV (Baseline)",
  "2. GLD / [75% PSQ + 25% SGOV]",
  "3. GLD / [25% PSQ + 75% SGOV]",
  "4. GLD / [50% SOXS + 50% SGOV]",
  "5. GLD / [75% SOXS + 25% SGOV]",
  "6. GLD / [50% FNGD + 50% SGOV]",
  "7. GLD / [75% FNGD + 25% SGOV]",
  "8. GLD / [50% QID + 50% SGOV]",
  "9. GLD / [75% QID + 25% SGOV]",
  "10. GLD / [50% SQQQ + 50% SGOV]",
  "11. GLD / [75% SQQQ + 25% SGOV]",
  "12. GLD / [25% FNGD + 25% PSQ + 50% SGOV]",
  "13. GLD / [25% FNGD + 25% SQQQ + 50% SGOV]",
  "14. GLD / [25% QID + 25% FNGD + 50% SGOV]"
)

fallback_funcs <- list(
  # 1. 100% SGOV
  function(t) sgov_v[t],
  # 2. 75% PSQ + 25% SGOV
  function(t) 0.75 * psq_v[t] + 0.25 * sgov_v[t],
  # 3. 25% PSQ + 75% SGOV
  function(t) 0.25 * psq_v[t] + 0.75 * sgov_v[t],
  # 4. 50% SOXS + 50% SGOV
  function(t) 0.50 * soxs_v[t] + 0.50 * sgov_v[t],
  # 5. 75% SOXS + 25% SGOV
  function(t) 0.75 * soxs_v[t] + 0.25 * sgov_v[t],
  # 6. 50% FNGD + 50% SGOV
  function(t) 0.50 * fngd_v[t] + 0.50 * sgov_v[t],
  # 7. 75% FNGD + 25% SGOV
  function(t) 0.75 * fngd_v[t] + 0.25 * sgov_v[t],
  # 8. 50% QID + 50% SGOV
  function(t) 0.50 * qid_v[t] + 0.50 * sgov_v[t],
  # 9. 75% QID + 25% SGOV
  function(t) 0.75 * qid_v[t] + 0.25 * sgov_v[t],
  # 10. 50% SQQQ + 50% SGOV
  function(t) 0.50 * sqqq_v[t] + 0.50 * sgov_v[t],
  # 11. 75% SQQQ + 25% SGOV
  function(t) 0.75 * sqqq_v[t] + 0.25 * sgov_v[t],
  # 12. 25% FNGD + 25% PSQ + 50% SGOV
  function(t) 0.25 * fngd_v[t] + 0.25 * psq_v[t] + 0.50 * sgov_v[t],
  # 13. 25% FNGD + 25% SQQQ + 50% SGOV
  function(t) 0.25 * fngd_v[t] + 0.25 * sqqq_v[t] + 0.50 * sgov_v[t],
  # 14. 25% QID + 25% FNGD + 50% SGOV
  function(t) 0.25 * qid_v[t] + 0.25 * fngd_v[t] + 0.50 * sgov_v[t]
)

n_strats <- length(strat_names)

# Build daily return matrix
daily_mat <- matrix(0.0, nrow = n_total, ncol = n_strats)
for (k in 1:n_strats) {
  fb <- fallback_funcs[[k]]
  daily_mat[, k] <- sapply(1:n_total, function(t) {
    if (macro_bull[t]) return(qqq_v[t])
    if (gld_bull[t]) return(gld_v[t])
    return(fb(t))
  })
}

# Load 10-year walk-forward cache for full swing portfolio
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
  cat(sprintf(" HORIZON: %s (%d Bars | %s to %s)\n", h_name, n_bars, as.character(eval_dates[sub_idx[1]]), as.character(tail(eval_dates[sub_idx], 1))))
  cat(sprintf("========================================================================================\n"))
  cat(sprintf("%-42s | %10s | %9s | %8s | %7s | %7s | %11s\n",
              "Strategy", "Cum Return", "CAGR", "Max DD", "Sharpe", "Sortino", "End Wealth"))
  cat(paste(rep("-", 106), collapse=""), "\n")
  
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
    
    cat(sprintf("%-42s | %+9.2f%% | %+8.2f%% | %7.2f%% | %7.2f | %7.2f | $%10.2f\n",
                s_name, pm$cum_ret, pm$ann_ret, pm$max_dd, pm$sharpe, pm$sortino, pm$term_w))
  }
}

df_port <- do.call(rbind, port_rows)
df_cash <- do.call(rbind, cash_rows)

write.csv(df_port, "output/inverse_gld_portfolio_scorecard.csv", row.names = FALSE)
write.csv(df_cash, "output/inverse_gld_standalone_scorecard.csv", row.names = FALSE)
cat("\n[Saved] Output saved to output/inverse_gld_portfolio_scorecard.csv and output/inverse_gld_standalone_scorecard.csv\n")
