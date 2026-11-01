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

# FNGD proxy before Jan 23, 2018: SQQQ (-3x tech bear)
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

# -------------------------------------------------------------
# Part 1: Exactly how many days in the hedge in percentage & absolute
# -------------------------------------------------------------
hedge_weight_portfolio <- numeric(n_total)
regime_names <- character(n_total)

for (t in 1:n_total) {
  is_bull <- macro_bull[t]
  is_gld  <- gld_bull[t]
  
  if (is_bull) {
    regime_names[t] <- "QQQ_BULL"
    eff_max_pos <- 3
    eff_p_long  <- 0.58
  } else if (is_gld) {
    regime_names[t] <- "GLD_BULL"
    eff_max_pos <- 2
    eff_p_long  <- 0.65
  } else {
    regime_names[t] <- "HEDGE_FALLBACK"
    eff_max_pos <- 2
    eff_p_long  <- 0.65
  }
  
  p_ups   <- mat_prob[t, ]
  rs_vals <- mat_rs[t, ]
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
  
  # Hedge exposure is only positive when in HEDGE_FALLBACK regime
  if (regime_names[t] == "HEDGE_FALLBACK") {
    # If 0 positions: 100% in hedge
    # If 1 position: 66.7% in hedge
    # If 2 positions: 33.3% in hedge
    hedge_weight_portfolio[t] <- 1.0 - (n_sel / 3.0)
  } else {
    hedge_weight_portfolio[t] <- 0.0
  }
}

cat("=== PART 1: EXACT DAYS & PERCENTAGES IN THE HEDGE (10-YEAR / 2,520 TRADING DAYS) ===\n")
cat("Total trading days analyzed: 2,520\n\n")

# Distribution of hedge exposure
days_0pct   <- sum(hedge_weight_portfolio == 0.0)
days_33pct  <- sum(abs(hedge_weight_portfolio - 1/3) < 0.01)
days_67pct  <- sum(abs(hedge_weight_portfolio - 2/3) < 0.01)
days_100pct <- sum(abs(hedge_weight_portfolio - 1.0) < 0.01)

cat(sprintf("1. Days NOT in the hedge AT ALL (0%% hedge exposure):\n"))
cat(sprintf("   - Absolute: %d trading days\n", days_0pct))
cat(sprintf("   - Percentage: %.2f%%\n", days_0pct / n_total * 100))
cat(sprintf("   (Breakdown: %d days in QQQ Bullish + %d days in GLD Bullish)\n\n", 
            sum(regime_names == "QQQ_BULL"), sum(regime_names == "GLD_BULL")))

cat(sprintf("2. Days NOT in the hedge FULLY (less than 100%% hedge exposure):\n"))
days_not_fully <- sum(hedge_weight_portfolio < 0.99)
cat(sprintf("   - Absolute: %d trading days\n", days_not_fully))
cat(sprintf("   - Percentage: %.2f%%\n\n", days_not_fully / n_total * 100))

cat(sprintf("3. Days PARTIALLY in the hedge (33.3%% or 66.7%%):\n"))
cat(sprintf("   - Exactly 33.3%% hedge (2 swing positions active): %d days (%.2f%%)\n", 
            days_33pct, days_33pct / n_total * 100))
cat(sprintf("   - Exactly 66.7%% hedge (1 swing position active):  %d days (%.2f%%)\n\n", 
            days_67pct, days_67pct / n_total * 100))

cat(sprintf("4. Days FULLY in the hedge (100%% hedge exposure):\n"))
cat(sprintf("   - Absolute: %d trading days\n", days_100pct))
cat(sprintf("   - Percentage: %.2f%%\n\n", days_100pct / n_total * 100))

# -------------------------------------------------------------
# Part 2: Compare Fallback Allocations:
# Alloc A: 56.25% FNGD + 18.75% SQQQ + 25.0% SGOV
# Alloc B: 50.00% FNGD + 16.70% SQQQ + 33.3% SGOV
# vs Prior Champion: 75% FNGD + 25% SQQQ (0% SGOV)
# vs Baseline: 100% SGOV
# -------------------------------------------------------------
cat("=== PART 2: COMPARING FALLBACK ALLOCATION SWITCHES ===\n")

# Daily fallback returns
fb_champion <- 0.75 * fngd_v + 0.25 * sqqq_v
fb_alloc_a  <- 0.5625 * fngd_v + 0.1875 * sqqq_v + 0.25 * sgov_v
fb_alloc_b  <- 0.5000 * fngd_v + 0.1667 * sqqq_v + 0.3333 * sgov_v
fb_baseline <- sgov_v

fbs <- list(
  "Alloc A: 56.25% FNGD + 18.75% SQQQ + 25% SGOV" = fb_alloc_a,
  "Alloc B: 50% FNGD + 16.7% SQQQ + 33.3% SGOV"    = fb_alloc_b,
  "Prior Champion: 75% FNGD + 25% SQQQ (0% SGOV)"   = fb_champion,
  "Baseline: 100% SGOV (0% Inverse)"                = fb_baseline
)

compare_strats <- names(fbs)
n_comp <- length(compare_strats)

daily_comp_mat <- matrix(0.0, nrow = n_total, ncol = n_comp)
for (k in 1:n_comp) {
  fb_vec <- fbs[[k]]
  daily_comp_mat[, k] <- sapply(1:n_total, function(t) {
    if (macro_bull[t]) return(qqq_v[t])
    if (gld_bull[t]) return(gld_v[t])
    return(fb_vec[t])
  })
}

sim_portfolio_comp <- function(strat_idx, sub_idx) {
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
    cash_day_ret <- daily_comp_mat[t, strat_idx]
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
  "36 Months" = 756,
  "60 Months" = 1260,
  "84 Months" = 1764,
  "120 Months"= 2520
)

run_comp_suite <- function(is_portfolio = FALSE) {
  rows <- list()
  for (k in 1:n_comp) {
    s_name <- compare_strats[k]
    for (h_name in names(horizon_defs)) {
      h_days <- horizon_defs[[h_name]]
      sub_idx <- (n_total - h_days + 1):n_total
      if (is_portfolio) {
        r_series <- sim_portfolio_comp(k, sub_idx)
      } else {
        r_series <- daily_comp_mat[sub_idx, k]
      }
      eq_curve <- cumprod(1 + r_series)
      tot_ret <- tail(eq_curve, 1) - 1
      ann_factor <- 252 / h_days
      cagr <- (1 + tot_ret)^ann_factor - 1
      roll_max <- cummax(eq_curve)
      dd <- (eq_curve - roll_max) / roll_max
      mdd <- min(dd)
      ann_vol <- sd(r_series) * sqrt(252)
      sharpe <- if (ann_vol > 0) (mean(r_series) * 252 - 0.04) / ann_vol else 0
      final_10k <- 10000 * (1 + tot_ret)
      rows[[length(rows) + 1]] <- data.frame(
        Strategy = s_name,
        Horizon = h_name,
        Days = h_days,
        Total_Return_Pct = tot_ret * 100,
        Ann_Return_CAGR = cagr * 100,
        Final_10k = final_10k,
        Max_Drawdown_Pct = mdd * 100,
        Sharpe_Ratio = sharpe,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

df_comp_port  <- run_comp_suite(is_portfolio = TRUE)
df_comp_stand <- run_comp_suite(is_portfolio = FALSE)

print_comp_tables <- function(df, title) {
  cat("\n========================================================================================\n")
  cat(paste(" RANKING SUMMARY:", title, "\n"))
  cat("========================================================================================\n\n")
  
  df_10y <- df[df$Horizon == "120 Months", ]
  df_10y <- df_10y[order(-df_10y$Final_10k), ]
  ranked_strats <- df_10y$Strategy
  
  h_names <- c("1 Month", "6 Months", "12 Months", "36 Months", "60 Months", "84 Months", "120 Months")
  
  cat("--- ANNUALIZED RETURNS (CAGR %) ACROSS ALL TIMELINES ---\n")
  header <- sprintf("%-48s | %8s | %8s | %8s | %8s | %8s | %8s | %9s", 
                    "Strategy", "1M CAGR", "6M CAGR", "12M CAGR", "36M CAGR", "60M CAGR", "84M CAGR", "120M CAGR")
  cat(header, "\n", paste(rep("-", nchar(header)), collapse=""), "\n")
  
  for (st in ranked_strats) {
    vals <- sapply(h_names, function(hn) {
      sub_r <- df[df$Strategy == st & df$Horizon == hn, ]
      sprintf("%+7.2f%%", sub_r$Ann_Return_CAGR)
    })
    cat(sprintf("%-48s | %s | %s | %s | %s | %s | %s | %s\n",
                st, vals[1], vals[2], vals[3], vals[4], vals[5], vals[6], vals[7]))
  }
  
  cat("\n--- 10-YEAR TERMINAL METRICS (Growth of $10,000) ---\n")
  header2 <- sprintf("%-48s | %14s | %10s | %10s | %8s",
                     "Strategy", "Final $ ($10k)", "10Y CAGR", "Max DD", "Sharpe")
  cat(header2, "\n", paste(rep("-", nchar(header2)), collapse=""), "\n")
  for (st in ranked_strats) {
    sub_10y <- df[df$Strategy == st & df$Horizon == "120 Months", ]
    final_str <- if (sub_10y$Final_10k >= 1e6) sprintf("$%.2fM", sub_10y$Final_10k / 1e6) else sprintf("$%.0f", sub_10y$Final_10k)
    cat(sprintf("%-48s | %14s | %+9.2f%% | %9.2f%% | %7.2f\n",
                st, final_str, sub_10y$Ann_Return_CAGR, sub_10y$Max_Drawdown_Pct, sub_10y$Sharpe_Ratio))
  }
}

print_comp_tables(df_comp_port, "FULL INTEGRATED SWING PORTFOLIO (~140 STOCKS)")
print_comp_tables(df_comp_stand, "STANDALONE CASH SWITCH STRATEGY (100% SWITCH)")

