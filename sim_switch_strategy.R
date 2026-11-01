suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(TTR)
})

source("R/01_data_loader.R")
source("R/08_metrics.R")

CACHE_DIR  <- "data/cache_wf"
qqq_ohlcv  <- readRDS("data/data_qqq.rds")
qqq_cl     <- Cl(qqq_ohlcv)
qqq_ret_all <- na.omit(qqq_cl / lag.xts(qqq_cl, 1) - 1)

read_universe_symbols <- function(filepath) {
  if (!file.exists(filepath)) return(character(0))
  lines <- readLines(filepath, warn = FALSE)
  lines <- gsub("#.*", "", lines)
  syms  <- unique(toupper(trimws(unlist(strsplit(lines, "[, \\t\\r\\n]+")))))
  syms[syms != ""]
}
broad_syms <- read_universe_symbols("symbols_broad.txt")

SECTOR_MAP <- if (file.exists("sector_map.json")) jsonlite::fromJSON("sector_map.json") else list()
get_sym_sector <- function(s) {
  if (!is.null(SECTOR_MAP[[s]])) return(SECTOR_MAP[[s]])
  return("General_Tech")
}

base_sim_data <- list()
for (s in broad_syms) {
  cf <- file.path(CACHE_DIR, sprintf("%s_1260wf.rds", tolower(gsub("[^A-Za-z0-9]", "_", s))))
  if (file.exists(cf)) base_sim_data[[s]] <- readRDS(cf)
}

simulate_bracket_for_hold_days <- function(sym, wf_obj, max_hold_days = 7) {
  df <- tryCatch(load_symbol_data(sym, start_date = "2020-01-01"), error = function(e) NULL)
  if (is.null(df) || nrow(df) < 50) return(xts(rep(0, length(index(wf_obj$pred_prob))), order.by = index(wf_obj$pred_prob)))
  
  common_idx <- intersect(index(df), index(wf_obj$pred_prob))
  if (length(common_idx) < 30) return(xts(rep(0, length(index(wf_obj$pred_prob))), order.by = index(wf_obj$pred_prob)))
  
  df_sub <- df[common_idx]
  c_sub  <- as.numeric(Cl(df_sub))
  h_sub  <- as.numeric(Hi(df_sub))
  l_sub  <- as.numeric(Lo(df_sub))
  n_bars <- length(common_idx)
  daily_ret_mtm <- numeric(n_bars)
  
  atr_series <- as.numeric(ATR(HLC(df_sub), n = 14)$atr)
  atr_series[is.na(atr_series)] <- c_sub[is.na(atr_series)] * 0.02
  
  pos_active <- FALSE
  entry_p <- 0; stop_p <- 0; pt_p <- 0; bars_held <- 0; prev_c <- 0
  
  for (i in 1:n_bars) {
    curr_c <- c_sub[i]; curr_h <- h_sub[i]; curr_l <- l_sub[i]; curr_atr <- atr_series[i]
    if (pos_active) {
      bars_held <- bars_held + 1
      is_stopped <- (curr_l <= stop_p)
      is_pt      <- (curr_h >= pt_p)
      is_time    <- (bars_held >= max_hold_days)
      
      if (is_stopped) {
        exit_p <- min(prev_c, stop_p)
        daily_ret_mtm[i] <- (exit_p / prev_c) - 1
        pos_active <- FALSE
      } else if (is_pt) {
        exit_p <- max(prev_c, pt_p)
        daily_ret_mtm[i] <- (exit_p / prev_c) - 1
        pos_active <- FALSE
      } else if (is_time) {
        daily_ret_mtm[i] <- (curr_c / prev_c) - 1
        pos_active <- FALSE
      } else {
        daily_ret_mtm[i] <- (curr_c / prev_c) - 1
        stop_p <- max(stop_p, curr_c - 1.5 * curr_atr)
        prev_c <- curr_c
      }
    } else {
      entry_p <- curr_c
      stop_p  <- entry_p - 1.5 * curr_atr
      pt_p    <- entry_p + 2.5 * curr_atr
      prev_c  <- entry_p
      bars_held <- 0
      pos_active <- TRUE
      daily_ret_mtm[i] <- 0
    }
  }
  return(xts(daily_ret_mtm, order.by = common_idx))
}

custom_brackets <- list()
for (s in names(base_sim_data)) {
  custom_brackets[[s]] <- simulate_bracket_for_hold_days(s, base_sim_data[[s]], 7)
}

avail_syms <- names(custom_brackets)
all_b_dates <- unlist(lapply(custom_brackets, function(x) as.character(index(x))))
date_counts <- table(all_b_dates)
shared_dates <- as.Date(names(date_counts)[date_counts >= max(3, floor(0.20 * length(avail_syms)))])
shared_dates <- sort(intersect(shared_dates, index(qqq_ret_all)))

eval_dates <- tail(shared_dates, 1260)
n_eval <- length(eval_dates)

qqq_idx <- match(eval_dates, index(qqq_ret_all))
qqq_sub <- as.numeric(qqq_ret_all)[qqq_idx]
qqq_sub[is.na(qqq_sub)] <- 0

n_syms <- length(avail_syms)
mat_prob  <- matrix(0.5,  nrow = n_eval, ncol = n_syms)
mat_rs    <- matrix(0.0,  nrow = n_eval, ncol = n_syms)
mat_vol   <- matrix(0.25, nrow = n_eval, ncol = n_syms)
mat_ret_b <- matrix(0.0,  nrow = n_eval, ncol = n_syms)

for (j in seq_along(avail_syms)) {
  s <- avail_syms[j]
  obj <- base_sim_data[[s]]
  b_ret_series <- custom_brackets[[s]]
  
  idx_p <- match(eval_dates, index(obj$pred_prob))
  v_p <- !is.na(idx_p)
  if (any(v_p)) mat_prob[v_p, j] <- as.numeric(obj$pred_prob)[idx_p[v_p]]
  
  idx_rs <- match(eval_dates, index(obj$rs_spy))
  v_rs <- !is.na(idx_rs)
  if (any(v_rs)) mat_rs[v_rs, j] <- as.numeric(obj$rs_spy)[idx_rs[v_rs]]
  
  idx_v <- match(eval_dates, index(obj$garch_vol))
  v_v <- !is.na(idx_v)
  if (any(v_v)) {
    raw_vol <- as.numeric(obj$garch_vol)[idx_v[v_v]]
    raw_vol[is.na(raw_vol) | raw_vol < 0.05] <- 0.25
    mat_vol[v_v, j] <- raw_vol
  }
  
  idx_b <- match(eval_dates, index(b_ret_series))
  v_b <- !is.na(idx_b)
  if (any(v_b)) mat_ret_b[v_b, j] <- as.numeric(b_ret_series)[idx_b[v_b]]
}

sym_sectors <- sapply(avail_syms, get_sym_sector)

get_daily_rets <- function(max_pos_val, sec_cap_val, sizing_mode = "vince") {
  rets <- numeric(n_eval)
  for (t in 1:n_eval) {
    p_ups   <- mat_prob[t, ]
    rs_vals <- mat_rs[t, ]
    vols    <- mat_vol[t, ]
    rets_b  <- mat_ret_b[t, ]
    
    qualify <- which(p_ups >= 0.58 & rs_vals > 0)
    top_idx <- integer(0)
    
    if (length(qualify) > 0) {
      scores <- p_ups[qualify] * (1 + rs_vals[qualify])
      ranked_candidates <- qualify[order(-scores)]
      
      sec_counts <- list()
      for (cand in ranked_candidates) {
        c_sec <- sym_sectors[cand]
        cnt <- if (is.null(sec_counts[[c_sec]])) 0 else sec_counts[[c_sec]]
        if (cnt < sec_cap_val) {
          top_idx <- c(top_idx, cand)
          sec_counts[[c_sec]] <- cnt + 1
          if (length(top_idx) >= max_pos_val) break
        }
      }
    }
    
    n_sel <- length(top_idx)
    if (n_sel > 0) {
      if (sizing_mode == "equal") {
        weights <- rep(1 / n_sel, n_sel)
      } else {
        inv_vols <- 1 / vols[top_idx]
        weights  <- inv_vols / sum(inv_vols)
      }
      swing_exposure <- min(1.0, n_sel / max_pos_val)
      swing_ret <- sum(weights * rets_b[top_idx])
      cash_ret  <- qqq_sub[t]
      rets[t] <- swing_exposure * swing_ret + (1 - swing_exposure) * cash_ret
    } else {
      rets[t] <- qqq_sub[t]
    }
  }
  rets
}

r2_v <- get_daily_rets(2, 2, "vince")
r2_e <- get_daily_rets(2, 2, "equal")
r5_v <- get_daily_rets(5, 5, "vince")
r5_e <- get_daily_rets(5, 5, "equal")

# Evaluate combinations
calc_stats <- function(r_vec) {
  x <- xts(r_vec, order.by = eval_dates)
  m <- calc_performance_metrics(x)$raw
  w <- 10000 * prod(1 + r_vec)
  list(wealth = w, ret = (w / 10000 - 1) * 100, sharpe = m$sharpe_ratio, dd = m$max_drawdown * 100)
}

# 1. Static 2
s2_v <- calc_stats(r2_v)
s2_e <- calc_stats(r2_e)

# 2. Static 5
s5_v <- calc_stats(r5_v)
s5_e <- calc_stats(r5_e)

# 3. User's proposed switch: Start Month 1-24 at Pos 5 / Cap 5, then Month 25-60 at Pos 2 / Cap 2
# In chronological order: bars 1:504 (Months 1-24), bars 505:1259 (Months 25-60)
sw_v <- calc_stats(c(r5_v[1:504], r2_v[505:n_eval]))
sw_e <- calc_stats(c(r5_e[1:504], r2_e[505:n_eval]))

# 4. Reverse switch: Start Month 1-36 at Pos 2 / Cap 2, then Month 37-60 at Pos 5 / Cap 5
rev_v <- calc_stats(c(r2_v[1:755], r5_v[756:n_eval]))
rev_e <- calc_stats(c(r2_e[1:755], r5_e[756:n_eval]))

cat("\n=================== 60-MONTH SIMULATION RESULTS ===================\n")
cat(sprintf("%-32s | %-14s | %-10s | %-7s | %-7s\n", "Strategy", "Terminal Wealth", "Cum Return", "Sharpe", "Max DD"))
cat("--------------------------------------------------------------------------------\n")
report_line <- function(lbl, s) {
  cat(sprintf("%-32s | $%13.2f | %+9.1f%% | %7.2f | %6.2f%%\n", lbl, s$wealth, s$ret, s$sharpe, s$dd))
}

report_line("Static MAX_POS=2 (Vince)", s2_v)
report_line("Static MAX_POS=2 (Equal)", s2_e)
report_line("Static MAX_POS=5 (Vince)", s5_v)
report_line("Static MAX_POS=5 (Equal)", s5_e)
report_line("Switch 5->2 at M24 (Vince)", sw_v)
report_line("Switch 5->2 at M24 (Equal)", sw_e)
report_line("Switch 2->5 at M36 (Vince)", rev_v)
report_line("Switch 2->5 at M36 (Equal)", rev_e)

# Now inspect the compound growth in subperiods
cat("\n=================== PERIOD SUB-DIVISIONS ===================\n")
# First 36 Months (Bars 1 to 755: Oct 2021 to Oct 2024)
w_p2_v_m36 <- 10000 * prod(1 + r2_v[1:755])
w_p5_v_m36 <- 10000 * prod(1 + r5_v[1:755])
w_p2_e_m36 <- 10000 * prod(1 + r2_e[1:755])
w_p5_e_m36 <- 10000 * prod(1 + r5_e[1:755])

cat(sprintf("Month 1 to 36 (Oct 2021 - Oct 2024, Bear + Early Recovery):\n"))
cat(sprintf("  MAX_POS=2 (Vince): $10,000 -> $%10.2f\n", w_p2_v_m36))
cat(sprintf("  MAX_POS=5 (Vince): $10,000 -> $%10.2f\n", w_p5_v_m36))
cat(sprintf("  MAX_POS=2 (Equal): $10,000 -> $%10.2f\n", w_p2_e_m36))
cat(sprintf("  MAX_POS=5 (Equal): $10,000 -> $%10.2f\n", w_p5_e_m36))

# Last 24 Months (Bars 756 to 1259: Oct 2024 to Oct 2026)
mult_p2_v_last24 <- prod(1 + r2_v[756:n_eval])
mult_p5_v_last24 <- prod(1 + r5_v[756:n_eval])
mult_p2_e_last24 <- prod(1 + r2_e[756:n_eval])
mult_p5_e_last24 <- prod(1 + r5_e[756:n_eval])

cat(sprintf("\nLast 24 Months (Oct 2024 - Oct 2026, Recent AI Rally):\n"))
cat(sprintf("  MAX_POS=2 (Vince): %.2fx gain (from $10k alone: $%10.2f)\n", mult_p2_v_last24, 10000 * mult_p2_v_last24))
cat(sprintf("  MAX_POS=5 (Vince): %.2fx gain (from $10k alone: $%10.2f)\n", mult_p5_v_last24, 10000 * mult_p5_v_last24))
cat(sprintf("  MAX_POS=2 (Equal): %.2fx gain (from $10k alone: $%10.2f)\n", mult_p2_e_last24, 10000 * mult_p2_e_last24))
cat(sprintf("  MAX_POS=5 (Equal): %.2fx gain (from $10k alone: $%10.2f)\n", mult_p5_e_last24, 10000 * mult_p5_e_last24))
