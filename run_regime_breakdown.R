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
cat(sprintf("[RegimeSim] Total 60M bars evaluated: %d from %s to %s\n", n_eval, as.character(eval_dates[1]), as.character(tail(eval_dates, 1))))

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

simulate_portfolio <- function(max_pos_vec, sec_cap_vec, sizing_mode = "vince") {
  port_daily_ret <- numeric(n_eval)
  for (t in seq_along(eval_dates)) {
    mp <- max_pos_vec[t]
    sc <- sec_cap_vec[t]
    
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
        if (cnt < sc) {
          top_idx <- c(top_idx, cand)
          sec_counts[[c_sec]] <- cnt + 1
          if (length(top_idx) >= mp) break
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
      swing_exposure <- min(1.0, n_sel / mp)
      swing_ret <- sum(weights * rets_b[top_idx])
      cash_ret  <- qqq_sub[t]
      port_daily_ret[t] <- swing_exposure * swing_ret + (1 - swing_exposure) * cash_ret
    } else {
      port_daily_ret[t] <- qqq_sub[t]
    }
  }
  
  port_xts <- xts(port_daily_ret, order.by = eval_dates)
  m_all <- calc_performance_metrics(port_xts)$raw
  
  # Year 1 (bars 1:252: ~Oct 2021 - Oct 2022: 2022 Bear Market)
  m_y1 <- calc_performance_metrics(port_xts[1:252])$raw
  # Years 2-3 (bars 253:756: ~Oct 2022 - Oct 2024: Bull recovery)
  m_y23 <- calc_performance_metrics(port_xts[253:756])$raw
  # Recent 24M (bars 757:n_eval: ~Oct 2024 - Oct 2026: AI rally)
  m_rec24 <- calc_performance_metrics(port_xts[757:n_eval])$raw
  
  list(
    cum_ret = m_all$cum_ret,
    terminal_wealth = 10000 * (1 + m_all$cum_ret),
    ann_ret = m_all$ann_ret,
    sharpe  = m_all$sharpe_ratio,
    max_dd  = m_all$max_drawdown,
    w_y1    = 10000 * (1 + m_y1$cum_ret),
    dd_y1   = m_y1$max_drawdown,
    ret_y1  = m_y1$cum_ret,
    w_y23   = 10000 * (1 + m_y23$cum_ret),
    ret_y23 = m_y23$cum_ret,
    w_rec24 = 10000 * (1 + m_rec24$cum_ret),
    ret_rec24 = m_rec24$cum_ret,
    dd_rec24 = m_rec24$max_drawdown,
    sharpe_rec24 = m_rec24$sharpe_ratio,
    daily_xts = port_xts
  )
}

# Evaluate configurations
p2_vince <- simulate_portfolio(rep(2, n_eval), rep(2, n_eval), "vince")
p2_equal <- simulate_portfolio(rep(2, n_eval), rep(2, n_eval), "equal")

p5_vince <- simulate_portfolio(rep(5, n_eval), rep(5, n_eval), "vince")
p5_equal <- simulate_portfolio(rep(5, n_eval), rep(5, n_eval), "equal")

# Switch Plan: First 24 Months MaxPos=5 / Cap=5, then remaining (next 36 Months) MaxPos=2 / Cap=2
n_first24 <- 504
n_remain  <- n_eval - n_first24
sw_fwd_vince <- simulate_portfolio(c(rep(5, n_first24), rep(2, n_remain)), c(rep(5, n_first24), rep(2, n_remain)), "vince")
sw_fwd_equal <- simulate_portfolio(c(rep(5, n_first24), rep(2, n_remain)), c(rep(5, n_first24), rep(2, n_remain)), "equal")

# Inverse Switch: Start Pos=2 (first 36M), then switch to Pos=5 for recent 24M
sw_rec_vince <- simulate_portfolio(c(rep(2, n_eval - 504), rep(5, 504)), c(rep(2, n_eval - 504), rep(5, 504)), "vince")
sw_rec_equal <- simulate_portfolio(c(rep(2, n_eval - 504), rep(5, 504)), c(rep(2, n_eval - 504), rep(5, 504)), "equal")

cat("\n=================== 60-MONTH PERFORMANCE SUMMARY ===================\n")
cat(sprintf("%-24s | %-12s | %-10s | %-8s | %-8s\n", "Strategy", "Term Wealth", "Cum Return", "Sharpe", "Max DD"))
cat("-----------------------------------------------------------------------\n")
print_row <- function(label, res) {
  cat(sprintf("%-24s | $%11.2f | %+9.1f%% | %8.2f | %7.2f%%\n",
              label, res$terminal_wealth, res$cum_ret * 100, res$sharpe, res$max_dd * 100))
}

print_row("Static Pos 2 / Cap 2 (V)", p2_vince)
print_row("Static Pos 2 / Cap 2 (E)", p2_equal)
print_row("Static Pos 5 / Cap 5 (V)", p5_vince)
print_row("Static Pos 5 / Cap 5 (E)", p5_equal)
print_row("Switch 5->2 (Vince)", sw_fwd_vince)
print_row("Switch 5->2 (Equal)", sw_fwd_equal)
print_row("Switch 2->5 (Vince)", sw_rec_vince)
print_row("Switch 2->5 (Equal)", sw_rec_equal)

cat("\n=================== REGIME DECOMPOSITION ===================\n")
cat("Period 1: 2021-2022 Bear Market (Bars 1-252)\n")
cat(sprintf("  Pos 2 (Vince): Cum Ret = %+6.1f%% | Max DD = %5.1f%%\n", p2_vince$ret_y1 * 100, p2_vince$dd_y1 * 100))
cat(sprintf("  Pos 5 (Vince): Cum Ret = %+6.1f%% | Max DD = %5.1f%%\n", p5_vince$ret_y1 * 100, p5_vince$dd_y1 * 100))

cat("\nPeriod 2: 2022-2024 Recovery (Bars 253-756)\n")
cat(sprintf("  Pos 2 (Vince): Cum Ret = %+6.1f%%\n", p2_vince$ret_y23 * 100))
cat(sprintf("  Pos 5 (Vince): Cum Ret = %+6.1f%%\n", p5_vince$ret_y23 * 100))

cat("\nPeriod 3: Recent 24 Months (Oct 2024 - Oct 2026, Bars 757-1259)\n")
cat(sprintf("  Pos 2 (Vince): Cum Ret = %+6.1f%% | Sharpe = %4.2f | Max DD = %5.1f%%\n", p2_vince$ret_rec24 * 100, p2_vince$sharpe_rec24, p2_vince$dd_rec24 * 100))
cat(sprintf("  Pos 5 (Vince): Cum Ret = %+6.1f%% | Sharpe = %4.2f | Max DD = %5.1f%%\n", p5_vince$ret_rec24 * 100, p5_vince$sharpe_rec24, p5_vince$dd_rec24 * 100))

