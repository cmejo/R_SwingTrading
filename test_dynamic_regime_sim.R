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
shared_dates <- index(custom_brackets[[avail_syms[1]]])
for (s in avail_syms[-1]) shared_dates <- intersect(shared_dates, index(custom_brackets[[s]]))
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

simulate_portfolio <- function(max_pos_schedule, sector_cap_schedule, sizing_mode = "vince") {
  port_daily_ret <- numeric(n_eval)
  for (t in seq_along(eval_dates)) {
    mp <- max_pos_schedule[t]
    sc <- sector_cap_schedule[t]
    
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
  
  # Also split into first 36 months (bars 1:756) and last 24 months (bars 757:1260)
  m_first36 <- calc_performance_metrics(port_xts[1:756])$raw
  m_last24  <- calc_performance_metrics(port_xts[757:1260])$raw
  
  list(
    cum_ret = m_all$cum_ret,
    terminal_wealth = 10000 * (1 + m_all$cum_ret),
    ann_ret = m_all$ann_ret,
    sharpe  = m_all$sharpe_ratio,
    max_dd  = m_all$max_drawdown,
    w_first36 = 10000 * (1 + m_first36$cum_ret),
    w_last24  = 10000 * (1 + m_last24$cum_ret),
    dd_first36 = m_first36$max_drawdown,
    dd_last24  = m_last24$max_drawdown
  )
}

# 1. Static MaxPos=2, Cap=2
sched_pos_2 <- rep(2, n_eval)
sched_cap_2 <- rep(2, n_eval)

# 2. Static MaxPos=5, Cap=5
sched_pos_5 <- rep(5, n_eval)
sched_cap_5 <- rep(5, n_eval)

# 3. User's proposed switch: MaxPos=5/Cap=5 for first 24M (or last 24M), then switch
# Chronological timeline: 60M window starts at bar 1 (late 2021) and ends at bar 1260 (today, late 2026).
# Recent 24 months = bars 757 to 1260!
# User asked: "MAX_POS=5 for first 24 months and then change for next 36 months to MAX_POS=2"
sched_pos_switch_fwd <- c(rep(5, 504), rep(2, n_eval - 504))
sched_cap_switch_fwd <- c(rep(5, 504), rep(2, n_eval - 504))

# Also test: Start at Pos 2 (for first 36M), then switch to Pos 5 (recent 24M)
sched_pos_switch_rec <- c(rep(2, 756), rep(5, n_eval - 756))
sched_cap_switch_rec <- c(rep(2, 756), rep(5, n_eval - 756))

cat("Simulating across strategies...\n")
res_2_vince <- simulate_portfolio(sched_pos_2, sched_cap_2, "vince")
res_2_equal <- simulate_portfolio(sched_pos_2, sched_cap_2, "equal")

res_5_vince <- simulate_portfolio(sched_pos_5, sched_cap_5, "vince")
res_5_equal <- simulate_portfolio(sched_pos_5, sched_cap_5, "equal")

res_sw_fwd_vince <- simulate_portfolio(sched_pos_switch_fwd, sched_cap_switch_fwd, "vince")
res_sw_fwd_equal <- simulate_portfolio(sched_pos_switch_fwd, sched_cap_switch_fwd, "equal")

res_sw_rec_vince <- simulate_portfolio(sched_pos_switch_rec, sched_cap_switch_rec, "vince")
res_sw_rec_equal <- simulate_portfolio(sched_pos_switch_rec, sched_cap_switch_rec, "equal")

df_results <- data.frame(
  Strategy = c("Static Pos 2 / Cap 2", "Static Pos 5 / Cap 5", "Switch Fwd (5->2)", "Switch Rec (2->5)"),
  Vince_60M_Wealth = c(res_2_vince$terminal_wealth, res_5_vince$terminal_wealth, res_sw_fwd_vince$terminal_wealth, res_sw_rec_vince$terminal_wealth),
  Vince_Sharpe = c(res_2_vince$sharpe, res_5_vince$sharpe, res_sw_fwd_vince$sharpe, res_sw_rec_vince$sharpe),
  Vince_MaxDD = c(res_2_vince$max_dd, res_5_vince$max_dd, res_sw_fwd_vince$max_dd, res_sw_rec_vince$max_dd),
  Equal_60M_Wealth = c(res_2_equal$terminal_wealth, res_5_equal$terminal_wealth, res_sw_fwd_equal$terminal_wealth, res_sw_rec_equal$terminal_wealth),
  Equal_Sharpe = c(res_2_equal$sharpe, res_5_equal$sharpe, res_sw_fwd_equal$sharpe, res_sw_rec_equal$sharpe),
  Equal_MaxDD = c(res_2_equal$max_dd, res_5_equal$max_dd, res_sw_fwd_equal$max_dd, res_sw_rec_equal$max_dd)
)

print(df_results)

cat("\nSub-Period Performance (First 36M: Oct 2021 - Oct 2024 vs Recent 24M: Oct 2024 - Oct 2026):\n")
cat(sprintf("Pos 2 (Vince): First 36M Wealth = $%.2f (DD: %.1f%%) | Recent 24M Wealth = $%.2f (DD: %.1f%%)\n",
    res_2_vince$w_first36, res_2_vince$dd_first36 * 100, res_2_vince$w_last24, res_2_vince$dd_last24 * 100))
cat(sprintf("Pos 5 (Vince): First 36M Wealth = $%.2f (DD: %.1f%%) | Recent 24M Wealth = $%.2f (DD: %.1f%%)\n",
    res_5_vince$w_first36, res_5_vince$dd_first36 * 100, res_5_vince$w_last24, res_5_vince$dd_last24 * 100))
