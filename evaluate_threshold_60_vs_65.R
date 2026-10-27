suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(TTR)
  library(jsonlite)
})

source("R/08_metrics.R")

cat("================================================================================\n")
cat(" RIGOROUS EMPIRICAL EVALUATION: LOWERING DEFENSIVE THRESHOLD (65% -> 60%)\n")
cat(" Walk-Forward Out-of-Sample Simulation (1,259 Trading Days: Oct 2021 - Oct 2026)\n")
cat("================================================================================\n\n")

CACHE_DIR  <- "data/cache_wf"
qqq_ohlcv  <- readRDS("data/data_qqq.rds")
qqq_cl     <- Cl(qqq_ohlcv)
qqq_ret_all <- na.omit(qqq_cl / lag.xts(qqq_cl, 1) - 1)

# Compute 50-day linear regression moving average for QQQ macro gate
compute_lmma <- function(x_xts, n = 50) {
  x <- as.numeric(x_xts)
  res <- rep(NA_real_, length(x))
  time_seq <- seq_len(n)
  for (i in n:length(x)) {
    window_vals <- x[(i - n + 1):i]
    if (!any(is.na(window_vals))) {
      fit <- lm(window_vals ~ time_seq)
      res[i] <- as.numeric(tail(fitted(fit), 1))
    }
  }
  xts(res, order.by = index(x_xts))
}

qqq_lmma <- compute_lmma(qqq_cl, 50)
qqq_macro_bull <- as.numeric(qqq_cl) >= as.numeric(qqq_lmma)
qqq_macro_bull[is.na(qqq_macro_bull)] <- TRUE

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

avail_syms <- names(base_sim_data)
cat(sprintf("Loaded cached walk-forward data for %d symbols across broad universe.\n", length(avail_syms)))

# Find shared evaluation dates
all_dates <- unlist(lapply(base_sim_data, function(x) as.character(index(x$bracket_ret))))
date_counts <- table(all_dates)
shared_dates <- as.Date(names(date_counts)[date_counts >= max(3, floor(0.20 * length(avail_syms)))])
shared_dates <- sort(intersect(shared_dates, index(qqq_ret_all)))

eval_dates <- tail(shared_dates, 1259)
n_eval <- length(eval_dates)

qqq_idx <- match(eval_dates, index(qqq_ret_all))
qqq_sub <- as.numeric(qqq_ret_all)[qqq_idx]
qqq_sub[is.na(qqq_sub)] <- 0

qqq_bull_idx <- match(eval_dates, index(qqq_cl))
is_bull_vec <- qqq_macro_bull[qqq_bull_idx]
is_bull_vec[is.na(is_bull_vec)] <- TRUE

n_bull_days <- sum(is_bull_vec)
n_def_days  <- sum(!is_bull_vec)
cat(sprintf("Total Simulation Horizon: %d trading days (%.1f years)\n", n_eval, n_eval / 252))
cat(sprintf("  -> Bull Days (QQQ >= 50D lmMA):     %d days (%.1f%%)\n", n_bull_days, n_bull_days / n_eval * 100))
cat(sprintf("  -> Defensive Days (QQQ < 50D lmMA): %d days (%.1f%%)\n\n", n_def_days, n_def_days / n_eval * 100))

n_syms <- length(avail_syms)
mat_prob  <- matrix(0.5,  nrow = n_eval, ncol = n_syms)
mat_rs    <- matrix(0.0,  nrow = n_eval, ncol = n_syms)
mat_vol   <- matrix(0.25, nrow = n_eval, ncol = n_syms)
mat_ret_b <- matrix(0.0,  nrow = n_eval, ncol = n_syms)

for (j in seq_along(avail_syms)) {
  s <- avail_syms[j]
  obj <- base_sim_data[[s]]
  
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
  
  idx_b <- match(eval_dates, index(obj$bracket_ret))
  v_b <- !is.na(idx_b)
  if (any(v_b)) mat_ret_b[v_b, j] <- as.numeric(obj$bracket_ret)[idx_b[v_b]]
}

sym_sectors <- sapply(avail_syms, get_sym_sector)

# Simulation function
simulate_strategy <- function(defensive_p_cutoff, bull_p_cutoff = 0.58, max_pos_bull = 3, max_pos_bear = 2, sec_cap = 3) {
  rets <- numeric(n_eval)
  trade_counts_def <- 0
  trade_counts_bull <- 0
  ret_def_trades   <- numeric(0)
  
  # Track exposure
  def_active_days <- 0
  
  for (t in 1:n_eval) {
    p_ups   <- mat_prob[t, ]
    rs_vals <- mat_rs[t, ]
    vols    <- mat_vol[t, ]
    rets_b  <- mat_ret_b[t, ]
    
    is_bull <- is_bull_vec[t]
    p_req   <- if (is_bull) bull_p_cutoff else defensive_p_cutoff
    mp      <- if (is_bull) max_pos_bull else max_pos_bear
    
    qualify <- which(p_ups >= p_req & rs_vals > 0)
    top_idx <- integer(0)
    
    if (length(qualify) > 0) {
      scores <- p_ups[qualify] * (1 + rs_vals[qualify])
      ranked_candidates <- qualify[order(-scores)]
      
      sec_counts <- list()
      for (cand in ranked_candidates) {
        c_sec <- sym_sectors[cand]
        cnt <- if (is.null(sec_counts[[c_sec]])) 0 else sec_counts[[c_sec]]
        if (cnt < sec_cap) {
          top_idx <- c(top_idx, cand)
          sec_counts[[c_sec]] <- cnt + 1
          if (length(top_idx) >= mp) break
        }
      }
    }
    
    n_sel <- length(top_idx)
    if (n_sel > 0) {
      inv_vols <- 1 / vols[top_idx]
      weights  <- inv_vols / sum(inv_vols)
      swing_exposure <- min(1.0, n_sel / mp)
      swing_ret <- sum(weights * rets_b[top_idx])
      
      cash_ret  <- if (is_bull) qqq_sub[t] else 0.0
      rets[t] <- swing_exposure * swing_ret + (1 - swing_exposure) * cash_ret
      
      if (!is_bull) {
        def_active_days  <- def_active_days + 1
        trade_counts_def <- trade_counts_def + n_sel
        ret_def_trades   <- c(ret_def_trades, rets_b[top_idx])
      } else {
        trade_counts_bull <- trade_counts_bull + n_sel
      }
    } else {
      rets[t] <- if (is_bull) qqq_sub[t] else 0.0
    }
  }
  
  port_xts <- xts(rets, order.by = eval_dates)
  m_all <- calc_performance_metrics(port_xts)$raw
  
  # Sub-periods
  # 1. 2022 Full Bear Market: Oct 2021 to Dec 2022 (first 315 bars)
  idx_2022 <- 1:min(315, n_eval)
  m_2022 <- calc_performance_metrics(port_xts[idx_2022])$raw
  
  # 2. 2023 Recovery & Chop: Jan 2023 to Dec 2023 (bars 316 to 567)
  idx_2023 <- 316:min(567, n_eval)
  m_2023 <- calc_performance_metrics(port_xts[idx_2023])$raw
  
  # 3. 2024-2026 AI Expansion & Recent Chop: Jan 2024 to present (bars 568 to n_eval)
  idx_recent <- min(568, n_eval):n_eval
  m_recent <- calc_performance_metrics(port_xts[idx_recent])$raw
  
  # Defensive Trades Performance
  def_win_rate <- if (length(ret_def_trades) > 0) mean(ret_def_trades > 0) * 100 else 0
  def_avg_ret  <- if (length(ret_def_trades) > 0) mean(ret_def_trades) * 100 else 0
  pos_trades   <- ret_def_trades[ret_def_trades > 0]
  neg_trades   <- ret_def_trades[ret_def_trades < 0]
  def_pf       <- if (length(neg_trades) > 0 && abs(sum(neg_trades)) > 0) sum(pos_trades) / abs(sum(neg_trades)) else 1.0
  
  list(
    thresh            = defensive_p_cutoff,
    terminal_wealth   = 10000 * (1 + m_all$cum_ret),
    cum_ret           = m_all$cum_ret * 100,
    ann_ret           = m_all$ann_ret * 100,
    sharpe            = if (!is.null(m_all$sharpe)) as.numeric(m_all$sharpe) else 0,
    sortino           = if (!is.null(m_all$sortino)) as.numeric(m_all$sortino) else 0,
    max_dd            = if (!is.null(m_all$max_dd)) as.numeric(m_all$max_dd) * 100 else 0,
    ret_2022_bear     = m_2022$cum_ret * 100,
    dd_2022_bear      = if (!is.null(m_2022$max_dd)) as.numeric(m_2022$max_dd) * 100 else 0,
    ret_2023_recov    = m_2023$cum_ret * 100,
    ret_recent_bull   = m_recent$cum_ret * 100,
    def_trades_count  = trade_counts_def,
    def_active_days   = def_active_days,
    def_win_rate      = def_win_rate,
    def_avg_ret       = def_avg_ret,
    def_profit_factor = def_pf
  )
}

# Run the comparative analysis
cutoffs <- c(0.70, 0.68, 0.65, 0.62, 0.60, 0.58)
results <- lapply(cutoffs, simulate_strategy)

cat("===================================================================================================================\n")
cat(sprintf("%-10s | %-13s | %-9s | %-7s | %-7s | %-9s | %-11s | %-11s | %-11s\n",
            "Def Cutoff", "Terminal $", "Cum Ret", "Sharpe", "Max DD", "2022 Ret", "2022 MaxDD", "2023 Recov", "2024-26 Bull"))
cat("-------------------------------------------------------------------------------------------------------------------\n")
for (r in results) {
  is_cur <- if (r$thresh == 0.65) " (Current)" else if (r$thresh == 0.60) " (Proposed)" else ""
  lbl <- sprintf("%2.0f%%%s", r$thresh * 100, is_cur)
  cat(sprintf("%-10s | $%11.2f | %+7.1f%% | %7.2f | %6.2f%% | %+7.1f%% | %10.2f%% | %+9.1f%% | %+10.1f%%\n",
              lbl, r$terminal_wealth, r$cum_ret, r$sharpe, r$max_dd, r$ret_2022_bear, r$dd_2022_bear, r$ret_2023_recov, r$ret_recent_bull))
}
cat("===================================================================================================================\n\n")

cat("========================================================================================================\n")
cat(" DEFENSIVE TRADING BEHAVIOR (Trades taken while QQQ was in Bearish Trend Deficit)\n")
cat("========================================================================================================\n")
cat(sprintf("%-10s | %-12s | %-12s | %-10s | %-11s | %-12s\n",
            "Def Cutoff", "Total Trades", "Days Active", "Win Rate", "Avg Trade", "Profit Factor"))
cat("--------------------------------------------------------------------------------------------------------\n")
for (r in results) {
  lbl <- sprintf("%2.0f%%", r$thresh * 100)
  cat(sprintf("%-10s | %12d | %12d | %9.1f%% | %+10.2f%% | %12.2f\n",
              lbl, r$def_trades_count, r$def_active_days, r$def_win_rate, r$def_avg_ret, r$def_profit_factor))
}
cat("========================================================================================================\n\n")

# Save output to JSON
jsonlite::write_json(results, "output/threshold_eval_results.json", auto_unbox = TRUE, pretty = TRUE)
cat("Detailed metrics successfully saved to output/threshold_eval_results.json\n")
