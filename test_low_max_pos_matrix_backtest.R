#!/usr/bin/env Rscript
# Comprehensive Backtest Matrix for:
# Max Positions: 2 vs 3 vs 4 vs 5
# Sector Caps: 2 vs 3 vs 4 vs 5 (with cap <= max_pos)
# Sizing Modes: "vince" vs "equal"
# Universe: symbols_broad.txt
# Horizons: 2M (42 bars), 6M (126 bars), 12M (252 bars), 24M (504 bars), 60M (1260 bars)
# Hold Period: 7 Days (current active model)
# Engine: Mark-to-Market Bracket Simulator

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(TTR)
})

source("R/01_data_loader.R")
source("R/08_metrics.R")

OUTPUT_DIR <- "output"
CACHE_DIR  <- "data/cache_wf"

# Load Benchmarks
vti_ohlcv <- readRDS("data/data_vti.rds")
qqq_ohlcv <- readRDS("data/data_qqq.rds")
vti_cl <- Cl(vti_ohlcv)
qqq_cl <- Cl(qqq_ohlcv)
vti_ret_all <- na.omit(vti_cl / lag.xts(vti_cl, 1) - 1)
qqq_ret_all <- na.omit(qqq_cl / lag.xts(qqq_cl, 1) - 1)

read_universe_symbols <- function(filepath) {
  if (!file.exists(filepath)) return(character(0))
  lines <- readLines(filepath, warn = FALSE)
  lines <- gsub("#.*", "", lines)
  syms  <- unique(toupper(trimws(unlist(strsplit(lines, "[, \\t\\r\\n]+")))))
  syms[syms != ""]
}

broad_syms <- read_universe_symbols("symbols_broad.txt")

# Load Sector Map
sector_map_file <- "sector_map.json"
SECTOR_MAP <- if (file.exists(sector_map_file)) jsonlite::fromJSON(sector_map_file) else list()
get_sym_sector <- function(s) {
  if (!is.null(SECTOR_MAP[[s]])) return(SECTOR_MAP[[s]])
  return("General_Tech")
}

# Load cached wf objects
base_sim_data <- list()
for (s in broad_syms) {
  cf <- file.path(CACHE_DIR, sprintf("%s_1260wf.rds", tolower(gsub("[^A-Za-z0-9]", "_", s))))
  if (file.exists(cf)) {
    base_sim_data[[s]] <- readRDS(cf)
  }
}

# Authentic Mark-to-Market Bracket Simulator for Hold Days = 7
simulate_bracket_for_hold_days <- function(sym, wf_obj, max_hold_days = 7) {
  ohlcv <- tryCatch(load_stock_data(sym, offline_only = TRUE), error = function(e) NULL)
  if (is.null(ohlcv)) return(wf_obj$bracket_ret)
  
  common_d <- wf_obj$dates
  cl <- Cl(ohlcv)[common_d]
  op <- Op(ohlcv)[common_d]
  hi <- Hi(ohlcv)[common_d]
  lo <- Lo(ohlcv)[common_d]
  
  atr_series <- tryCatch({
    as.numeric(TTR::ATR(HLC(ohlcv), n = 14)$atr[common_d])
  }, error = function(e) as.numeric(cl) * 0.02)
  atr_series[is.na(atr_series) | atr_series <= 0] <- as.numeric(cl)[is.na(atr_series) | atr_series <= 0] * 0.02
  
  gvol_v <- as.numeric(wf_obj$garch_vol)
  prob_v <- as.numeric(wf_obj$pred_prob)
  rs_v   <- as.numeric(wf_obj$rs_spy)
  
  n_days <- length(common_d)
  bracket_ret_vec <- numeric(n_days)
  
  in_pos  <- FALSE
  entry_p <- 0
  stop_p  <- 0
  t1_p    <- 0
  t2_p    <- 0
  high_p  <- 0
  days_h  <- 0
  risk_1r <- 0
  t1_hit  <- FALSE
  t2_hit  <- FALSE
  pos_shares_pct <- 0
  
  for (d in 2:n_days) {
    cur_o   <- as.numeric(op[d])
    cur_h   <- as.numeric(hi[d])
    cur_l   <- as.numeric(lo[d])
    cur_c   <- as.numeric(cl[d])
    prev_c  <- as.numeric(cl[d - 1])
    cur_atr <- atr_series[d]
    
    # Check entry at bar open
    if (!in_pos && !is.na(prob_v[d - 1]) && prob_v[d - 1] >= 0.58 && !is.na(rs_v[d - 1]) && rs_v[d - 1] > 0) {
      in_pos  <- TRUE
      entry_p <- ifelse(!is.na(cur_o) && cur_o > 0, cur_o, cur_c)
      high_p  <- entry_p
      days_h  <- 0
      t1_hit  <- FALSE
      t2_hit  <- FALSE
      pos_shares_pct <- 1.0
      
      daily_v <- pmax(0.05, gvol_v[d - 1]) / sqrt(252)
      risk_1r <- 2.0 * daily_v * entry_p
      stop_p  <- entry_p - risk_1r
      t1_p    <- entry_p + 1.5 * risk_1r
      t2_p    <- entry_p + 3.0 * risk_1r
      
      bracket_ret_vec[d] <- (cur_c - entry_p) / entry_p - (10 / 10000)
      next
    }
    
    if (in_pos) {
      days_h <- days_h + 1
      high_p <- max(high_p, cur_h)
      chand_stop <- high_p - 2.5 * cur_atr
      
      # 1. Stop loss hit
      if (cur_l <= stop_p) {
        exit_p <- min(cur_o, stop_p)
        bar_r  <- (exit_p - prev_c) / prev_c
        bracket_ret_vec[d] <- pos_shares_pct * bar_r - (10 / 10000)
        in_pos <- FALSE
        pos_shares_pct <- 0
        next
      }
      
      # 2. Tier 1 target hit: exit 50%
      if (!t1_hit && cur_h >= t1_p) {
        t1_hit <- TRUE
        pos_shares_pct <- 0.50
        stop_p <- max(entry_p, chand_stop)
      }
      
      # 3. Tier 2 target hit: exit 25%, leave 25% runner
      if (t1_hit && !t2_hit && cur_h >= t2_p) {
        t2_hit <- TRUE
        pos_shares_pct <- 0.25
        stop_p <- max(stop_p, chand_stop)
      }
      
      # Trailing stop update
      if (t1_hit) {
        stop_p <- max(stop_p, chand_stop)
      }
      
      # Time-decay ratchet: if held for (max_hold_days - 1) days and >= 1.0R profit, ratchet Tier 1 down to +1.1R
      if (!t1_hit && days_h >= (max_hold_days - 1) && cur_h >= (entry_p + 1.0 * risk_1r)) {
        t1_p <- entry_p + 1.1 * risk_1r
      }
      
      # Expiration at max_hold_days for non-runners
      if (!t2_hit && days_h >= max_hold_days) {
        bar_r <- (cur_c - prev_c) / prev_c
        bracket_ret_vec[d] <- pos_shares_pct * bar_r - (10 / 10000)
        in_pos <- FALSE
        pos_shares_pct <- 0
        next
      }
      
      bar_r <- (cur_c - prev_c) / prev_c
      bracket_ret_vec[d] <- pos_shares_pct * bar_r
    }
  }
  
  xts(bracket_ret_vec, order.by = common_d)
}

cat("Precomputing 7-day bracket series for symbols_broad.txt...\n")
custom_brackets <- list()
for (s in names(base_sim_data)) {
  custom_brackets[[s]] <- simulate_bracket_for_hold_days(s, base_sim_data[[s]], max_hold_days = 7)
}
cat("Precomputing complete.\n\n")

simulate_portfolio_matrix <- function(horizon_bars, max_pos, max_per_sector = 5, sizing_mode = "vince") {
  avail_syms <- intersect(broad_syms, names(base_sim_data))
  all_dates <- do.call(c, lapply(avail_syms, function(s) base_sim_data[[s]]$dates))
  date_counts <- table(all_dates)
  shared_dates <- as.Date(names(date_counts)[date_counts >= max(3, floor(0.20 * length(avail_syms)))])
  shared_dates <- sort(intersect(shared_dates, index(qqq_ret_all)))
  
  eval_dates <- if (length(shared_dates) < horizon_bars) shared_dates else tail(shared_dates, horizon_bars)
  n_eval <- length(eval_dates)
  
  port_daily_ret <- numeric(n_eval)
  qqq_idx <- match(eval_dates, index(qqq_ret_all))
  qqq_sub <- as.numeric(qqq_ret_all)[qqq_idx]
  qqq_sub[is.na(qqq_sub)] <- 0
  
  vti_idx <- match(eval_dates, index(vti_ret_all))
  vti_sub <- as.numeric(vti_ret_all)[vti_idx]
  vti_sub[is.na(vti_sub)] <- 0
  
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
  
  for (t in seq_along(eval_dates)) {
    p_ups   <- mat_prob[t, ]
    rs_vals <- mat_rs[t, ]
    vols    <- mat_vol[t, ]
    rets_b  <- mat_ret_b[t, ]
    
    qualify <- which(p_ups >= 0.58 & rs_vals > 0)
    top_idx <- integer(0)
    
    if (length(qualify) > 0) {
      scores <- p_ups[qualify] * (1 + rs_vals[qualify])
      ranked_candidates <- qualify[order(-scores)]
      
      # Enforce sector cap & max_pos
      sec_counts <- list()
      for (cand in ranked_candidates) {
        c_sec <- sym_sectors[cand]
        cnt <- if (is.null(sec_counts[[c_sec]])) 0 else sec_counts[[c_sec]]
        if (cnt < max_per_sector) {
          top_idx <- c(top_idx, cand)
          sec_counts[[c_sec]] <- cnt + 1
          if (length(top_idx) >= max_pos) break
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
      
      swing_exposure <- min(1.0, n_sel / max_pos)
      swing_ret <- sum(weights * rets_b[top_idx])
      cash_ret  <- qqq_sub[t]
      port_daily_ret[t] <- swing_exposure * swing_ret + (1 - swing_exposure) * cash_ret
    } else {
      port_daily_ret[t] <- qqq_sub[t]
    }
  }
  
  port_daily_ret <- xts(port_daily_ret, order.by = eval_dates)
  m_strat <- calc_performance_metrics(port_daily_ret)$raw
  term_wealth <- 10000 * (1 + m_strat$cum_ret)
  
  list(
    trading_days  = n_eval,
    terminal_wealth = sprintf("$%.2f", term_wealth),
    cum_return    = sprintf("%+.2f%%", m_strat$cum_ret * 100),
    ann_return    = sprintf("%+.2f%%", m_strat$ann_ret * 100),
    volatility    = sprintf("%.2f%%", m_strat$ann_vol * 100),
    sharpe        = sprintf("%.2f", m_strat$sharpe),
    sortino       = sprintf("%.2f", m_strat$sortino),
    max_dd        = sprintf("%.2f%%", m_strat$max_dd * 100),
    win_rate      = sprintf("%.2f%%", m_strat$win_rate * 100),
    profit_factor = sprintf("%.2f", m_strat$profit_factor),
    wealth_num    = term_wealth,
    sharpe_num    = m_strat$sharpe,
    max_dd_num    = m_strat$max_dd
  )
}

horizons <- list(
  "2 Months"  = 42,
  "6 Months"  = 126,
  "12 Months" = 252,
  "24 Months" = 504,
  "60 Months" = 1260
)

# Test matrix:
# MAX_POS in c(2, 3, 4, 5)
# MAX_PER_SECTOR in c(2, 3, 4, 5) where sector_cap <= max_pos
# Sizing in c("vince", "equal")

max_pos_list <- c(2, 3, 4, 5)
sector_caps  <- c(2, 3, 4, 5)
sizing_modes <- c("vince", "equal")
results <- list()

for (mp in max_pos_list) {
  # Only test sector_caps that are <= mp, plus mp itself
  valid_caps <- unique(pmin(mp, sector_caps))
  for (cap in valid_caps) {
    for (sm in sizing_modes) {
      cat(sprintf("Testing MAX_POS = %d | Sector Cap = %d | Sizing = '%s'...\n", mp, cap, sm))
      for (h_name in names(horizons)) {
        h_bars <- horizons[[h_name]]
        res <- simulate_portfolio_matrix(h_bars, max_pos = mp, max_per_sector = cap, sizing_mode = sm)
        results[[length(results) + 1]] <- data.frame(
          Horizon         = h_name,
          Max_Positions   = mp,
          Sector_Cap      = cap,
          Sizing_Mode     = sm,
          Trading_Days    = res$trading_days,
          Terminal_Wealth = res$terminal_wealth,
          Cum_Return      = res$cum_return,
          Ann_Return      = res$ann_return,
          Volatility      = res$volatility,
          Sharpe_Ratio    = res$sharpe,
          Sortino_Ratio   = res$sortino,
          Max_Drawdown    = res$max_dd,
          Win_Rate        = res$win_rate,
          Profit_Factor   = res$profit_factor,
          Wealth_Num      = res$wealth_num,
          Sharpe_Num      = res$sharpe_num,
          Max_DD_Num      = res$max_dd_num,
          stringsAsFactors = FALSE
        )
      }
    }
  }
}

df_res <- do.call(rbind, results)
write.csv(df_res, file.path(OUTPUT_DIR, "low_max_pos_matrix_scorecard.csv"), row.names = FALSE)
cat(sprintf("\n[Saved] Low Max Pos matrix scorecard written to output/low_max_pos_matrix_scorecard.csv (%d configurations)\n", nrow(df_res)))
