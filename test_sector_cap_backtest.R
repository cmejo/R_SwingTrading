#!/usr/bin/env Rscript
# Backtest comparing Sector Concentration Cap = 2 vs Sector Concentration Cap = 5
# Across 5d and 7d holding periods, 4 universes, and 5 horizons (2M, 6M, 12M, 24M, 60M)

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(TTR)
})

source("R/01_data_loader.R")
source("R/08_metrics.R")

OUTPUT_DIR   <- "output"
CACHE_DIR    <- "data/cache_wf"
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

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

universes <- list(
  "Broad (symbols_broad.txt)" = read_universe_symbols("symbols_broad.txt"),
  "All2 (symbols_all2.txt)"   = read_universe_symbols("symbols_all2.txt"),
  "300+ (symbols_300.txt)"    = read_universe_symbols("symbols_300.txt"),
  "Focus (symbols.txt)"       = read_universe_symbols("symbols.txt")
)

all_unique_syms <- unique(unlist(universes))

# Sector map helper
sector_map_file <- "sector_map.json"
SECTOR_MAP <- if (file.exists(sector_map_file)) jsonlite::fromJSON(sector_map_file) else list()
get_sym_sector <- function(s) {
  if (!is.null(SECTOR_MAP[[s]])) return(SECTOR_MAP[[s]])
  return("General_Tech")
}

# Load cached simulation data
base_sim_data <- list()
for (s in all_unique_syms) {
  cf <- file.path(CACHE_DIR, sprintf("%s_1260wf.rds", tolower(gsub("[^A-Za-z0-9]", "_", s))))
  if (file.exists(cf)) {
    base_sim_data[[s]] <- readRDS(cf)
  }
}

# Bracket simulation helper for hold days
simulate_bracket_for_hold_days <- function(sym, wf_obj, max_hold_days) {
  ohlcv <- tryCatch(load_stock_data(sym, offline_only = TRUE), error = function(e) NULL)
  if (is.null(ohlcv)) return(wf_obj$bracket_ret)
  
  common_d <- wf_obj$dates
  cl <- Cl(ohlcv)[common_d]
  op <- Op(ohlcv)[common_d]
  hi <- Hi(ohlcv)[common_d]
  lo <- Lo(ohlcv)[common_d]
  
  atr_v <- tryCatch({
    as.numeric(TTR::ATR(HLC(ohlcv), n = 14)$atr[common_d])
  }, error = function(e) as.numeric(cl) * 0.02)
  atr_v[is.na(atr_v) | atr_v <= 0] <- as.numeric(cl)[is.na(atr_v) | atr_v <= 0] * 0.02
  
  n_days <- length(common_d)
  b_ret  <- numeric(n_days)
  pred_prob <- as.numeric(wf_obj$pred_prob)
  rs_val    <- as.numeric(wf_obj$rs_spy)
  garch_vol <- as.numeric(wf_obj$garch_vol)
  garch_vol[is.na(garch_vol) | garch_vol < 0.05] <- 0.25
  
  in_pos   <- FALSE
  entry_p  <- 0
  stop_p   <- 0
  t1_p     <- 0
  t2_p     <- 0
  t1_hit   <- FALSE
  t2_hit   <- FALSE
  is_runner <- FALSE
  days_h   <- 0
  high_p   <- 0
  risk_1r  <- 0
  pos_shares <- 0
  rem_weight <- 0
  
  for (t in 2:n_days) {
    cur_o <- as.numeric(op[t])
    cur_h <- as.numeric(hi[t])
    cur_l <- as.numeric(lo[t])
    cur_c <- as.numeric(cl[t])
    cur_atr <- atr_v[t]
    
    # 1. Active position management
    if (in_pos) {
      days_h <- days_h + 1
      high_p <- max(high_p, cur_h)
      chandelier_p <- high_p - 2.5 * cur_atr
      cur_stop <- if (t1_hit) max(entry_p, chandelier_p) else stop_p
      
      # Stop breach
      if (cur_l <= cur_stop) {
        realized_ret <- (cur_stop / entry_p - 1)
        b_ret[t] <- b_ret[t] + rem_weight * realized_ret
        in_pos <- FALSE
        rem_weight <- 0
      } else {
        # Tier 1 target (+1.5R)
        if (!t1_hit && cur_h >= t1_p) {
          t1_hit <- TRUE
          scale_weight <- rem_weight * 0.50
          realized_ret <- (t1_p / entry_p - 1)
          b_ret[t] <- b_ret[t] + scale_weight * realized_ret
          rem_weight <- rem_weight - scale_weight
          cur_stop <- max(entry_p, high_p - 2.5 * cur_atr)
        }
        
        # Tier 2 target (+3.0R)
        if (t1_hit && !t2_hit && cur_h >= t2_p) {
          t2_hit <- TRUE
          is_runner <- TRUE
          scale_weight <- rem_weight * 0.50
          realized_ret <- (t2_p / entry_p - 1)
          b_ret[t] <- b_ret[t] + scale_weight * realized_ret
          rem_weight <- rem_weight - scale_weight
        }
        
        # Ratchet
        if (!t1_hit && days_h >= (max_hold_days - 1) && cur_h >= (entry_p + 1.0 * risk_1r)) {
          t1_p <- entry_p + 1.1 * risk_1r
        }
        
        # Time expiration
        if (!t2_hit && days_h >= max_hold_days) {
          realized_ret <- (cur_c / entry_p - 1)
          b_ret[t] <- b_ret[t] + rem_weight * realized_ret
          in_pos <- FALSE
          rem_weight <- 0
        } else {
          # Mark-to-market daily drift
          prev_c <- as.numeric(cl[t - 1])
          day_drift <- (cur_c / prev_c - 1)
          b_ret[t] <- b_ret[t] + rem_weight * day_drift
        }
      }
    }
    
    # 2. Check for fresh buy entry at bar t
    if (!in_pos) {
      prev_p <- pred_prob[t - 1]
      prev_rs <- rs_val[t - 1]
      if (!is.na(prev_p) && prev_p >= 0.58 && !is.na(prev_rs) && prev_rs > 0) {
        in_pos <- TRUE
        entry_p <- cur_o
        high_p  <- cur_o
        sig_vol <- garch_vol[t - 1] / sqrt(252)
        risk_1r <- max(0.01 * entry_p, 2.0 * sig_vol * entry_p)
        stop_p  <- entry_p - risk_1r
        t1_p    <- entry_p + 1.5 * risk_1r
        t2_p    <- entry_p + 3.0 * risk_1r
        t1_hit  <- FALSE
        t2_hit  <- FALSE
        is_runner <- FALSE
        days_h  <- 0
        rem_weight <- 1.0
        
        # Day 1 intraday drift
        day_drift <- (cur_c / entry_p - 1)
        b_ret[t] <- b_ret[t] + rem_weight * day_drift
      }
    }
  }
  
  xts(b_ret, order.by = common_d)
}

# Portfolio simulation enforcing sector cap
simulate_portfolio <- function(u_syms, horizon_bars, hold_days, max_per_sector, custom_bracket_rets) {
  avail_syms <- intersect(u_syms, names(base_sim_data))
  if (length(avail_syms) == 0) return(NULL)
  
  all_dates <- do.call(c, lapply(avail_syms, function(s) base_sim_data[[s]]$dates))
  date_counts <- table(all_dates)
  shared_dates <- as.Date(names(date_counts)[date_counts >= max(3, floor(0.20 * length(avail_syms)))])
  shared_dates <- sort(intersect(shared_dates, index(qqq_ret_all)))
  
  eval_dates <- if (length(shared_dates) < horizon_bars) shared_dates else tail(shared_dates, horizon_bars)
  n_eval <- length(eval_dates)
  if (n_eval < 10) return(NULL)
  
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
    b_ret_series <- custom_bracket_rets[[s]]
    
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
      
      # Enforce sector cap
      sec_counts <- list()
      for (cand in ranked_candidates) {
        c_sec <- sym_sectors[cand]
        cnt <- if (is.null(sec_counts[[c_sec]])) 0 else sec_counts[[c_sec]]
        if (cnt < max_per_sector) {
          top_idx <- c(top_idx, cand)
          sec_counts[[c_sec]] <- cnt + 1
          if (length(top_idx) >= 5) break
        }
      }
    }
    
    n_sel <- length(top_idx)
    if (n_sel > 0) {
      inv_vols <- 1 / vols[top_idx]
      weights  <- inv_vols / sum(inv_vols)
      swing_exposure <- min(1.0, n_sel / 5)
      swing_ret <- sum(weights * rets_b[top_idx])
      cash_ret  <- qqq_sub[t]
      port_daily_ret[t] <- swing_exposure * swing_ret + (1 - swing_exposure) * cash_ret
    } else {
      port_daily_ret[t] <- qqq_sub[t]
    }
  }
  
  port_daily_ret <- xts(port_daily_ret, order.by = eval_dates)
  qqq_xts        <- xts(qqq_sub, order.by = eval_dates)
  vti_xts        <- xts(vti_sub, order.by = eval_dates)
  
  m_strat <- calc_performance_metrics(port_daily_ret)$raw
  m_qqq   <- calc_performance_metrics(qqq_xts)$raw
  m_vti   <- calc_performance_metrics(vti_xts)$raw
  
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
    qqq_return    = sprintf("%+.2f%%", m_qqq$cum_ret * 100),
    vti_return    = sprintf("%+.2f%%", m_vti$cum_ret * 100),
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

hold_days_list <- c(5, 7)
sector_caps <- c(2, 5)

results <- list()

for (hd in hold_days_list) {
  cat(sprintf("\n>>> Generating bracket returns for Hold = %d Days <<<\n", hd))
  custom_brackets <- list()
  for (s in names(base_sim_data)) {
    custom_brackets[[s]] <- simulate_bracket_for_hold_days(s, base_sim_data[[s]], max_hold_days = hd)
  }
  
  for (cap in sector_caps) {
    cat(sprintf("Evaluating Sector Cap = %d (Hold = %dd)...\n", cap, hd))
    for (h_name in names(horizons)) {
      h_bars <- horizons[[h_name]]
      for (u_name in names(universes)) {
        u_syms <- universes[[u_name]]
        res <- simulate_portfolio(u_syms, h_bars, hd, cap, custom_brackets)
        if (!is.null(res)) {
          results[[length(results) + 1]] <- data.frame(
            Universe        = u_name,
            Sector_Cap      = cap,
            Hold_Days       = hd,
            Horizon         = h_name,
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
            QQQ_Return      = res$qqq_return,
            VTI_Return      = res$vti_return,
            Wealth_Num      = res$wealth_num,
            Sharpe_Num      = res$sharpe_num,
            Max_DD_Num      = res$max_dd_num,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
}

df_res <- do.call(rbind, results)
write.csv(df_res, file.path(OUTPUT_DIR, "sector_cap_comparison_scorecard.csv"), row.names = FALSE)
cat(sprintf("\n[Saved] Results written to %s/sector_cap_comparison_scorecard.csv (%d configurations)\n", OUTPUT_DIR, nrow(df_res)))
