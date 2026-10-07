#!/usr/bin/env Rscript
# ==============================================================================
# Comprehensive Multi-Universe & Multi-Horizon Backtest Engine
# Evaluates 5 Stock Universes across 5 Key Horizons:
#
# Universes:
#   1. Focus Watchlist (symbols.txt)
#   2. Broad Universe (symbols_broad.txt)
#   3. Institutional 300+ (symbols_300.txt)
#   4. All Stocks Flat (symbols_all.txt)
#   5. All Stocks Structured (symbols_all2.txt)
#
# Horizons:
#   1. 2 Months (42 Trading Days)
#   2. 12 Months (252 Trading Days)
#   3. 24 Months (504 Trading Days)
#   4. 60 Months (1,260 Trading Days / 5 Years)
#   5. June through September 2026 (83 Trading Days: 2026-06-01 to 2026-09-30)
#
# Strategy Models:
#   - BASELINE Model (Equal-weight basket, Target Vol = 0.30, 0% Cash Yield, Standard Bracket)
#   - ENHANCED Model (Top-5 RS Ranking, Target Vol = 0.45, Core-Satellite QQQ Cash Yield, 25% Chandelier Runner)
#   - Benchmarks: SPY & QQQ
# ==============================================================================

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(glmnet)
  library(tseries)
  library(TTR)
})

source("R/01_data_loader.R")
source("R/02_trend_lmMA.R")
source("R/03_volatility_garch.R")
source("R/04_feature_pipeline.R")
source("R/05_logistic_model.R")
source("R/06_swing_backtest.R")
source("R/08_metrics.R")

OUTPUT_DIR   <- "output"
CACHE_DIR    <- "data/cache_wf"
ARTIFACT_DIR <- "/Users/cmejo/.gemini/antigravity/brain/785859a9-21ba-4446-b7f5-980fd7d1dd48"
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(CACHE_DIR,  showWarnings = FALSE, recursive = TRUE)

TOTAL_BARS_REQ <- 1260 # 60 Months

cat("\n========================================================================================\n")
cat(sprintf(" COMPREHENSIVE 5-UNIVERSE MULTI-HORIZON BACKTEST ENGINE (MAX BARS: %d / 60 MONTHS)\n", TOTAL_BARS_REQ))
cat("========================================================================================\n\n")

# Load Benchmarks
spy_ohlcv <- load_stock_data("SPY", offline_only = TRUE)
qqq_ohlcv <- load_stock_data("QQQ", offline_only = TRUE)
spy_cl    <- Cl(spy_ohlcv)
qqq_cl    <- Cl(qqq_ohlcv)

spy_ret_all <- na.omit(spy_cl / lag.xts(spy_cl, 1) - 1)
qqq_ret_all <- na.omit(qqq_cl / lag.xts(qqq_cl, 1) - 1)

# Helper to read symbols file
read_universe_symbols <- function(filepath) {
  if (!file.exists(filepath)) return(character(0))
  lines <- readLines(filepath, warn = FALSE)
  lines <- gsub("#.*", "", lines)
  syms  <- unique(toupper(trimws(unlist(strsplit(lines, "[, \\t\\r\\n]+")))))
  syms[syms != ""]
}

syms_focus <- read_universe_symbols("symbols.txt")
syms_broad <- read_universe_symbols("symbols_broad.txt")
syms_300   <- read_universe_symbols("symbols_300.txt")
syms_all   <- read_universe_symbols("symbols_all.txt")
syms_all2  <- read_universe_symbols("symbols_all2.txt")

universes <- list(
  "Focus (symbols.txt)"        = syms_focus,
  "Broad (symbols_broad.txt)"  = syms_broad,
  "300+ (symbols_300.txt)"     = syms_300,
  "All (symbols_all.txt)"      = syms_all,
  "All2 (symbols_all2.txt)"    = syms_all2
)

all_unique_syms <- unique(c(syms_focus, syms_broad, syms_300, syms_all, syms_all2))
cat(sprintf("Loaded 5 Universes:\n"))
for (un in names(universes)) {
  cat(sprintf("  - %-25s: %d symbols\n", un, length(universes[[un]])))
}
cat(sprintf("\nTotal Distinct Unique Symbols to Simulate: %d\n\n", length(all_unique_syms)))

# ------------------------------------------------------------------------------
# 1. Individual Stock Walk-Forward Simulation (up to 1,260 bars / 60 months)
# ------------------------------------------------------------------------------
simulate_stock_walkforward <- function(sym) {
  cache_file <- file.path(CACHE_DIR, sprintf("%s_1260wf.rds", tolower(gsub("[^A-Za-z0-9]", "_", sym))))
  if (file.exists(cache_file)) {
    return(readRDS(cache_file))
  }
  
  res <- tryCatch({
    ohlcv <- load_stock_data(sym, offline_only = TRUE)
    if (is.null(ohlcv) || nrow(ohlcv) < 180) return(NULL)
    
    n_raw <- nrow(ohlcv)
    approx_tr_end <- max(1, n_raw - TOTAL_BARS_REQ - 55)
    pipe <- build_feature_dataset(ohlcv, fast_n = 20, slow_n = 50, look_ahead = 5, train_idx = 1:approx_tr_end)
    df_m <- pipe$model_data
    feat_names <- pipe$feature_names
    total_bars <- nrow(df_m)
    
    if (total_bars < 150) return(NULL)
    
    act_test_bars  <- min(TOTAL_BARS_REQ, total_bars - 50)
    test_start_idx <- total_bars - act_test_bars + 1
    test_dates     <- df_m$Date[test_start_idx:total_bars]
    
    train_window  <- min(200, test_start_idx - 1)
    pred_class    <- numeric(act_test_bars)
    pred_probs    <- numeric(act_test_bars)
    current_model <- NULL
    
    # Monthly walk-forward retraining with Purged CV & Platt calibration
    for (i in 1:act_test_bars) {
      cur_idx <- test_start_idx + i - 1
      if (i %% 20 == 1 || is.null(current_model)) {
        tr_start <- max(1, cur_idx - train_window)
        tr_end   <- cur_idx - 5 # 5-day embargo
        if (tr_end > (tr_start + 25)) {
          X_tr <- as.matrix(df_m[tr_start:tr_end, feat_names])
          y_tr <- df_m$TargetBinary[tr_start:tr_end]
          if (length(unique(y_tr)) >= 2) {
            current_model <- train_swing_model(
              X_train       = X_tr,
              y_train       = y_tr,
              feature_names = feat_names,
              alpha         = 0.5,
              calibrate     = TRUE,
              embargo_days  = 5
            )
          }
        }
      }
      
      if (is.null(current_model)) {
        pred_class[i] <- 0
        pred_probs[i] <- 0.5
        next
      }
      
      x_cur <- matrix(as.numeric(df_m[cur_idx, feat_names]), nrow = 1)
      raw_p <- as.numeric(predict(current_model$cv_fit, newx = x_cur, s = "lambda.min", type = "response"))
      if (!is.null(current_model$calibrator)) {
        raw_link <- as.numeric(predict(current_model$cv_fit, newx = x_cur, s = "lambda.min", type = "link"))
        cal_p <- as.numeric(predict(current_model$calibrator, newdata = data.frame(Link = raw_link), type = "response"))
        p <- if (!is.na(cal_p)) cal_p else raw_p
      } else {
        p <- raw_p
      }
      pred_probs[i] <- p
      pred_class[i] <- ifelse(p >= 0.58, 1, ifelse(p <= 0.42, -1, 0))
    }
    
    # Extract asset prices aligned to test dates
    cl <- Cl(ohlcv)[test_dates]
    op <- Op(ohlcv)[test_dates]
    hi <- Hi(ohlcv)[test_dates]
    lo <- Lo(ohlcv)[test_dates]
    
    asset_ret <- na.omit(diff(cl) / lag.xts(cl, 1))
    common_d  <- index(asset_ret)
    
    # Next-day open-to-close entry return (eliminates overnight gap lookahead)
    open_ret <- (cl[common_d] - op[common_d]) / pmax(op[common_d], 1e-4)
    
    # GARCH Volatility series for sizing
    garch_vol_vec <- df_m$GARCH_Vol[test_start_idx:total_bars]
    garch_vol_xts <- xts(garch_vol_vec, order.by = test_dates)[common_d]
    
    # Align signals with lag 1
    sig_xts <- xts(pred_class, order.by = test_dates)
    pos_xts <- lag.xts(sig_xts, 1)[common_d]
    pos_xts[is.na(pos_xts)] <- 0
    
    # 20-Day Relative Strength vs SPY
    spy_sub <- spy_cl[test_dates]
    spy_20  <- lag.xts(spy_sub, 20)
    cl_20   <- lag.xts(cl, 20)
    rs_xts  <- ((cl / cl_20) / (spy_sub / spy_20) - 1)[common_d]
    rs_xts[is.na(rs_xts)] <- 0
    
    prob_xts <- xts(pred_probs, order.by = test_dates)[common_d]
    
    # Compute ATR14 for Chandelier Trailing Stop
    atr_xts <- tryCatch({
      TTR::ATR(HLC(ohlcv), n = 14)$atr[common_d]
    }, error = function(e) cl[common_d] * 0.02)
    
    # Baseline Strategy Returns: target_vol = 0.30, cost = 10 bps
    vol_weights_base <- pmin(1.0, 0.30 / pmax(garch_vol_xts, 0.05))
    vol_weights_base <- lag.xts(vol_weights_base, 1)
    vol_weights_base[is.na(vol_weights_base)] <- 1.0
    eff_pos_base <- pos_xts * vol_weights_base
    
    is_entry <- (pos_xts > 0 & lag.xts(pos_xts, 1) == 0)
    is_entry[is.na(is_entry)] <- FALSE
    bar_rets <- ifelse(is_entry, open_ret, asset_ret)
    base_gross <- eff_pos_base * bar_rets
    pos_chg <- abs(diff(eff_pos_base))
    pos_chg[is.na(pos_chg)] <- 0
    base_net_ret <- base_gross - pos_chg * (10 / 10000)
    
    # Enhanced Bracket with 25% Chandelier Runner Lot
    n_days <- length(common_d)
    c_p_vec <- as.numeric(cl[common_d])
    o_p_vec <- as.numeric(op[common_d])
    h_p_vec <- as.numeric(hi[common_d])
    l_p_vec <- as.numeric(lo[common_d])
    atr_vec <- as.numeric(atr_xts)
    gvol_v  <- as.numeric(garch_vol_xts)
    sig_v   <- as.numeric(sig_xts[common_d])
    
    bracket_ret_vec <- numeric(n_days)
    in_pos <- FALSE
    pos_shares_pct <- 0
    entry_p <- 0
    stop_p  <- 0
    t1_p    <- 0
    t2_p    <- 0
    high_p  <- 0
    days_h  <- 0
    t1_hit  <- FALSE
    t2_hit  <- FALSE
    
    for (d in 2:n_days) {
      cur_sig <- sig_v[d - 1]
      cur_o   <- o_p_vec[d]
      cur_h   <- h_p_vec[d]
      cur_l   <- l_p_vec[d]
      cur_c   <- c_p_vec[d]
      prev_c  <- c_p_vec[d - 1]
      cur_atr <- ifelse(is.na(atr_vec[d]), cur_c * 0.02, atr_vec[d])
      
      # Entry trigger
      if (!in_pos && !is.na(cur_sig) && cur_sig == 1) {
        in_pos <- TRUE
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
        
        # Stop loss hit
        if (cur_l <= stop_p) {
          exit_p <- min(cur_o, stop_p)
          bar_r  <- (exit_p - prev_c) / prev_c
          bracket_ret_vec[d] <- pos_shares_pct * bar_r - (10 / 10000)
          in_pos <- FALSE
          pos_shares_pct <- 0
          next
        }
        
        # Tier 1 target hit: exit 50%
        if (!t1_hit && cur_h >= t1_p) {
          t1_hit <- TRUE
          pos_shares_pct <- 0.50
          stop_p <- max(entry_p, chand_stop)
        }
        
        # Tier 2 target hit: exit 25%, leave 25% runner
        if (t1_hit && !t2_hit && cur_h >= t2_p) {
          t2_hit <- TRUE
          pos_shares_pct <- 0.25
          stop_p <- max(stop_p, chand_stop)
        }
        
        # Trailing stop update for active position / runner
        if (t1_hit) {
          stop_p <- max(stop_p, chand_stop)
        }
        
        # 5-day expiration for non-runner
        if (!t2_hit && days_h >= 5) {
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
    
    bracket_ret_xts <- xts(bracket_ret_vec, order.by = common_d)
    
    out_obj <- list(
      symbol        = sym,
      dates         = common_d,
      cl            = cl[common_d],
      asset_ret     = asset_ret,
      base_ret      = base_net_ret,
      bracket_ret   = bracket_ret_xts,
      garch_vol     = garch_vol_xts,
      pred_prob     = prob_xts,
      pred_class    = pos_xts,
      rs_spy        = rs_xts
    )
    
    saveRDS(out_obj, cache_file)
    out_obj
  }, error = function(e) {
    NULL
  })
  
  return(res)
}

# Run walk-forward simulation across all unique symbols sequentially with progress logging
cat(sprintf("Simulating walk-forward models across %d unique symbols...\n", length(all_unique_syms)))
t_start <- Sys.time()

sim_results <- list()
for (idx in seq_along(all_unique_syms)) {
  s <- all_unique_syms[idx]
  res <- simulate_stock_walkforward(s)
  if (!is.null(res)) {
    sim_results[[s]] <- res
  }
  if (idx %% 20 == 0 || idx == length(all_unique_syms)) {
    elapsed <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
    pct <- round(idx / length(all_unique_syms) * 100, 1)
    cat(sprintf("  [%3d/%3d - %5.1f%%] Valid: %3d | Elapsed: %5.1fs\n",
                idx, length(all_unique_syms), pct, length(sim_results), elapsed))
  }
}

t_end <- Sys.time()
cat(sprintf("\nWalk-Forward Simulation complete! Valid assets: %d / %d (Elapsed: %.1f seconds)\n\n",
            length(sim_results), length(all_unique_syms), as.numeric(difftime(t_end, t_start, units = "secs"))))

# ------------------------------------------------------------------------------
# 2. Portfolio Construction & Evaluation Engine
# ------------------------------------------------------------------------------
evaluate_portfolio <- function(sym_list, horizon_spec, mode = c("BASELINE", "ENHANCED"), universe_name = "Focus") {
  mode <- match.arg(mode)
  
  avail_syms <- intersect(sym_list, names(sim_results))
  if (length(avail_syms) == 0) return(NULL)
  
  # Determine common dates across available assets
  all_dates <- do.call(c, lapply(avail_syms, function(s) sim_results[[s]]$dates))
  date_counts <- table(all_dates)
  shared_dates <- as.Date(names(date_counts)[date_counts >= max(3, floor(0.20 * length(avail_syms)))])
  shared_dates <- sort(intersect(shared_dates, index(spy_ret_all)))
  
  if (is.character(horizon_spec) && horizon_spec == "SUMMER_2026") {
    date_mask <- (shared_dates >= as.Date("2026-06-01") & shared_dates <= as.Date("2026-09-30"))
    eval_dates <- shared_dates[date_mask]
    horizon_label <- "Jun-Sep 2026"
  } else {
    horizon_bars <- as.numeric(horizon_spec)
    if (length(shared_dates) < horizon_bars) {
      eval_dates <- shared_dates
    } else {
      eval_dates <- tail(shared_dates, horizon_bars)
    }
    horizon_label <- if (horizon_bars == 42) "2 Months" else if (horizon_bars == 252) "12 Months" else if (horizon_bars == 504) "24 Months" else "60 Months"
  }
  
  n_eval <- length(eval_dates)
  if (n_eval < 10) return(NULL)
  
  port_daily_ret <- numeric(n_eval)
  
  # Benchmark returns for these exact dates
  spy_idx <- match(eval_dates, index(spy_ret_all))
  spy_sub <- as.numeric(spy_ret_all)[spy_idx]
  spy_sub[is.na(spy_sub)] <- 0
  
  qqq_idx <- match(eval_dates, index(qqq_ret_all))
  qqq_sub <- as.numeric(qqq_ret_all)[qqq_idx]
  qqq_sub[is.na(qqq_sub)] <- 0
  
  if (mode == "BASELINE") {
    # BASELINE: Equal-weight all assets in universe, target_vol = 0.30, 0% cash yield
    ret_matrix <- matrix(0, nrow = n_eval, ncol = length(avail_syms))
    
    for (j in seq_along(avail_syms)) {
      s <- avail_syms[j]
      obj <- sim_results[[s]]
      idx_m <- match(eval_dates, index(obj$base_ret))
      valid <- !is.na(idx_m)
      if (any(valid)) {
        ret_matrix[valid, j] <- as.numeric(obj$base_ret)[idx_m[valid]]
      }
    }
    
    port_daily_ret <- rowMeans(ret_matrix, na.rm = TRUE)
    port_daily_ret[is.na(port_daily_ret)] <- 0
    
  } else if (mode == "ENHANCED") {
    # ENHANCED: Top-5 RS Ranking, Target Vol = 0.45, Core-Satellite QQQ Cash Yield, 25% Runner
    n_syms <- length(avail_syms)
    mat_prob  <- matrix(0.5,  nrow = n_eval, ncol = n_syms)
    mat_rs    <- matrix(0.0,  nrow = n_eval, ncol = n_syms)
    mat_vol   <- matrix(0.25, nrow = n_eval, ncol = n_syms)
    mat_ret_b <- matrix(0.0,  nrow = n_eval, ncol = n_syms)
    
    for (j in seq_along(avail_syms)) {
      s <- avail_syms[j]
      obj <- sim_results[[s]]
      
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
    
    for (t in seq_along(eval_dates)) {
      p_ups   <- mat_prob[t, ]
      rs_vals <- mat_rs[t, ]
      vols    <- mat_vol[t, ]
      rets_b  <- mat_ret_b[t, ]
      
      # Quality score = P(Up) * (1 + RS) when P(Up) >= 0.58 and RS > 0
      qualify <- which(p_ups >= 0.58 & rs_vals > 0)
      
      if (length(qualify) > 0) {
        scores <- p_ups[qualify] * (1 + rs_vals[qualify])
        top_order <- qualify[order(-scores)]
        top_idx   <- head(top_order, 5) # Cap at Top-5 positions
      } else {
        top_idx <- integer(0)
      }
      
      k_slots <- length(top_idx)
      if (k_slots == 0) {
        # 100% idle cash -> 100% Core-Satellite QQQ allocation
        port_daily_ret[t] <- qqq_sub[t]
      } else {
        stock_alloc_total <- 0
        stock_pnl_total   <- 0
        slot_size         <- 1.0 / 5.0 # Max 5 slots (20% each)
        
        for (idx in top_idx) {
          vol_scale <- pmin(1.5, 0.45 / vols[idx])
          w_i <- slot_size * vol_scale
          stock_alloc_total <- stock_alloc_total + slot_size
          stock_pnl_total   <- stock_pnl_total + (w_i * rets_b[idx])
        }
        
        unalloc_cash <- max(0, 1.0 - stock_alloc_total)
        cash_yield_pnl <- unalloc_cash * qqq_sub[t]
        
        port_daily_ret[t] <- stock_pnl_total + cash_yield_pnl
      }
    }
  }
  
  strat_name <- sprintf("%s | %s (%s)", universe_name, mode, horizon_label)
  m_strat <- calc_performance_metrics(port_daily_ret, name = strat_name)$raw
  m_spy   <- calc_performance_metrics(spy_sub, name = "SPY")$raw
  m_qqq   <- calc_performance_metrics(qqq_sub, name = "QQQ")$raw
  
  list(
    universe      = universe_name,
    horizon       = horizon_label,
    bars          = n_eval,
    dates         = eval_dates,
    mode          = mode,
    strat_ret     = m_strat$cum_ret,
    strat_ann_ret = m_strat$ann_ret,
    strat_vol     = m_strat$ann_vol,
    strat_sharpe  = m_strat$sharpe,
    strat_sortino = m_strat$sortino,
    strat_max_dd  = m_strat$max_dd,
    strat_winrate = m_strat$win_rate,
    strat_pf      = m_strat$profit_factor,
    spy_ret       = m_spy$cum_ret,
    spy_sharpe    = m_spy$sharpe,
    spy_max_dd    = m_spy$max_dd,
    qqq_ret       = m_qqq$cum_ret,
    qqq_sharpe    = m_qqq$sharpe,
    qqq_max_dd    = m_qqq$max_dd,
    daily_returns = port_daily_ret,
    spy_daily     = spy_sub,
    qqq_daily     = qqq_sub
  )
}

# ------------------------------------------------------------------------------
# 3. Execution Matrix across All 5 Universes & 5 Horizons
# ------------------------------------------------------------------------------
horizons_list <- list(
  "2 Months"      = 42,
  "12 Months"     = 252,
  "24 Months"     = 504,
  "60 Months"     = 1260,
  "Jun-Sep 2026"  = "SUMMER_2026"
)

all_evals <- list()
scorecard_rows <- list()

cat("Executing Backtest Grid: 5 Universes x 5 Horizons x 2 Model Modes...\n\n")

for (h_name in names(horizons_list)) {
  h_spec <- horizons_list[[h_name]]
  cat(sprintf(">>> Running Horizon: %s <<<\n", h_name))
  
  for (u_name in names(universes)) {
    u_syms <- universes[[u_name]]
    
    # 1. BASELINE Model
    ev_base <- evaluate_portfolio(u_syms, h_spec, mode = "BASELINE", universe_name = u_name)
    if (!is.null(ev_base)) {
      all_evals[[length(all_evals) + 1]] <- ev_base
      scorecard_rows[[length(scorecard_rows) + 1]] <- data.frame(
        Universe         = u_name,
        Horizon          = ev_base$horizon,
        Bars             = ev_base$bars,
        Mode             = "BASELINE",
        Cum_Return       = sprintf("%+.2f%%", ev_base$strat_ret * 100),
        Ann_Return       = sprintf("%+.2f%%", ev_base$strat_ann_ret * 100),
        Volatility       = sprintf("%.2f%%",  ev_base$strat_vol * 100),
        Sharpe_Ratio     = sprintf("%.2f",    ev_base$strat_sharpe),
        Sortino_Ratio    = sprintf("%.2f",    ev_base$strat_sortino),
        Max_Drawdown     = sprintf("%.2f%%",  ev_base$strat_max_dd * 100),
        Win_Rate         = sprintf("%.2f%%",  ev_base$strat_winrate * 100),
        Profit_Factor    = sprintf("%.2f",    ev_base$strat_pf),
        SPY_Return       = sprintf("%+.2f%%", ev_base$spy_ret * 100),
        SPY_Sharpe       = sprintf("%.2f",    ev_base$spy_sharpe),
        QQQ_Return       = sprintf("%+.2f%%", ev_base$qqq_ret * 100),
        QQQ_Sharpe       = sprintf("%.2f",    ev_base$qqq_sharpe),
        stringsAsFactors = FALSE
      )
    }
    
    # 2. ENHANCED Model
    ev_enh <- evaluate_portfolio(u_syms, h_spec, mode = "ENHANCED", universe_name = u_name)
    if (!is.null(ev_enh)) {
      all_evals[[length(all_evals) + 1]] <- ev_enh
      scorecard_rows[[length(scorecard_rows) + 1]] <- data.frame(
        Universe         = u_name,
        Horizon          = ev_enh$horizon,
        Bars             = ev_enh$bars,
        Mode             = "ENHANCED",
        Cum_Return       = sprintf("%+.2f%%", ev_enh$strat_ret * 100),
        Ann_Return       = sprintf("%+.2f%%", ev_enh$strat_ann_ret * 100),
        Volatility       = sprintf("%.2f%%",  ev_enh$strat_vol * 100),
        Sharpe_Ratio     = sprintf("%.2f",    ev_enh$strat_sharpe),
        Sortino_Ratio    = sprintf("%.2f",    ev_enh$strat_sortino),
        Max_Drawdown     = sprintf("%.2f%%",  ev_enh$strat_max_dd * 100),
        Win_Rate         = sprintf("%.2f%%",  ev_enh$strat_winrate * 100),
        Profit_Factor    = sprintf("%.2f",    ev_enh$strat_pf),
        SPY_Return       = sprintf("%+.2f%%", ev_enh$spy_ret * 100),
        SPY_Sharpe       = sprintf("%.2f",    ev_enh$spy_sharpe),
        QQQ_Return       = sprintf("%+.2f%%", ev_enh$qqq_ret * 100),
        QQQ_Sharpe       = sprintf("%.2f",    ev_enh$qqq_sharpe),
        stringsAsFactors = FALSE
      )
      
      cat(sprintf("  %-25s | %-12s | Ret: %6.2f%% | Sharpe: %4.2f | MaxDD: %5.2f%% | SPY: %6.2f%% | QQQ: %6.2f%%\n",
                  u_name, ev_enh$horizon, ev_enh$strat_ret * 100, ev_enh$strat_sharpe,
                  ev_enh$strat_max_dd * 100, ev_enh$spy_ret * 100, ev_enh$qqq_ret * 100))
    }
  }
  cat("\n")
}

scorecard_df <- do.call(rbind, scorecard_rows)

# Save Scorecard and Raw Evaluation Data
csv_out <- file.path(OUTPUT_DIR, "comprehensive_5universe_scorecard.csv")
rds_out <- file.path(OUTPUT_DIR, "comprehensive_5universe_data.rds")
write.csv(scorecard_df, csv_out, row.names = FALSE)
saveRDS(list(evals = all_evals, scorecard = scorecard_df), rds_out)
cat(sprintf("[Saved] Scorecard CSV: %s (%d rows)\n", csv_out, nrow(scorecard_df)))
cat(sprintf("[Saved] Raw Data RDS:  %s\n\n", rds_out))

# ------------------------------------------------------------------------------
# 4. Generate High-Resolution 6-Panel Visualization
# ------------------------------------------------------------------------------
plot_png <- file.path(OUTPUT_DIR, "comprehensive_5universe_backtest_chart.png")
png(plot_png, width = 2200, height = 1500, res = 150)
par(mfrow = c(2, 3), mar = c(4.5, 4.8, 3.2, 1), oma = c(1, 1, 3.8, 1))

# Palette
col_focus <- "#e41a1c" # Vibrant Red
col_broad <- "#377eb8" # Blue
col_300   <- "#4daf4a" # Green
col_all   <- "#984ea3" # Purple
col_all2  <- "#ff7f00" # Orange
col_spy   <- "#555555" # Dark Gray
col_qqq   <- "#f781bf" # Magenta

univ_keys <- c(
  "Focus (symbols.txt)",
  "Broad (symbols_broad.txt)",
  "300+ (symbols_300.txt)",
  "All (symbols_all.txt)",
  "All2 (symbols_all2.txt)"
)

horizon_panels <- c("2 Months", "12 Months", "24 Months", "60 Months", "Jun-Sep 2026")

for (hp in horizon_panels) {
  eq_curves <- list()
  spy_eq <- NULL
  qqq_eq <- NULL
  
  for (uk in univ_keys) {
    for (ev in all_evals) {
      if (ev$universe == uk && ev$horizon == hp && ev$mode == "ENHANCED") {
        eq_curves[[uk]] <- cumprod(1 + ev$daily_returns)
        if (is.null(spy_eq)) spy_eq <- cumprod(1 + ev$spy_daily)
        if (is.null(qqq_eq)) qqq_eq <- cumprod(1 + ev$qqq_daily)
      }
    }
  }
  
  if (length(eq_curves) > 0 && !is.null(eq_curves[[1]])) {
    all_vals <- c(unlist(eq_curves), spy_eq, qqq_eq)
    ymin <- min(all_vals, na.rm = TRUE) * 0.95
    ymax <- max(all_vals, na.rm = TRUE) * 1.05
    
    plot(eq_curves[[1]], type = "l", col = col_focus, lwd = 2.4, ylim = c(ymin, ymax),
         main = sprintf("Horizon: %s (Enhanced Model)", hp),
         xlab = "Trading Days", ylab = "Equity Growth (Base 1.0)",
         cex.main = 1.25, cex.axis = 0.95, font.main = 2)
    grid(col = "gray85")
    
    if (length(eq_curves) >= 2) lines(eq_curves[[2]], col = col_broad, lwd = 2.4)
    if (length(eq_curves) >= 3) lines(eq_curves[[3]], col = col_300,   lwd = 2.4)
    if (length(eq_curves) >= 4) lines(eq_curves[[4]], col = col_all,   lwd = 2.0, lty = 2)
    if (length(eq_curves) >= 5) lines(eq_curves[[5]], col = col_all2,  lwd = 2.0, lty = 3)
    
    if (!is.null(spy_eq)) lines(spy_eq, col = col_spy, lwd = 2.0, lty = 4)
    if (!is.null(qqq_eq)) lines(qqq_eq, col = col_qqq, lwd = 2.0, lty = 5)
    
    leg_text <- c(
      sprintf("Focus (%+.1f%%)",    (tail(eq_curves[[1]], 1) - 1) * 100),
      sprintf("Broad (%+.1f%%)",    (tail(eq_curves[[2]], 1) - 1) * 100),
      sprintf("300+ (%+.1f%%)",     (tail(eq_curves[[3]], 1) - 1) * 100),
      sprintf("All Flat (%+.1f%%)", (tail(eq_curves[[4]], 1) - 1) * 100),
      sprintf("All2 Sec (%+.1f%%)", (tail(eq_curves[[5]], 1) - 1) * 100),
      sprintf("SPY (%+.1f%%)",      (tail(spy_eq, 1) - 1) * 100),
      sprintf("QQQ (%+.1f%%)",      (tail(qqq_eq, 1) - 1) * 100)
    )
    
    legend("topleft", legend = leg_text,
           col = c(col_focus, col_broad, col_300, col_all, col_all2, col_spy, col_qqq),
           lwd = c(2.4, 2.4, 2.4, 2.0, 2.0, 2.0, 2.0),
           lty = c(1, 1, 1, 2, 3, 4, 5),
           cex = 0.70, bg = rgb(1, 1, 1, 0.90), box.col = "gray75")
  }
}

# Panel 6: Risk-Adjusted Efficiency: Sharpe Ratio Comparison across Universes
barplot_data <- matrix(0, nrow = 5, ncol = 2)
colnames(barplot_data) <- c("12-Month Sharpe", "24-Month Sharpe")
rownames(barplot_data) <- c("Focus", "Broad", "300+", "All", "All2")

for (i in seq_along(univ_keys)) {
  uk <- univ_keys[i]
  for (ev in all_evals) {
    if (ev$universe == uk && ev$mode == "ENHANCED") {
      if (ev$horizon == "12 Months") barplot_data[i, 1] <- max(0, ev$strat_sharpe)
      if (ev$horizon == "24 Months") barplot_data[i, 2] <- max(0, ev$strat_sharpe)
    }
  }
}

barplot(t(barplot_data), beside = TRUE, col = c("#2b83ba", "#d7191c"),
        main = "Risk-Adjusted Efficiency: Sharpe Comparison",
        ylab = "Sharpe Ratio", cex.main = 1.25, cex.axis = 0.95,
        ylim = c(0, max(barplot_data, 1.0) * 1.25), font.main = 2)
grid(col = "gray85")
legend("topright", legend = c("12-Month Sharpe", "24-Month Sharpe"),
       fill = c("#2b83ba", "#d7191c"), cex = 0.85, bg = rgb(1, 1, 1, 0.90))

mtext("Comprehensive Quantitative Backtest: 5 Universes across 5 Market Horizons",
      outer = TRUE, cex = 1.45, font = 2, col = "#111111")
dev.off()
cat(sprintf("[Saved] Multi-panel comparison chart saved to: %s\n", plot_png))

if (dir.exists(ARTIFACT_DIR)) {
  file.copy(plot_png, file.path(ARTIFACT_DIR, "comprehensive_5universe_backtest_chart.png"), overwrite = TRUE)
  cat(sprintf("[Saved] Chart copied to artifact directory: %s\n\n", file.path(ARTIFACT_DIR, "comprehensive_5universe_backtest_chart.png")))
}

cat("Execution completed successfully!\n")
