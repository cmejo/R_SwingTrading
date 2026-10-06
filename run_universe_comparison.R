#!/usr/bin/env Rscript
# ==============================================================================
# Comprehensive Multi-Universe & Multi-Horizon Backtest Comparison
# Compares symbols.txt, symbols_broad.txt, and symbols_300.txt across:
#   - 2 Months (42 Trading Days)
#   - 12 Months (252 Trading Days)
#   - 24 Months (504 Trading Days)
# Evaluates:
#   1. BASELINE Model (Equal-weight basket, Target Vol = 0.30, 0% Cash Yield, Standard Bracket)
#   2. ENHANCED Model (Top-5 RS Ranking, Target Vol = 0.45, Core-Satellite SPY Cash, 25% Chandelier Runner)
# ==============================================================================

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(glmnet)
  library(tseries)
  library(TTR)
  library(parallel)
})

source("R/01_data_loader.R")
source("R/02_trend_lmMA.R")
source("R/03_volatility_garch.R")
source("R/04_feature_pipeline.R")
source("R/05_logistic_model.R")
source("R/06_swing_backtest.R")
source("R/08_metrics.R")

OUTPUT_DIR <- "output"
CACHE_DIR  <- "data/cache_wf"
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(CACHE_DIR,  showWarnings = FALSE, recursive = TRUE)

TOTAL_BARS_REQ <- 504 # 24 Months
NUM_CORES      <- min(6, parallel::detectCores())

cat("\n========================================================================================\n")
cat(sprintf(" MULTI-UNIVERSE BACKTEST ENGINE (CORES: %d | MAX HORIZON: 24 MONTHS / %d BARS)\n", NUM_CORES, TOTAL_BARS_REQ))
cat("========================================================================================\n\n")

# Load Benchmarks
spy_ohlcv <- load_stock_data("SPY")
qqq_ohlcv <- load_stock_data("QQQ")
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

all_unique_syms <- unique(c(syms_focus, syms_broad, syms_300))
cat(sprintf("Loaded Universes: Focus Watchlist (%d), Broad (%d), 300+ Institutional (%d)\n",
            length(syms_focus), length(syms_broad), length(syms_300)))
cat(sprintf("Total Distinct Symbols to Simulate: %d\n\n", length(all_unique_syms)))

# ------------------------------------------------------------------------------
# 1. Individual Stock Walk-Forward Simulation (with on-disk caching)
# ------------------------------------------------------------------------------
simulate_stock_walkforward <- function(sym) {
  cache_file <- file.path(CACHE_DIR, sprintf("%s_504wf.rds", tolower(sym)))
  if (file.exists(cache_file)) {
    return(readRDS(cache_file))
  }
  
  res <- tryCatch({
    ohlcv <- load_stock_data(sym)
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
    
    # Enhanced Bracket with 25% Chandelier Runner Lot (Enhancement 4)
    # Simulate single stock bracket execution with runner
    n_days <- length(common_d)
    c_p_vec <- as.numeric(cl[common_d])
    o_p_vec <- as.numeric(op[common_d])
    h_p_vec <- as.numeric(hi[common_d])
    l_p_vec <- as.numeric(lo[common_d])
    atr_vec <- as.numeric(atr_xts)
    gvol_v  <- as.numeric(garch_vol_xts)
    sig_v   <- as.numeric(sig_xts[common_d])
    
    # Simulate daily bracket returns
    bracket_ret_vec <- numeric(n_days)
    in_pos <- FALSE
    pos_shares_pct <- 0 # 1.0 = full position
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
        pos_shares_pct <- 1.0 # 100% position
        
        daily_v <- pmax(0.05, gvol_v[d - 1]) / sqrt(252)
        risk_1r <- 2.0 * daily_v * entry_p
        stop_p  <- entry_p - risk_1r
        t1_p    <- entry_p + 1.5 * risk_1r
        t2_p    <- entry_p + 3.0 * risk_1r
        
        # Entry day return: open to close
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
          stop_p <- max(entry_p, chand_stop) # Move to breakeven or Chandelier
        }
        
        # 3. Tier 2 target hit: exit 25%, leave 25% runner!
        if (t1_hit && !t2_hit && cur_h >= t2_p) {
          t2_hit <- TRUE
          pos_shares_pct <- 0.25 # 25% RUNNER REMAINS
          stop_p <- max(stop_p, chand_stop)
        }
        
        # Trailing stop update for active position / runner
        if (t1_hit) {
          stop_p <- max(stop_p, chand_stop)
        }
        
        # 4. Non-runner 5-day expiration (only applies if before Tier 2)
        if (!t2_hit && days_h >= 5) {
          bar_r <- (cur_c - prev_c) / prev_c
          bracket_ret_vec[d] <- pos_shares_pct * bar_r - (10 / 10000)
          in_pos <- FALSE
          pos_shares_pct <- 0
          next
        }
        
        # Daily return of remaining shares
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

# Run walk-forward simulation across all unique symbols in parallel
cat("Running walk-forward model estimation across universe (using cache where available)...\n")
t_start <- Sys.time()

# Process in parallel
sim_results <- parallel::mclapply(all_unique_syms, simulate_stock_walkforward, mc.cores = NUM_CORES)
names(sim_results) <- all_unique_syms
sim_results <- sim_results[!sapply(sim_results, is.null)]

t_end <- Sys.time()
cat(sprintf("Walk-Forward Simulation complete! Valid assets: %d / %d (Elapsed: %.1f seconds)\n\n",
            length(sim_results), length(all_unique_syms), as.numeric(difftime(t_end, t_start, units = "secs"))))

# ------------------------------------------------------------------------------
# 2. Portfolio Construction Engines (Baseline vs Enhanced)
# ------------------------------------------------------------------------------
evaluate_portfolio <- function(sym_list, horizon_bars, mode = c("BASELINE", "ENHANCED"), universe_name = "Focus") {
  mode <- match.arg(mode)
  
  # Filter to available simulated assets in this universe
  avail_syms <- intersect(sym_list, names(sim_results))
  if (length(avail_syms) == 0) return(NULL)
  
  # Determine common dates across available assets
  all_dates <- do.call(c, lapply(avail_syms, function(s) sim_results[[s]]$dates))
  date_counts <- table(all_dates)
  shared_dates <- as.Date(names(date_counts)[date_counts >= max(3, floor(0.3 * length(avail_syms)))])
  shared_dates <- sort(intersect(shared_dates, index(spy_ret_all)))
  
  if (length(shared_dates) < horizon_bars) {
    eval_dates <- shared_dates
  } else {
    eval_dates <- tail(shared_dates, horizon_bars)
  }
  
  n_eval <- length(eval_dates)
  port_daily_ret <- numeric(n_eval)
  
  # Benchmark returns for these exact dates
  spy_idx <- match(eval_dates, index(spy_ret_all))
  spy_sub <- as.numeric(spy_ret_all)[spy_idx]
  spy_sub[is.na(spy_sub)] <- 0
  
  qqq_idx <- match(eval_dates, index(qqq_ret_all))
  qqq_sub <- as.numeric(qqq_ret_all)[qqq_idx]
  qqq_sub[is.na(qqq_sub)] <- 0
  
  if (mode == "BASELINE") {
    # --------------------------------------------------------------------------
    # BASELINE: Equal-weight all assets in universe, target_vol = 0.30, 0% cash yield
    # --------------------------------------------------------------------------
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
    # --------------------------------------------------------------------------
    # ENHANCED:
    #   1. Top-5 Cross-Sectional Ranking by P(Up) * RS (P(Up) >= 0.58, RS > 0)
    #   2. Target Vol = 0.45 (or 1.5x Margin Sizing)
    #   3. Core-Satellite SPY Cash Yield on idle cash
    #   4. 25% Chandelier Runner Lot on active positions
    # --------------------------------------------------------------------------
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
        # 100% idle cash -> 100% Core-Satellite SPY allocation!
        port_daily_ret[t] <- spy_sub[t]
      } else {
        # Allocate to top stocks
        stock_alloc_total <- 0
        stock_pnl_total   <- 0
        slot_size         <- 1.0 / 5.0 # Max 5 slots (20% each)
        
        for (idx in top_idx) {
          # Enhancement 2: Target Vol = 0.45 (or up to 1.5x margin)
          vol_scale <- pmin(1.5, 0.45 / vols[idx])
          w_i <- slot_size * vol_scale
          stock_alloc_total <- stock_alloc_total + slot_size
          stock_pnl_total   <- stock_pnl_total + (w_i * rets_b[idx])
        }
        
        # Enhancement 3: Core-Satellite SPY Cash Yield on unallocated cash
        unalloc_cash <- max(0, 1.0 - stock_alloc_total)
        cash_yield_pnl <- unalloc_cash * spy_sub[t]
        
        port_daily_ret[t] <- stock_pnl_total + cash_yield_pnl
      }
    }
  }
  
  # Calculate standardized performance metrics
  strat_name <- sprintf("%s | %s (%s)", universe_name, mode, ifelse(horizon_bars == 42, "2M", ifelse(horizon_bars == 252, "12M", "24M")))
  m_strat <- calc_performance_metrics(port_daily_ret, name = strat_name)$raw
  m_spy   <- calc_performance_metrics(spy_sub, name = "SPY")$raw
  m_qqq   <- calc_performance_metrics(qqq_sub, name = "QQQ")$raw
  
  list(
    universe      = universe_name,
    horizon       = ifelse(horizon_bars == 42, "2 Months", ifelse(horizon_bars == 252, "12 Months", "24 Months")),
    bars          = n_eval,
    mode          = mode,
    strat_ret     = m_strat$cum_ret,
    strat_ann_ret = m_strat$ann_ret,
    strat_vol     = m_strat$ann_vol,
    strat_sharpe  = m_strat$sharpe,
    strat_max_dd  = m_strat$max_dd,
    strat_win_r   = m_strat$win_rate,
    strat_pf      = m_strat$profit_factor,
    spy_ret       = m_spy$cum_ret,
    spy_sharpe    = m_spy$sharpe,
    qqq_ret       = m_qqq$cum_ret,
    qqq_sharpe    = m_qqq$sharpe,
    alpha_spy     = m_strat$cum_ret - m_spy$cum_ret,
    alpha_qqq     = m_strat$cum_ret - m_qqq$cum_ret,
    daily_returns = port_daily_ret
  )
}

# ------------------------------------------------------------------------------
# 3. Execute Grid Across 3 Universes x 3 Horizons x 2 Modes
# ------------------------------------------------------------------------------
horizons   <- list("2 Months" = 42, "12 Months" = 252, "24 Months" = 504)
universes  <- list(
  "Focus (symbols.txt)"          = syms_focus,
  "Broad (symbols_broad.txt)"    = syms_broad,
  "Institutional (symbols_300.txt)" = syms_300
)

all_evals <- list()

cat("Evaluating Portfolios across 3 Universes x 3 Horizons x 2 Strategy Modes...\n")
for (u_name in names(universes)) {
  u_syms <- universes[[u_name]]
  for (h_name in names(horizons)) {
    h_bars <- horizons[[h_name]]
    
    # Run Baseline
    res_base <- evaluate_portfolio(u_syms, h_bars, mode = "BASELINE", universe_name = u_name)
    if (!is.null(res_base)) all_evals[[length(all_evals) + 1]] <- res_base
    
    # Run Enhanced
    res_enh  <- evaluate_portfolio(u_syms, h_bars, mode = "ENHANCED", universe_name = u_name)
    if (!is.null(res_enh)) all_evals[[length(all_evals) + 1]] <- res_enh
  }
}

# ------------------------------------------------------------------------------
# 4. Format & Print Comparison Tables
# ------------------------------------------------------------------------------
results_df <- do.call(rbind, lapply(all_evals, function(e) {
  data.frame(
    Universe       = e$universe,
    Horizon        = e$horizon,
    Strategy_Mode  = e$mode,
    Cumulative_Ret = sprintf("%+6.2f%%", e$strat_ret * 100),
    Annualized_Ret = sprintf("%+6.2f%%", e$strat_ann_ret * 100),
    Annualized_Vol = sprintf("%5.2f%%",  e$strat_vol * 100),
    Sharpe_Ratio   = sprintf("%5.2f",    e$strat_sharpe),
    Max_Drawdown   = sprintf("-%5.2f%%", e$strat_max_dd * 100),
    Win_Rate       = sprintf("%5.1f%%",  e$strat_win_r * 100),
    Profit_Factor  = sprintf("%5.2f",    ifelse(is.na(e$strat_pf), 0, e$strat_pf)),
    SP500_Ret      = sprintf("%+6.2f%%", e$spy_ret * 100),
    SP500_Sharpe   = sprintf("%5.2f",    e$spy_sharpe),
    Nasdaq_Ret     = sprintf("%+6.2f%%", e$qqq_ret * 100),
    Alpha_vs_SPY   = sprintf("%+6.2f%%", e$alpha_spy * 100),
    stringsAsFactors = FALSE
  )
}))

cat("\n========================================================================================\n")
cat("                       MULTI-UNIVERSE BACKTEST COMPARISON SCORECARD                      \n")
cat("========================================================================================\n\n")

print(results_df, row.names = FALSE)

# Save results CSV
write.csv(results_df, file = file.path(OUTPUT_DIR, "universe_comparison_scorecard.csv"), row.names = FALSE)
cat(sprintf("\n[Saved] Detailed results table saved to: %s\n", file.path(OUTPUT_DIR, "universe_comparison_scorecard.csv")))

# Save RDS for full analysis
saveRDS(list(evals = all_evals, table = results_df), file = file.path(OUTPUT_DIR, "universe_comparison_data.rds"))
cat("[Saved] Raw evaluation data saved to output/universe_comparison_data.rds\n\n")
