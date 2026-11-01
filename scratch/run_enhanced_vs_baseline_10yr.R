suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(TTR)
  library(parallel)
  library(glmnet)
})

source("R/01_data_loader.R")
source("R/02_trend_lmMA.R")
source("R/03_volatility_garch.R")
source("R/04_feature_pipeline.R")
source("R/05_logistic_model.R")
source("R/08_metrics.R")

cat("========================================================================================\n")
cat(" 10-YEAR $10,000 BACKTEST: ENHANCED MODEL (NEW ALPHA FEATURES + RATCHET) VS. BASELINE\n")
cat(" Allocation: 75% FNGD + 25% SQQQ | 15-Day Time Stop | -12.0% Circuit Breaker\n")
cat(" Period: 2,520 Trading Days (2016 - 2026)\n")
cat("========================================================================================\n\n")

DATA_DIR_10Y      <- "data/data_10yr"
CACHE_DIR_BASE    <- "data/cache_10yr"
CACHE_DIR_ENH     <- "data/cache_10yr_enhanced"
dir.create(CACHE_DIR_ENH, showWarnings = FALSE, recursive = TRUE)

spy_ohlcv <- readRDS(file.path(DATA_DIR_10Y, "data_spy.rds"))

base_files <- list.files(CACHE_DIR_BASE, pattern = "_10yr_wf[.]rds$", full.names = TRUE)
base_syms <- character(length(base_files))
for (i in seq_along(base_files)) {
  obj <- readRDS(base_files[i])
  base_syms[i] <- obj$symbol
}

# Step 1: Build Enhanced 10-Year Walk-Forward Cache
# Combines Core 18-Feature Trend/Vol Backbone + Dedicated 7-Feature Microstructure/Squeeze/Catalyst Alpha Sub-Model
# + Post-Tier-1 Profit-Lock Ratchet
process_symbol_enhanced_10yr <- function(s) {
  cache_file <- file.path(CACHE_DIR_ENH, sprintf("%s_10yr_enh_wf.rds", tolower(gsub("[^A-Za-z0-9]", "_", s))))
  if (file.exists(cache_file)) return(s)
  
  base_cf <- file.path(CACHE_DIR_BASE, sprintf("%s_10yr_wf.rds", tolower(gsub("[^A-Za-z0-9]", "_", s))))
  df_file <- file.path(DATA_DIR_10Y, sprintf("data_%s.rds", tolower(gsub("[^A-Za-z0-9]", "_", s))))
  if (!file.exists(df_file) || !file.exists(base_cf)) return(NULL)
  
  b_obj <- tryCatch(readRDS(base_cf), error = function(e) NULL)
  df    <- tryCatch(readRDS(df_file), error = function(e) NULL)
  if (is.null(df) || is.null(b_obj) || nrow(df) < 150) return(NULL)
  
  c_idx <- intersect(index(df), index(spy_ohlcv))
  if (length(c_idx) < 150) return(NULL)
  
  df_sub  <- df[c_idx]
  spy_sub <- spy_ohlcv[c_idx]
  
  tryCatch({
    invisible(capture.output({
      pipe_res <- build_feature_dataset(df_sub, benchmark_ohlcv = spy_sub, look_ahead = 7, use_garch = FALSE)
    }))
    df_raw <- na.omit(pipe_res$model_data)
    if (is.null(df_raw) || nrow(df_raw) < 120) return(NULL)
    
    # Dedicated 7 New Money-Making Alpha Features Sub-Model (Regularized via Elastic-Net + 7-bar embargo)
    alpha_cols <- c("VolSqueeze", "SqueezeRelease", "CLV_5D", "IntradayVsGap_10D",
                    "PEAD_Catalyst", "PullbackInUptrend", "BreakoutWithVolume")
    X_alpha <- as.matrix(df_raw[, alpha_cols, drop = FALSE])
    y_tb    <- as.numeric(df_raw$TargetBinary)
    
    n_bars <- nrow(X_alpha)
    p_alpha <- rep(0.5, n_bars)
    
    step <- 60
    embargo <- 7
    for (start_i in seq(101, n_bars, by = step)) {
      end_i <- min(start_i + step - 1, n_bars)
      train_end <- max(80, start_i - embargo - 1)
      X_tr <- X_alpha[1:train_end, , drop = FALSE]
      y_tr <- y_tb[1:train_end]
      
      fit_a <- tryCatch(
        glmnet::glmnet(X_tr, y_tr, family = "binomial", alpha = 0.5, lambda = 0.008),
        error = function(e) NULL
      )
      if (!is.null(fit_a)) {
        X_te <- X_alpha[start_i:end_i, , drop = FALSE]
        p_raw <- tryCatch(
          as.numeric(predict(fit_a, newx = X_te, type = "response")),
          error = function(e) rep(0.5, nrow(X_te))
        )
        p_alpha[start_i:end_i] <- p_raw
      }
    }
    p_alpha[is.na(p_alpha)] <- 0.5
    alpha_prob_xts <- xts(p_alpha, order.by = as.Date(df_raw$Date))
    
    # Extract structural microstructure/squeeze conviction bonus
    sq_rel_v <- as.numeric(df_raw$SqueezeRelease)
    clv_v    <- as.numeric(df_raw$CLV_5D)
    pead_v   <- as.numeric(df_raw$PEAD_Catalyst)
    pb_v     <- as.numeric(df_raw$PullbackInUptrend)
    bo_v     <- as.numeric(df_raw$BreakoutWithVolume)
    
    # Conviction tilt: positive when institutional accumulation (CLV_5D > 0), Squeeze Release, or PEAD catalyst align;
    # slightly negative (-0.02) when distribution into the close is severe (CLV_5D < -0.35)
    conv_tilt <- (
      0.015 * pmin(sq_rel_v, 2.0) +
      0.025 * clv_v +
      0.020 * pmin(pead_v, 2.0) +
      0.015 * pmin(pb_v + bo_v, 2.0)
    )
    conv_tilt <- pmin(0.06, pmax(-0.03, conv_tilt))
    tilt_xts  <- xts(conv_tilt, order.by = as.Date(df_raw$Date))
    
    # Align with baseline pred_prob
    idx_common <- intersect(index(b_obj$pred_prob), index(alpha_prob_xts))
    p_base_vec  <- as.numeric(b_obj$pred_prob[idx_common])
    p_alpha_vec <- as.numeric(alpha_prob_xts[idx_common])
    tilt_vec    <- as.numeric(tilt_xts[idx_common])
    
    # Stacked Ensemble Probability:
    # 82% Core Trend/Vol Model + 18% Orthogonal 7-Feature Alpha Sub-Model + Microstructure/Squeeze Tilt
    p_ens_feats <- pmin(0.999, pmax(0.001, 0.85 * p_base_vec + 0.15 * p_alpha_vec + 0.5 * tilt_vec))
    p_ens_full  <- pmin(0.999, pmax(0.001, 0.82 * p_base_vec + 0.18 * p_alpha_vec + tilt_vec))
    
    ohlcv_sub <- df_sub[idx_common]
    cl <- as.numeric(Cl(ohlcv_sub))
    op <- as.numeric(Op(ohlcv_sub))
    hi <- as.numeric(Hi(ohlcv_sub))
    lo <- as.numeric(Lo(ohlcv_sub))
    
    atr <- as.numeric(ATR(HLC(ohlcv_sub), n = 14)$atr)
    atr[is.na(atr) | atr <= 0] <- cl[is.na(atr) | atr <= 0] * 0.02
    
    rs_vec <- as.numeric(b_obj$rs_spy[idx_common])
    nb <- length(idx_common)
    
    simulate_brackets <- function(p_vec, use_ratchet = FALSE) {
      ret_out <- numeric(nb)
      in_pos <- FALSE
      entry_p <- 0; stop_p <- 0; t1_p <- 0; t2_p <- 0; high_p <- 0; days_h <- 0
      risk_1r <- 0
      t1_hit <- FALSE; t2_hit <- FALSE; pos_shares_pct <- 0
      
      for (d in 2:nb) {
        cur_o   <- op[d]
        cur_h   <- hi[d]
        cur_l   <- lo[d]
        cur_c   <- cl[d]
        prev_c  <- cl[d - 1]
        cur_atr <- atr[d]
        
        if (!in_pos && !is.na(p_vec[d - 1]) && p_vec[d - 1] >= 0.58 && !is.na(rs_vec[d - 1]) && rs_vec[d - 1] > 0) {
          in_pos  <- TRUE
          entry_p <- ifelse(!is.na(cur_o) && cur_o > 0, cur_o, cur_c)
          high_p  <- entry_p
          days_h  <- 0
          t1_hit  <- FALSE
          t2_hit  <- FALSE
          pos_shares_pct <- 1.0
          
          risk_1r <- 2.0 * cur_atr
          stop_p  <- entry_p - risk_1r
          t1_p    <- entry_p + 1.5 * risk_1r
          t2_p    <- entry_p + 3.0 * risk_1r
          
          ret_out[d] <- (cur_c - entry_p) / entry_p - (10 / 10000)
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
            ret_out[d] <- pos_shares_pct * bar_r - (10 / 10000)
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
          
          # Addition #6: Post-Tier-1 Profit-Lock Ratchet (+0.85R floor once high >= +2.2R)
          if (t1_hit) {
            stop_p <- max(stop_p, chand_stop)
            if (use_ratchet && high_p >= (entry_p + 2.2 * risk_1r)) {
              stop_p <- max(stop_p, entry_p + 0.85 * risk_1r)
            }
          }
          
          # 4. Max hold period (7 trading days) for non-runners
          if (!t2_hit && days_h >= 7) {
            bar_r <- (cur_c - prev_c) / prev_c
            ret_out[d] <- pos_shares_pct * bar_r - (10 / 10000)
            in_pos <- FALSE
            pos_shares_pct <- 0
            next
          }
          
          bar_r <- (cur_c - prev_c) / prev_c
          ret_out[d] <- pos_shares_pct * bar_r
        }
      }
      xts(ret_out, order.by = idx_common)
    }
    
    bracket_std     <- simulate_brackets(p_ens_feats, use_ratchet = FALSE)
    bracket_ratchet <- simulate_brackets(p_ens_full,  use_ratchet = TRUE)
    
    saveRDS(list(
      symbol          = s,
      pred_prob       = xts(p_ens_feats, order.by = idx_common),
      pred_prob_boost = xts(p_ens_full,  order.by = idx_common),
      rs_spy          = b_obj$rs_spy[idx_common],
      garch_vol       = b_obj$garch_vol[idx_common],
      bracket_ret     = bracket_std,
      bracket_ratchet = bracket_ratchet,
      dates           = as.Date(idx_common)
    ), cache_file)
    return(s)
  }, error = function(e) {
    return(NULL)
  })
}

cat(sprintf("[Step 1/3] Computing Stacked Ensemble 26-Feature 10-Year Walk-Forward for %d symbols in parallel...\n", length(base_syms)))
t0 <- Sys.time()
num_cores <- min(8, detectCores())
res_enh <- mclapply(base_syms, process_symbol_enhanced_10yr, mc.cores = num_cores)
valid_enh <- unlist(res_enh)
cat(sprintf("[Step 1/3] Completed %d / %d symbols in %.1f seconds.\n\n",
            length(valid_enh), length(base_syms), as.numeric(difftime(Sys.time(), t0, units = "secs"))))

# Step 2: Load Macro & Defensive Hedge Data (2016 - 2026, 2,520 Trading Days)
symbols <- c("QQQ", "GLD", "BIL", "SGOV", "SQQQ", "FNGD")
raw_data <- list()
for (s in symbols) {
  raw_data[[s]] <- suppressWarnings(getSymbols(s, src = "yahoo", from = "2010-01-01", auto.assign = FALSE))
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

base_fb_75_25 <- 0.75 * fngd_v + 0.25 * sqqq_v

get_cash_series <- function(base_fb, max_days = 15, max_loss = NULL) {
  cash_ret <- numeric(n_total)
  in_ep <- FALSE
  ep_d <- 0
  ep_cum_ret <- 1.0
  rot_active <- FALSE
  rot_days <- 0
  
  for (t in 1:n_total) {
    if (macro_bull[t]) {
      cash_ret[t] <- qqq_v[t]
      in_ep <- FALSE; ep_d <- 0; ep_cum_ret <- 1.0; rot_active <- FALSE
    } else if (gld_bull[t]) {
      cash_ret[t] <- gld_v[t]
      in_ep <- FALSE; ep_d <- 0; ep_cum_ret <- 1.0; rot_active <- FALSE
    } else {
      if (!in_ep) {
        in_ep <- TRUE; ep_d <- 0; ep_cum_ret <- 1.0; rot_active <- FALSE
      }
      ep_d <- ep_d + 1
      if (rot_active) {
        cash_ret[t] <- sgov_v[t]
        rot_days <- rot_days + 1
      } else {
        day_r <- base_fb[t]
        ep_cum_ret <- ep_cum_ret * (1 + day_r)
        trigger <- FALSE
        if (!is.null(max_days) && ep_d >= max_days) trigger <- TRUE
        if (!is.null(max_loss) && (ep_cum_ret - 1.0) <= -max_loss) trigger <- TRUE
        cash_ret[t] <- day_r
        if (trigger) rot_active <- TRUE
      }
    }
  }
  list(series = cash_ret, rot_days = rot_days)
}

SECTOR_MAP <- if (file.exists("sector_map.json")) jsonlite::fromJSON("sector_map.json") else list()
get_sym_sector <- function(s) {
  if (!is.null(SECTOR_MAP[[s]])) return(SECTOR_MAP[[s]])
  return("General_Tech")
}

build_matrix_bundle <- function(cache_dir, pattern_str, use_boost_prob = FALSE, use_ratchet_ret = FALSE) {
  c_files <- list.files(cache_dir, pattern = pattern_str, full.names = TRUE)
  s_data <- list()
  for (cf in c_files) {
    obj <- readRDS(cf)
    s_data[[obj$symbol]] <- obj
  }
  a_syms <- names(s_data)
  ns <- length(a_syms)
  
  m_prob  <- matrix(0.5,   nrow = n_total, ncol = ns)
  m_rs    <- matrix(0.0,   nrow = n_total, ncol = ns)
  m_vol   <- matrix(0.25,  nrow = n_total, ncol = ns)
  m_ret_b <- matrix(0.0,   nrow = n_total, ncol = ns)
  m_valid <- matrix(FALSE, nrow = n_total, ncol = ns)
  
  for (j in seq_along(a_syms)) {
    s <- a_syms[j]
    obj <- s_data[[s]]
    p_xts <- if (use_boost_prob && !is.null(obj$pred_prob_boost)) obj$pred_prob_boost else obj$pred_prob
    r_xts <- if (use_ratchet_ret && !is.null(obj$bracket_ratchet)) obj$bracket_ratchet else obj$bracket_ret
    
    idx_p <- match(eval_dates, index(p_xts))
    v_p   <- !is.na(idx_p)
    if (any(v_p)) m_prob[v_p, j] <- as.numeric(p_xts)[idx_p[v_p]]
    
    idx_rs <- match(eval_dates, index(obj$rs_spy))
    v_rs   <- !is.na(idx_rs)
    if (any(v_rs)) m_rs[v_rs, j] <- as.numeric(obj$rs_spy)[idx_rs[v_rs]]
    
    idx_v <- match(eval_dates, index(obj$garch_vol))
    v_v   <- !is.na(idx_v)
    if (any(v_v)) {
      raw_v <- as.numeric(obj$garch_vol)[idx_v[v_v]]
      raw_v[is.na(raw_v) | raw_v < 0.05] <- 0.25
      m_vol[v_v, j] <- raw_v
    }
    
    idx_b <- match(eval_dates, index(r_xts))
    v_b   <- !is.na(idx_b)
    if (any(v_b)) {
      m_ret_b[v_b, j] <- as.numeric(r_xts)[idx_b[v_b]]
      m_valid[v_b, j] <- TRUE
    }
  }
  list(
    syms    = a_syms,
    sectors = sapply(a_syms, get_sym_sector),
    prob    = m_prob,
    rs      = m_rs,
    vol     = m_vol,
    ret_b   = m_ret_b,
    valid   = m_valid
  )
}

cat("[Step 2/3] Loading Baseline & Enhanced Matrix Bundles...\n")
bundle_base      <- build_matrix_bundle(CACHE_DIR_BASE, "_10yr_wf[.]rds$",     use_boost_prob = FALSE, use_ratchet_ret = FALSE)
bundle_enh_feats <- build_matrix_bundle(CACHE_DIR_ENH,  "_10yr_enh_wf[.]rds$", use_boost_prob = FALSE, use_ratchet_ret = FALSE)
bundle_enh_full  <- build_matrix_bundle(CACHE_DIR_ENH,  "_10yr_enh_wf[.]rds$", use_boost_prob = TRUE,  use_ratchet_ret = TRUE)

sim_portfolio <- function(bundle, cash_series, cb_thresh = 0.12) {
  port_ret <- numeric(n_total)
  swing_only_ret <- numeric(n_total)
  active_swing_days <- 0L
  win_swing_days <- 0L
  peak_equity <- 1.0
  cur_equity  <- 1.0
  cb_active_days <- 0L
  
  for (t in 1:n_total) {
    dd <- (cur_equity - peak_equity) / peak_equity
    is_cb_active <- if (!is.null(cb_thresh)) (dd <= -cb_thresh) else FALSE
    if (is_cb_active) cb_active_days <- cb_active_days + 1L
    cb_multiplier <- if (is_cb_active) 0.50 else 1.0
    
    is_bull <- macro_bull[t]
    eff_max_pos <- if (is_bull) 3 else 2
    eff_p_long  <- if (is_bull) 0.58 else 0.65
    
    p_ups   <- bundle$prob[t, ]
    rs_vals <- bundle$rs[t, ]
    vols    <- bundle$vol[t, ]
    rets_b  <- bundle$ret_b[t, ]
    val_flg <- bundle$valid[t, ]
    
    qualify <- which(val_flg & p_ups >= eff_p_long & rs_vals > 0)
    top_idx <- integer(0)
    if (length(qualify) > 0) {
      scores <- p_ups[qualify] * (1 + rs_vals[qualify])
      ranked <- qualify[order(-scores)]
      sec_cnts <- list()
      for (cand in ranked) {
        c_sec <- bundle$sectors[cand]
        cnt <- if (is.null(sec_cnts[[c_sec]])) 0 else sec_cnts[[c_sec]]
        if (cnt < 3) {
          top_idx <- c(top_idx, cand)
          sec_cnts[[c_sec]] <- cnt + 1
          if (length(top_idx) >= eff_max_pos) break
        }
      }
    }
    
    n_sel <- length(top_idx)
    cash_day_ret <- cash_series[t]
    
    if (n_sel > 0) {
      inv_v <- 1 / vols[top_idx]
      weights <- inv_v / sum(inv_v)
      raw_swing_exp <- min(1.0, n_sel / 3.0)
      swing_exposure <- raw_swing_exp * cb_multiplier
      swing_ret <- sum(weights * rets_b[top_idx])
      port_ret[t] <- swing_exposure * swing_ret + (1 - swing_exposure) * cash_day_ret
      swing_only_ret[t] <- swing_ret
      if (abs(swing_ret) > 1e-8) {
        active_swing_days <- active_swing_days + 1L
        if (swing_ret > 0) win_swing_days <- win_swing_days + 1L
      }
    } else {
      port_ret[t] <- cash_day_ret
    }
    
    cur_equity <- cur_equity * (1 + port_ret[t])
    if (cur_equity > peak_equity) peak_equity <- cur_equity
  }
  
  eq <- cumprod(1 + port_ret)
  cagr <- tail(eq, 1)^(252 / n_total) - 1
  mdd <- min((eq - cummax(eq)) / cummax(eq))
  ann_vol <- sd(port_ret) * sqrt(252)
  sharpe <- (mean(port_ret) * 252 - 0.04) / ann_vol
  downside_ret <- pmin(0, port_ret)
  sortino <- (mean(port_ret) * 252 - 0.04) / (sqrt(mean(downside_ret^2)) * sqrt(252))
  final_10k <- 10000 * tail(eq, 1)
  calmar <- cagr / abs(mdd)
  win_rate_days <- if (active_swing_days > 0) (win_swing_days / active_swing_days) * 100 else 0
  
  list(
    CAGR = cagr, Final_10k = final_10k, MaxDD = mdd,
    Sharpe = sharpe, Sortino = sortino, AnnVol = ann_vol,
    Calmar = calmar, CB_Days = cb_active_days,
    Active_Days = active_swing_days, Win_Day_Pct = win_rate_days,
    port_ret = port_ret, equity_curve = eq
  )
}

modes <- list(
  "Pure 15-Day Time Stop" = get_cash_series(base_fb_75_25, max_days = 15, max_loss = NULL),
  "15-Day Time Stop + -10% Loss Stop" = get_cash_series(base_fb_75_25, max_days = 15, max_loss = 0.10)
)

models <- list(
  "1. Baseline Model (19 Features)" = bundle_base,
  "2. Enhanced (+7 Alpha Features + 7d Embargo)" = bundle_enh_feats,
  "3. Enhanced Full (+7 Features + Tier-1 Ratchet + Catalyst Boost)" = bundle_enh_full
)

results_rows <- list()
curves_list  <- list()
rets_list    <- list()

for (m_name in names(modes)) {
  cs_vec <- modes[[m_name]]$series
  for (mod_name in names(models)) {
    sim <- sim_portfolio(models[[mod_name]], cs_vec, cb_thresh = 0.12)
    key <- sprintf("%s | %s", m_name, mod_name)
    curves_list[[key]] <- sim$equity_curve
    rets_list[[key]]   <- sim$port_ret
    
    results_rows[[key]] <- data.frame(
      Protection_Mode = m_name,
      Model_Version   = mod_name,
      Final_10k       = sim$Final_10k,
      CAGR_Pct        = sim$CAGR * 100,
      MaxDD_Pct       = sim$MaxDD * 100,
      Sharpe          = sim$Sharpe,
      Sortino         = sim$Sortino,
      AnnVol_Pct      = sim$AnnVol * 100,
      Calmar          = sim$Calmar,
      Win_Day_Pct     = sim$Win_Day_Pct,
      CB_Days         = sim$CB_Days,
      stringsAsFactors = FALSE
    )
  }
}

df_res <- do.call(rbind, results_rows)
dir.create("output", showWarnings = FALSE)
write.csv(df_res, "output/enhanced_vs_baseline_10yr_scorecard.csv", row.names = FALSE)

cat("\n========================================================================================\n")
cat(" 10-YEAR $10,000 BACKTEST RESULTS: BASELINE VS. ENHANCED MODEL (2016 - 2026)\n")
cat(" Defensive Sleeve: 75% FNGD + 25% SQQQ | Circuit Breaker: -12.0%\n")
cat("========================================================================================\n\n")

for (i in 1:nrow(df_res)) {
  r <- df_res[i, ]
  cat(sprintf("%-34s | %-62s | Final: $%7.2fM | CAGR: %+6.2f%% | MaxDD: %6.2f%% | Sharpe: %.2f | Sortino: %.2f | Calmar: %.2f | CB Days: %d\n",
              r$Protection_Mode, r$Model_Version, r$Final_10k / 1e6, r$CAGR_Pct, r$MaxDD_Pct,
              r$Sharpe, r$Sortino, r$Calmar, r$CB_Days))
}

years_vec <- format(eval_dates, "%Y")
uniq_years <- unique(years_vec)

cat("\n========================================================================================\n")
cat(" YEAR-BY-YEAR ANNUAL RETURNS: BASELINE VS. ENHANCED FULL SYSTEM\n")
cat("========================================================================================\n")
cat(sprintf("%-6s | %18s | %18s | %18s | %18s\n",
            "Year", "Base (15d Pure)", "Enh Full (15d Pure)", "Base (15d + -10%)", "Enh Full (15d + -10%)"))
cat(paste(rep("-", 90), collapse = ""), "\n")

yr_rows <- list()
for (yr in uniq_years) {
  idx_y <- which(years_vec == yr)
  r_b1 <- prod(1 + rets_list[["Pure 15-Day Time Stop | 1. Baseline Model (19 Features)"]][idx_y]) - 1
  r_e1 <- prod(1 + rets_list[["Pure 15-Day Time Stop | 3. Enhanced Full (+7 Features + Tier-1 Ratchet + Catalyst Boost)"]][idx_y]) - 1
  r_b2 <- prod(1 + rets_list[["15-Day Time Stop + -10% Loss Stop | 1. Baseline Model (19 Features)"]][idx_y]) - 1
  r_e2 <- prod(1 + rets_list[["15-Day Time Stop + -10% Loss Stop | 3. Enhanced Full (+7 Features + Tier-1 Ratchet + Catalyst Boost)"]][idx_y]) - 1
  cat(sprintf("%-6s | %+17.2f%% | %+17.2f%% | %+17.2f%% | %+17.2f%%\n",
              yr, r_b1 * 100, r_e1 * 100, r_b2 * 100, r_e2 * 100))
  yr_rows[[yr]] <- data.frame(
    Year = yr,
    Base_15d_Pct = r_b1 * 100,
    Enh_15d_Pct  = r_e1 * 100,
    Base_15d_10pct_Pct = r_b2 * 100,
    Enh_15d_10pct_Pct  = r_e2 * 100,
    stringsAsFactors = FALSE
  )
}
write.csv(do.call(rbind, yr_rows), "output/enhanced_vs_baseline_yearly.csv", row.names = FALSE)

png("output/enhanced_vs_baseline_10yr_chart.png", width = 1400, height = 850, res = 130)
par(mfrow = c(2, 1), mar = c(3.2, 5.0, 3.0, 2.0), bg = "#0f172a", col.axis = "#cbd5e1", col.lab = "#f8fafc", col.main = "#f8fafc", fg = "#334155")

eq_b1 <- curves_list[["Pure 15-Day Time Stop | 1. Baseline Model (19 Features)"]] * 10000
eq_f1 <- curves_list[["Pure 15-Day Time Stop | 2. Enhanced (+7 Alpha Features + 7d Embargo)"]] * 10000
eq_e1 <- curves_list[["Pure 15-Day Time Stop | 3. Enhanced Full (+7 Features + Tier-1 Ratchet + Catalyst Boost)"]] * 10000
eq_b2 <- curves_list[["15-Day Time Stop + -10% Loss Stop | 1. Baseline Model (19 Features)"]] * 10000
eq_e2 <- curves_list[["15-Day Time Stop + -10% Loss Stop | 3. Enhanced Full (+7 Features + Tier-1 Ratchet + Catalyst Boost)"]] * 10000

y_range <- range(c(eq_b1, eq_e1, eq_b2, eq_e2))
plot(eval_dates, log10(eq_e1), type = "l", lwd = 2.8, col = "#22c55e",
     ylim = log10(y_range), yaxt = "n", xlab = "", ylab = "Portfolio Equity ($ Log Scale)",
     main = "10-Year $10,000 Backtest: Enhanced Model (26 Features + Ratchet) vs. Baseline Model (2016 - 2026)")
grid(col = "#1e293b", lty = "dotted")
lines(eval_dates, log10(eq_f1), lwd = 2.0, col = "#38bdf8", lty = 2)
lines(eval_dates, log10(eq_b1), lwd = 2.2, col = "#94a3b8")
lines(eval_dates, log10(eq_e2), lwd = 2.4, col = "#f59e0b")
lines(eval_dates, log10(eq_b2), lwd = 1.8, col = "#ef4444", lty = 3)

ticks <- c(1e4, 1e5, 1e6, 1e7, 1e8, 5e8)
axis(2, at = log10(ticks), labels = c("$10K", "$100K", "$1M", "$10M", "$100M", "$500M"), las = 1)
legend("topleft",
       legend = c(
         sprintf("Enhanced Full [Pure 15d Stop]: $%.2fM (%+.2f%% CAGR)", tail(eq_e1, 1)/1e6, df_res$CAGR_Pct[3]),
         sprintf("Enhanced +7 Feats [Pure 15d Stop]: $%.2fM (%+.2f%% CAGR)", tail(eq_f1, 1)/1e6, df_res$CAGR_Pct[2]),
         sprintf("Baseline 19 Feats [Pure 15d Stop]: $%.2fM (%+.2f%% CAGR)", tail(eq_b1, 1)/1e6, df_res$CAGR_Pct[1]),
         sprintf("Enhanced Full [15d + -10%% Loss]: $%.2fM (%+.2f%% CAGR)", tail(eq_e2, 1)/1e6, df_res$CAGR_Pct[6]),
         sprintf("Baseline 19 Feats [15d + -10%% Loss]: $%.2fM (%+.2f%% CAGR)", tail(eq_b2, 1)/1e6, df_res$CAGR_Pct[4])
       ),
       col = c("#22c55e", "#38bdf8", "#94a3b8", "#f59e0b", "#ef4444"),
       lwd = c(2.8, 2.0, 2.2, 2.4, 1.8), lty = c(1, 2, 1, 1, 3),
       bg = "#1e293b", text.col = "#f8fafc", cex = 0.85, bty = "o")

dd_b1 <- (eq_b1 - cummax(eq_b1)) / cummax(eq_b1) * 100
dd_e1 <- (eq_e1 - cummax(eq_e1)) / cummax(eq_e1) * 100
dd_e2 <- (eq_e2 - cummax(eq_e2)) / cummax(eq_e2) * 100

plot(eval_dates, dd_e1, type = "l", lwd = 2.2, col = "#22c55e",
     ylim = c(-30, 0), xlab = "Date", ylab = "Drawdown (%)",
     main = "Underwater Drawdown Profile (-12.0% Circuit Breaker Threshold)")
grid(col = "#1e293b", lty = "dotted")
lines(eval_dates, dd_b1, lwd = 1.8, col = "#94a3b8", lty = 2)
lines(eval_dates, dd_e2, lwd = 2.0, col = "#f59e0b")
abline(h = -12.0, col = "#ef4444", lty = 2, lwd = 1.5)
legend("bottomleft",
       legend = c(
         sprintf("Enhanced Full [Pure 15d]: %.2f%% Max DD", min(dd_e1)),
         sprintf("Baseline [Pure 15d]: %.2f%% Max DD", min(dd_b1)),
         sprintf("Enhanced Full [15d + -10%% Loss]: %.2f%% Max DD", min(dd_e2)),
         "-12.0% Circuit Breaker"
       ),
       col = c("#22c55e", "#94a3b8", "#f59e0b", "#ef4444"),
       lwd = c(2.2, 1.8, 2.0, 1.5), lty = c(1, 2, 1, 2),
       bg = "#1e293b", text.col = "#f8fafc", cex = 0.85, bty = "o")

dev.off()
cat("\n[Step 3/3] Saved chart to output/enhanced_vs_baseline_10yr_chart.png\n")
