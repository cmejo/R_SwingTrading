#!/usr/bin/env Rscript
# ==============================================================================
# 10-Year Head-to-Head Performance Audit (2016 - 2026, 2,520 Trading Days)
# Compares Step-by-Step Attribution:
#   Step 1: Original 19-Feature Baseline (With UNH, 162 symbols)
#   Step 2: Prior 26-Feature Model (With UNH, 162 symbols)
#   Step 3: Prior 26-Feature Model (UNH Removed, 161 symbols)
#   Step 4: + 4 Orthogonal Features & Pruned Collinear (UNH Removed, 161 symbols)
#   Step 5: LATEST Full Upgrade (+ 20D Adaptive Runner + Quality_Score Ranker, 161 symbols)
# ==============================================================================

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(TTR)
  library(jsonlite)
})

source("R/02_trend_lmMA.R")

DATA_DIR_10Y   <- "data/data_10yr"
CACHE_DIR_BASE <- "data/cache_10yr"
CACHE_DIR_ENH  <- "data/cache_10yr_enhanced"
CACHE_DIR_LAT  <- "data/cache_10yr_latest"

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

get_cash_series <- function(base_fb, max_days = 15, max_loss = 0.10) {
  cash_ret <- numeric(n_total)
  in_ep <- FALSE; ep_d <- 0; ep_cum_ret <- 1.0; rot_active <- FALSE
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
  cash_ret
}

SECTOR_MAP <- if (file.exists("sector_map.json")) jsonlite::fromJSON("sector_map.json") else list()
get_sym_sector <- function(s) {
  if (!is.null(SECTOR_MAP[[s]])) return(SECTOR_MAP[[s]])
  if (s == "UNH") return("Healthcare_Biotech")
  return("General_Tech")
}

build_matrix_bundle <- function(cache_dir, pattern_str, exclude_syms = character(0),
                                use_boost_prob = TRUE, use_ratchet_ret = TRUE, ret_override_dir = NULL, ret_override_pat = NULL) {
  c_files <- list.files(cache_dir, pattern = pattern_str, full.names = TRUE)
  s_data <- list()
  for (cf in c_files) {
    obj <- readRDS(cf)
    if (!(obj$symbol %in% exclude_syms)) {
      s_data[[obj$symbol]] <- obj
    }
  }
  a_syms <- names(s_data)
  ns <- length(a_syms)

  # Optional override for bracket return vector (to isolate feature probability lift vs adaptive runner horizon)
  override_data <- list()
  if (!is.null(ret_override_dir)) {
    o_files <- list.files(ret_override_dir, pattern = ret_override_pat, full.names = TRUE)
    for (of in o_files) {
      o_obj <- readRDS(of)
      override_data[[o_obj$symbol]] <- o_obj
    }
  }

  m_prob  <- matrix(0.5,   nrow = n_total, ncol = ns)
  m_rs    <- matrix(0.0,   nrow = n_total, ncol = ns)
  m_vol   <- matrix(0.25,  nrow = n_total, ncol = ns)
  m_tq    <- matrix(0.50,  nrow = n_total, ncol = ns)
  m_idio  <- matrix(0.0,   nrow = n_total, ncol = ns)
  m_ret_b <- matrix(0.0,   nrow = n_total, ncol = ns)
  m_valid <- matrix(FALSE, nrow = n_total, ncol = ns)

  for (j in seq_along(a_syms)) {
    s <- a_syms[j]
    obj <- s_data[[s]]
    p_xts <- if (use_boost_prob && !is.null(obj$pred_prob_boost)) obj$pred_prob_boost else obj$pred_prob
    r_xts <- if (!is.null(ret_override_dir) && !is.null(override_data[[s]])) {
      override_data[[s]]$bracket_ratchet
    } else if (use_ratchet_ret && !is.null(obj$bracket_ratchet)) {
      obj$bracket_ratchet
    } else {
      obj$bracket_ret
    }

    idx_p <- match(eval_dates, index(p_xts))
    v_p   <- !is.na(idx_p)
    if (any(v_p)) m_prob[v_p, j] <- as.numeric(p_xts)[idx_p[v_p]]

    idx_rs <- match(eval_dates, index(obj$rs_spy))
    v_rs   <- !is.na(idx_rs)
    if (any(v_rs)) m_rs[v_rs, j] <- as.numeric(obj$rs_spy)[idx_rs[v_rs]]

    if (!is.null(obj$trend_quality)) {
      idx_tq <- match(eval_dates, index(obj$trend_quality))
      v_tq   <- !is.na(idx_tq)
      if (any(v_tq)) m_tq[v_tq, j] <- as.numeric(obj$trend_quality)[idx_tq[v_tq]]
    }

    if (!is.null(obj$idio_alpha)) {
      idx_id <- match(eval_dates, index(obj$idio_alpha))
      v_id   <- !is.na(idx_id)
      if (any(v_id)) m_idio[v_id, j] <- as.numeric(obj$idio_alpha)[idx_id[v_id]]
    }

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
    tq      = m_tq,
    idio    = m_idio,
    ret_b   = m_ret_b,
    valid   = m_valid
  )
}

# Build 5 incremental bundles to isolate each change:
b1_base19       <- build_matrix_bundle(CACHE_DIR_BASE, "_10yr_wf[.]rds$",     exclude_syms = character(0), use_boost_prob = FALSE, use_ratchet_ret = FALSE)
b2_prior26_unh  <- build_matrix_bundle(CACHE_DIR_ENH,  "_10yr_enh_wf[.]rds$", exclude_syms = character(0), use_boost_prob = TRUE,  use_ratchet_ret = TRUE)
b3_prior26_nounh<- build_matrix_bundle(CACHE_DIR_ENH,  "_10yr_enh_wf[.]rds$", exclude_syms = c("UNH"),     use_boost_prob = TRUE,  use_ratchet_ret = TRUE)
# Step 4: New 26 Orthogonal Features probabilities, but using prior fixed-horizon bracket returns & prior ranker
b4_orth_feats   <- build_matrix_bundle(CACHE_DIR_LAT,  "_10yr_lat_wf[.]rds$", exclude_syms = c("UNH"),     use_boost_prob = TRUE,  use_ratchet_ret = TRUE,
                                       ret_override_dir = CACHE_DIR_ENH, ret_override_pat = "_10yr_enh_wf[.]rds$")
# Step 5: Full Latest System (26 Orthogonal Features + Adaptive Runner Horizon + Upgraded Quality_Score Ranker)
b5_latest_full  <- build_matrix_bundle(CACHE_DIR_LAT,  "_10yr_lat_wf[.]rds$", exclude_syms = c("UNH"),     use_boost_prob = TRUE,  use_ratchet_ret = TRUE)

sim_portfolio <- function(bundle, cash_series, cb_thresh = 0.12, use_upgraded_ranker = FALSE) {
  port_ret <- numeric(n_total)
  swing_only_ret <- numeric(n_total)
  active_swing_days <- 0L
  win_swing_days <- 0L
  peak_equity <- 1.0
  cur_equity  <- 1.0

  for (t in 1:n_total) {
    dd <- (cur_equity - peak_equity) / peak_equity
    is_cb_active <- if (!is.null(cb_thresh)) (dd <= -cb_thresh) else FALSE
    cb_multiplier <- if (is_cb_active) 0.50 else 1.0

    is_bull <- macro_bull[t]
    eff_max_pos <- if (is_bull) 3 else 2
    eff_p_long  <- if (is_bull) 0.58 else 0.65

    p_ups   <- bundle$prob[t, ]
    rs_vals <- bundle$rs[t, ]
    vols    <- bundle$vol[t, ]
    tqs     <- bundle$tq[t, ]
    idios   <- bundle$idio[t, ]
    rets_b  <- bundle$ret_b[t, ]
    val_flg <- bundle$valid[t, ]

    qualify <- which(val_flg & p_ups >= eff_p_long & rs_vals > 0)
    top_idx <- integer(0)
    if (length(qualify) > 0) {
      if (use_upgraded_ranker) {
        tq_boost   <- pmax(0, (tqs[qualify] - 0.50) * 0.25)
        idio_boost <- pmax(0, idios[qualify] * 0.08)
        scores <- p_ups[qualify] * (1 + rs_vals[qualify] + tq_boost + idio_boost)
      } else {
        scores <- p_ups[qualify] * (1 + rs_vals[qualify])
      }
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
      swing_only_ret[t] <- swing_exposure * swing_ret + (1 - swing_exposure) * sgov_v[t]
      if (abs(swing_ret) > 1e-8) {
        active_swing_days <- active_swing_days + 1L
        if (swing_ret > 0) win_swing_days <- win_swing_days + 1L
      }
    } else {
      port_ret[t] <- cash_day_ret
      swing_only_ret[t] <- sgov_v[t]
    }

    cur_equity <- cur_equity * (1 + port_ret[t])
    if (cur_equity > peak_equity) peak_equity <- cur_equity
  }

  calc_stats <- function(r_vec) {
    eq <- cumprod(1 + r_vec)
    cagr <- (tail(eq, 1)^(252 / n_total) - 1) * 100
    mdd <- min((eq - cummax(eq)) / cummax(eq)) * 100
    ann_vol <- sd(r_vec) * sqrt(252) * 100
    sharpe <- (mean(r_vec) * 252 - 0.04) / (sd(r_vec) * sqrt(252))
    downside_ret <- pmin(0, r_vec)
    sortino <- (mean(r_vec) * 252 - 0.04) / (sqrt(mean(downside_ret^2)) * sqrt(252))
    calmar <- cagr / abs(mdd)
    list(CAGR = cagr, MaxDD = mdd, Sharpe = sharpe, Sortino = sortino, AnnVol = ann_vol, Calmar = calmar, ret = r_vec)
  }

  win_rate_days <- if (active_swing_days > 0) (win_swing_days / active_swing_days) * 100 else 0
  list(Full = calc_stats(port_ret), Swing = calc_stats(swing_only_ret), Win_Day_Pct = win_rate_days, Active_Days = active_swing_days)
}

cash_15d_10pct <- get_cash_series(base_fb_75_25, max_days = 15, max_loss = 0.10)

s1 <- sim_portfolio(b1_base19,        cash_15d_10pct, cb_thresh = 0.12, use_upgraded_ranker = FALSE)
s2 <- sim_portfolio(b2_prior26_unh,   cash_15d_10pct, cb_thresh = 0.12, use_upgraded_ranker = FALSE)
s3 <- sim_portfolio(b3_prior26_nounh, cash_15d_10pct, cb_thresh = 0.12, use_upgraded_ranker = FALSE)
s4 <- sim_portfolio(b4_orth_feats,    cash_15d_10pct, cb_thresh = 0.12, use_upgraded_ranker = FALSE)
s5 <- sim_portfolio(b5_latest_full,   cash_15d_10pct, cb_thresh = 0.12, use_upgraded_ranker = TRUE)

labels <- c(
  "1. Original 19-Feature Baseline (With UNH, 162 syms)",
  "2. Prior 26-Feature Model (With UNH, 162 syms)",
  "3. Prior 26-Feature Model (UNH Removed, 161 syms)",
  "4. + 4 Orthogonal Features & Pruned Collinear (161 syms)",
  "5. LATEST Full Upgrade (+ Adaptive Runner + Ranker, 161 syms)"
)

df_swing <- data.frame(
  Step_Configuration = labels,
  Swing_CAGR_Pct = round(c(s1$Swing$CAGR, s2$Swing$CAGR, s3$Swing$CAGR, s4$Swing$CAGR, s5$Swing$CAGR), 2),
  MaxDD_Pct      = round(c(s1$Swing$MaxDD, s2$Swing$MaxDD, s3$Swing$MaxDD, s4$Swing$MaxDD, s5$Swing$MaxDD), 2),
  Sharpe         = round(c(s1$Swing$Sharpe, s2$Swing$Sharpe, s3$Swing$Sharpe, s4$Swing$Sharpe, s5$Swing$Sharpe), 2),
  Sortino        = round(c(s1$Swing$Sortino, s2$Swing$Sortino, s3$Swing$Sortino, s4$Swing$Sortino, s5$Swing$Sortino), 2),
  AnnVol_Pct     = round(c(s1$Swing$AnnVol, s2$Swing$AnnVol, s3$Swing$AnnVol, s4$Swing$AnnVol, s5$Swing$AnnVol), 2),
  Calmar         = round(c(s1$Swing$Calmar, s2$Swing$Calmar, s3$Swing$Calmar, s4$Swing$Calmar, s5$Swing$Calmar), 2),
  Win_Day_Pct    = round(c(s1$Win_Day_Pct, s2$Win_Day_Pct, s3$Win_Day_Pct, s4$Win_Day_Pct, s5$Win_Day_Pct), 2),
  stringsAsFactors = FALSE
)

df_full <- data.frame(
  Step_Configuration = labels,
  Full_CAGR_Pct  = round(c(s1$Full$CAGR, s2$Full$CAGR, s3$Full$CAGR, s4$Full$CAGR, s5$Full$CAGR), 2),
  MaxDD_Pct      = round(c(s1$Full$MaxDD, s2$Full$MaxDD, s3$Full$MaxDD, s4$Full$MaxDD, s5$Full$MaxDD), 2),
  Sharpe         = round(c(s1$Full$Sharpe, s2$Full$Sharpe, s3$Full$Sharpe, s4$Full$Sharpe, s5$Full$Sharpe), 2),
  Sortino        = round(c(s1$Full$Sortino, s2$Full$Sortino, s3$Full$Sortino, s4$Full$Sortino, s5$Full$Sortino), 2),
  AnnVol_Pct     = round(c(s1$Full$AnnVol, s2$Full$AnnVol, s3$Full$AnnVol, s4$Full$AnnVol, s5$Full$AnnVol), 2),
  Calmar         = round(c(s1$Full$Calmar, s2$Full$Calmar, s3$Full$Calmar, s4$Full$Calmar, s5$Full$Calmar), 2),
  Win_Day_Pct    = round(c(s1$Win_Day_Pct, s2$Win_Day_Pct, s3$Win_Day_Pct, s4$Win_Day_Pct, s5$Win_Day_Pct), 2),
  stringsAsFactors = FALSE
)

cat("========================================================================================\n")
cat(" TABLE 1: PURE UNLEVERED EQUITY SWING SLEEVE (+ SGOV T-BILLS WHEN FLAT, 2016 - 2026)\n")
cat("========================================================================================\n")
print(df_swing, row.names = FALSE)

cat("\n========================================================================================\n")
cat(" TABLE 2: FULL 10-YEAR CORE-SATELLITE SYSTEM (SWING SLEEVE + 75% FNGD / 25% SQQQ HEDGE)\n")
cat("========================================================================================\n")
print(df_full, row.names = FALSE)

years_vec <- format(eval_dates, "%Y")
u_years   <- sort(unique(years_vec))
yr_rows   <- list()
for (yr in u_years) {
  idx_y <- which(years_vec == yr)
  r_qqq   <- (prod(1 + qqq_v[idx_y]) - 1) * 100
  r_sw2   <- (prod(1 + s2$Swing$ret[idx_y]) - 1) * 100
  r_sw3   <- (prod(1 + s3$Swing$ret[idx_y]) - 1) * 100
  r_sw5   <- (prod(1 + s5$Swing$ret[idx_y]) - 1) * 100
  r_fl2   <- (prod(1 + s2$Full$ret[idx_y]) - 1) * 100
  r_fl5   <- (prod(1 + s5$Full$ret[idx_y]) - 1) * 100

  yr_rows[[length(yr_rows) + 1]] <- data.frame(
    Year                = yr,
    QQQ_Pct             = round(r_qqq, 1),
    Swing_Prior_UNH_Pct = round(r_sw2, 1),
    Swing_No_UNH_Pct    = round(r_sw3, 1),
    Swing_Latest_Pct    = round(r_sw5, 1),
    Swing_Lift_Pct      = sprintf("%+.1f%%", r_sw5 - r_sw2),
    Full_Prior_Pct      = round(r_fl2, 1),
    Full_Latest_Pct     = round(r_fl5, 1),
    Full_Lift_Pct       = sprintf("%+.1f%%", r_fl5 - r_fl2),
    stringsAsFactors = FALSE
  )
}
df_yearly <- do.call(rbind, yr_rows)

cat("\n========================================================================================\n")
cat(" TABLE 3: YEAR-BY-YEAR PERFORMANCE COMPARISON (2016 - 2026)\n")
cat("========================================================================================\n")
print(df_yearly, row.names = FALSE)

write.csv(df_swing,  "output/comparison_swing_sleeve_prior_vs_latest.csv", row.names = FALSE)
write.csv(df_full,   "output/comparison_full_portfolio_prior_vs_latest.csv", row.names = FALSE)
write.csv(df_yearly, "output/comparison_yearly_prior_vs_latest.csv", row.names = FALSE)
