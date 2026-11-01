#!/usr/bin/env Rscript
# ==============================================================================
# scratch/test_advanced_cagr_enhancers.R
# Deep-Dive on Dual-Lookback and Momentum Signal Enhancements
# ==============================================================================

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
})

HIST <- "data/data_historical"
ART  <- "/Users/cmejo/.gemini/antigravity/brain/64019830-0cac-4bb1-904e-d4560e71468c"

EVAL_START <- as.Date("2006-10-02")
EVAL_END   <- as.Date("2026-10-06")
COST       <- 0.0010
MARGIN_SPREAD <- 0.0175

adj <- function(sym) {
  p <- file.path(HIST, sprintf("data_%s.rds", tolower(sym)))
  if (!file.exists(p)) return(NULL)
  d <- readRDS(p)
  a <- d[, grep("Adjusted$", colnames(d))]
  a[a <= 0] <- NA
  a
}

qqq_p <- adj("QQQ")
qld_p <- adj("QLD")
gld_p <- adj("GLD_FULL")

cal <- index(qqq_p)[index(qqq_p) >= as.Date("2004-01-01") & index(qqq_p) <= EVAL_END]

# QQQ returns & SMAs
q <- as.numeric(qqq_p[cal])
rq <- c(0, q[-1] / q[-length(q)] - 1)
sma200_q <- as.numeric(stats::filter(q, rep(1/200, 200), sides = 1))
qqq_trend_up <- q >= sma200_q; qqq_trend_up[is.na(qqq_trend_up)] <- TRUE

# QLD returns
m_qld <- match(cal, as.Date(index(qld_p)))
qld_px <- as.numeric(qld_p)[m_qld]
qld_px <- zoo::na.locf(qld_px, na.rm = FALSE)
r_qld <- c(0, qld_px[-1] / qld_px[-length(qld_px)] - 1)
r_qld[is.na(r_qld)] <- 2 * rq[is.na(r_qld)]

# GLD returns
m_gld <- match(cal, as.Date(index(gld_p)))
gld_px <- as.numeric(gld_p)[m_gld]
gld_px <- zoo::na.locf(gld_px, na.rm = FALSE)
r_gld <- c(0, gld_px[-1] / gld_px[-length(gld_px)] - 1); r_gld[is.na(r_gld)] <- 0

# Risk-free & margin borrowing rates
irx <- readRDS(file.path(HIST, "data_^irx.rds"))
rf  <- as.numeric(zoo::na.locf(irx[, 4], na.rm = FALSE)[cal])
rf  <- zoo::na.locf(rf, na.rm = FALSE); rf[is.na(rf)] <- 2
rf_rate <- rf / 100; rf_daily <- rf_rate / 252
margin_borrow_daily <- (rf_rate + MARGIN_SPREAD) / 252

# Universe stocks
EXCLUDE <- c("QQQ", "QLD", "TQQQ", "SQQQ", "SPY", "VTI", "VFINX", "SMH", "SOXL", "XLK", "GLD_FULL",
             "AMUU", "NVDU", "PLTU", "SNDQ", "SMHC", "SKHY", "^VIX", "^IRX")
files   <- list.files(HIST, pattern = "^data_[a-z0-9_]+[.]rds$")
syms    <- setdiff(toupper(sub("^data_", "", sub("[.]rds$", "", files))), EXCLUDE)

P <- matrix(NA_real_, length(cal), length(syms), dimnames = list(NULL, syms))
for (s in syms) {
  a <- tryCatch(adj(s), error = function(e) NULL)
  if (is.null(a)) next
  m <- match(cal, as.Date(index(a)))
  P[, s] <- as.numeric(a)[m]
}
P <- P[, colSums(!is.na(P)) > 300, drop = FALSE]

for (j in seq_len(ncol(P))) {
  v <- P[, j]
  f <- which(!is.na(v))[1]
  if (!is.na(f)) v[f:length(v)] <- zoo::na.locf(v[f:length(v)], na.rm = FALSE)
  P[, j] <- v
}

R  <- rbind(NA, P[-1, , drop = FALSE] / P[-nrow(P), , drop = FALSE] - 1); R[is.na(R)] <- 0
roll_vol_20 <- rollapply(rq, width = 20, FUN = function(x) sd(x) * sqrt(252), align = "right", fill = 0.20)

ev <- which(cal >= EVAL_START)
dates <- cal[ev]
month_end <- c(format(cal[-1], "%m") != format(cal[-length(cal)], "%m"), TRUE)

sim_engine_signal <- function(signal_mode = c("126d", "dual_60_40", "dual_50_50", "triple_126_63_21", "vol_adj_rs"),
                              portfolio_margin = 1.50,
                              top_n = 5,
                              core_pct = 0.30,
                              vol_target = 0.22,
                              disaster_stop = 0.20,
                              label = "Signal Test") {
  signal_mode <- match.arg(signal_mode)
  sat_pct <- 1 - core_pct
  
  eq <- 1.0; peak_eq <- 1.0
  out_ret <- numeric(length(ev))
  turn_tot <- 0
  
  curr_core_w  <- 0.0
  curr_sat_w   <- rep(0.0, ncol(P)); names(curr_sat_w) <- colnames(P)
  curr_def_w   <- 0.0
  
  entry_prices <- rep(NA_real_, ncol(P)); names(entry_prices) <- colnames(P)
  stopped_out  <- rep(FALSE, ncol(P)); names(stopped_out) <- colnames(P)
  
  prev_eq <- eq
  
  for (k in seq_along(ev)) {
    t <- ev[k]
    r_core <- r_qld[t]
    r_sat  <- R[t, ]
    r_def  <- r_gld[t]
    
    tot_sat_val  <- sum(curr_sat_w * (1 + r_sat))
    tot_core_val <- curr_core_w * (1 + r_core)
    tot_def_val  <- curr_def_w * (1 + r_def)
    
    gross_exp <- curr_core_w + sum(curr_sat_w) + curr_def_w
    borrow_w  <- max(0, gross_exp - 1.0)
    cash_w    <- max(0, 1.0 - gross_exp)
    
    borrow_cost <- borrow_w * margin_borrow_daily[t]
    cash_gain   <- cash_w * rf_daily[t]
    
    day_gain <- (curr_core_w * r_core) + sum(curr_sat_w * r_sat) + (curr_def_w * r_def) - borrow_cost + cash_gain
    
    eq <- eq * (1 + day_gain)
    if (eq > peak_eq) peak_eq <- eq
    
    curr_core_w <- tot_core_val / (1 + day_gain)
    curr_sat_w  <- (curr_sat_w * (1 + r_sat)) / (1 + day_gain)
    curr_def_w  <- tot_def_val / (1 + day_gain)
    
    active_sats <- names(curr_sat_w)[curr_sat_w > 0]
    for (sym in active_sats) {
      px_now <- P[t, sym]
      if (!is.na(entry_prices[sym]) && entry_prices[sym] > 0) {
        if ((px_now / entry_prices[sym] - 1) <= -disaster_stop && !stopped_out[sym]) {
          to <- curr_sat_w[sym]
          eq <- eq * (1 - COST * to)
          turn_tot <- turn_tot + to
          curr_sat_w[sym] <- 0
          stopped_out[sym] <- TRUE
        }
      }
    }
    
    s <- t - 1
    if (month_end[s] || k == 1) {
      realized_v <- roll_vol_20[s]
      scale_mult <- min(1.0, vol_target / max(0.05, realized_v))
      
      active_margin <- portfolio_margin
      if (portfolio_margin > 1.0 && !isTRUE(qqq_trend_up[s])) {
        active_margin <- 1.0
      }
      
      core_active <- isTRUE(qqq_trend_up[s])
      tgt_core_w <- if (core_active) core_pct * scale_mult * active_margin else 0.0
      
      lb_252 <- pmax(1, s - 252); lb_126 <- pmax(1, s - 126); lb_63 <- pmax(1, s - 63); lb_21 <- pmax(1, s - 21)
      ok <- colnames(P)[!is.na(P[s, ]) & !is.na(P[lb_252, ]) & (s >= 252)]
      
      tgt_sat_w <- rep(0.0, ncol(P)); names(tgt_sat_w) <- colnames(P)
      if (length(ok) > 0) {
        ret_126 <- (P[s, ok] / P[lb_126, ok] - 1) - (q[s] / q[lb_126] - 1)
        ret_63  <- (P[s, ok] / P[lb_63, ok] - 1)  - (q[s] / q[lb_63] - 1)
        ret_21  <- (P[s, ok] / P[lb_21, ok] - 1)  - (q[s] / q[lb_21] - 1)
        
        if (signal_mode == "126d") {
          rs <- ret_126
        } else if (signal_mode == "dual_60_40") {
          rs <- 0.60 * ret_126 + 0.40 * ret_63
        } else if (signal_mode == "dual_50_50") {
          rs <- 0.50 * ret_126 + 0.50 * ret_63
        } else if (signal_mode == "triple_126_63_21") {
          rs <- 0.50 * ret_126 + 0.30 * ret_63 + 0.20 * ret_21
        } else if (signal_mode == "vol_adj_rs") {
          # Sharpe-like momentum: RS / trailing 63d vol
          v_63 <- apply(R[lb_63:s, ok, drop = FALSE], 2, function(x) sd(x) * sqrt(252))
          v_63[is.na(v_63) | v_63 < 0.10] <- 0.10
          rs <- (0.60 * ret_126 + 0.40 * ret_63) / v_63
        }
        
        sma200_stocks <- sapply(ok, function(sym) mean(P[(s - 199):s, sym], na.rm = TRUE))
        pass_trend <- P[s, ok] >= sma200_stocks
        ok_filtered <- ok[pass_trend]
        if (length(ok_filtered) >= top_n) {
          ok <- ok_filtered; rs <- rs[ok]
        }
        
        top_syms <- names(sort(rs, decreasing = TRUE))[1:min(top_n, length(rs))]
        if (length(top_syms) > 0) {
          tgt_sat_w[top_syms] <- (sat_pct * scale_mult * active_margin) / length(top_syms)
        }
        
        new_entrants <- setdiff(top_syms, names(curr_sat_w)[curr_sat_w > 0])
        for (ne in new_entrants) {
          entry_prices[ne] <- P[t, ne]
          stopped_out[ne]  <- FALSE
        }
      }
      
      base_equity_w <- (if (core_active) core_pct else 0.0) * scale_mult + (sat_pct * scale_mult)
      tgt_def_w <- max(0, 1.0 - base_equity_w)
      
      to <- abs(tgt_core_w - curr_core_w) + sum(abs(tgt_sat_w - curr_sat_w)) + abs(tgt_def_w - curr_def_w)
      if (to > 1e-6) {
        eq <- eq * (1 - COST * to)
        turn_tot <- turn_tot + to
      }
      
      curr_core_w <- tgt_core_w
      curr_sat_w  <- tgt_sat_w
      curr_def_w  <- tgt_def_w
    }
    
    out_ret[k] <- if (k == 1) day_gain else eq / prev_eq - 1
    prev_eq <- eq
  }
  
  attr(out_ret, "turnover_yr") <- turn_tot / (length(ev) / 252)
  attr(out_ret, "label") <- label
  out_ret
}

cat("Simulating Momentum Signal Variations (2006-2026)...\n")

runs_sig <- list(
  "1. Baseline 126d RS"                      = sim_engine_signal(signal_mode = "126d", label = "Baseline 126d"),
  "2. Dual RS: 60% 126d + 40% 63d"           = sim_engine_signal(signal_mode = "dual_60_40", label = "Dual 60/40"),
  "3. Dual RS: 50% 126d + 50% 63d"           = sim_engine_signal(signal_mode = "dual_50_50", label = "Dual 50/50"),
  "4. Triple RS: 50% 126d + 30% 63d + 20% 21d"= sim_engine_signal(signal_mode = "triple_126_63_21", label = "Triple RS"),
  "5. Volatility-Adjusted RS (Sharpe Mom)"   = sim_engine_signal(signal_mode = "vol_adj_rs", label = "Vol-Adjusted RS")
)

calc_metrics <- function(r) {
  c_r <- cumprod(1 + r); n <- length(r); dd <- c_r / cummax(c_r) - 1
  mdd <- -min(dd) * 100
  cagr <- (tail(c_r, 1)^(252 / n) - 1) * 100
  term <- 8000 * tail(c_r, 1)
  vol <- sd(r) * sqrt(252) * 100
  rfm <- rf_daily[ev]
  sharpe <- if (sd(r) > 0) mean(r - rfm) / sd(r) * sqrt(252) else 0
  sortino <- {
    d_ret <- pmin(r - rfm, 0)
    d_sd  <- sqrt(mean(d_ret^2)) * sqrt(252)
    if (d_sd > 0) mean(r - rfm) * 252 / d_sd else 0
  }
  calmar <- if (mdd > 0) cagr / mdd else 0
  
  c(CAGR = cagr, Terminal_8k = term, MaxDD = mdd, Vol = vol,
    Sharpe = sharpe, Sortino = sortino, Calmar = calmar)
}

tbl_sig <- t(sapply(runs_sig, calc_metrics))
cat("\n========================================================================================================================\n")
cat("                       MOMENTUM SIGNAL VARIATION SCORECARD (2006 - 2026)\n")
cat("========================================================================================================================\n")
print(round(tbl_sig, 2))
cat("========================================================================================================================\n")
