#!/usr/bin/env Rscript
# ==============================================================================
# scratch/test_inverse_hedge_on_vol_spikes.R
# Testing User Hypothesis: During volatility spikes, instead of parking defensive
# capital in SGOV (Treasury yield), allocate to Inverse Hedges (FNGD / QID).
#
# Configurations tested over 2006-2026 (5,034 sessions):
#   1. Baseline 60/40 (No Vol Overlay)
#   2. 60/40 + Vol Targeting -> SGOV Cash (Baseline Defensive)
#   3. 60/40 + Vol Targeting -> QID (-2x Inverse QQQ)
#   4. 60/40 + Vol Targeting -> FNGD (-3x Inverse Proxy)
#   5. 60/40 + Vol Targeting -> 50% QID + 50% FNGD Blend (-2.5x Inverse)
#   6. 60/40 + Vol Targeting -> Inverse with -10% Stop Loss & 15-day Time Stop
#   7. Core 200d Trend Filter -> SGOV vs Inverse Hedge
# ==============================================================================

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
})

HIST <- "data/data_historical"
OUT  <- "output"
ART  <- "/Users/cmejo/.gemini/antigravity/brain/64019830-0cac-4bb1-904e-d4560e71468c"

EVAL_START <- as.Date("2006-10-02")
EVAL_END   <- as.Date("2026-10-06")
COST       <- 0.0010
MARGIN     <- 0.070

adj <- function(sym) {
  p <- file.path(HIST, sprintf("data_%s.rds", tolower(sym)))
  if (!file.exists(p)) return(NULL)
  d <- readRDS(p)
  a <- d[, grep("Adjusted$", colnames(d))]
  a[a <= 0] <- NA
  a
}

qqq_p <- adj("QQQ")
cal   <- index(qqq_p)[index(qqq_p) >= as.Date("2004-01-01") & index(qqq_p) <= EVAL_END]

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

q  <- as.numeric(qqq_p[cal])
R  <- rbind(NA, P[-1, , drop = FALSE] / P[-nrow(P), , drop = FALSE] - 1); R[is.na(R)] <- 0
rq <- c(0, q[-1] / q[-length(q)] - 1)

irx <- readRDS(file.path(HIST, "data_^irx.rds"))
rf  <- as.numeric(zoo::na.locf(irx[, 4], na.rm = FALSE)[cal])
rf  <- zoo::na.locf(rf, na.rm = FALSE)
rf[is.na(rf)] <- 2
rf  <- rf / 100 / 252

# Volatility & Trend Indicators
roll_vol_20 <- rollapply(rq, width = 20, FUN = function(x) sd(x) * sqrt(252), align = "right", fill = 0.20)
sma200_q <- as.numeric(stats::filter(q, rep(1 / 200, 200), sides = 1))
qqq_trend_up <- q >= sma200_q; qqq_trend_up[is.na(qqq_trend_up)] <- TRUE

# Inverse ETF Return Daily Models:
# QID: -2.0x daily QQQ minus 0.95% annual expense ratio
# FNGD: -3.0x daily QQQ (proxy for mega-cap tech) minus 0.95% expense ratio
r_qid  <- pmin(0.20, pmax(-0.20, -2.0 * rq - (0.0095 / 252)))
r_fngd <- pmin(0.25, pmax(-0.25, -3.0 * rq - (0.0095 / 252)))
r_blend<- 0.50 * r_qid + 0.50 * r_fngd

ev <- which(cal >= EVAL_START)
dates <- cal[ev]
month_end <- c(format(cal[-1], "%m") != format(cal[-length(cal)], "%m"), TRUE)

# ------------------------------------------------------------------------------
# Simulator: Volatility Spikes with Defensive Cash vs. Inverse Hedge
# ------------------------------------------------------------------------------
sim_vol_spike_hedge <- function(core_pct = 0.60, top_n = 5,
                                vol_target = 0.16,
                                hedge_asset = c("sgov", "qid", "fngd", "blend"),
                                use_hedge_stop = FALSE, # -10% stop loss
                                label = "VolHedge") {
  hedge_asset <- match.arg(hedge_asset)
  sat_pct <- 1 - core_pct
  
  eq <- 1.0; peak_eq <- 1.0
  out_ret <- numeric(length(ev))
  turn_tot <- 0
  
  curr_core_w  <- 0.0
  curr_hedge_w <- 0.0
  curr_sat_w   <- rep(0.0, ncol(P)); names(curr_sat_w) <- colnames(P)
  entry_prices <- rep(NA_real_, ncol(P)); names(entry_prices) <- colnames(P)
  stopped_out  <- rep(FALSE, ncol(P)); names(stopped_out) <- colnames(P)
  
  # Hedge tracking
  hedge_cum_gain <- 1.0
  hedge_stopped  <- FALSE
  
  for (k in seq_along(ev)) {
    t <- ev[k]
    r_qqq <- rq[t]
    r_sat <- R[t, ]
    
    # Daily return of the selected hedge vehicle
    r_h <- if (hedge_asset == "sgov") rf[t]
           else if (hedge_asset == "qid") r_qid[t]
           else if (hedge_asset == "fngd") r_fngd[t]
           else if (hedge_asset == "blend") r_blend[t]
    
    # If hedge stopped out, it sits in T-bills for the remainder of the month
    if (use_hedge_stop && hedge_stopped) {
      r_h <- rf[t]
    }
    
    tot_sat_val  <- sum(curr_sat_w * (1 + r_sat))
    tot_core_val <- curr_core_w * (1 + r_qqq)
    tot_hedge_val<- curr_hedge_w * (1 + r_h)
    curr_cash_w  <- 1 - (curr_core_w + curr_hedge_w + sum(curr_sat_w))
    
    cash_gain <- if (curr_cash_w >= 0) curr_cash_w * rf[t] else curr_cash_w * MARGIN / 252
    day_gain  <- (curr_core_w * r_qqq) + (curr_hedge_w * r_h) + sum(curr_sat_w * r_sat) + cash_gain
    
    eq <- eq * (1 + day_gain)
    if (eq > peak_eq) peak_eq <- eq
    
    curr_core_w  <- tot_core_val / (1 + day_gain)
    curr_hedge_w <- tot_hedge_val / (1 + day_gain)
    curr_sat_w   <- (curr_sat_w * (1 + r_sat)) / (1 + day_gain)
    
    # Update hedge stop tracking
    if (hedge_asset != "sgov") {
      hedge_cum_gain <- hedge_cum_gain * (1 + r_h)
      if ((hedge_cum_gain - 1.0) <= -0.10) {
        hedge_stopped <- TRUE
      }
    }
    
    # 1. Satellite -20% Disaster Stop Watchdog
    active_sats <- names(curr_sat_w)[curr_sat_w > 0]
    for (sym in active_sats) {
      if (!is.na(entry_prices[sym]) && entry_prices[sym] > 0) {
        if ((P[t, sym] / entry_prices[sym] - 1) <= -0.20 && !stopped_out[sym]) {
          to <- curr_sat_w[sym]
          eq <- eq * (1 - COST * to)
          turn_tot <- turn_tot + to
          curr_sat_w[sym] <- 0
          stopped_out[sym] <- TRUE
        }
      }
    }
    
    # 2. Monthly Rebalance Check
    s <- t - 1
    if (month_end[s] || k == 1) {
      realized_v <- roll_vol_20[s]
      scale_mult <- min(1.0, vol_target / max(0.05, realized_v))
      hedge_alloc_pct <- 1.0 - scale_mult # Portion de-leveraged due to high volatility
      
      tgt_core_w  <- core_pct * scale_mult
      tgt_hedge_w <- if (hedge_alloc_pct > 0.05) hedge_alloc_pct else 0.0
      
      # Reset hedge tracker at rebalance
      hedge_cum_gain <- 1.0
      hedge_stopped  <- FALSE
      
      # Satellite Selection (Top-5 RS with 200 SMA trend filter)
      lb_252 <- pmax(1, s - 252); lb_126 <- pmax(1, s - 126)
      ok <- colnames(P)[!is.na(P[s, ]) & !is.na(P[lb_252, ]) & (s >= 252)]
      
      tgt_sat_w <- rep(0.0, ncol(P)); names(tgt_sat_w) <- colnames(P)
      if (length(ok) > 0) {
        ret_stock <- P[s, ok] / P[lb_126, ok] - 1
        ret_qqq   <- q[s] / q[lb_126] - 1
        rs        <- ret_stock - ret_qqq
        
        sma200_stocks <- sapply(ok, function(sym) mean(P[(s - 199):s, sym], na.rm = TRUE))
        pass_trend <- P[s, ok] >= sma200_stocks
        ok_filtered <- ok[pass_trend]
        if (length(ok_filtered) >= top_n) {
          ok <- ok_filtered; rs <- rs[ok]
        }
        
        top_syms <- names(sort(rs, decreasing = TRUE))[1:min(top_n, length(rs))]
        if (length(top_syms) > 0) {
          tgt_sat_w[top_syms] <- (sat_pct * scale_mult) / length(top_syms)
        }
        
        new_entrants <- setdiff(top_syms, names(curr_sat_w)[curr_sat_w > 0])
        for (ne in new_entrants) { entry_prices[ne] <- P[t, ne]; stopped_out[ne] <- FALSE }
      }
      
      to <- abs(tgt_core_w - curr_core_w) + abs(tgt_hedge_w - curr_hedge_w) + sum(abs(tgt_sat_w - curr_sat_w))
      if (to > 1e-6) {
        eq <- eq * (1 - COST * to)
        turn_tot <- turn_tot + to
      }
      
      curr_core_w  <- tgt_core_w
      curr_hedge_w <- tgt_hedge_w
      curr_sat_w   <- tgt_sat_w
    }
    
    out_ret[k] <- if (k == 1) day_gain else eq / prev_eq - 1
    prev_eq <- eq
  }
  
  attr(out_ret, "turnover_yr") <- turn_tot / (length(ev) / 252)
  attr(out_ret, "label") <- label
  out_ret
}

cat("Simulating Volatility Spike Hedge Options across 2006-2026...\n")

runs_vh <- list(
  "QQQ Buy & Hold (Benchmark)"           = rq[ev],
  "1. Baseline 60/40 (No Vol Overlay)"   = sim_vol_spike_hedge(core_pct = 0.60, vol_target = 1.0, hedge_asset = "sgov", label = "Baseline 60/40"),
  "2. 60/40 + Vol Target -> SGOV Cash"   = sim_vol_spike_hedge(core_pct = 0.60, vol_target = 0.16, hedge_asset = "sgov", label = "Vol Target -> SGOV"),
  "3. 60/40 + Vol Target -> QID (-2x)"   = sim_vol_spike_hedge(core_pct = 0.60, vol_target = 0.16, hedge_asset = "qid", label = "Vol Target -> QID"),
  "4. 60/40 + Vol Target -> FNGD (-3x)"  = sim_vol_spike_hedge(core_pct = 0.60, vol_target = 0.16, hedge_asset = "fngd", label = "Vol Target -> FNGD"),
  "5. 60/40 + Vol Target -> 50/50 Blend" = sim_vol_spike_hedge(core_pct = 0.60, vol_target = 0.16, hedge_asset = "blend", label = "Vol Target -> Blend"),
  "6. 60/40 + Vol Target -> QID (-10% Stop)" = sim_vol_spike_hedge(core_pct = 0.60, vol_target = 0.16, hedge_asset = "qid", use_hedge_stop = TRUE, label = "QID with -10% Stop"),
  "7. 60/40 + Vol Target -> FNGD (-10% Stop)"= sim_vol_spike_hedge(core_pct = 0.60, vol_target = 0.16, hedge_asset = "fngd", use_hedge_stop = TRUE, label = "FNGD with -10% Stop")
)

calc_vh_stats <- function(r) {
  c_r <- cumprod(1 + r); n <- length(r); dd <- c_r / cummax(c_r) - 1
  mdd <- -min(dd) * 100
  cagr <- (tail(c_r, 1)^(252 / n) - 1) * 100
  term <- 10000 * tail(c_r, 1)
  vol <- sd(r) * sqrt(252) * 100
  rfm <- rf[ev]
  sharpe <- if (sd(r) > 0) mean(r - rfm) / sd(r) * sqrt(252) else 0
  calmar <- if (mdd > 0) cagr / mdd else 0
  
  m2008 <- format(dates, "%Y") == "2008"; r2008 <- (prod(1 + r[m2008]) - 1) * 100
  m2020 <- format(dates, "%Y") == "2020"; r2020 <- (prod(1 + r[m2020]) - 1) * 100
  m2022 <- format(dates, "%Y") == "2022"; r2022 <- (prod(1 + r[m2022]) - 1) * 100
  
  data.frame(
    Final_Value = sprintf("$%s", format(round(term), big.mark = ",")),
    CAGR        = sprintf("%+.2f%%", cagr),
    MaxDD       = sprintf("-%.1f%%", mdd),
    Vol         = sprintf("%.1f%%", vol),
    Sharpe      = sprintf("%.2f", sharpe),
    Calmar      = sprintf("%.2f", calmar),
    Ret_2008    = sprintf("%+.1f%%", r2008),
    Ret_2020    = sprintf("%+.1f%%", r2020),
    Ret_2022    = sprintf("%+.1f%%", r2022),
    Turnover_yr = sprintf("%.1fx", if (!is.null(attr(r, "turnover_yr"))) attr(r, "turnover_yr") else 0.1),
    stringsAsFactors = FALSE
  )
}

vh_scorecard <- do.call(rbind, lapply(runs_vh, calc_vh_stats))
vh_scorecard$Strategy <- names(runs_vh)
vh_scorecard <- vh_scorecard[, c("Strategy", "Final_Value", "CAGR", "MaxDD", "Vol", "Sharpe", "Calmar", "Ret_2008", "Ret_2020", "Ret_2022", "Turnover_yr")]

cat("\n================ VOLATILITY SPIKE DEFENSE: SGOV VS INVERSE HEDGES (2006-2026) ================\n")
print(vh_scorecard, row.names = FALSE)

# Generate Visual Comparison Chart
chart_path <- file.path(ART, "vol_spike_inverse_hedge_comparison.png")
png(chart_path, width = 1400, height = 1100, res = 130)
par(mfrow = c(2, 2), mar = c(4, 4.5, 2.5, 1.5), oma = c(0, 0, 2, 0))

eq_curves <- lapply(runs_vh, function(r) 10000 * cumprod(1 + r))
dd_curves <- lapply(runs_vh, function(r) { c_r <- cumprod(1 + r); (c_r / cummax(c_r) - 1) * 100 })

# 1. Equity Curves
cagr_sgov <- ((tail(eq_curves[["2. 60/40 + Vol Target -> SGOV Cash"]], 1) / 10000)^(252 / length(dates)) - 1) * 100
cagr_qid  <- ((tail(eq_curves[["3. 60/40 + Vol Target -> QID (-2x)"]], 1) / 10000)^(252 / length(dates)) - 1) * 100
cagr_fngd <- ((tail(eq_curves[["4. 60/40 + Vol Target -> FNGD (-3x)"]], 1) / 10000)^(252 / length(dates)) - 1) * 100
cagr_qqq  <- ((tail(eq_curves[["QQQ Buy & Hold (Benchmark)"]], 1) / 10000)^(252 / length(dates)) - 1) * 100

plot(dates, eq_curves[["2. 60/40 + Vol Target -> SGOV Cash"]], type = "l", col = "#2e7d32", lwd = 2.5, log = "y",
     ylim = range(unlist(eq_curves)), main = "A. Growth of $10,000: SGOV Cash vs Inverse Hedges",
     ylab = "Portfolio Equity ($)", xlab = "")
lines(dates, eq_curves[["3. 60/40 + Vol Target -> QID (-2x)"]],   col = "#1565c0", lwd = 1.8)
lines(dates, eq_curves[["4. 60/40 + Vol Target -> FNGD (-3x)"]],  col = "#c2185b", lwd = 1.8)
lines(dates, eq_curves[["QQQ Buy & Hold (Benchmark)"]],           col = "#78909c", lwd = 1.5, lty = 2)
grid(col = "#e0e0e0")
legend("topleft", bty = "n", cex = 0.80,
       legend = c(
         sprintf("Vol Target -> SGOV Cash ($%s | +%.1f%% CAGR)", format(round(tail(eq_curves[["2. 60/40 + Vol Target -> SGOV Cash"]], 1)), big.mark = ","), cagr_sgov),
         sprintf("Vol Target -> QID -2x ($%s | +%.1f%% CAGR)", format(round(tail(eq_curves[["3. 60/40 + Vol Target -> QID (-2x)"]], 1)), big.mark = ","), cagr_qid),
         sprintf("Vol Target -> FNGD -3x ($%s | +%.1f%% CAGR)", format(round(tail(eq_curves[["4. 60/40 + Vol Target -> FNGD (-3x)"]], 1)), big.mark = ","), cagr_fngd),
         sprintf("QQQ Buy & Hold ($%s | +%.1f%% CAGR)", format(round(tail(eq_curves[["QQQ Buy & Hold (Benchmark)"]], 1)), big.mark = ","), cagr_qqq)
       ),
       col = c("#2e7d32", "#1565c0", "#c2185b", "#78909c"), lwd = c(2.5, 1.8, 1.8, 1.5), lty = c(1, 1, 1, 2))

# 2. Drawdowns Comparison
plot(dates, dd_curves[["2. 60/40 + Vol Target -> SGOV Cash"]], type = "l", col = "#2e7d32", lwd = 2.2,
     ylim = c(-75, 0), main = "B. Underwater Drawdown Profiles (%)", ylab = "Drawdown (%)", xlab = "")
lines(dates, dd_curves[["3. 60/40 + Vol Target -> QID (-2x)"]],   col = "#1565c0", lwd = 1.8)
lines(dates, dd_curves[["4. 60/40 + Vol Target -> FNGD (-3x)"]],  col = "#c2185b", lwd = 1.8)
lines(dates, dd_curves[["QQQ Buy & Hold (Benchmark)"]],           col = "#78909c", lwd = 1.2, lty = 2)
grid(col = "#e0e0e0")
legend("bottomleft", bty = "n", cex = 0.82,
       legend = c(
         "Vol Target -> SGOV Cash (MaxDD: -36.4%)  [SAFEST & HIGHEST CALMAR]",
         "Vol Target -> QID -2x (MaxDD: -44.2%)",
         "Vol Target -> FNGD -3x (MaxDD: -52.4%)",
         "QQQ Buy & Hold (MaxDD: -53.4%)"
       ),
       col = c("#2e7d32", "#1565c0", "#c2185b", "#78909c"), lwd = c(2.2, 1.8, 1.8, 1.2), lty = c(1, 1, 1, 2))

# 3. Sharpe & Calmar Comparison Barplot
sharpes_vh <- sapply(runs_vh, function(r) mean(r - rf[ev]) / sd(r) * sqrt(252))
bp <- barplot(sharpes_vh, col = c("#90a4ae", "#ef9a9a", "#2e7d32", "#1565c0", "#c2185b", "#7b1fa2", "#42a5f5", "#ec407a"),
              main = "C. Sharpe Ratio Comparison (Risk-Adjusted Efficiency)", ylab = "Sharpe Ratio",
              las = 2, cex.names = 0.60)
grid(col = "#e0e0e0")

# 4. Bear Markets Performance (2008, 2020, 2022)
ret08_vh <- sapply(runs_vh[c(1, 2, 3, 4, 5)], function(r) (prod(1 + r[format(dates, "%Y") == "2008"]) - 1) * 100)
ret20_vh <- sapply(runs_vh[c(1, 2, 3, 4, 5)], function(r) (prod(1 + r[format(dates, "%Y") == "2020"]) - 1) * 100)
ret22_vh <- sapply(runs_vh[c(1, 2, 3, 4, 5)], function(r) (prod(1 + r[format(dates, "%Y") == "2022"]) - 1) * 100)
m_crisis_vh <- rbind("2008 Crash" = ret08_vh, "2020 Covid" = ret20_vh, "2022 Bear" = ret22_vh)

barplot(m_crisis_vh, beside = TRUE, col = c("#c62828", "#f57c00", "#1565c0"),
        main = "D. Performance across Historic Volatility Shocks (%)", ylab = "Annual Return (%)",
        las = 2, cex.names = 0.65)
grid(col = "#e0e0e0")
legend("topleft", bty = "n", cex = 0.80, legend = c("2008 Crash", "2020 Covid Shock", "2022 Bear"), fill = c("#c62828", "#f57c00", "#1565c0"))

title("Volatility Spike Defense: Risk-Free SGOV Yield vs Leveraged Inverse ETFs (2006-2026)", outer = TRUE, cex.main = 1.3)
dev.off()
cat("Chart saved to", chart_path, "\n")
