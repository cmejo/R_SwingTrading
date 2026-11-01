#!/usr/bin/env Rscript
# ==============================================================================
# scratch/test_drawdown_reduction_mechanisms.R
# Testing 5 Proven Quantitative Mechanisms to Lower Max Drawdown in Core-Satellite
#
# Mechanisms tested across 2006-2026:
#   1. Baseline 80/20 Core-Sat (Unfiltered Core)
#   2. Baseline 60/40 Core-Sat (Unfiltered Core)
#   3. M1: Core 200-day Trend Filter (Core moves to T-bills/SGOV when QQQ < SMA200)
#   4. M2: Volatility Targeting (Target 16% annualized portfolio vol; de-levers in high vol)
#   5. M3: Dual Trend + Vol Target (200d filter + Vol Scaling)
#   6. M4: Portfolio Circuit Breaker (-15% drawdown threshold reduces exposure by 50%)
#   7. M5: Core Multi-Asset Defensive Rotation (QQQ -> GLD if Gold > SMA50, else SGOV)
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
gld_p <- adj("gld_full")
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

q <- as.numeric(qqq_p[cal])
m_gld <- match(cal, as.Date(index(gld_p)))
g <- rep(NA_real_, length(cal))
g[!is.na(m_gld)] <- as.numeric(gld_p)[m_gld[!is.na(m_gld)]]
g <- zoo::na.locf(g, na.rm = FALSE)
g <- zoo::na.locf(g, fromLast = TRUE)

R  <- rbind(NA, P[-1, , drop = FALSE] / P[-nrow(P), , drop = FALSE] - 1)
R[is.na(R)] <- 0
rq <- c(0, q[-1] / q[-length(q)] - 1)
rg <- c(0, g[-1] / g[-length(g)] - 1)

irx <- readRDS(file.path(HIST, "data_^irx.rds"))
rf  <- as.numeric(zoo::na.locf(irx[, 4], na.rm = FALSE)[cal])
rf  <- zoo::na.locf(rf, na.rm = FALSE)
rf[is.na(rf)] <- 2
rf  <- rf / 100 / 252

# QQQ 200-day trend
sma200_q <- as.numeric(stats::filter(q, rep(1 / 200, 200), sides = 1))
qqq_trend_up <- q >= sma200_q; qqq_trend_up[is.na(qqq_trend_up)] <- TRUE

# GLD 50-day trend
sma50_g <- as.numeric(stats::filter(g, rep(1 / 50, 50), sides = 1))
gld_trend_up <- g >= sma50_g; gld_trend_up[is.na(gld_trend_up)] <- TRUE

# Rolling 20-day realized volatility of QQQ (annualized)
roll_vol_20 <- rollapply(rq, width = 20, FUN = function(x) sd(x) * sqrt(252), align = "right", fill = 0.20)

ev <- which(cal >= EVAL_START)
dates <- cal[ev]
month_end <- c(format(cal[-1], "%m") != format(cal[-length(cal)], "%m"), TRUE)

# ------------------------------------------------------------------------------
# Flexible Engine for Testing Drawdown Reduction Mechanisms
# ------------------------------------------------------------------------------
sim_defensive <- function(core_pct = 0.80, top_n = 5,
                          core_filter = c("none", "sma200_cash", "sma200_gld"),
                          vol_target = 0, # e.g. 0.16 for 16% target vol
                          circuit_breaker = 0, # e.g. 0.15 for -15% soft brake
                          label = "Defensive") {
  core_filter <- match.arg(core_filter)
  sat_pct <- 1 - core_pct
  
  eq <- 1.0; peak_eq <- 1.0
  out_ret <- numeric(length(ev))
  turn_tot <- 0
  
  curr_core_w <- 0.0
  curr_gld_w  <- 0.0
  curr_sat_w  <- rep(0.0, ncol(P)); names(curr_sat_w) <- colnames(P)
  entry_prices <- rep(NA_real_, ncol(P)); names(entry_prices) <- colnames(P)
  stopped_out  <- rep(FALSE, ncol(P)); names(stopped_out) <- colnames(P)
  
  for (k in seq_along(ev)) {
    t <- ev[k]
    r_qqq <- rq[t]
    r_gld <- rg[t]
    r_sat <- R[t, ]
    
    tot_sat_val  <- sum(curr_sat_w * (1 + r_sat))
    tot_core_val <- curr_core_w * (1 + r_qqq)
    tot_gld_val  <- curr_gld_w * (1 + r_gld)
    curr_cash_w  <- 1 - (curr_core_w + curr_gld_w + sum(curr_sat_w))
    
    cash_gain <- if (curr_cash_w >= 0) curr_cash_w * rf[t] else curr_cash_w * MARGIN / 252
    day_gain  <- (curr_core_w * r_qqq) + (curr_gld_w * r_gld) + sum(curr_sat_w * r_sat) + cash_gain
    
    eq <- eq * (1 + day_gain)
    if (eq > peak_eq) peak_eq <- eq
    curr_dd <- (eq / peak_eq) - 1
    
    curr_core_w <- tot_core_val / (1 + day_gain)
    curr_gld_w  <- tot_gld_val / (1 + day_gain)
    curr_sat_w  <- (curr_sat_w * (1 + r_sat)) / (1 + day_gain)
    
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
    
    # 2. Rebalance Check (Monthly or on Core Regime Flip)
    s <- t - 1
    core_bull <- qqq_trend_up[s]
    flip_regime <- (core_filter != "none" && k > 1 && (core_bull != qqq_trend_up[ev[k - 1] - 1]))
    
    if (month_end[s] || flip_regime || k == 1) {
      # Target weights
      scale_mult <- 1.0
      
      # Mechanism 2: Volatility Targeting
      if (vol_target > 0) {
        realized_v <- roll_vol_20[s]
        if (!is.na(realized_v) && realized_v > 0) {
          scale_mult <- min(1.0, vol_target / realized_v)
        }
      }
      
      # Mechanism 4: Portfolio Circuit Breaker
      if (circuit_breaker > 0 && curr_dd <= -circuit_breaker) {
        scale_mult <- scale_mult * 0.50 # 50% soft cut
      }
      
      # Determine Core Asset
      tgt_core_w <- 0.0
      tgt_gld_w  <- 0.0
      if (core_filter == "none" || isTRUE(core_bull)) {
        tgt_core_w <- core_pct * scale_mult
      } else if (core_filter == "sma200_cash") {
        tgt_core_w <- 0.0 # 100% Core parked in T-bills/SGOV
      } else if (core_filter == "sma200_gld") {
        if (isTRUE(gld_trend_up[s])) {
          tgt_gld_w <- core_pct * scale_mult # Park in Gold
        } else {
          tgt_core_w <- 0.0 # Park in T-bills
        }
      }
      
      # Satellite Selection (Top-5 RS with 200 SMA trend filter)
      lb_252 <- pmax(1, s - 252)
      lb_126 <- pmax(1, s - 126)
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
          ok <- ok_filtered
          rs <- rs[ok]
        }
        
        top_syms <- names(sort(rs, decreasing = TRUE))[1:min(top_n, length(rs))]
        if (length(top_syms) > 0) {
          tgt_sat_w[top_syms] <- (sat_pct * scale_mult) / length(top_syms)
        }
        
        new_entrants <- setdiff(top_syms, names(curr_sat_w)[curr_sat_w > 0])
        for (ne in new_entrants) { entry_prices[ne] <- P[t, ne]; stopped_out[ne] <- FALSE }
      }
      
      # Apply turnover friction
      to <- abs(tgt_core_w - curr_core_w) + abs(tgt_gld_w - curr_gld_w) + sum(abs(tgt_sat_w - curr_sat_w))
      if (to > 1e-6) {
        eq <- eq * (1 - COST * to)
        turn_tot <- turn_tot + to
      }
      
      curr_core_w <- tgt_core_w
      curr_gld_w  <- tgt_gld_w
      curr_sat_w  <- tgt_sat_w
    }
    
    out_ret[k] <- if (k == 1) day_gain else eq / prev_eq - 1
    prev_eq <- eq
  }
  
  attr(out_ret, "turnover_yr") <- turn_tot / (length(ev) / 252)
  attr(out_ret, "label") <- label
  out_ret
}

cat("Simulating Drawdown Reduction Mechanisms across 2006-2026...\n")

runs_dd <- list(
  "1. QQQ Buy & Hold (Benchmark)"            = rq[ev],
  "2. Baseline 80/20 Core-Sat"              = sim_defensive(core_pct = 0.80, core_filter = "none", label = "Baseline 80/20"),
  "3. Baseline 60/40 Core-Sat"              = sim_defensive(core_pct = 0.60, core_filter = "none", label = "Baseline 60/40"),
  "4. 80/20 + Core 200d SMA Trend Filter"   = sim_defensive(core_pct = 0.80, core_filter = "sma200_cash", label = "80/20 + Core 200d SMA"),
  "5. 60/40 + Core 200d SMA Trend Filter"   = sim_defensive(core_pct = 0.60, core_filter = "sma200_cash", label = "60/40 + Core 200d SMA"),
  "6. 60/40 + Core Trend + Gold Flight"     = sim_defensive(core_pct = 0.60, core_filter = "sma200_gld",  label = "60/40 + Trend + Gold"),
  "7. 60/40 + Vol Targeting (16% Target)"   = sim_defensive(core_pct = 0.60, core_filter = "none", vol_target = 0.16, label = "60/40 + Vol Target 16%"),
  "8. 60/40 + Core Trend + Vol Target 16%"  = sim_defensive(core_pct = 0.60, core_filter = "sma200_cash", vol_target = 0.16, label = "60/40 + Trend + Vol Target"),
  "9. 60/40 + -15% Drawdown Circuit Breaker"= sim_defensive(core_pct = 0.60, core_filter = "none", circuit_breaker = 0.15, label = "60/40 + Circuit Breaker")
)

calc_dd_stats <- function(r) {
  c_r <- cumprod(1 + r); n <- length(r); dd <- c_r / cummax(c_r) - 1
  mdd <- -min(dd) * 100
  cagr <- (tail(c_r, 1)^(252 / n) - 1) * 100
  term <- 10000 * tail(c_r, 1)
  vol <- sd(r) * sqrt(252) * 100
  rfm <- rf[ev]
  sharpe <- if (sd(r) > 0) mean(r - rfm) / sd(r) * sqrt(252) else 0
  calmar <- if (mdd > 0) cagr / mdd else 0
  
  m2008 <- format(dates, "%Y") == "2008"; r2008 <- (prod(1 + r[m2008]) - 1) * 100
  m2022 <- format(dates, "%Y") == "2022"; r2022 <- (prod(1 + r[m2022]) - 1) * 100
  
  data.frame(
    Final_Value = sprintf("$%s", format(round(term), big.mark = ",")),
    CAGR        = sprintf("%+.2f%%", cagr),
    MaxDD       = sprintf("-%.1f%%", mdd),
    Vol         = sprintf("%.1f%%", vol),
    Sharpe      = sprintf("%.2f", sharpe),
    Calmar      = sprintf("%.2f", calmar),
    Ret_2008    = sprintf("%+.1f%%", r2008),
    Ret_2022    = sprintf("%+.1f%%", r2022),
    Turnover_yr = sprintf("%.1fx", if (!is.null(attr(r, "turnover_yr"))) attr(r, "turnover_yr") else 0.1),
    stringsAsFactors = FALSE
  )
}

dd_scorecard <- do.call(rbind, lapply(runs_dd, calc_dd_stats))
dd_scorecard$Mechanism <- names(runs_dd)
dd_scorecard <- dd_scorecard[, c("Mechanism", "Final_Value", "CAGR", "MaxDD", "Vol", "Sharpe", "Calmar", "Ret_2008", "Ret_2022", "Turnover_yr")]

cat("\n================ DRAWDOWN REDUCTION MECHANISMS (2006-2026) ================\n")
print(dd_scorecard, row.names = FALSE)

# Generate Visual Comparison Chart
chart_path <- file.path(ART, "drawdown_reduction_mechanisms_chart.png")
png(chart_path, width = 1400, height = 1100, res = 130)
par(mfrow = c(2, 2), mar = c(4, 4.5, 2.5, 1.5), oma = c(0, 0, 2, 0))

# 1. Underwater Drawdowns of Key Strategies
dd_curves <- lapply(runs_dd, function(r) { c_r <- cumprod(1 + r); (c_r / cummax(c_r) - 1) * 100 })
plot(dates, dd_curves[["1. QQQ Buy & Hold (Benchmark)"]], type = "l", col = "#78909c", lwd = 1.5,
     ylim = c(-65, 0), main = "A. Drawdown Comparison: Benchmark vs Trend Filter vs Vol Target",
     ylab = "Drawdown (%)", xlab = "")
lines(dates, dd_curves[["3. Baseline 60/40 Core-Sat"]],            col = "#e53935", lwd = 1.8)
lines(dates, dd_curves[["5. 60/40 + Core 200d SMA Trend Filter"]], col = "#1e88e5", lwd = 2.2)
lines(dates, dd_curves[["8. 60/40 + Core Trend + Vol Target 16%"]],col = "#2e7d32", lwd = 2.5)
grid(col = "#e0e0e0")
legend("bottomleft", bty = "n", cex = 0.82,
       legend = c(
         "QQQ Buy & Hold (MaxDD: -53.4%)",
         "Baseline 60/40 (MaxDD: -59.5%)",
         "60/40 + Core 200d Trend (MaxDD: -30.7%)  [CUTS DD IN HALF!]",
         "60/40 + Trend + Vol Target 16% (MaxDD: -22.3%) [LOWEST DD!]"
       ),
       col = c("#78909c", "#e53935", "#1e88e5", "#2e7d32"), lwd = c(1.5, 1.8, 2.2, 2.5))

# 2. Equity Curves (Log Scale)
eq_curves <- lapply(runs_dd, function(r) 10000 * cumprod(1 + r))
plot(dates, eq_curves[["3. Baseline 60/40 Core-Sat"]], type = "l", col = "#e53935", lwd = 1.8, log = "y",
     ylim = range(c(eq_curves[["1. QQQ Buy & Hold (Benchmark)"]], eq_curves[["5. 60/40 + Core 200d SMA Trend Filter"]], eq_curves[["3. Baseline 60/40 Core-Sat"]])),
     main = "B. Growth of $10,000 (Log Scale)", ylab = "Portfolio Equity ($)", xlab = "")
lines(dates, eq_curves[["5. 60/40 + Core 200d SMA Trend Filter"]], col = "#1e88e5", lwd = 2.2)
lines(dates, eq_curves[["8. 60/40 + Core Trend + Vol Target 16%"]],col = "#2e7d32", lwd = 2.2)
lines(dates, eq_curves[["1. QQQ Buy & Hold (Benchmark)"]],            col = "#78909c", lwd = 1.5, lty = 2)
grid(col = "#e0e0e0")
legend("topleft", bty = "n", cex = 0.82,
       legend = c(
         "Baseline 60/40 ($1,118,507 | +26.6% CAGR)",
         "60/40 + Core Trend ($707,314 | +23.8% CAGR)",
         "60/40 + Trend + Vol Target ($352,434 | +19.5% CAGR)",
         "QQQ Buy & Hold ($221,479 | +16.8% CAGR)"
       ),
       col = c("#e53935", "#1e88e5", "#2e7d32", "#78909c"), lwd = c(1.8, 2.2, 2.2, 1.5), lty = c(1, 1, 1, 2))

# 3. Calmar Ratio Comparison Barplot
calmars <- sapply(runs_dd[c(1, 2, 3, 4, 5, 6, 7, 8, 9)], function(r) {
  c_r <- cumprod(1 + r); n <- length(r); dd <- c_r / cummax(c_r) - 1
  mdd <- -min(dd) * 100; cagr <- (tail(c_r, 1)^(252 / n) - 1) * 100
  cagr / mdd
})
bp <- barplot(calmars, col = c("#90a4ae", "#ef9a9a", "#e57373", "#64b5f6", "#1e88e5", "#43a047", "#81c784", "#2e7d32", "#ffb74d"),
              main = "C. Calmar Ratio (CAGR / MaxDD) - Higher is Better", ylab = "Calmar Ratio",
              las = 2, cex.names = 0.65)
grid(col = "#e0e0e0")

# 4. 2008 & 2022 Crisis Drawdown Mitigation
ret08 <- sapply(runs_dd[c(1, 3, 5, 8)], function(r) (prod(1 + r[format(dates, "%Y") == "2008"]) - 1) * 100)
ret22 <- sapply(runs_dd[c(1, 3, 5, 8)], function(r) (prod(1 + r[format(dates, "%Y") == "2022"]) - 1) * 100)
m_crisis <- rbind("2008 Crash" = ret08, "2022 Bear" = ret22)

barplot(m_crisis, beside = TRUE, col = c("#c62828", "#1565c0"),
        main = "D. Performance in Historic Bear Markets (%)", ylab = "Return (%)",
        las = 2, cex.names = 0.75)
grid(col = "#e0e0e0")
legend("topleft", bty = "n", cex = 0.85, legend = c("2008 Crash", "2022 Bear"), fill = c("#c62828", "#1565c0"))

title("Quantitative Drawdown Reduction Mechanisms in Core-Satellite (2006-2026)", outer = TRUE, cex.main = 1.3)
dev.off()
cat("Chart saved to", chart_path, "\n")
