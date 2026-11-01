#!/usr/bin/env Rscript
# ==============================================================================
# scratch/test_defensive_sleeve_alternatives.R
# Comparing Alternatives to the 30% SGOV Defensive Sleeve to Increase CAGR
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

# Load Gold (GLD)
gld_p <- adj("GLD_FULL")
m_gld <- match(cal, as.Date(index(gld_p)))
gld_px <- as.numeric(gld_p)[m_gld]
gld_px <- zoo::na.locf(gld_px, na.rm = FALSE)
r_gld  <- c(0, gld_px[-1] / gld_px[-length(gld_px)] - 1); r_gld[is.na(r_gld)] <- 0
sma200_gld <- as.numeric(stats::filter(gld_px, rep(1 / 200, 200), sides = 1))
gld_trend_up <- gld_px >= sma200_gld; gld_trend_up[is.na(gld_trend_up)] <- TRUE

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

roll_vol_20  <- rollapply(rq, width = 20, FUN = function(x) sd(x) * sqrt(252), align = "right", fill = 0.20)
sma200_q     <- as.numeric(stats::filter(q, rep(1 / 200, 200), sides = 1))
qqq_trend_up <- q >= sma200_q; qqq_trend_up[is.na(qqq_trend_up)] <- TRUE

ev <- which(cal >= EVAL_START)
dates <- cal[ev]
month_end <- c(format(cal[-1], "%m") != format(cal[-length(cal)], "%m"), TRUE)

# Simulation Engine with Alternative Defensive Assets & Volatility Rules
sim_defensive_alternative <- function(core_pct = 0.30, top_n = 5,
                                      vol_target = 0.16,
                                      defensive_asset = c("sgov", "gld", "gld_trend_sgov", "none"),
                                      asymmetric_vol_target = FALSE, # Only vol-target when QQQ < 200 SMA
                                      core_bull_leverage = 1.25,
                                      disaster_stop = 0.20,
                                      label = "Test") {
  defensive_asset <- match.arg(defensive_asset)
  sat_pct <- 1 - core_pct
  
  eq <- 1.0; peak_eq <- 1.0
  out_ret <- numeric(length(ev))
  turn_tot <- 0
  
  curr_core_w  <- 0.0
  curr_sat_w   <- rep(0.0, ncol(P)); names(curr_sat_w) <- colnames(P)
  curr_def_w   <- 0.0 # Weight of defensive asset (SGOV or GLD)
  curr_def_sym <- "SGOV"
  
  entry_prices <- rep(NA_real_, ncol(P)); names(entry_prices) <- colnames(P)
  stopped_out  <- rep(FALSE, ncol(P)); names(stopped_out) <- colnames(P)
  
  prev_eq <- eq
  
  for (k in seq_along(ev)) {
    t <- ev[k]
    r_qqq <- rq[t]
    r_sat <- R[t, ]
    
    # Return of defensive asset for the day
    r_def <- if (curr_def_sym == "GLD") r_gld[t] else rf[t]
    
    tot_sat_val  <- sum(curr_sat_w * (1 + r_sat))
    tot_core_val <- curr_core_w * (1 + r_qqq)
    tot_def_val  <- curr_def_w * (1 + r_def)
    curr_cash_w  <- 1 - (curr_core_w + sum(curr_sat_w) + curr_def_w)
    
    cash_gain <- if (curr_cash_w >= 0) curr_cash_w * rf[t] else curr_cash_w * MARGIN / 252
    day_gain  <- (curr_core_w * r_qqq) + sum(curr_sat_w * r_sat) + (curr_def_w * r_def) + cash_gain
    
    eq <- eq * (1 + day_gain)
    if (eq > peak_eq) peak_eq <- eq
    
    curr_core_w <- tot_core_val / (1 + day_gain)
    curr_sat_w  <- (curr_sat_w * (1 + r_sat)) / (1 + day_gain)
    curr_def_w  <- tot_def_val / (1 + day_gain)
    
    # Satellite Disaster Stops (-20%)
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
    
    # Monthly Rebalance Check
    s <- t - 1
    if (month_end[s] || k == 1) {
      realized_v <- roll_vol_20[s]
      
      # Determine scale factor
      scale_mult <- 1.0
      if (defensive_asset != "none" && vol_target > 0) {
        if (!asymmetric_vol_target) {
          # Symmetric: scale down whenever vol > vol_target
          scale_mult <- min(1.0, vol_target / max(0.05, realized_v))
        } else {
          # Asymmetric: Only scale down if QQQ is in a breakdown / bear market (QQQ < 200 SMA)
          if (!isTRUE(qqq_trend_up[s])) {
            scale_mult <- min(1.0, vol_target / max(0.05, realized_v))
          } else {
            scale_mult <- 1.0 # 100% invested during bull uptrends!
          }
        }
      }
      
      # Core trend rule
      core_active <- isTRUE(qqq_trend_up[s])
      core_lev <- 1.0
      if (core_active && core_bull_leverage > 1.0 && isTRUE(qqq_trend_up[s]) && realized_v <= 0.16) {
        core_lev <- core_bull_leverage
      }
      
      tgt_core_w <- if (core_active) core_pct * scale_mult * core_lev else 0.0
      
      # Satellite Selection
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
        for (ne in new_entrants) {
          entry_prices[ne] <- P[t, ne]
          stopped_out[ne]  <- FALSE
        }
      }
      
      # Determine Defensive Sleeve Allocation
      tot_equity_alloc <- tgt_core_w + sum(tgt_sat_w)
      tgt_def_w <- max(0, 1.0 - tot_equity_alloc)
      
      # Determine which defensive asset to hold
      if (defensive_asset == "none") {
        tgt_def_w <- 0.0
        curr_def_sym <- "CASH"
      } else if (defensive_asset == "sgov") {
        curr_def_sym <- "SGOV"
      } else if (defensive_asset == "gld") {
        curr_def_sym <- "GLD"
      } else if (defensive_asset == "gld_trend_sgov") {
        curr_def_sym <- if (isTRUE(gld_trend_up[s])) "GLD" else "SGOV"
      }
      
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

cat("Simulating Defensive Sleeve Alternatives across 2006-2026...\n")

runs <- list(
  "QQQ Buy & Hold (Benchmark)"                   = rq[ev],
  "1. Baseline: 30/70 + 16% VT to SGOV"          = sim_defensive_alternative(core_pct = 0.30, vol_target = 0.16, defensive_asset = "sgov", label = "SGOV 16% VT"),
  "2. Gold Alternative: 30/70 + 16% VT to GLD"   = sim_defensive_alternative(core_pct = 0.30, vol_target = 0.16, defensive_asset = "gld", label = "GLD 16% VT"),
  "3. Dual Trend: 30/70 + 16% VT to Trend GLD/SGOV" = sim_defensive_alternative(core_pct = 0.30, vol_target = 0.16, defensive_asset = "gld_trend_sgov", label = "Trend GLD/SGOV"),
  "4. Asymmetric VT: 100% Equities in Bull, SGOV in Bear" = sim_defensive_alternative(core_pct = 0.30, vol_target = 0.16, defensive_asset = "sgov", asymmetric_vol_target = TRUE, label = "Asymmetric VT"),
  "5. Relaxed Vol Target: 20% VT to SGOV (Less Drag)" = sim_defensive_alternative(core_pct = 0.30, vol_target = 0.20, defensive_asset = "sgov", label = "20% VT SGOV"),
  "6. Relaxed Vol Target: 22% VT to SGOV"        = sim_defensive_alternative(core_pct = 0.30, vol_target = 0.22, defensive_asset = "sgov", label = "22% VT SGOV"),
  "7. Pure Momentum: 100% Equities (0% SGOV / No VT)" = sim_defensive_alternative(core_pct = 0.30, defensive_asset = "none", label = "100% Equities (No VT)")
)

calc_metrics <- function(r) {
  c_r <- cumprod(1 + r); n <- length(r); dd <- c_r / cummax(c_r) - 1
  mdd <- -min(dd) * 100
  cagr <- (tail(c_r, 1)^(252 / n) - 1) * 100
  term <- 8000 * tail(c_r, 1)
  vol <- sd(r) * sqrt(252) * 100
  rfm <- rf[ev]
  sharpe <- if (sd(r) > 0) mean(r - rfm) / sd(r) * sqrt(252) else 0
  sortino <- {
    d_ret <- pmin(r - rfm, 0)
    d_sd  <- sqrt(mean(d_ret^2)) * sqrt(252)
    if (d_sd > 0) mean(r - rfm) * 252 / d_sd else 0
  }
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
    Sortino     = sprintf("%.2f", sortino),
    Calmar      = sprintf("%.2f", calmar),
    Ret_2008    = sprintf("%+.1f%%", r2008),
    Ret_2020    = sprintf("%+.1f%%", r2020),
    Ret_2022    = sprintf("%+.1f%%", r2022),
    Turnover_yr = sprintf("%.1fx", if (!is.null(attr(r, "turnover_yr"))) attr(r, "turnover_yr") else 0.1),
    stringsAsFactors = FALSE
  )
}

scorecard <- do.call(rbind, lapply(runs, calc_metrics))
scorecard$Configuration <- names(runs)
scorecard <- scorecard[, c("Configuration", "Final_Value", "CAGR", "MaxDD", "Vol", "Sharpe", "Sortino", "Calmar", "Ret_2008", "Ret_2020", "Ret_2022", "Turnover_yr")]

cat("\n================ DEFENSIVE SLEEVE ALTERNATIVES SCORECARD (2006-2026) ================\n")
print(scorecard, row.names = FALSE)

# Generate Visual Comparison Chart
chart_path <- file.path(ART, "defensive_sleeve_alternatives_chart.png")
png(chart_path, width = 1400, height = 1100, res = 130)
par(mfrow = c(2, 2), mar = c(4, 4.5, 2.5, 1.5), oma = c(0, 0, 2, 0))

eq_curves <- lapply(runs, function(r) 8000 * cumprod(1 + r))
dd_curves <- lapply(runs, function(r) { c_r <- cumprod(1 + r); (c_r / cummax(c_r) - 1) * 100 })
cols <- c("#78909c", "#1565c0", "#f57f17", "#2e7d32", "#c2185b", "#7b1fa2", "#00838f", "#d84315")

# 1. Equity Curves (Log Scale)
plot(dates, eq_curves[[2]], type = "l", col = cols[2], lwd = 2.0, log = "y",
     ylim = range(unlist(eq_curves)), main = "A. Growth of $8,000 (Log Scale, 2006-2026)",
     ylab = "Portfolio Equity ($)", xlab = "")
for (i in 3:length(eq_curves)) {
  lines(dates, eq_curves[[i]], col = cols[i], lwd = if (i %in% c(4, 5, 6)) 2.4 else 1.8)
}
lines(dates, eq_curves[[1]], col = cols[1], lwd = 1.4, lty = 2)
grid(col = "#e0e0e0")
legend("topleft", bty = "n", cex = 0.72,
       legend = sapply(2:length(runs), function(i) {
         sprintf("%s ($%s | %s CAGR)", names(runs)[i],
                 format(round(tail(eq_curves[[i]], 1)), big.mark = ","),
                 scorecard$CAGR[i])
       }),
       col = cols[2:length(runs)], lwd = 2.0)

# 2. Drawdowns Comparison
plot(dates, dd_curves[[2]], type = "l", col = cols[2], lwd = 2.0,
     ylim = c(-60, 0), main = "B. Underwater Drawdown Comparison (%)",
     ylab = "Drawdown (%)", xlab = "")
for (i in 3:length(dd_curves)) {
  lines(dates, dd_curves[[i]], col = cols[i], lwd = if (i %in% c(4, 5, 6)) 2.2 else 1.6)
}
lines(dates, dd_curves[[1]], col = cols[1], lwd = 1.2, lty = 2)
grid(col = "#e0e0e0")
legend("bottomleft", bty = "n", cex = 0.72,
       legend = sapply(2:length(runs), function(i) {
         sprintf("%s (MaxDD: %s)", names(runs)[i], scorecard$MaxDD[i])
       }),
       col = cols[2:length(runs)], lwd = 2.0)

# 3. Bar Chart: Pre-Tax CAGR (%)
cagr_vals <- as.numeric(sub("%", "", scorecard$CAGR))
b <- barplot(cagr_vals, names.arg = FALSE, col = cols,
             main = "C. 20-Year Compound Annual Growth Rate (CAGR)",
             ylab = "CAGR (%)", ylim = c(0, max(cagr_vals) * 1.15))
grid(nx = NA, ny = NULL, col = "#e0e0e0")
text(b, cagr_vals + 1.2, sprintf("%.1f%%", cagr_vals), cex = 0.8, font = 2)
axis(1, at = b, labels = c("QQQ", "SGOV 16%", "GLD 16%", "Trend GLD", "Asymm VT", "20% VT", "22% VT", "No VT"), cex.axis = 0.72, las = 2)

# 4. Risk-Return Scatter (Calmar vs CAGR)
calmar_vals <- as.numeric(scorecard$Calmar)
plot(cagr_vals, calmar_vals, pch = 19, col = cols, cex = 2.2,
     main = "D. Risk-Adjusted Quality: Calmar vs CAGR",
     xlab = "CAGR (%)", ylab = "Calmar Ratio (CAGR / MaxDD)",
     xlim = c(min(cagr_vals) - 2, max(cagr_vals) + 3),
     ylim = c(min(calmar_vals) - 0.1, max(calmar_vals) + 0.15))
grid(col = "#e0e0e0")
text(cagr_vals, calmar_vals + 0.04, c("QQQ", "1. SGOV 16%", "2. GLD 16%", "3. Trend GLD", "4. Asymm VT", "5. 20% VT", "6. 22% VT", "7. No VT"),
     cex = 0.75, pos = 3)

dev.off()
cat(sprintf("Chart saved to: %s\n", chart_path))
