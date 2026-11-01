#!/usr/bin/env Rscript
# ==============================================================================
# scratch/test_novel_alpha_and_dd_features.R
# Testing 5 Advanced Quantitative Features to Increase CAGR & Decrease Drawdown
#
# Candidate Features Tested:
#   1. Baseline 40/60 + 16% Vol Target to SGOV (Control)
#   2. F1: Smooth Momentum / Information Discreteness (Rank by RS / Realized Vol)
#   3. F2: Dynamic Core Bull Leverage (1.25x QQQ Core when QQQ > 200 SMA & Vol < 16%)
#   4. F3: Trailing Chandelier Peak Stop (-15% from 30-day peak price for Satellites)
#   5. F4: Market Breadth Guard (De-lever Satellite when < 40% of universe > 50 SMA)
#   6. F5: Dual Upgrade (F1 Smooth RS + F2 Dynamic Bull Core Leverage)
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

roll_vol_20 <- rollapply(rq, width = 20, FUN = function(x) sd(x) * sqrt(252), align = "right", fill = 0.20)
sma200_q    <- as.numeric(stats::filter(q, rep(1 / 200, 200), sides = 1))
qqq_trend_up<- q >= sma200_q; qqq_trend_up[is.na(qqq_trend_up)] <- TRUE

# Precompute 50-day moving average breadth for universe
P_sma50 <- matrix(NA_real_, nrow(P), ncol(P))
for (j in seq_len(ncol(P))) {
  P_sma50[, j] <- as.numeric(stats::filter(P[, j], rep(1 / 50, 50), sides = 1))
}
breadth_50d <- rowMeans(P >= P_sma50, na.rm = TRUE)

ev <- which(cal >= EVAL_START)
dates <- cal[ev]
month_end <- c(format(cal[-1], "%m") != format(cal[-length(cal)], "%m"), TRUE)

# ------------------------------------------------------------------------------
# Feature Testing Engine
# ------------------------------------------------------------------------------
sim_feature_test <- function(core_pct = 0.40, top_n = 5,
                             vol_target = 0.16,
                             ranking_mode = c("raw_rs", "smooth_rs"), # F1
                             core_bull_leverage = 1.0,               # F2 (e.g. 1.25x in low-vol bull)
                             use_trailing_stop = FALSE,              # F3 (e.g. -15% from 30d peak)
                             use_breadth_guard = FALSE,              # F4 (de-lever when breadth < 40%)
                             label = "Test") {
  ranking_mode <- match.arg(ranking_mode)
  sat_pct <- 1 - core_pct
  
  eq <- 1.0; peak_eq <- 1.0
  out_ret <- numeric(length(ev))
  turn_tot <- 0
  
  curr_core_w  <- 0.0
  curr_sat_w   <- rep(0.0, ncol(P)); names(curr_sat_w) <- colnames(P)
  entry_prices <- rep(NA_real_, ncol(P)); names(entry_prices) <- colnames(P)
  peak_prices  <- rep(NA_real_, ncol(P)); names(peak_prices) <- colnames(P)
  stopped_out  <- rep(FALSE, ncol(P)); names(stopped_out) <- colnames(P)
  
  for (k in seq_along(ev)) {
    t <- ev[k]
    r_qqq <- rq[t]
    r_sat <- R[t, ]
    
    tot_sat_val  <- sum(curr_sat_w * (1 + r_sat))
    tot_core_val <- curr_core_w * (1 + r_qqq)
    curr_cash_w  <- 1 - (curr_core_w + sum(curr_sat_w))
    
    cash_gain <- if (curr_cash_w >= 0) curr_cash_w * rf[t] else curr_cash_w * MARGIN / 252
    day_gain  <- (curr_core_w * r_qqq) + sum(curr_sat_w * r_sat) + cash_gain
    
    eq <- eq * (1 + day_gain)
    if (eq > peak_eq) peak_eq <- eq
    
    curr_core_w <- tot_core_val / (1 + day_gain)
    curr_sat_w  <- (curr_sat_w * (1 + r_sat)) / (1 + day_gain)
    
    # Update satellite peak prices and stop watchdogs
    active_sats <- names(curr_sat_w)[curr_sat_w > 0]
    for (sym in active_sats) {
      px_now <- P[t, sym]
      if (is.na(peak_prices[sym]) || px_now > peak_prices[sym]) peak_prices[sym] <- px_now
      
      # Stop condition: either -20% from entry OR (if F3 active) -15% from rolling peak
      hit_stop <- FALSE
      if (!is.na(entry_prices[sym]) && entry_prices[sym] > 0) {
        if ((px_now / entry_prices[sym] - 1) <= -0.20) hit_stop <- TRUE
      }
      if (use_trailing_stop && !is.na(peak_prices[sym]) && peak_prices[sym] > 0) {
        if ((px_now / peak_prices[sym] - 1) <= -0.15 && (peak_prices[sym] / entry_prices[sym] - 1) >= 0.10) {
          hit_stop <- TRUE # Trailing stop triggered after locking in at least 10% gain
        }
      }
      
      if (hit_stop && !stopped_out[sym]) {
        to <- curr_sat_w[sym]
        eq <- eq * (1 - COST * to)
        turn_tot <- turn_tot + to
        curr_sat_w[sym] <- 0
        stopped_out[sym] <- TRUE
      }
    }
    
    # Monthly Rebalance Check
    s <- t - 1
    if (month_end[s] || k == 1) {
      realized_v <- roll_vol_20[s]
      scale_mult <- min(1.0, vol_target / max(0.05, realized_v))
      
      # F4: Breadth Guard (if fewer than 40% of stocks are in an uptrend, scale satellite down by 30%)
      if (use_breadth_guard && !is.na(breadth_50d[s]) && breadth_50d[s] < 0.40) {
        scale_mult <- scale_mult * 0.70
      }
      
      # F2: Dynamic Core Bull Leverage
      core_lev <- 1.0
      if (core_bull_leverage > 1.0 && isTRUE(qqq_trend_up[s]) && realized_v <= 0.16) {
        core_lev <- core_bull_leverage # e.g. 1.25x
      }
      
      tgt_core_w <- core_pct * scale_mult * core_lev
      
      # Satellite Selection
      lb_252 <- pmax(1, s - 252); lb_126 <- pmax(1, s - 126)
      ok <- colnames(P)[!is.na(P[s, ]) & !is.na(P[lb_252, ]) & (s >= 252)]
      
      tgt_sat_w <- rep(0.0, ncol(P)); names(tgt_sat_w) <- colnames(P)
      if (length(ok) > 0) {
        ret_stock <- P[s, ok] / P[lb_126, ok] - 1
        ret_qqq   <- q[s] / q[lb_126] - 1
        rs        <- ret_stock - ret_qqq
        
        # 200 SMA trend filter
        sma200_stocks <- sapply(ok, function(sym) mean(P[(s - 199):s, sym], na.rm = TRUE))
        pass_trend <- P[s, ok] >= sma200_stocks
        ok_filtered <- ok[pass_trend]
        if (length(ok_filtered) >= top_n) {
          ok <- ok_filtered; rs <- rs[ok]
        }
        
        # F1: Ranking Mode (Raw RS vs Smooth RS adjusted by realized volatility)
        final_score <- rs
        if (ranking_mode == "smooth_rs") {
          vol_stocks <- sapply(ok, function(sym) {
            rets <- R[(s - 125):s, sym]
            sd(rets, na.rm = TRUE) * sqrt(252)
          })
          vol_stocks[is.na(vol_stocks) | vol_stocks <= 0.05] <- 0.30
          final_score <- rs / vol_stocks # Sortino/Sharpe-like Information Ratio
        }
        
        top_syms <- names(sort(final_score, decreasing = TRUE))[1:min(top_n, length(final_score))]
        if (length(top_syms) > 0) {
          tgt_sat_w[top_syms] <- (sat_pct * scale_mult) / length(top_syms)
        }
        
        new_entrants <- setdiff(top_syms, names(curr_sat_w)[curr_sat_w > 0])
        for (ne in new_entrants) {
          entry_prices[ne] <- P[t, ne]
          peak_prices[ne]  <- P[t, ne]
          stopped_out[ne]  <- FALSE
        }
      }
      
      to <- abs(tgt_core_w - curr_core_w) + sum(abs(tgt_sat_w - curr_sat_w))
      if (to > 1e-6) {
        eq <- eq * (1 - COST * to)
        turn_tot <- turn_tot + to
      }
      
      curr_core_w <- tgt_core_w
      curr_sat_w  <- tgt_sat_w
    }
    
    out_ret[k] <- if (k == 1) day_gain else eq / prev_eq - 1
    prev_eq <- eq
  }
  
  attr(out_ret, "turnover_yr") <- turn_tot / (length(ev) / 252)
  attr(out_ret, "label") <- label
  out_ret
}

cat("Simulating Candidate Advanced Features across 2006-2026...\n")

runs_feat <- list(
  "QQQ Buy & Hold (Benchmark)"              = rq[ev],
  "1. Baseline 40/60 + 16% VT (Control)"    = sim_feature_test(core_pct = 0.40, vol_target = 0.16, label = "Baseline 40/60"),
  "2. F1: Smooth RS (Quality Momentum)"     = sim_feature_test(core_pct = 0.40, vol_target = 0.16, ranking_mode = "smooth_rs", label = "F1 Smooth RS"),
  "3. F2: Dynamic Core Bull Leverage (1.25x)"= sim_feature_test(core_pct = 0.40, vol_target = 0.16, core_bull_leverage = 1.25, label = "F2 Dynamic Core Lev"),
  "4. F3: Trailing Peak Stop (-15% Profit Lock)"= sim_feature_test(core_pct = 0.40, vol_target = 0.16, use_trailing_stop = TRUE, label = "F3 Trailing Stop"),
  "5. F4: Market Breadth Guard"             = sim_feature_test(core_pct = 0.40, vol_target = 0.16, use_breadth_guard = TRUE, label = "F4 Breadth Guard"),
  "6. F5: Dual Upgrade (F1 Smooth RS + F2 Core Lev)" = sim_feature_test(core_pct = 0.40, vol_target = 0.16, ranking_mode = "smooth_rs", core_bull_leverage = 1.25, label = "F5 Dual Upgrade")
)

calc_feat_metrics <- function(r) {
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

feat_scorecard <- do.call(rbind, lapply(runs_feat, calc_feat_metrics))
feat_scorecard$Feature_Configuration <- names(runs_feat)
feat_scorecard <- feat_scorecard[, c("Feature_Configuration", "Final_Value", "CAGR", "MaxDD", "Vol", "Sharpe", "Calmar", "Ret_2008", "Ret_2020", "Ret_2022", "Turnover_yr")]

cat("\n================ ADVANCED FEATURE EVALUATION SCORECARD (2006-2026) ================\n")
print(feat_scorecard, row.names = FALSE)

# Generate Chart
chart_path <- file.path(ART, "advanced_features_comparison_chart.png")
png(chart_path, width = 1400, height = 1100, res = 130)
par(mfrow = c(2, 2), mar = c(4, 4.5, 2.5, 1.5), oma = c(0, 0, 2, 0))

eq_curves <- lapply(runs_feat, function(r) 10000 * cumprod(1 + r))
dd_curves <- lapply(runs_feat, function(r) { c_r <- cumprod(1 + r); (c_r / cummax(c_r) - 1) * 100 })
cols <- c("#78909c", "#1565c0", "#2e7d32", "#e65100", "#c2185b", "#7b1fa2", "#00838f")

# 1. Equity Curves
plot(dates, eq_curves[[2]], type = "l", col = cols[2], lwd = 1.8, log = "y",
     ylim = range(unlist(eq_curves)), main = "A. Growth of $10,000 (Log Scale)", ylab = "Portfolio Equity ($)", xlab = "")
for (i in 3:length(eq_curves)) {
  lines(dates, eq_curves[[i]], col = cols[i], lwd = if (i %in% c(4, 7)) 2.5 else 1.8)
}
lines(dates, eq_curves[[1]], col = cols[1], lwd = 1.5, lty = 2)
grid(col = "#e0e0e0")
legend("topleft", bty = "n", cex = 0.78,
       legend = sapply(2:length(runs_feat), function(i) {
         sprintf("%s ($%s | %s)", names(runs_feat)[i], format(round(tail(eq_curves[[i]], 1)), big.mark = ","), feat_scorecard$CAGR[i])
       }),
       col = cols[2:length(runs_feat)], lwd = 2)

# 2. Drawdowns Comparison
plot(dates, dd_curves[[2]], type = "l", col = cols[2], lwd = 1.8,
     ylim = c(-50, 0), main = "B. Drawdown Profiles: Advanced Features (%)", ylab = "Drawdown (%)", xlab = "")
lines(dates, dd_curves[[3]], col = cols[3], lwd = 2.0)
lines(dates, dd_curves[[5]], col = cols[5], lwd = 2.0)
lines(dates, dd_curves[[7]], col = cols[7], lwd = 2.5)
lines(dates, dd_curves[[1]], col = cols[1], lwd = 1.2, lty = 2)
grid(col = "#e0e0e0")
legend("bottomleft", bty = "n", cex = 0.80,
       legend = c(
         sprintf("Control 40/60 VT (MaxDD: %s)", feat_scorecard$MaxDD[2]),
         sprintf("F1 Smooth RS (MaxDD: %s)", feat_scorecard$MaxDD[3]),
         sprintf("F3 Trailing Stop (MaxDD: %s)", feat_scorecard$MaxDD[5]),
         sprintf("F5 Dual Upgrade (MaxDD: %s)", feat_scorecard$MaxDD[7]),
         sprintf("QQQ Benchmark (MaxDD: %s)", feat_scorecard$MaxDD[1])
       ),
       col = c(cols[2], cols[3], cols[5], cols[7], cols[1]), lwd = 2, lty = c(1, 1, 1, 1, 2))

# 3. Sharpe & Calmar Ratios
sharpes_f <- as.numeric(feat_scorecard$Sharpe[2:7])
calmars_f <- as.numeric(feat_scorecard$Calmar[2:7])
mat_f <- rbind("Sharpe" = sharpes_f, "Calmar" = calmars_f)
colnames(mat_f) <- c("Control", "Smooth RS", "Core Lev", "Trail Stop", "Breadth", "Dual Upgrade")

barplot(mat_f, beside = TRUE, col = c("#1565c0", "#2e7d32"),
        main = "C. Risk-Adjusted Efficiency (Sharpe & Calmar)", ylab = "Ratio Value",
        las = 1, cex.names = 0.75)
grid(col = "#e0e0e0")
legend("topleft", bty = "n", cex = 0.85, legend = c("Sharpe Ratio", "Calmar Ratio"), fill = c("#1565c0", "#2e7d32"))

# 4. Bear Markets Performance (2008 & 2022)
r08_f <- as.numeric(sub("%", "", feat_scorecard$Ret_2008[2:7]))
r22_f <- as.numeric(sub("%", "", feat_scorecard$Ret_2022[2:7]))
mat_b_f <- rbind("2008 Crash" = r08_f, "2022 Bear" = r22_f)
colnames(mat_b_f) <- c("Control", "Smooth RS", "Core Lev", "Trail Stop", "Breadth", "Dual Upgrade")

barplot(mat_b_f, beside = TRUE, col = c("#c62828", "#1565c0"),
        main = "D. Bear Market Performance: 2008 vs 2022 (%)", ylab = "Annual Return (%)",
        las = 1, cex.names = 0.75)
grid(col = "#e0e0e0")
legend("topleft", bty = "n", cex = 0.85, legend = c("2008 Crash", "2022 Bear"), fill = c("#c62828", "#1565c0"))

title("Advanced Feature Engineering for Core-Satellite (2006-2026)", outer = TRUE, cex.main = 1.3)
dev.off()
cat("Chart saved to", chart_path, "\n")
