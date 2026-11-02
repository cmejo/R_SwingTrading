#!/usr/bin/env Rscript
# ==============================================================================
# scratch/backtest_20yr_earnings_option_a.R
# Head-to-Head 20-Year Backtest:
#   1. Baseline 30/70 Model (Option 6: 30% QLD Core + 70% Satellites, 1.5x Margin)
#   2. 100% Stock Model (1.5x Margin, Top 5 Stocks) - No Blackout
#   3. 100% Stock Model (1.5x Margin, Top 5 Stocks) - WITH Option A Earnings Shield
# ==============================================================================

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(jsonlite)
})

src <- readLines("scratch/compare_symbol_lists.R")
idx <- grep("^lists <- c", src)
eval(parse(text = paste(src[1:(idx - 1)], collapse = "\n")))

# Set universe to broad 170-name list
set_universe(read_list("symbols_broad.txt"))

S20 <- as.Date("2007-01-02")
E20 <- as.Date("2026-10-06")
S10 <- as.Date("2016-10-03")
E10 <- as.Date("2026-10-06")

# Load Historical Earnings Dates
earn_json <- jsonlite::fromJSON("data/historical_earnings_dates.json")

# Fast precomputation of 5-day pre-earnings window matrix
cal_dates <- as.Date(cal)
earn_within_5d_mat <- matrix(FALSE, length(cal), ncol(P), dimnames = list(NULL, colnames(P)))

for (sym in colnames(P)) {
  e_dates_str <- earn_json[[sym]]
  if (is.null(e_dates_str) || length(e_dates_str) == 0) next
  e_dates <- as.Date(e_dates_str)
  
  for (ed in e_dates) {
    match_idx <- which(cal_dates >= ed)[1]
    if (is.na(match_idx)) next
    start_idx <- max(1, match_idx - 5)
    end_idx   <- match_idx
    earn_within_5d_mat[start_idx:end_idx, sym] <- TRUE
  }
}

solve_min_var <- function(cov_mat, max_w = 0.35) {
  N <- ncol(cov_mat)
  if (N == 1) return(c(1.0))
  inv_var <- 1 / diag(cov_mat)
  inv_var[!is.finite(inv_var) | inv_var < 0] <- 0
  if (sum(inv_var) <= 0) return(rep(1 / N, N))
  w <- inv_var / sum(inv_var)
  
  if (requireNamespace("quadprog", quietly = TRUE)) {
    tryCatch({
      Dmat <- cov_mat + diag(1e-5, N)
      dvec <- rep(0, N)
      Amat <- cbind(rep(1, N), diag(N), -diag(N))
      bvec <- c(1, rep(0, N), rep(-max_w, N))
      qp <- quadprog::solve.QP(Dmat, dvec, Amat, bvec, meq = 1)
      if (!is.null(qp$solution)) w <- pmax(0, qp$solution) / sum(pmax(0, qp$solution))
    }, error = function(e) NULL)
  }
  w
}

sim_backtest <- function(model_type = c("baseline_30_70", "100pct_no_blackout", "100pct_option_a_blackout"),
                         margin = 1.5,
                         top_n = 5,
                         vt = 0.22,
                         start = S20,
                         end = E20) {
  model_type <- match.arg(model_type)
  is_100pct  <- (model_type != "baseline_30_70")
  use_option_a_shield <- (model_type == "100pct_option_a_blackout")
  
  core_pct <- if (is_100pct) 0.0 else 0.30
  sat_pct  <- 1.0 - core_pct
  
  U <- syms
  ev <- which(cal >= start & cal <= end)
  sma_g <- SMAQ[["200"]]
  r_core <- r_qld
  
  cw <- 0
  sw <- setNames(rep(0, length(U)), U)
  dw <- 0
  ep <- setNames(rep(NA_real_, length(U)), U)
  
  eq <- 1
  peak <- 1
  turn_tot <- 0
  out <- numeric(length(ev))
  
  for (k in seq_along(ev)) {
    t <- ev[k]
    prev <- eq
    gross <- cw + sum(sw) + dw
    
    # Portfolio daily return
    g <- cw * r_core[t] + sum(sw * R[t, ]) + dw * r_gld[t] - 
         max(0, gross - 1) * borrow_daily[t] + max(0, 1 - gross) * rf_daily[t]
    
    eq <- eq * (1 + g)
    peak <- max(peak, eq)
    
    cw <- cw * (1 + r_core[t]) / (1 + g)
    sw <- sw * (1 + R[t, ]) / (1 + g)
    dw <- dw * (1 + r_gld[t]) / (1 + g)
    
    # Disaster Stop (-20%)
    for (s in names(sw)[sw > 0]) {
      hit <- (!is.na(ep[s]) && P[t, s] / ep[s] - 1 <= -0.20)
      if (isTRUE(hit)) {
        w <- sw[s]
        eq <- eq * (1 - COST * w)
        turn_tot <- turn_tot + w
        sw[s] <- 0
        if (!is_100pct && isTRUE(q[t] >= sma_g[t])) {
          cw <- cw + w  # Option 6 redeploys to core
        }
      }
    }
    
    s0 <- t - 1
    # Monthly rebalance
    if (month_end[s0] || k == 1) {
      sc <- min(1, vt / max(0.05, vol20[s0]))
      bull <- isTRUE(q[s0] >= sma_g[s0]) || is.na(sma_g[s0])
      am <- if (margin > 1 && !bull) 1 else margin
      
      tc <- if (bull && core_pct > 0) core_pct * sc * am else 0
      ts <- setNames(rep(0, length(U)), U)
      td <- 0
      
      ok <- U[MEM[s0, ] & !is.na(P[s0, ]) & !is.na(P[max(1, s0 - 252), ])]
      
      if (bull || !is_100pct) {
        # BULL REGIME (or Baseline Model in Bear which holds GLD/cash)
        if (bull && length(ok) >= top_n) {
          r126 <- P[s0, ok] / P[s0 - 126, ok] - 1 - (q[s0] / q[s0 - 126] - 1)
          r63  <- P[s0, ok] / P[s0 - 63, ok]  - 1 - (q[s0] / q[s0 - 63] - 1)
          rs   <- 0.50 * r126 + 0.50 * r63
          
          # 200 SMA trend filter on stocks
          okf <- ok[!is.na(SMA200[s0, ok]) & P[s0, ok] >= SMA200[s0, ok]]
          if (length(okf) >= top_n) { ok <- okf; rs <- rs[ok] }
          
          # 100% Realized Volatility Cap
          k2 <- ok[!is.na(VOL63[s0, ok]) & VOL63[s0, ok] <= 1.00]
          if (length(k2) >= top_n) { ok <- k2; rs <- rs[k2] }
          
          # ------------------------------------------------------------------
          # OPTION A: EARNINGS BLACKOUT ENTRY GATE (SKIP IF EARNINGS IN <=5 DAYS)
          # ------------------------------------------------------------------
          if (use_option_a_shield) {
            # Filter out candidate stocks reporting earnings within 5 days
            not_in_blackout <- !earn_within_5d_mat[s0, ok]
            if (sum(not_in_blackout) >= top_n) {
              ok <- ok[not_in_blackout]
              rs <- rs[ok]
            }
          }
          
          rk <- names(sort(rs, decreasing = TRUE))
          
          # Existing positions held through if still qualified; new entrants fill remainder
          held <- intersect(names(sw)[sw > 0], rk[seq_len(min(length(rk), top_n))])
          top  <- unique(c(held, rk))[1:top_n]
          top  <- top[!is.na(top)]
          
          wts <- rep(1 / top_n, length(top))
          ts[top] <- sat_pct * sc * am * wts
          
          for (ne in setdiff(top, names(sw)[sw > 0])) {
            ep[ne] <- P[t, ne]
          }
        }
        
        # Volatility target buffer parked in GLD
        base_eq <- (if (bull) core_pct else 0) * sc + sat_pct * sc
        td <- max(0, 1.0 - base_eq)
        
      } else {
        # ====================================================================
        # BEAR / CRISIS REGIME: MINIMUM VARIANCE EQUITIES PORTFOLIO (MVP)
        # ====================================================================
        if (length(ok) >= top_n) {
          vol_ranks <- sort(VOL63[s0, ok])
          low_vol_syms <- names(head(vol_ranks, top_n * 2))
          R_sub <- R[(s0 - 62):s0, low_vol_syms, drop = FALSE]
          valid_cols <- apply(R_sub, 2, function(x) !any(is.na(x)) && sd(x) > 0)
          if (sum(valid_cols) >= top_n) {
            low_vol_syms <- names(valid_cols)[valid_cols][1:top_n]
            cov_mat <- cov(R_sub[, low_vol_syms, drop = FALSE])
            w_mvp <- solve_min_var(cov_mat)
            ts[low_vol_syms] <- sat_pct * w_mvp
            for (ne in setdiff(low_vol_syms, names(sw)[sw > 0])) ep[ne] <- P[t, ne]
          }
        }
        tc <- 0
        td <- 0
      }
      
      to <- abs(tc - cw) + sum(abs(ts - sw)) + abs(td - dw)
      if (to > 1e-6) {
        eq <- eq * (1 - COST * to)
        turn_tot <- turn_tot + to
      }
      
      cw <- tc
      sw <- ts
      dw <- td
    }
    
    out[k] <- if (k == 1) g else eq / prev - 1
  }
  
  attr(out, "turnover_yr") <- turn_tot / (length(ev) / 252)
  attr(out, "final_equity") <- eq
  out
}

calc_perf <- function(r, rf_series) {
  c_r <- cumprod(1 + r)
  n <- length(r)
  dd <- c_r / cummax(c_r) - 1
  mdd <- -min(dd) * 100
  cagr <- (tail(c_r, 1)^(252 / n) - 1) * 100
  term <- 8000 * tail(c_r, 1)
  vol <- sd(r) * sqrt(252) * 100
  rf_mean <- mean(rf_series) * 252
  sharpe <- if (sd(r) > 0) (mean(r) * 252 - rf_mean) / (sd(r) * sqrt(252)) else 0
  sortino <- {
    d_ret <- pmin(r - rf_series, 0)
    d_sd  <- sqrt(mean(d_ret^2)) * sqrt(252)
    if (d_sd > 0) (mean(r) - mean(rf_series)) * 252 / d_sd else 0
  }
  calmar <- if (mdd > 0) cagr / mdd else 0
  turn <- if (!is.null(attr(r, "turnover_yr"))) attr(r, "turnover_yr") else 0
  
  c(CAGR = cagr, Terminal_8k = term, MaxDD = mdd, Vol = vol,
    Sharpe = sharpe, Sortino = sortino, Calmar = calmar, Turnover_Yr = turn)
}

cat("========================================================================================\n")
cat("      RUNNING 20-YEAR HEAD-TO-HEAD BACKTEST (2007 - 2026: 19.8 YEARS)                  \n")
cat("========================================================================================\n\n")

ev20 <- which(cal >= S20 & cal <= E20)
rf20 <- rf_daily[ev20]

res_base_20 <- sim_backtest("baseline_30_70", start = S20, end = E20)
res_100_20  <- sim_backtest("100pct_no_blackout", start = S20, end = E20)
res_optA_20 <- sim_backtest("100pct_option_a_blackout", start = S20, end = E20)

m_base_20 <- calc_perf(res_base_20, rf20)
m_100_20  <- calc_perf(res_100_20, rf20)
m_optA_20 <- calc_perf(res_optA_20, rf20)

table_20 <- rbind(
  "1. Baseline (30% QLD / 70% Stock, 1.5x Margin)"     = m_base_20,
  "2. 100% Stock (1.5x Margin, 5 Stocks, No Blackout)" = m_100_20,
  "3. 100% Stock (1.5x Margin, Option A Earnings Shield)" = m_optA_20
)

cat("--- 20-YEAR SCORECARD (2007 - 2026) ---\n")
print(round(table_20, 2))
cat("\n")

cat("========================================================================================\n")
cat("      RUNNING 10-YEAR HEAD-TO-HEAD BACKTEST (2016 - 2026: 10.0 YEARS)                  \n")
cat("========================================================================================\n\n")

ev10 <- which(cal >= S10 & cal <= E10)
rf10 <- rf_daily[ev10]

res_base_10 <- sim_backtest("baseline_30_70", start = S10, end = E10)
res_100_10  <- sim_backtest("100pct_no_blackout", start = S10, end = E10)
res_optA_10 <- sim_backtest("100pct_option_a_blackout", start = S10, end = E10)

m_base_10 <- calc_perf(res_base_10, rf10)
m_100_10  <- calc_perf(res_100_10, rf10)
m_optA_10 <- calc_perf(res_optA_10, rf10)

table_10 <- rbind(
  "1. Baseline (30% QLD / 70% Stock, 1.5x Margin)"     = m_base_10,
  "2. 100% Stock (1.5x Margin, 5 Stocks, No Blackout)" = m_100_10,
  "3. 100% Stock (1.5x Margin, Option A Earnings Shield)" = m_optA_10
)

cat("--- 10-YEAR SCORECARD (2016 - 2026) ---\n")
print(round(table_10, 2))
cat("\n")

# Save Scorecard CSV
write.csv(table_20, "scratch/scorecard_20yr_earnings_option_a.csv")
write.csv(table_10, "scratch/scorecard_10yr_earnings_option_a.csv")

# ------------------------------------------------------------------------------
# Generate High-Resolution 20-Year Chart
# ------------------------------------------------------------------------------
ART <- "/Users/cmejo/.gemini/antigravity/brain/64019830-0cac-4bb1-904e-d4560e71468c"
chart_png <- file.path(ART, "backtest_20yr_earnings_option_a_comparison.png")

png(chart_png, width = 1400, height = 900, res = 120)
par(mfrow = c(2, 1), mar = c(3, 4, 3, 2), oma = c(1, 1, 1, 1))

dates20 <- cal[ev20]
eq_base <- cumprod(1 + res_base_20) * 8000
eq_100  <- cumprod(1 + res_100_20) * 8000
eq_optA <- cumprod(1 + res_optA_20) * 8000

# Top Panel: Equity Curves (Log Scale)
plot(dates20, eq_optA, type = "l", col = "#10b981", lwd = 2.5, log = "y",
     main = "20-Year Backtest: 100% Stock Model (Option A Earnings Shield) vs Baseline 30/70 (2007-2026)",
     ylab = "Portfolio Equity ($ - Log Scale)", xlab = "", xaxt = "n", yaxt = "n",
     ylim = range(c(eq_base, eq_100, eq_optA)))

lines(dates20, eq_100, col = "#3b82f6", lwd = 2, lty = 2)
lines(dates20, eq_base, col = "#ef4444", lwd = 2)
grid(col = "#e2e8f0", lty = 1)

# Format axes
axis.Date(1, at = seq(S20, E20, by = "2 years"), format = "%Y")
y_ticks <- c(8000, 50000, 250000, 1000000, 5000000, 20000000, 100000000)
axis(2, at = y_ticks, labels = paste0("$", format(y_ticks, scientific = FALSE, big.mark = ",")), las = 1)

legend("topleft", 
       legend = c(
         sprintf("100%% Stock + Option A Earnings Shield: %+.1f%% CAGR | MaxDD: %.1f%% ($%.0f)", m_optA_20["CAGR"], m_optA_20["MaxDD"], m_optA_20["Terminal_8k"]),
         sprintf("100%% Stock (No Blackout Filter):     %+.1f%% CAGR | MaxDD: %.1f%% ($%.0f)", m_100_20["CAGR"], m_100_20["MaxDD"], m_100_20["Terminal_8k"]),
         sprintf("Baseline 30/70 (30%% QLD + 70%% Stock): %+.1f%% CAGR | MaxDD: %.1f%% ($%.0f)", m_base_20["CAGR"], m_base_20["MaxDD"], m_base_20["Terminal_8k"])
       ),
       col = c("#10b981", "#3b82f6", "#ef4444"),
       lty = c(1, 2, 1), lwd = c(2.5, 2, 2), bty = "o", bg = "white", cex = 0.85)

# Bottom Panel: Underwater Drawdowns
dd_base <- (cumprod(1 + res_base_20) / cummax(cumprod(1 + res_base_20)) - 1) * 100
dd_100  <- (cumprod(1 + res_100_20) / cummax(cumprod(1 + res_100_20)) - 1) * 100
dd_optA <- (cumprod(1 + res_optA_20) / cummax(cumprod(1 + res_optA_20)) - 1) * 100

plot(dates20, dd_optA, type = "l", col = "#10b981", lwd = 1.8,
     main = "Underwater Drawdown Profile (2007 - 2026)",
     ylab = "Drawdown (%)", xlab = "", xaxt = "n", ylim = c(-75, 0))
lines(dates20, dd_100, col = "#3b82f6", lwd = 1.2, lty = 2)
lines(dates20, dd_base, col = "#ef4444", lwd = 1.5)
grid(col = "#e2e8f0", lty = 1)
axis.Date(1, at = seq(S20, E20, by = "2 years"), format = "%Y")

dev.off()
cat(sprintf("\n✅ 20-Year comparison chart saved to: %s\n", chart_png))
