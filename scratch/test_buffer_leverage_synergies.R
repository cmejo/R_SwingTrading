#!/usr/bin/env Rscript
# ==============================================================================
# scratch/test_buffer_leverage_synergies.R
# Deep Dive on the Winner Retention Buffer & Turbo Combinations
# ==============================================================================

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(jsonlite)
})

cat("Loading price and universe datasets...\n")
src <- readLines("scratch/compare_symbol_lists.R")
idx <- grep("^lists <- c", src)
eval(parse(text = paste(src[1:(idx - 1)], collapse = "\n")))
set_universe(read_list("symbols_broad.txt"))

source("scratch/test_crisis_min_variance.R")
source("R/02_trend_lmMA.R")

S20 <- as.Date("2007-01-02")
E20 <- as.Date("2026-10-06")
S10 <- as.Date("2016-10-03")
E10 <- as.Date("2026-10-06")

U <- syms

# 1. Macro Diagnostics
qqq_ts <- xts(q, order.by = cal)
sma50_q  <- as.numeric(stats::filter(q, rep(1/50, 50), sides = 1))
sma200_q <- as.numeric(stats::filter(q, rep(1/200, 200), sides = 1))
lm50_q   <- lmMA(qqq_ts, 50)
slope50_q <- as.numeric(lm50_q$slope)
rsq50_q   <- as.numeric(lm50_q$r.squared)
benchmark_bull <- !(slope50_q < 0 & rsq50_q > 0.15)

super_bull_q <- (q > sma50_q & sma50_q > sma200_q & slope50_q > 0)
super_bull_q[is.na(super_bull_q)] <- FALSE

# 50-day SMA matrix for Market Breadth
sma50_matrix <- matrix(NA_real_, nrow = nrow(P), ncol = ncol(P), dimnames = dimnames(P))
for (j in 1:ncol(P)) {
  sma50_matrix[, j] <- as.numeric(stats::filter(P[, j], rep(1/50, 50), sides = 1))
}
breadth_50 <- rowMeans(P > sma50_matrix, na.rm = TRUE)
breadth_50[is.na(breadth_50)] <- 0.50

# Returns relative to QQQ
r126_mat <- matrix(NA_real_, nrow = nrow(P), ncol = ncol(P), dimnames = dimnames(P))
r63_mat  <- matrix(NA_real_, nrow = nrow(P), ncol = ncol(P), dimnames = dimnames(P))
q_lag126 <- c(rep(NA, 126), q[1:(length(q) - 126)])
q_lag63  <- c(rep(NA, 63), q[1:(length(q) - 63)])
q_r126   <- q / q_lag126 - 1
q_r63    <- q / q_lag63 - 1

for (j in 1:ncol(P)) {
  px <- P[, j]
  px_lag126 <- c(rep(NA, 126), px[1:(length(px) - 126)])
  px_lag63  <- c(rep(NA, 63), px[1:(length(px) - 63)])
  r126_mat[, j] <- (px / px_lag126 - 1) - q_r126
  r63_mat[, j]  <- (px / px_lag63 - 1)  - q_r63
}

sim_buffer_engine <- function(
    buffer_rank = 8,
    disaster_stop_pct = 0.20,
    super_bull_leverage = 1.75,
    use_breadth_turbo = FALSE,
    start = S20,
    end = E20
) {
  ev <- which(cal >= start & cal <= end)
  sat_pct <- 1.0
  vt <- 0.22
  top_n <- 5
  
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
    
    g <- cw * rq[t] + sum(sw * R[t, ]) + dw * r_gld[t] - 
         max(0, gross - 1) * borrow_daily[t] + max(0, 1 - gross) * rf_daily[t]
    
    eq <- eq * (1 + g)
    peak <- max(peak, eq)
    
    cw <- cw * (1 + rq[t]) / (1 + g)
    sw <- sw * (1 + R[t, ]) / (1 + g)
    dw <- dw * (1 + r_gld[t]) / (1 + g)
    
    s0 <- t - 1
    is_sb <- isTRUE(super_bull_q[s0])
    
    # 1. Disaster Stop Watchdog
    stopped_symbols <- character()
    for (s in names(sw)[sw > 0]) {
      hit_disaster <- (!is.na(ep[s]) && P[t, s] / ep[s] - 1 <= -disaster_stop_pct)
      if (isTRUE(hit_disaster)) {
        w <- sw[s]
        eq <- eq * (1 - COST * w)
        turn_tot <- turn_tot + w
        sw[s] <- 0
        ep[s] <- NA
        stopped_symbols <- c(stopped_symbols, s)
      }
    }
    
    # 2. Immediate Stop Replacement
    if (length(stopped_symbols) > 0 && isTRUE(benchmark_bull[s0])) {
      ok <- U[MEM[s0, ] & !is.na(P[s0, ]) & !is.na(P[max(1, s0 - 126), ])]
      if (length(ok) >= 10) {
        r126 <- r126_mat[s0, ok]
        r63  <- r63_mat[s0, ok]
        rs   <- 0.50 * r126 + 0.50 * r63
        k2   <- ok[!is.na(VOL63[s0, ok]) & VOL63[s0, ok] <= 1.00]
        if (length(k2) >= 10) { ok <- k2; rs <- rs[k2] }
        rk <- names(sort(rs, decreasing = TRUE))
        unheld <- setdiff(rk, c(names(sw)[sw > 0], stopped_symbols))
        
        act_lev <- if (is_sb) super_bull_leverage else 1.50
        target_w_per_slot <- act_lev / top_n
        
        for (rep_sym in head(unheld, length(stopped_symbols))) {
          sw[rep_sym] <- target_w_per_slot
          ep[rep_sym] <- P[t, rep_sym]
          eq <- eq * (1 - COST * target_w_per_slot)
          turn_tot <- turn_tot + target_w_per_slot
        }
      }
    }
    
    # 3. Monthly Rebalance Trigger
    if (month_end[s0] || k == 1) {
      sc <- min(1, vt / max(0.05, vol20[s0]))
      bull <- isTRUE(benchmark_bull[s0])
      
      active_margin <- 1.0
      if (bull) {
        if (is_sb) {
          if (use_breadth_turbo) {
            if (breadth_50[s0] >= 0.65) {
              active_margin <- super_bull_leverage
            } else if (breadth_50[s0] < 0.45) {
              active_margin <- 1.50
            } else {
              active_margin <- 1.75
            }
          } else {
            active_margin <- super_bull_leverage
          }
        } else {
          active_margin <- 1.50
        }
      }
      
      ts <- setNames(rep(0, length(U)), U)
      td <- 0
      ok <- U[MEM[s0, ] & !is.na(P[s0, ]) & !is.na(P[max(1, s0 - 252), ])]
      
      if (bull) {
        if (length(ok) >= top_n) {
          r126 <- r126_mat[s0, ok]
          r63  <- r63_mat[s0, ok]
          rs   <- 0.50 * r126 + 0.50 * r63
          
          k2 <- ok[!is.na(VOL63[s0, ok]) & VOL63[s0, ok] <= 1.00]
          if (length(k2) >= top_n) { ok <- k2; rs <- rs[k2] }
          
          rk <- names(sort(rs, decreasing = TRUE))
          
          if (buffer_rank > top_n) {
            # Hysteresis buffer
            cur_held <- names(sw)[sw > 0]
            buffer_qual <- intersect(cur_held, rk[1:min(length(rk), buffer_rank)])
            new_picks <- setdiff(rk, buffer_qual)
            slots_needed <- max(0, top_n - length(buffer_qual))
            top <- c(buffer_qual, head(new_picks, slots_needed))[1:top_n]
          } else {
            held <- intersect(names(sw)[sw > 0], rk[1:top_n])
            top <- unique(c(held, rk))[1:top_n]
          }
          top <- top[!is.na(top)]
          
          n_top <- length(top)
          wts <- rep(1 / n_top, n_top)
          ts[top] <- sat_pct * sc * active_margin * wts
          
          for (ne in setdiff(top, names(sw)[sw > 0])) {
            ep[ne] <- P[t, ne]
          }
        }
      } else {
        # MVP in Bear
        if (length(ok) >= 5) {
          vol_ranks <- sort(VOL63[s0, ok])
          low_vol_syms <- names(head(vol_ranks, 10))
          R_sub <- R[(s0 - 62):s0, low_vol_syms, drop = FALSE]
          valid_cols <- apply(R_sub, 2, function(x) !any(is.na(x)) && sd(x) > 0)
          if (sum(valid_cols) >= 5) {
            low_vol_syms <- names(valid_cols)[valid_cols][1:5]
            cov_mat <- cov(R_sub[, low_vol_syms, drop = FALSE])
            w_mvp <- solve_min_var(cov_mat)
            ts[low_vol_syms] <- sat_pct * sc * active_margin * w_mvp
            for (ne in setdiff(low_vol_syms, names(sw)[sw > 0])) ep[ne] <- P[t, ne]
          }
        }
      }
      
      to <- sum(abs(ts - sw)) + abs(td - dw)
      eq <- eq * (1 - COST * to)
      turn_tot <- turn_tot + to
      sw <- ts
      dw <- td
    }
    
    out[k] <- eq / prev - 1
  }
  
  attr(out, "turnover_yr") <- turn_tot / (length(ev) / 252)
  out
}

calc_metrics <- function(r, start_d, end_d) {
  ev_idx <- which(cal >= start_d & cal <= end_d)
  c_r <- cumprod(1 + r); n <- length(r); dd <- c_r / cummax(c_r) - 1
  mdd <- -min(dd) * 100
  cagr <- (tail(c_r, 1)^(252 / n) - 1) * 100
  term <- 8000 * tail(c_r, 1)
  vol <- sd(r) * sqrt(252) * 100
  rfm <- rf_daily[ev_idx]
  sharpe <- if (sd(r) > 0) mean(r - rfm) / sd(r) * sqrt(252) else 0
  sortino <- {
    d_ret <- pmin(r - rfm, 0)
    d_sd  <- sqrt(mean(d_ret^2)) * sqrt(252)
    if (d_sd > 0) mean(r - rfm) * 252 / d_sd else 0
  }
  calmar <- if (mdd > 0) cagr / mdd else 0
  turn <- if (!is.null(attr(r, "turnover_yr"))) attr(r, "turnover_yr") else 0
  
  c(CAGR = cagr, Terminal_8k = term, MaxDD = mdd, Vol = vol,
    Sharpe = sharpe, Sortino = sortino, Calmar = calmar, Turnover = turn)
}

combos <- list(
  "Baseline: Power Alpha Turbo (No Buffer, 1.75x, -20% Stop)" = 
    list(buffer = 5, stop = 0.20, lev = 1.75, brd = FALSE),
    
  "Winner Buffer Top 7 (1.75x, -20% Stop)" = 
    list(buffer = 7, stop = 0.20, lev = 1.75, brd = FALSE),
    
  "Winner Buffer Top 8 (1.75x, -20% Stop)" = 
    list(buffer = 8, stop = 0.20, lev = 1.75, brd = FALSE),
    
  "Winner Buffer Top 10 (1.75x, -20% Stop)" = 
    list(buffer = 10, stop = 0.20, lev = 1.75, brd = FALSE),
    
  "Winner Buffer Top 8 + Wide -25% Stop (1.75x)" = 
    list(buffer = 8, stop = 0.25, lev = 1.75, brd = FALSE),
    
  "Winner Buffer Top 8 + 1.85x Breadth Turbo" = 
    list(buffer = 8, stop = 0.20, lev = 1.85, brd = TRUE),
    
  "Winner Buffer Top 8 + 2.0x Super-Bull Turbo" = 
    list(buffer = 8, stop = 0.20, lev = 2.00, brd = FALSE),
    
  "Ultimate Alpha Master: Buffer Top 8 + 2.0x Turbo + -25% Stop" = 
    list(buffer = 8, stop = 0.25, lev = 2.00, brd = FALSE)
)

cat("\n========================================================================================\n")
cat(" 10-YEAR EVALUATION OF BUFFER & TURBO COMBOS (2016 - 2026)\n")
cat("========================================================================================\n")

res_10 <- lapply(names(combos), function(nm) {
  p <- combos[[nm]]
  sim_buffer_engine(buffer_rank = p$buffer, disaster_stop_pct = p$stop,
                    super_bull_leverage = p$lev, use_breadth_turbo = p$brd,
                    start = S10, end = E10)
})
names(res_10) <- names(combos)
tbl_10 <- t(sapply(res_10, calc_metrics, start_d = S10, end_d = E10))
print(round(tbl_10, 2))
write.csv(tbl_10, "scratch/scorecard_10yr_buffer_combos.csv")

cat("\n========================================================================================\n")
cat(" 20-YEAR EVALUATION OF BUFFER & TURBO COMBOS (2007 - 2026)\n")
cat("========================================================================================\n")

res_20 <- lapply(names(combos), function(nm) {
  p <- combos[[nm]]
  sim_buffer_engine(buffer_rank = p$buffer, disaster_stop_pct = p$stop,
                    super_bull_leverage = p$lev, use_breadth_turbo = p$brd,
                    start = S20, end = E20)
})
names(res_20) <- names(combos)
tbl_20 <- t(sapply(res_20, calc_metrics, start_d = S20, end_d = E20))
print(round(tbl_20, 2))
write.csv(tbl_20, "scratch/scorecard_20yr_buffer_combos.csv")

# Generate High-Res Chart
ART <- "/Users/cmejo/.gemini/antigravity/brain/64019830-0cac-4bb1-904e-d4560e71468c"
chart_path <- file.path(ART, "buffer_and_turbo_combos_comparison.png")
png(chart_path, width = 1650, height = 950, res = 120)
par(mfrow = c(1, 2), mar = c(5, 18, 4, 2))

cagr_10 <- tbl_10[, "CAGR"]
base_10 <- cagr_10[1]
cols_10 <- ifelse(cagr_10 >= base_10 + 10, "#059669", ifelse(cagr_10 > base_10, "#3b82f6", "#ef4444"))
barplot(rev(cagr_10), horiz = TRUE, las = 1, col = rev(cols_10),
        main = "10-Year CAGR (%): Buffer & Turbo Combos",
        xlab = "CAGR (%)", xlim = c(0, max(cagr_10) * 1.15))
grid(col = "gray85")
abline(v = base_10, col = "darkblue", lty = 2, lwd = 2)

cagr_20 <- tbl_20[, "CAGR"]
base_20 <- cagr_20[1]
cols_20 <- ifelse(cagr_20 >= base_20 + 5, "#059669", ifelse(cagr_20 > base_20, "#3b82f6", "#ef4444"))
barplot(rev(cagr_20), horiz = TRUE, las = 1, col = rev(cols_20),
        main = "20-Year CAGR (%): Buffer & Turbo Combos",
        xlab = "CAGR (%)", xlim = c(0, max(cagr_20) * 1.15))
grid(col = "gray85")
abline(v = base_20, col = "darkblue", lty = 2, lwd = 2)

dev.off()
cat("Chart generated at:", chart_path, "\n")
