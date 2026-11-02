#!/usr/bin/env Rscript
# ==============================================================================
# scratch/test_100pct_broad_10yr.R
# 10-Year Backtest: 100% in symbols_broad.txt universe vs 30% Core / 70% Stock baseline
# ==============================================================================
suppressPackageStartupMessages({
  library(xts)
  library(zoo)
})

src <- readLines("scratch/compare_symbol_lists.R")
idx <- grep("^lists <- c", src)
eval(parse(text = paste(src[1:(idx - 1)], collapse = "\n")))

S10 <- as.Date("2016-10-03")
E10 <- as.Date("2026-10-06")
ev10 <- which(cal >= S10 & cal <= E10)

# Set universe to symbols_broad.txt
set_universe(read_list("symbols_broad.txt"))

# Enhanced sim function that allows choosing core_asset ("QQQ" vs "QLD") and core_pct (0.0 to 1.0)
sim_custom <- function(margin = 1.0, 
                       core_asset = "QQQ", 
                       core_pct = 0.30, 
                       top_n = 5, 
                       vol_cap = 1.00,
                       stop_to = "cash",
                       vt = 0.22, 
                       use_vt = TRUE,
                       stop_pct = 0.20,
                       start = S10, 
                       end = E10) {
  
  U <- syms
  ev <- which(cal >= start & cal <= end)
  sat_pct <- max(0, 1 - core_pct)
  sma_g <- SMAQ[["200"]]
  
  # Core asset returns
  r_core <- if (core_asset == "QQQ") rq else r_qld
  
  cw <- 0
  sw <- setNames(rep(0, length(U)), U)
  dw <- 0
  ep <- setNames(rep(NA_real_, length(U)), U)
  
  eq <- 1
  peak <- 1
  out <- numeric(length(ev))
  bull_now <- TRUE
  
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
    
    # Check disaster stops (-20%)
    for (s in names(sw)[sw > 0]) {
      hit <- (!is.na(ep[s]) && P[t, s] / ep[s] - 1 <= -stop_pct)
      if (isTRUE(hit)) {
        w <- sw[s]
        eq <- eq * (1 - COST * w)
        sw[s] <- 0
        if (stop_to == "core" && bull_now && core_pct > 0) {
          cw <- cw + w
          eq <- eq * (1 - COST * w)
        } else if (stop_to == "gld") {
          dw <- dw + w
          eq <- eq * (1 - COST * w)
        }
        # If stop_to == "cash", it naturally remains in cash/deleveraged
      }
    }
    
    s0 <- t - 1
    # Monthly rebalance
    if (month_end[s0] || k == 1) {
      sc <- if (use_vt) min(1, vt / max(0.05, vol20[s0])) else 1.0
      bull <- isTRUE(q[s0] >= sma_g[s0]) || is.na(sma_g[s0])
      bull_now <- bull
      am <- if (margin > 1 && !bull) 1 else margin
      
      tc <- if (bull && core_pct > 0) core_pct * sc * am else 0
      ts <- setNames(rep(0, length(U)), U)
      
      ok <- U[MEM[s0, ] & !is.na(P[s0, ]) & !is.na(P[max(1, s0 - 252), ])]
      if (length(ok) >= top_n) {
        r126 <- P[s0, ok] / P[s0 - 126, ok] - 1 - (q[s0] / q[s0 - 126] - 1)
        r63  <- P[s0, ok] / P[s0 - 63, ok]  - 1 - (q[s0] / q[s0 - 63] - 1)
        rs <- 0.50 * r126 + 0.50 * r63
        
        # 200 SMA trend filter
        okf <- ok[!is.na(SMA200[s0, ok]) & P[s0, ok] >= SMA200[s0, ok]]
        if (length(okf) >= top_n) {
          ok <- okf
          rs <- rs[ok]
        }
        
        # 63d Realized Volatility Cap (100%)
        if (!is.na(vol_cap)) {
          k2 <- ok[!is.na(VOL63[s0, ok]) & VOL63[s0, ok] <= vol_cap]
          if (length(k2) >= top_n) {
            ok <- k2
            rs <- rs[k2]
          }
        }
        
        rk <- names(sort(rs, decreasing = TRUE))
        held <- intersect(names(sw)[sw > 0], rk[seq_len(min(length(rk), top_n))])
        top <- unique(c(held, rk))[1:top_n]
        top <- top[!is.na(top)]
        
        wts <- rep(1 / top_n, length(top))
        ts[top] <- sat_pct * sc * am * wts
        for (ne in setdiff(top, names(sw)[sw > 0])) ep[ne] <- P[t, ne]
      }
      
      # Defensive allocation
      td <- if (use_vt) max(0, 1 - ((if (bull) core_pct else 0) * sc + sat_pct * sc)) else 0
      to <- abs(tc - cw) + sum(abs(ts - sw)) + abs(td - dw)
      eq <- eq * (1 - COST * to)
      cw <- tc
      sw <- ts
      dw <- td
    }
    out[k] <- eq / prev - 1
  }
  out
}

met2 <- function(r) {
  c_r <- cumprod(1 + r)
  n <- length(r)
  dd <- c_r / cummax(c_r) - 1
  d <- cal[cal >= S10 & cal <= E10]
  c(
    CAGR     = (tail(c_r, 1)^(252 / n) - 1) * 100,
    MaxDD    = -min(dd) * 100,
    Vol      = sd(r) * sqrt(252) * 100,
    Sharpe   = mean(r) / sd(r) * sqrt(252),
    Calmar   = ((tail(c_r, 1)^(252 / n) - 1) * 100) / (-min(dd) * 100),
    Worst_Mo = min(tapply(r, format(d, "%Y-%m"), function(x) (prod(1 + x) - 1) * 100)),
    End_10k  = 10000 * tail(c_r, 1)
  )
}

yearly <- function(r) {
  d <- cal[cal >= S10 & cal <= E10]
  tapply(r, format(d, "%Y"), function(x) (prod(1 + x) - 1) * 100)
}

res <- list()
res[["Benchmark: QQQ Buy & Hold"]] <- rq[ev10]

# 1. Baseline: 30% QQQ + 70% Stocks (1.0x cash, top 5)
res[["Baseline: 30% QQQ / 70% Stock (1.0x, Top 5)"]] <- sim_custom(margin = 1.0, core_asset = "QQQ", core_pct = 0.30, top_n = 5, stop_to = "core")

# 2. Baseline: 30% QQQ + 70% Stocks (1.0x cash, top 3)
res[["Baseline: 30% QQQ / 70% Stock (1.0x, Top 3)"]] <- sim_custom(margin = 1.0, core_asset = "QQQ", core_pct = 0.30, top_n = 3, stop_to = "core")

# 3. 100% Stock (0% QQQ): 1.0x cash, Top 5
res[["100% Broad Stock (1.0x Cash, Top 5)"]] <- sim_custom(margin = 1.0, core_pct = 0.00, top_n = 5, stop_to = "cash")

# 4. 100% Broad Stock (1.0x Cash, Top 3)
res[["100% Broad Stock (1.0x Cash, Top 3)"]] <- sim_custom(margin = 1.0, core_pct = 0.00, top_n = 3, stop_to = "cash")

# 5. 100% Broad Stock (1.5x Margin, Top 5)
res[["100% Broad Stock (1.5x Margin, Top 5)"]] <- sim_custom(margin = 1.5, core_pct = 0.00, top_n = 5, stop_to = "cash")

# 6. 100% Broad Stock (1.5x Margin, Top 3)
res[["100% Broad Stock (1.5x Margin, Top 3)"]] <- sim_custom(margin = 1.5, core_pct = 0.00, top_n = 3, stop_to = "cash")

# 7. Supercharged Baseline: 30% QLD / 70% Stock (1.5x Margin, Top 5) - Current Active Option 6
res[["Active Opt6: 30% QLD / 70% Stock (1.5x, Top 5)"]] <- sim_custom(margin = 1.5, core_asset = "QLD", core_pct = 0.30, top_n = 5, stop_to = "core")

# 8. Active Opt6 with Top 3
res[["Active Opt6: 30% QLD / 70% Stock (1.5x, Top 3)"]] <- sim_custom(margin = 1.5, core_asset = "QLD", core_pct = 0.30, top_n = 3, stop_to = "core")

cat("\n========================================================================================\n")
cat(" 10-YEAR BACKTEST: 100% IN SYMBOLS_BROAD.TXT vs 30% CORE / 70% STOCK BASELINE\n")
cat(" Period: 2016-10-03 to 2026-10-06 (10 Full Years) | Initial Capital: $10,000\n")
cat("========================================================================================\n\n")

scorecard <- t(sapply(res, met2))
print(round(scorecard, 2))

cat("\n========================================================================================\n")
cat(" CALENDAR YEAR RETURNS (%)\n")
cat("========================================================================================\n")
cal_table <- sapply(res[c(1, 2, 4, 6, 8, 9)], yearly)
print(round(cal_table, 1))

# Write scorecard to CSV
write.csv(scorecard, "scratch/backtest_100pct_broad_10yr_scorecard.csv")
cat("\nScorecard saved to scratch/backtest_100pct_broad_10yr_scorecard.csv\n")
