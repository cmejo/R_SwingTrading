#!/usr/bin/env Rscript
# ==============================================================================
# scratch/test_crisis_min_variance.R
# Test Crisis-Mode Minimum Variance Portfolio (MVP) vs Cash / Gold / Buy & Hold
# ==============================================================================
suppressPackageStartupMessages({
  library(xts)
  library(zoo)
})

src <- readLines("scratch/compare_symbol_lists.R")
idx <- grep("^lists <- c", src)
eval(parse(text = paste(src[1:(idx - 1)], collapse = "\n")))

# Set universe to symbols_broad.txt
set_universe(read_list("symbols_broad.txt"))

S10 <- as.Date("2016-10-03")
E10 <- as.Date("2026-10-06")
S20 <- as.Date("2007-01-02")
E20 <- as.Date("2026-10-06")

# Quadratic solver for long-only Minimum Variance Portfolio
solve_min_var <- function(cov_mat, max_w = 0.35) {
  N <- ncol(cov_mat)
  if (N == 1) return(c(1.0))
  # Use solve() or quadprog if available, or analytical inverse-variance with shrinkage
  # Robust analytical approach: Ledoit-Wolf style diagonal shrinkage + inverse variance
  # or solve(Sigma) %*% 1 / (1^T solve(Sigma) 1) with non-negativity truncation
  inv_var <- 1 / diag(cov_mat)
  inv_var[!is.finite(inv_var) | inv_var < 0] <- 0
  if (sum(inv_var) <= 0) return(rep(1 / N, N))
  w <- inv_var / sum(inv_var)
  
  # Try exact QP if quadprog is installed
  if (requireNamespace("quadprog", quietly = TRUE)) {
    tryCatch({
      Dmat <- cov_mat + diag(1e-5, N) # regularization
      dvec <- rep(0, N)
      # Constraints: sum(w) == 1, w_i >= 0, w_i <= max_w
      Amat <- cbind(rep(1, N), diag(N), -diag(N))
      bvec <- c(1, rep(0, N), rep(-max_w, N))
      qp <- quadprog::solve.QP(Dmat, dvec, Amat, bvec, meq = 1)
      if (!is.null(qp$solution)) w <- pmax(0, qp$solution) / sum(pmax(0, qp$solution))
    }, error = function(e) NULL)
  }
  w
}

# Simulator supporting Crisis Minimum Variance Portfolio
sim_crisis_mvp <- function(margin = 1.0,
                           crisis_mode = "cash", # "cash", "gld", "min_var_stocks", "multi_asset_mvp"
                           top_n = 5,
                           mvp_n = 5,
                           core_pct = 0.0, # 0.0 = 100% stock, 0.30 = 30% core
                           core_asset = "QLD",
                           vt = 0.22,
                           use_vt = TRUE,
                           start = S10,
                           end = E10) {
  
  U <- syms
  ev <- which(cal >= start & cal <= end)
  sat_pct <- max(0, 1 - core_pct)
  sma_g <- SMAQ[["200"]]
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
    
    # 20% disaster stop
    for (s in names(sw)[sw > 0]) {
      hit <- (!is.na(ep[s]) && P[t, s] / ep[s] - 1 <= -0.20)
      if (isTRUE(hit)) {
        w <- sw[s]
        eq <- eq * (1 - COST * w)
        sw[s] <- 0
        # In disaster, moves to cash (or gold if bull)
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
      td <- 0
      
      ok <- U[MEM[s0, ] & !is.na(P[s0, ]) & !is.na(P[max(1, s0 - 252), ])]
      
      if (bull) {
        # ====================================================================
        # BULL REGIME: TOP-N RELATIVE STRENGTH MOMENTUM
        # ====================================================================
        if (length(ok) >= top_n) {
          r126 <- P[s0, ok] / P[s0 - 126, ok] - 1 - (q[s0] / q[s0 - 126] - 1)
          r63  <- P[s0, ok] / P[s0 - 63, ok]  - 1 - (q[s0] / q[s0 - 63] - 1)
          rs <- 0.50 * r126 + 0.50 * r63
          
          # 200 SMA trend filter
          okf <- ok[!is.na(SMA200[s0, ok]) & P[s0, ok] >= SMA200[s0, ok]]
          if (length(okf) >= top_n) { ok <- okf; rs <- rs[ok] }
          
          # 100% Vol Cap
          k2 <- ok[!is.na(VOL63[s0, ok]) & VOL63[s0, ok] <= 1.00]
          if (length(k2) >= top_n) { ok <- k2; rs <- rs[k2] }
          
          rk <- names(sort(rs, decreasing = TRUE))
          held <- intersect(names(sw)[sw > 0], rk[seq_len(min(length(rk), top_n))])
          top <- unique(c(held, rk))[1:top_n]
          top <- top[!is.na(top)]
          
          wts <- rep(1 / top_n, length(top))
          ts[top] <- sat_pct * sc * am * wts
          for (ne in setdiff(top, names(sw)[sw > 0])) ep[ne] <- P[t, ne]
        }
        td <- if (use_vt) max(0, 1 - ((if (bull) core_pct else 0) * sc + sat_pct * sc)) else 0
        
      } else {
        # ====================================================================
        # BEAR / CRISIS REGIME: CHANGING THE RULES!
        # ====================================================================
        if (crisis_mode == "cash") {
          # Traditional trend exit: 100% Cash / Deleveraged
          tc <- 0; ts[] <- 0; td <- 0
          
        } else if (crisis_mode == "gld") {
          # Flight to Gold: 100% GLD
          tc <- 0; ts[] <- 0; td <- 1.0
          
        } else if (crisis_mode == "min_var_stocks") {
          # Minimum Variance Equities: Stay invested in lowest-variance stocks!
          if (length(ok) >= mvp_n) {
            # Find the stocks with lowest realized volatility
            vol_ranks <- sort(VOL63[s0, ok])
            low_vol_syms <- names(head(vol_ranks, mvp_n * 2)) # Candidate pool
            
            # Extract 63-day return matrix
            R_sub <- R[(s0 - 62):s0, low_vol_syms, drop = FALSE]
            # Remove any columns with missing or zero data
            valid_cols <- apply(R_sub, 2, function(x) !any(is.na(x)) && sd(x) > 0)
            if (sum(valid_cols) >= mvp_n) {
              low_vol_syms <- names(valid_cols)[valid_cols][1:mvp_n]
              cov_mat <- cov(R_sub[, low_vol_syms, drop = FALSE])
              w_mvp <- solve_min_var(cov_mat)
              
              # Allocate 100% (or sat_pct) across the Minimum Variance Portfolio!
              ts[low_vol_syms] <- sat_pct * w_mvp
              for (ne in setdiff(low_vol_syms, names(sw)[sw > 0])) ep[ne] <- P[t, ne]
            }
          }
          tc <- 0
          td <- 0
          
        } else if (crisis_mode == "multi_asset_mvp") {
          # Multi-Asset MVP: Low-vol equities + Gold + Cash
          if (length(ok) >= 3) {
            vol_ranks <- sort(VOL63[s0, ok])
            low_vol_syms <- names(head(vol_ranks, 3))
            
            # Combine returns of low-vol stocks + GLD
            R_sub <- cbind(R[(s0 - 62):s0, low_vol_syms, drop = FALSE], GLD = r_gld[(s0 - 62):s0])
            valid_cols <- apply(R_sub, 2, function(x) !any(is.na(x)) && sd(x) > 0)
            if (sum(valid_cols) >= 3) {
              cov_mat <- cov(R_sub[, valid_cols, drop = FALSE])
              w_mvp <- solve_min_var(cov_mat, max_w = 0.50)
              
              stock_syms <- intersect(names(w_mvp), low_vol_syms)
              for (sym_mv in stock_syms) {
                ts[sym_mv] <- w_mvp[sym_mv]
                if (!is.na(ts[sym_mv]) && ts[sym_mv] > 0 && !(sym_mv %in% names(sw)[sw > 0])) {
                  ep[sym_mv] <- P[t, sym_mv]
                }
              }
              if ("GLD" %in% names(w_mvp)) td <- w_mvp["GLD"]
            }
          }
          tc <- 0
        }
      }
      
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

met2 <- function(r, start_d = S10, end_d = E10) {
  c_r <- cumprod(1 + r)
  n <- length(r)
  dd <- c_r / cummax(c_r) - 1
  d <- cal[cal >= start_d & cal <= end_d]
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

yearly <- function(r, start_d = S10, end_d = E10) {
  d <- cal[cal >= start_d & cal <= end_d]
  tapply(r, format(d, "%Y"), function(x) (prod(1 + x) - 1) * 100)
}

# Run 10-Year Test (2016-2026)
cat("Running 10-Year Backtests...\n")
ev10 <- which(cal >= S10 & cal <= E10)
res10 <- list()
res10[["Benchmark: QQQ Buy & Hold"]] <- rq[ev10]
res10[["Baseline: Bear -> 100% Cash"]] <- sim_crisis_mvp(margin = 1.0, crisis_mode = "cash", core_pct = 0.0, top_n = 5, start = S10, end = E10)
res10[["Baseline: Bear -> 100% Gold (GLD)"]] <- sim_crisis_mvp(margin = 1.0, crisis_mode = "gld", core_pct = 0.0, top_n = 5, start = S10, end = E10)
res10[["INCORPORATED: Bear -> Equity Minimum Variance (MVP)"]] <- sim_crisis_mvp(margin = 1.0, crisis_mode = "min_var_stocks", core_pct = 0.0, top_n = 5, mvp_n = 5, start = S10, end = E10)
res10[["INCORPORATED: Bear -> Multi-Asset MVP (Low-Vol + Gold)"]] <- sim_crisis_mvp(margin = 1.0, crisis_mode = "multi_asset_mvp", core_pct = 0.0, top_n = 5, start = S10, end = E10)

# Also test with 1.5x Margin
res10[["1.5x Margin: Bear -> Cash"]] <- sim_crisis_mvp(margin = 1.5, crisis_mode = "cash", core_pct = 0.0, top_n = 5, start = S10, end = E10)
res10[["1.5x Margin: Bear -> Equity MVP"]] <- sim_crisis_mvp(margin = 1.5, crisis_mode = "min_var_stocks", core_pct = 0.0, top_n = 5, mvp_n = 5, start = S10, end = E10)

cat("\n========================================================================================\n")
cat(" 10-YEAR SCORECARD (2016 - 2026): CRISIS MINIMUM VARIANCE vs CASH / GOLD\n")
cat("========================================================================================\n")
sc10 <- t(sapply(res10, met2, start_d = S10, end_d = E10))
print(round(sc10, 2))

cat("\n========================================================================================\n")
cat(" CALENDAR YEAR RETURNS (%)\n")
cat("========================================================================================\n")
print(round(sapply(res10, yearly, start_d = S10, end_d = E10), 1))

# Now Run Full 20-Year Test (2007-2026) to see 2008 Great Financial Crisis!
cat("\n\nRunning 20-Year Backtests (2007-2026 including 2008 Great Financial Crisis)...\n")
ev20 <- which(cal >= S20 & cal <= E20)
res20 <- list()
res20[["Benchmark: QQQ Buy & Hold"]] <- rq[ev20]
res20[["Baseline: Bear -> 100% Cash"]] <- sim_crisis_mvp(margin = 1.0, crisis_mode = "cash", core_pct = 0.0, top_n = 5, start = S20, end = E20)
res20[["Baseline: Bear -> 100% Gold (GLD)"]] <- sim_crisis_mvp(margin = 1.0, crisis_mode = "gld", core_pct = 0.0, top_n = 5, start = S20, end = E20)
res20[["INCORPORATED: Bear -> Equity Minimum Variance (MVP)"]] <- sim_crisis_mvp(margin = 1.0, crisis_mode = "min_var_stocks", core_pct = 0.0, top_n = 5, mvp_n = 5, start = S20, end = E20)
res20[["INCORPORATED: Bear -> Multi-Asset MVP (Low-Vol + Gold)"]] <- sim_crisis_mvp(margin = 1.0, crisis_mode = "multi_asset_mvp", core_pct = 0.0, top_n = 5, start = S20, end = E20)

cat("\n========================================================================================\n")
cat(" 20-YEAR SCORECARD (2007 - 2026): CRISIS MINIMUM VARIANCE vs CASH / GOLD\n")
cat("========================================================================================\n")
sc20 <- t(sapply(res20, met2, start_d = S20, end_d = E20))
print(round(sc20, 2))

cat("\n========================================================================================\n")
cat(" 2008 & 2022 CRISIS YEARS BREAKDOWN (%)\n")
cat("========================================================================================\n")
cal20 <- sapply(res20, yearly, start_d = S20, end_d = E20)
print(round(cal20[c("2008", "2011", "2018", "2020", "2022"), ], 1))

# Save results
write.csv(sc10, "scratch/crisis_mvp_10yr_scorecard.csv")
write.csv(sc20, "scratch/crisis_mvp_20yr_scorecard.csv")
