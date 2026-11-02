source("R/02_trend_lmMA.R")
source("R/10_option_d_features.R")
source("R/11_option_d_engine.R")

# Load Broad Universe
src <- readLines("scratch/compare_symbol_lists.R")
idx <- grep("^lists <- c", src)
eval(parse(text = src[1:(idx - 1)]))
invisible(set_universe(read_list("symbols_broad.txt")))

D <- prepare_option_d_data(list(cal = cal, syms = syms, P = P, R = R, MEM = MEM, q = q, rq = rq,
                                r_gld = r_gld, rf_daily = rf_daily, VOL63 = VOL63, vol20 = vol20,
                                month_end = month_end))

# Load TSMOM sleeve returns
tsmom_data <- readRDS("scratch/tsmom_sleeve_returns.rds")
tsmom_ls <- tsmom_data$tsmom_ls
tsmom_lo <- tsmom_data$tsmom_lo

# Custom simulation function that allows swapping the bear-regime sleeve
sim_bear_sleeve_portfolio <- function(D, mode = c("low_vol_equity", "cash", "gld", "tsmom_ls", "tsmom_lo"),
                                     start_date = "2007-01-03", end_date = "2026-10-06",
                                     base_margin = 1.50, turbo_margin = 1.60, use_turbo = TRUE,
                                     maintenance = 0.30, borrow_spread = 0.0820) {
  mode <- match.arg(mode)
  cfg <- option_d_config(
    preset = "legacy",
    base_margin = base_margin,
    turbo_margin = turbo_margin,
    use_turbo = use_turbo,
    maintenance = maintenance,
    borrow_spread = borrow_spread,
    drift_band = 0.15,
    residual_to = "gld",
    stop_replace_size = "proceeds"
  )
  
  P <- D$P; R <- D$R; U <- D$syms; MEM <- D$MEM
  ev <- which(D$cal >= as.Date(start_date) & D$cal <= as.Date(end_date))
  ev <- ev[ev >= 2]
  
  eq <- 1.0; sw <- setNames(rep(0, length(U)), U); ep <- setNames(rep(NA_real_, length(U)), U)
  dw <- 0; out <- numeric(length(ev)); turn <- 0; n_calls <- 0
  borrow_daily <- (D$rf_daily + cfg$borrow_spread / 252)
  gross_hist <- numeric(length(ev))
  
  for (k in seq_along(ev)) {
    t <- ev[k]; prev <- eq
    gross <- sum(sw) + dw
    stock_ret <- sum(sw * R[t, ])
    
    # Return on defensive/bear sleeve
    def_ret <- if (mode == "gld") D$r_gld[t]
               else if (mode == "cash") D$rf_daily[t]
               else if (mode == "tsmom_ls") tsmom_ls[t]
               else if (mode == "tsmom_lo") tsmom_lo[t]
               else D$r_gld[t] # for low_vol_equity residual
    
    g <- stock_ret + dw * def_ret - max(0, gross - 1) * borrow_daily[t] + max(0, 1 - gross) * D$rf_daily[t]
    
    if (1 + g <= 0) { out[k] <- -1; eq <- 0; break }
    eq <- eq * (1 + g)
    sw <- sw * (1 + R[t, ]) / (1 + g)
    dw <- dw * (1 + def_ret) / (1 + g)
    
    # Maintenance call check
    if (is_margin_breach(sum(sw) + dw, cfg$maintenance)) {
      n_calls <- n_calls + 1
      gr <- sum(sw) + dw; f <- cfg$margin_call_to / gr
      sold <- gr * (1 - f)
      eq <- eq * (1 - cfg$cost * sold); turn <- turn + sold
      sw <- sw * f; dw <- dw * f
    }
    
    s0 <- t - 1
    is_sb <- isTRUE(D$super_bull[s0])
    bull  <- isTRUE(D$benchmark_bull[s0])
    
    # 1. Disaster stops (in bull regime)
    stopped <- character(); stopped_w <- numeric()
    for (s in names(sw)[sw > 0]) {
      if (isTRUE(!is.na(ep[s]) && P[t, s] / ep[s] - 1 <= -cfg$stop_pct)) {
        w <- sw[[s]]
        eq <- eq * (1 - cfg$cost * w); turn <- turn + w
        sw[s] <- 0; ep[s] <- NA
        stopped <- c(stopped, s); stopped_w <- c(stopped_w, w)
      }
    }
    
    # Immediate stop replacement (bull only)
    if (cfg$stop_replacement && length(stopped) > 0 && bull) {
      ok <- U[MEM[s0, ] & !is.na(P[s0, ]) & !is.na(P[max(1, s0 - 126), ])]
      if (length(ok) >= 10) {
        rs <- option_d_rank_scores(D, cfg, s0, ok)
        k2 <- ok[!is.na(D$VOL63[s0, ok]) & D$VOL63[s0, ok] <= cfg$vol_cap]
        if (length(k2) >= 10) { ok <- k2; rs <- rs[k2] }
        rk <- names(sort(rs, decreasing = TRUE))
        unheld <- setdiff(rk, c(names(sw)[sw > 0], stopped))
        reps <- head(unheld, length(stopped))
        for (i in seq_along(reps)) {
          w_new <- stopped_w[i]
          sw[reps[i]] <- w_new; ep[reps[i]] <- P[t, reps[i]]
          eq <- eq * (1 - cfg$cost * w_new); turn <- turn + w_new
        }
      }
    }
    
    # Monthly rebalance
    if (D$month_end[s0] || k == 1) {
      sc <- min(1, cfg$vt / max(0.05, D$vol20[s0]))
      am <- 1.0
      if (bull) am <- if (cfg$use_turbo && is_sb) cfg$turbo_margin else cfg$base_margin
      unlev <- sc
      ts <- setNames(rep(0, length(U)), U)
      td <- 0
      
      if (bull) {
        ok <- U[MEM[s0, ] & !is.na(P[s0, ]) & !is.na(P[max(1, s0 - 252), ])]
        if (length(ok) >= cfg$top_n) {
          rs <- option_d_rank_scores(D, cfg, s0, ok)
          k2 <- ok[!is.na(D$VOL63[s0, ok]) & D$VOL63[s0, ok] <= cfg$vol_cap]
          if (length(k2) >= cfg$top_n) { ok <- k2; rs <- rs[k2] }
          rk <- names(sort(rs, decreasing = TRUE))
          kept <- intersect(names(sw)[sw > 0], rk[1:min(length(rk), cfg$buffer_rank)])
          top <- c(kept, head(setdiff(rk, kept), max(0, cfg$top_n - length(kept))))[1:cfg$top_n]
          sel <- top[!is.na(top)]
          wts <- rep(1 / length(sel), length(sel))
          ts[sel] <- unlev * am * wts
          for (ne in setdiff(sel, names(sw)[sw > 0])) ep[ne] <- P[t, ne]
        }
        td <- max(0, 1 - unlev) # residual to GLD
      } else {
        # BEAR REGIME HANDLING
        if (mode == "low_vol_equity") {
          ok <- U[MEM[s0, ] & !is.na(P[s0, ]) & !is.na(P[max(1, s0 - 252), ])]
          if (length(ok) >= 5) {
            vol_ranks <- sort(D$VOL63[s0, ok])
            low_vol <- names(head(vol_ranks, 10))
            R_sub <- R[(s0 - 62):s0, low_vol, drop = FALSE]
            valid <- apply(R_sub, 2, function(x) !any(is.na(x)) && stats::sd(x) > 0)
            if (sum(valid) >= 5) {
              sel <- names(valid)[valid][1:5]
              wts <- solve_min_var(stats::cov(R_sub[, sel, drop = FALSE]))
              ts[sel] <- unlev * am * wts
            }
          }
          td <- max(0, 1 - unlev)
        } else {
          # Non-equity bear sleeves: 100% allocation into the defensive engine
          # No equity holdings in bear regime (ts remains all 0)
          td <- 1.0 # 100% to the chosen defensive sleeve (cash, gld, tsmom_ls, tsmom_lo)
        }
      }
      
      to <- sum(abs(ts - sw)) + abs(td - dw)
      eq <- eq * (1 - cfg$cost * to); turn <- turn + to
      sw <- ts; dw <- td
    }
    
    out[k] <- eq / prev - 1
    gross_hist[k] <- sum(sw) + dw
  }
  
  attr(out, "dates") <- D$cal[ev]
  attr(out, "margin_calls") <- n_calls
  attr(out, "turnover_yr")  <- turn / (length(ev) / 252)
  out
}

# Run all 5 variants
modes <- c("low_vol_equity", "cash", "gld", "tsmom_ls", "tsmom_lo")
results <- list()

for (m in modes) {
  r_full <- sim_bear_sleeve_portfolio(D, mode = m, start_date = "2007-01-03", end_date = "2026-10-06")
  ev_full <- which(D$cal >= as.Date("2007-01-03") & D$cal <= as.Date("2026-10-06")); ev_full <- ev_full[ev_full >= 2]
  m_full <- option_d_metrics(r_full, D$rf_daily[ev_full])
  
  # Measure 2008 GFC specific drawdown
  ev_gfc <- which(D$cal >= as.Date("2007-10-01") & D$cal <= as.Date("2009-03-31"))
  r_gfc <- r_full[match(D$cal[ev_gfc], attr(r_full, "dates"))]
  r_gfc <- r_gfc[!is.na(r_gfc)]
  cum_gfc <- prod(1 + r_gfc) - 1
  dd_gfc <- -min(cumprod(1 + r_gfc) / cummax(cumprod(1 + r_gfc)) - 1) * 100
  
  # Measure 2022 Fed rate hike specific drawdown
  ev_2022 <- which(D$cal >= as.Date("2022-01-01") & D$cal <= as.Date("2022-12-31"))
  r_2022 <- r_full[match(D$cal[ev_2022], attr(r_full, "dates"))]
  r_2022 <- r_2022[!is.na(r_2022)]
  cum_2022 <- prod(1 + r_2022) - 1
  dd_2022 <- -min(cumprod(1 + r_2022) / cummax(cumprod(1 + r_2022)) - 1) * 100
  
  # 10-Year Test period (2017-2026)
  ev_test <- which(D$cal >= as.Date("2017-01-01") & D$cal <= as.Date("2026-10-06"))
  r_test <- r_full[match(D$cal[ev_test], attr(r_full, "dates"))]
  r_test <- r_test[!is.na(r_test)]
  m_test <- option_d_metrics(r_test, D$rf_daily[ev_test])
  
  results[[m]] <- data.frame(
    Sleeve_Mode = m,
    Full_CAGR = round(m_full["CAGR"], 2),
    Full_MaxDD = round(m_full["MaxDD"], 2),
    Full_Sharpe = round(m_full["Sharpe"], 3),
    Full_Calmar = round(m_full["Calmar"], 3),
    Test_10Yr_CAGR = round(m_test["CAGR"], 2),
    Test_10Yr_MaxDD = round(m_test["MaxDD"], 2),
    GFC_2008_Ret = round(cum_gfc * 100, 1),
    GFC_2008_MaxDD = round(dd_gfc, 1),
    Ret_2022 = round(cum_2022 * 100, 1),
    MaxDD_2022 = round(dd_2022, 1),
    Turnover = round(m_full["Turnover"], 1)
  )
}

res_table <- do.call(rbind, results)
write.csv(res_table, "scratch/scorecard_tsmom_vs_lowvol_crisis_sleeve.csv", row.names = FALSE)
print(res_table)
