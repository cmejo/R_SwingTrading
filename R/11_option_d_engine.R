#' ==============================================================================
#' R/11_option_d_engine.R
#' Parametrized Option D backtest engine (generalizes sim_buffer_engine() from
#' scratch/test_buffer_leverage_synergies.R).
#'
#' The engine takes all market data through a list D, so it runs unchanged on the
#' point-in-time Nasdaq-100 universe, on a static symbol list, or on synthetic data
#' in tests. With option_d_config(preset = "legacy") it reproduces the original
#' sim_buffer_engine() return stream exactly (see tests/test_option_d_parity.R).
#'
#' Required fields of D (all aligned to D$cal, rows oldest -> newest):
#'   cal, syms, P (prices, NA before listing), R (simple returns, NA -> 0),
#'   MEM (logical membership), q (benchmark close), rq (benchmark returns),
#'   r_gld, rf_daily, VOL63, vol20, month_end
#' ==============================================================================

if (!exists("ewma_cov")) source("R/10_option_d_features.R")
if (!exists("lmMA"))     source("R/02_trend_lmMA.R")

#' Broker financing / maintenance presets.
#'   legacy : the assumption every prior scorecard used (T-bill + 1.75%, no calls)
#'   ibkr   : IBKR Pro, benchmark + 1.5% (5.38% effective, Nov 2026)
#'   schwab : Schwab debit < $25k, 12.075% effective (Nov 2026) = benchmark + ~8.2%
#'            using the same benchmark implied by the IBKR quote. Schwab's discretionary
#'            50% house requirement on volatile names is NOT modeled (optimistic).
broker_preset <- function(broker = c("legacy", "ibkr", "schwab")) {
  broker <- match.arg(broker)
  switch(broker,
    legacy = list(borrow_spread = 0.0175, maintenance = NA_real_),
    ibkr   = list(borrow_spread = 0.0150, maintenance = 0.30),
    schwab = list(borrow_spread = 0.0820, maintenance = 0.30))
}

#' Engine configuration. Defaults = Option D as deployed, with legacy financing.
option_d_config <- function(...) {
  base <- list(
    top_n = 5, buffer_rank = 8, stop_pct = 0.25,
    base_margin = 1.50, turbo_margin = 2.00, use_turbo = TRUE,
    vt = 0.22, vol_cap = 1.00, lb_long = 126, lb_short = 63, w_long = 0.50,
    cost = 0.0010,
    # financing / margin
    borrow_spread = 0.0175, maintenance = NA_real_, margin_call_to = 1.0,
    # stop replacement: "slot" = act_lev / top_n (original), "proceeds" = stopped weight (live)
    stop_replacement = TRUE, stop_replace_size = "slot",
    # live-parity switches
    drift_band = 0.0,          # 0 = reset kept names to target each month; 0.15 = live
    residual_to = "tbill",     # "tbill" (original) or "gld" (live: 1 - vol_scale into GLD)
    # Feature 1
    leverage_mode = "turbo",   # "turbo" or "risk_kelly"
    sigma_target = 0.30, max_leverage = 2.0, kelly_fraction = 0.5,
    mu_mode = "expanding",     # "expanding" (own unlevered sleeve history) or "fixed"
    mu_hat = NA_real_, mu_min_obs = 504, cov_window = 126, ewma_lambda = 0.94,
    # Feature 2 / 3
    rank_mode = "raw",         # "raw" or "residual"
    skip = 0, resid_est = 504,
    # Frog-in-the-pan blend, identical to core_satellite_signal.R (USE_FIP):
    # score = (1 - fip_weight) * base + fip_weight * (FIP_63d - 0.5). Live uses 0.30.
    fip_weight = 0, fip_lookback = 63
  )
  modifyList(base, list(...))
}

#' Precompute regime series used by the engine.
prepare_option_d_data <- function(D, lmma_window = 50, rsq_threshold = 0.15) {
  q <- D$q
  D$sma50_q  <- as.numeric(stats::filter(q, rep(1 / 50, 50), sides = 1))
  D$sma200_q <- as.numeric(stats::filter(q, rep(1 / 200, 200), sides = 1))
  lm <- lmMA(xts::xts(q, order.by = D$cal), lmma_window)
  sl <- as.numeric(lm$slope); r2 <- as.numeric(lm$r.squared)
  D$benchmark_bull <- !(sl < 0 & r2 > rsq_threshold)
  sb <- q > D$sma50_q & D$sma50_q > D$sma200_q & sl > 0
  sb[is.na(sb)] <- FALSE
  D$super_bull <- sb
  D
}

#' Ranking score for candidates `ok` at signal bar s0 (higher = better).
option_d_rank_scores <- function(D, cfg, s0, ok) {
  base <- option_d_base_scores(D, cfg, s0, ok)
  if (cfg$fip_weight <= 0) return(base)
  idx <- max(1, s0 - cfg$fip_lookback):s0
  fip <- vapply(ok, function(s) calc_fip_ratio(D$P[idx, s], lookback = cfg$fip_lookback), numeric(1))
  setNames((1 - cfg$fip_weight) * base + cfg$fip_weight * (fip - 0.50), ok)
}

option_d_base_scores <- function(D, cfg, s0, ok) {
  if (cfg$rank_mode == "residual") {
    idx <- max(2, s0 - cfg$resid_est + 1):s0
    m <- D$rq[idx]
    sc <- vapply(ok, function(s) {
      px <- D$P[idx, s]; pv <- D$P[idx - 1, s]
      r <- D$R[idx, s]; r[is.na(px) | is.na(pv)] <- NA
      residual_dual_score(r, m, cfg$resid_est, cfg$lb_long, cfg$lb_short, cfg$w_long, cfg$skip)
    }, numeric(1))
    return(setNames(sc, ok))
  }
  e <- s0 - cfg$skip
  P <- D$P; q <- D$q
  r_l <- P[e, ok] / P[s0 - cfg$lb_long, ok] - 1 - (q[e] / q[s0 - cfg$lb_long] - 1)
  r_s <- P[e, ok] / P[s0 - cfg$lb_short, ok] - 1 - (q[e] / q[s0 - cfg$lb_short] - 1)
  setNames(cfg$w_long * r_l + (1 - cfg$w_long) * r_s, ok)
}

#' Run the Option D engine. Returns daily strategy returns with diagnostics attached.
sim_option_d <- function(D, cfg = option_d_config(), start = min(D$cal), end = max(D$cal)) {
  cfg <- modifyList(option_d_config(), cfg)
  if (is.null(D$super_bull)) D <- prepare_option_d_data(D)
  U <- D$syms; P <- D$P; R <- D$R; MEM <- D$MEM
  ev <- which(D$cal >= start & D$cal <= end)
  ev <- ev[ev >= 2]
  borrow_daily <- D$rf_daily + cfg$borrow_spread / 252
  top_n <- cfg$top_n

  sw <- setNames(rep(0, length(U)), U); dw <- 0
  ep <- setNames(rep(NA_real_, length(U)), U)
  eq <- 1; turn <- 0; n_calls <- 0
  out <- numeric(length(ev)); gross_hist <- numeric(length(ev))
  sleeve <- numeric(length(ev)); n_sleeve <- 0
  lev_log <- list()

  for (k in seq_along(ev)) {
    t <- ev[k]; prev <- eq
    gross <- sum(sw) + dw
    stock_ret <- sum(sw * R[t, ])
    g <- stock_ret + dw * D$r_gld[t] - max(0, gross - 1) * borrow_daily[t] + max(0, 1 - gross) * D$rf_daily[t]
    if (sum(sw) > 0) { n_sleeve <- n_sleeve + 1; sleeve[n_sleeve] <- stock_ret / sum(sw) }
    if (1 + g <= 0) {                      # account wiped out
      out[k] <- -1; eq <- 0
      attr(out, "wiped_out") <- as.character(D$cal[t])
      break
    }
    eq <- eq * (1 + g)
    sw <- sw * (1 + R[t, ]) / (1 + g)
    dw <- dw * (1 + D$r_gld[t]) / (1 + g)

    # 0. Maintenance-margin breach at the close -> forced deleverage
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

    # 1. Disaster stops (close-to-entry)
    stopped <- character(); stopped_w <- numeric()
    for (s in names(sw)[sw > 0]) {
      if (isTRUE(!is.na(ep[s]) && P[t, s] / ep[s] - 1 <= -cfg$stop_pct)) {
        w <- sw[[s]]
        eq <- eq * (1 - cfg$cost * w); turn <- turn + w
        sw[s] <- 0; ep[s] <- NA
        stopped <- c(stopped, s); stopped_w <- c(stopped_w, w)
      }
    }

    # 2. Immediate stop replacement (bull regime only)
    if (cfg$stop_replacement && length(stopped) > 0 && bull) {
      ok <- U[MEM[s0, ] & !is.na(P[s0, ]) & !is.na(P[max(1, s0 - 126), ])]
      if (length(ok) >= 10) {
        rs <- option_d_rank_scores(D, cfg, s0, ok)
        k2 <- ok[!is.na(D$VOL63[s0, ok]) & D$VOL63[s0, ok] <= cfg$vol_cap]
        if (length(k2) >= 10) { ok <- k2; rs <- rs[k2] }
        rk <- names(sort(rs, decreasing = TRUE))
        unheld <- setdiff(rk, c(names(sw)[sw > 0], stopped))
        act_lev <- if (cfg$use_turbo && is_sb) cfg$turbo_margin else cfg$base_margin
        reps <- head(unheld, length(stopped))
        for (i in seq_along(reps)) {
          w_new <- if (cfg$stop_replace_size == "slot" && cfg$leverage_mode == "turbo")
            act_lev / top_n else stopped_w[i]
          sw[reps[i]] <- w_new; ep[reps[i]] <- P[t, reps[i]]
          eq <- eq * (1 - cfg$cost * w_new); turn <- turn + w_new
        }
      }
    }

    # 3. Monthly rebalance
    if (D$month_end[s0] || k == 1) {
      sc <- min(1, cfg$vt / max(0.05, D$vol20[s0]))
      am <- 1.0
      if (bull) am <- if (cfg$use_turbo && is_sb) cfg$turbo_margin else cfg$base_margin
      unlev <- sc
      ts <- setNames(rep(0, length(U)), U)
      ok <- U[MEM[s0, ] & !is.na(P[s0, ]) & !is.na(P[max(1, s0 - 252), ])]
      sel <- character(); wts <- numeric()

      if (bull) {
        if (length(ok) >= top_n) {
          rs <- option_d_rank_scores(D, cfg, s0, ok)
          k2 <- ok[!is.na(D$VOL63[s0, ok]) & D$VOL63[s0, ok] <= cfg$vol_cap]
          if (length(k2) >= top_n) { ok <- k2; rs <- rs[k2] }
          rk <- names(sort(rs, decreasing = TRUE))
          if (cfg$buffer_rank > top_n) {
            kept <- intersect(names(sw)[sw > 0], rk[1:min(length(rk), cfg$buffer_rank)])
            top <- c(kept, head(setdiff(rk, kept), max(0, top_n - length(kept))))[1:top_n]
          } else {
            held <- intersect(names(sw)[sw > 0], rk[1:top_n])
            top <- unique(c(held, rk))[1:top_n]
          }
          sel <- top[!is.na(top)]
          wts <- rep(1 / length(sel), length(sel))
        }
      } else if (length(ok) >= 5) {
        vol_ranks <- sort(D$VOL63[s0, ok])
        low_vol <- names(head(vol_ranks, 10))
        R_sub <- R[(s0 - 62):s0, low_vol, drop = FALSE]
        valid <- apply(R_sub, 2, function(x) !any(is.na(x)) && stats::sd(x) > 0)
        if (sum(valid) >= 5) {
          sel <- names(valid)[valid][1:5]
          wts <- solve_min_var(stats::cov(R_sub[, sel, drop = FALSE]))
        }
      }

      if (length(sel) > 0) {
        if (cfg$leverage_mode == "risk_kelly") {
          Xw <- R[max(1, s0 - cfg$cov_window + 1):s0, sel, drop = FALSE]
          sigp <- portfolio_vol(wts, ewma_cov(Xw, cfg$ewma_lambda))
          mu <- if (cfg$mu_mode == "fixed") cfg$mu_hat else
            if (n_sleeve >= cfg$mu_min_obs) mean(sleeve[1:n_sleeve]) * 252 else NA_real_
          lk <- risk_kelly_leverage(sigp, cfg$sigma_target, cfg$max_leverage, bull = bull,
                                    mu_hat = mu, borrow_rate = borrow_daily[s0] * 252,
                                    kelly_fraction = cfg$kelly_fraction, no_mu_cap = cfg$base_margin)
          unlev <- min(1, lk$L); am <- if (unlev > 0) lk$L / unlev else 1
          lev_log[[length(lev_log) + 1]] <- data.frame(date = D$cal[t], L = lk$L,
                                                       binding = lk$binding, sigma_p = sigp, mu_hat = mu)
        }
        ts[sel] <- unlev * am * wts
        for (ne in setdiff(sel, names(sw)[sw > 0])) ep[ne] <- P[t, ne]
      }

      if (cfg$drift_band > 0) {
        for (s in names(ts)[ts > 0 & sw > 0]) {
          if (abs(sw[[s]] - ts[[s]]) < cfg$drift_band * ts[[s]]) ts[s] <- sw[[s]]
        }
      }
      td <- if (cfg$residual_to == "gld") max(0, 1 - unlev) else 0

      to <- sum(abs(ts - sw)) + abs(td - dw)
      eq <- eq * (1 - cfg$cost * to); turn <- turn + to
      sw <- ts; dw <- td
    }

    out[k] <- eq / prev - 1
    gross_hist[k] <- sum(sw) + dw
  }

  attr(out, "dates")        <- D$cal[ev]
  attr(out, "turnover_yr")  <- turn / (length(ev) / 252)
  attr(out, "margin_calls") <- n_calls
  attr(out, "avg_gross")    <- mean(gross_hist)
  attr(out, "leverage_log") <- if (length(lev_log)) do.call(rbind, lev_log) else NULL
  out
}

#' Standard scorecard for a daily return stream.
option_d_metrics <- function(r, rf_daily = 0) {
  c_r <- cumprod(1 + r); n <- length(r)
  ex <- r - rf_daily
  c(CAGR    = (tail(c_r, 1)^(252 / n) - 1) * 100,
    MaxDD   = -min(c_r / cummax(c_r) - 1) * 100,
    Vol     = stats::sd(r) * sqrt(252) * 100,
    Sharpe  = if (stats::sd(r) > 0) mean(ex) / stats::sd(r) * sqrt(252) else 0,
    Calmar  = { dd <- -min(c_r / cummax(c_r) - 1); if (dd > 0) ((tail(c_r, 1)^(252 / n) - 1)) / dd else NA },
    MarginCalls = if (!is.null(attr(r, "margin_calls"))) attr(r, "margin_calls") else NA,
    Ruined      = as.numeric(!is.null(attr(r, "wiped_out"))),
    AvgGross    = if (!is.null(attr(r, "avg_gross"))) attr(r, "avg_gross") else NA,
    Turnover    = if (!is.null(attr(r, "turnover_yr"))) attr(r, "turnover_yr") else NA)
}
