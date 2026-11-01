#!/usr/bin/env Rscript
# ==============================================================================
# scratch/compare_earnings_blackout_models.R
# Comprehensive 20-Year Empirical Comparison of Earnings Blackout Periods
# Evaluates:
#   1. No Blackout (Hold Through Earnings)
#   2. Entry Filter (Do not buy if earnings within 5 days)
#   3. Blackout [-1 day, +1 day] (Exit 1 day before, hold in GLD, re-enter 1 day after)
#   4. Blackout [-2 days, +1 day] (Exit 2 days before, hold in GLD, re-enter 1 day after)
#   5. Blackout [-3 days, +1 day] (Exit 3 days before, hold in GLD, re-enter 1 day after)
# ==============================================================================

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(jsonlite)
})

HIST <- "data/data_historical"
ART  <- "/Users/cmejo/.gemini/antigravity/brain/64019830-0cac-4bb1-904e-d4560e71468c"

EVAL_START <- as.Date("2006-10-02")
EVAL_END   <- as.Date("2026-10-06")
COST       <- 0.0010
MARGIN_SPREAD <- 0.0175

adj <- function(sym) {
  p <- file.path(HIST, sprintf("data_%s.rds", tolower(sym)))
  if (!file.exists(p)) return(NULL)
  d <- readRDS(p)
  a <- d[, grep("Adjusted$", colnames(d))]
  a[a <= 0] <- NA
  a
}

qqq_p <- adj("QQQ")
qld_p <- adj("QLD")
gld_p <- adj("GLD_FULL")

cal <- index(qqq_p)[index(qqq_p) >= as.Date("2004-01-01") & index(qqq_p) <= EVAL_END]

# QQQ returns
q <- as.numeric(qqq_p[cal])
rq <- c(0, q[-1] / q[-length(q)] - 1)
sma200_q <- as.numeric(stats::filter(q, rep(1/200, 200), sides = 1))
qqq_trend_up <- q >= sma200_q; qqq_trend_up[is.na(qqq_trend_up)] <- TRUE

# QLD returns
m_qld <- match(cal, as.Date(index(qld_p)))
qld_px <- as.numeric(qld_p)[m_qld]
qld_px <- zoo::na.locf(qld_px, na.rm = FALSE)
r_qld <- c(0, qld_px[-1] / qld_px[-length(qld_px)] - 1)
r_qld[is.na(r_qld)] <- 2 * rq[is.na(r_qld)]

# GLD returns
m_gld <- match(cal, as.Date(index(gld_p)))
gld_px <- as.numeric(gld_p)[m_gld]
gld_px <- zoo::na.locf(gld_px, na.rm = FALSE)
r_gld <- c(0, gld_px[-1] / gld_px[-length(gld_px)] - 1); r_gld[is.na(r_gld)] <- 0

# Risk-free & margin borrowing rates
irx <- readRDS(file.path(HIST, "data_^irx.rds"))
rf  <- as.numeric(zoo::na.locf(irx[, 4], na.rm = FALSE)[cal])
rf  <- zoo::na.locf(rf, na.rm = FALSE); rf[is.na(rf)] <- 2
rf_rate <- rf / 100; rf_daily <- rf_rate / 252
margin_borrow_daily <- (rf_rate + MARGIN_SPREAD) / 252

# Universe stocks
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

R  <- rbind(NA, P[-1, , drop = FALSE] / P[-nrow(P), , drop = FALSE] - 1); R[is.na(R)] <- 0
roll_vol_20 <- rollapply(rq, width = 20, FUN = function(x) sd(x) * sqrt(252), align = "right", fill = 0.20)

ev <- which(cal >= EVAL_START)
dates <- cal[ev]
month_end <- c(format(cal[-1], "%m") != format(cal[-length(cal)], "%m"), TRUE)

# Load Historical Earnings Dates
earn_json <- jsonlite::fromJSON("data/historical_earnings_dates.json")

# Build a fast lookup function for earnings blackout
# pre_days: trading days before earnings to exit
# post_days: trading days after earnings before allowing re-entry
make_earnings_blackout_matrix <- function(pre_days = 2, post_days = 1) {
  # matrix: length(cal) x ncol(P)
  mat <- matrix(FALSE, length(cal), ncol(P), dimnames = list(NULL, colnames(P)))
  cal_dates <- as.Date(cal)
  
  for (sym in colnames(P)) {
    e_dates_str <- earn_json[[sym]]
    if (is.null(e_dates_str) || length(e_dates_str) == 0) next
    e_dates <- as.Date(e_dates_str)
    
    for (ed in e_dates) {
      # find matching trading day or immediately following
      idx <- which(cal_dates >= ed)[1]
      if (is.na(idx)) next
      
      start_idx <- max(1, idx - pre_days)
      end_idx   <- min(length(cal), idx + post_days)
      mat[start_idx:end_idx, sym] <- TRUE
    }
  }
  mat
}

# Simulator supporting earnings blackout
sim_earnings_system <- function(blackout_type = c("none", "entry_filter_5d", "blackout_1d_1d", "blackout_2d_1d", "blackout_3d_1d"),
                                core_asset = "QLD",
                                portfolio_margin = 1.5,
                                margin_trend_gate = TRUE,
                                use_core_trend = TRUE,
                                core_pct = 0.30,
                                top_n = 5,
                                vol_target = 0.22,
                                disaster_stop = 0.20,
                                label = "Model") {
  blackout_type <- match.arg(blackout_type)
  sat_pct <- 1 - core_pct
  
  # Prepare blackout matrix if applicable
  blackout_mat <- NULL
  if (blackout_type == "blackout_1d_1d") {
    blackout_mat <- make_earnings_blackout_matrix(pre_days = 1, post_days = 1)
  } else if (blackout_type == "blackout_2d_1d") {
    blackout_mat <- make_earnings_blackout_matrix(pre_days = 2, post_days = 1)
  } else if (blackout_type == "blackout_3d_1d") {
    blackout_mat <- make_earnings_blackout_matrix(pre_days = 3, post_days = 1)
  } else if (blackout_type == "entry_filter_5d") {
    # Entry filter only: 5 days before earnings
    blackout_mat <- make_earnings_blackout_matrix(pre_days = 5, post_days = 0)
  }
  
  eq <- 1.0; peak_eq <- 1.0
  out_ret <- numeric(length(ev))
  turn_tot <- 0
  
  curr_core_w  <- 0.0
  curr_sat_w   <- rep(0.0, ncol(P)); names(curr_sat_w) <- colnames(P)
  curr_def_w   <- 0.0 # GLD
  
  entry_prices <- rep(NA_real_, ncol(P)); names(entry_prices) <- colnames(P)
  stopped_out  <- rep(FALSE, ncol(P)); names(stopped_out) <- colnames(P)
  in_blackout  <- rep(FALSE, ncol(P)); names(in_blackout)  <- colnames(P)
  
  prev_eq <- eq
  current_held_sats <- character()
  
  for (k in seq_along(ev)) {
    t <- ev[k]
    r_core <- if (core_asset == "QLD") r_qld[t] else rq[t]
    r_sat  <- R[t, ]
    r_def  <- r_gld[t]
    
    # 1. Check earnings blackout exits for active holdings
    if (blackout_type %in% c("blackout_1d_1d", "blackout_2d_1d", "blackout_3d_1d")) {
      active_held <- names(curr_sat_w)[curr_sat_w > 0]
      for (sym in active_held) {
        if (blackout_mat[t, sym]) {
          # Temporary exit into GLD
          w_exit <- curr_sat_w[sym]
          eq <- eq * (1 - COST * w_exit)
          turn_tot <- turn_tot + w_exit
          curr_def_w <- curr_def_w + w_exit # Move weight to defensive GLD!
          curr_sat_w[sym] <- 0
          in_blackout[sym] <- TRUE
        }
      }
      
      # Check if any blacked-out positions have cleared blackout
      cleared <- names(in_blackout)[in_blackout & !blackout_mat[t, ]]
      for (sym in cleared) {
        in_blackout[sym] <- FALSE
        # If still in the month's target set and trend is intact, restore position from GLD
        if (sym %in% current_held_sats && curr_def_w > 0.05 && P[t, sym] >= mean(P[max(1, t-199):t, sym], na.rm=TRUE)) {
          w_restore <- min(curr_def_w, (sat_pct * 1.5) / top_n)
          curr_def_w <- max(0, curr_def_w - w_restore)
          curr_sat_w[sym] <- w_restore
          entry_prices[sym] <- P[t, sym]
          stopped_out[sym] <- FALSE
          eq <- eq * (1 - COST * w_restore)
          turn_tot <- turn_tot + w_restore
        }
      }
    }
    
    tot_sat_val  <- sum(curr_sat_w * (1 + r_sat))
    tot_core_val <- curr_core_w * (1 + r_core)
    tot_def_val  <- curr_def_w * (1 + r_def)
    
    gross_exp <- curr_core_w + sum(curr_sat_w) + curr_def_w
    borrow_w  <- max(0, gross_exp - 1.0)
    cash_w    <- max(0, 1.0 - gross_exp)
    
    borrow_cost <- borrow_w * margin_borrow_daily[t]
    cash_gain   <- cash_w * rf_daily[t]
    
    day_gain <- (curr_core_w * r_core) + sum(curr_sat_w * r_sat) + (curr_def_w * r_def) - borrow_cost + cash_gain
    
    eq <- eq * (1 + day_gain)
    if (eq > peak_eq) peak_eq <- eq
    
    curr_core_w <- tot_core_val / (1 + day_gain)
    curr_sat_w  <- (curr_sat_w * (1 + r_sat)) / (1 + day_gain)
    curr_def_w  <- tot_def_val / (1 + day_gain)
    
    # 2. Disaster Stop Check (-20%)
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
    
    # 3. Monthly Rebalance Check
    s <- t - 1
    if (month_end[s] || k == 1) {
      realized_v <- roll_vol_20[s]
      scale_mult <- min(1.0, vol_target / max(0.05, realized_v))
      
      active_margin <- portfolio_margin
      if (portfolio_margin > 1.0 && margin_trend_gate && !isTRUE(qqq_trend_up[s])) {
        active_margin <- 1.0
      }
      
      core_active <- TRUE
      if (use_core_trend && !isTRUE(qqq_trend_up[s])) {
        core_active <- FALSE
      }
      
      tgt_core_w <- if (core_active) core_pct * scale_mult * active_margin else 0.0
      
      # Satellite Selection: Top-5 RS
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
        
        # If entry_filter_5d or active blackout, exclude stocks currently in blackout from new entry
        if (blackout_type != "none" && !is.null(blackout_mat)) {
          not_in_blackout <- !blackout_mat[s, ok_filtered]
          if (sum(not_in_blackout) >= top_n) {
            ok_filtered <- ok_filtered[not_in_blackout]
          }
        }
        
        if (length(ok_filtered) >= top_n) {
          ok <- ok_filtered; rs <- rs[ok]
        }
        
        top_syms <- names(sort(rs, decreasing = TRUE))[1:min(top_n, length(rs))]
        current_held_sats <- top_syms
        
        if (length(top_syms) > 0) {
          tgt_sat_w[top_syms] <- (sat_pct * scale_mult * active_margin) / length(top_syms)
        }
        
        new_entrants <- setdiff(top_syms, names(curr_sat_w)[curr_sat_w > 0])
        for (ne in new_entrants) {
          entry_prices[ne] <- P[t, ne]
          stopped_out[ne]  <- FALSE
          in_blackout[ne]  <- FALSE
        }
      }
      
      base_equity_w <- (if (core_active) core_pct else 0.0) * scale_mult + (sat_pct * scale_mult)
      tgt_def_w <- max(0, 1.0 - base_equity_w)
      
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

cat("Simulating 5 Earnings Blackout Variations on Option 6 (QLD Core + 1.5x Margin)...\n")

runs <- list(
  "1. No Blackout (Hold Through Earnings)"    = sim_earnings_system(blackout_type = "none", label = "No Blackout"),
  "2. Entry Filter (Skip if Earn in <=5d)"     = sim_earnings_system(blackout_type = "entry_filter_5d", label = "Entry Filter <=5d"),
  "3. Blackout [-1d, +1d] (Exit into GLD)"     = sim_earnings_system(blackout_type = "blackout_1d_1d", label = "Blackout [-1d, +1d]"),
  "4. Blackout [-2d, +1d] (Exit into GLD)"     = sim_earnings_system(blackout_type = "blackout_2d_1d", label = "Blackout [-2d, +1d]"),
  "5. Blackout [-3d, +1d] (Exit into GLD)"     = sim_earnings_system(blackout_type = "blackout_3d_1d", label = "Blackout [-3d, +1d]")
)

calc_metrics <- function(r) {
  c_r <- cumprod(1 + r); n <- length(r); dd <- c_r / cummax(c_r) - 1
  mdd <- -min(dd) * 100
  cagr <- (tail(c_r, 1)^(252 / n) - 1) * 100
  term <- 8000 * tail(c_r, 1)
  vol <- sd(r) * sqrt(252) * 100
  rfm <- rf_daily[ev]
  sharpe <- if (sd(r) > 0) mean(r - rfm) / sd(r) * sqrt(252) else 0
  sortino <- {
    d_ret <- pmin(r - rfm, 0)
    d_sd  <- sqrt(mean(d_ret^2)) * sqrt(252)
    if (d_sd > 0) mean(r - rfm) * 252 / d_sd else 0
  }
  calmar <- if (mdd > 0) cagr / mdd else 0
  turn <- if (!is.null(attr(r, "turnover_yr"))) attr(r, "turnover_yr") else 0
  
  c(CAGR = cagr, Terminal_8k = term, MaxDD = mdd, Vol = vol,
    Sharpe = sharpe, Sortino = sortino, Calmar = calmar, Turnover_Yr = turn)
}

tbl <- t(sapply(runs, calc_metrics))
cat("\n========================================================================================================================\n")
cat("                       20-YEAR EARNINGS BLACKOUT PERFORMANCE COMPARISON (2006 - 2026)\n")
cat("========================================================================================================================\n")
print(round(tbl, 2))
cat("========================================================================================================================\n")

# Generate Chart
png(file.path(ART, "earnings_blackout_comparison.png"), width = 1600, height = 950, res = 120)
par(mfrow = c(2, 1), mar = c(3.5, 4.5, 2.5, 1.5))

cum_mat <- sapply(runs, function(r) cumprod(1 + r) * 8000)
cols <- c("#c0392b", "#2980b9", "#27ae60", "#f39c12", "#8e44ad")
ltys <- c(1, 1, 1, 1, 1)
lwds <- c(2.5, 2, 2.5, 2, 2)

matplot(dates, cum_mat, type = "l", lty = ltys, lwd = lwds, col = cols, log = "y",
        main = "20-Year Growth of $8,000: Impact of Earnings Blackout Periods (2006-2026)",
        ylab = "Portfolio Value (Log Scale, $)", xlab = "", yaxt = "n")
grid(col = "gray85")

ticks <- c(8000, 25000, 100000, 500000, 2000000, 10000000, 50000000)
labels <- c("$8k", "$25k", "$100k", "$500k", "$2M", "$10M", "$50M")
axis(2, at = ticks, labels = labels, las = 2)
legend("topleft", legend = rownames(tbl), col = cols, lty = ltys, lwd = lwds, bty = "n", cex = 0.85)

# Drawdown Plot
dd_mat <- sapply(runs, function(r) {
  cr <- cumprod(1 + r)
  (cr / cummax(cr) - 1) * 100
})

matplot(dates, dd_mat, type = "l", lty = ltys, lwd = lwds, col = cols,
        main = "Underwater Drawdowns: Earnings Blackout vs Holding Through Earnings",
        ylab = "Drawdown (%)", xlab = "Year", ylim = c(-75, 0))
grid(col = "gray85")
abline(h = -50, col = "red", lty = 3)

dev.off()
cat("Chart saved to:", file.path(ART, "earnings_blackout_comparison.png"), "\n")
