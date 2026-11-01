#!/usr/bin/env Rscript
# ==============================================================================
# scratch/test_tqqq_core_margin_models.R
# 20-Year Empirical Backtest of TQQQ Core + 1.5x Margin when > 200 SMA
# Compared against QLD Core, QQQ Core, and Benchmarks
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
MARGIN_SPREAD <- 0.0175 # Fed Funds + 1.75% (IBKR Pro margin borrowing rate)

adj <- function(sym) {
  p <- file.path(HIST, sprintf("data_%s.rds", tolower(sym)))
  if (!file.exists(p)) return(NULL)
  d <- readRDS(p)
  a <- d[, grep("Adjusted$", colnames(d))]
  a[a <= 0] <- NA
  a
}

qqq_p  <- adj("QQQ")
qld_p  <- adj("QLD")
tqqq_p <- adj("TQQQ")
gld_p  <- adj("GLD_FULL")

cal <- index(qqq_p)[index(qqq_p) >= as.Date("2004-01-01") & index(qqq_p) <= EVAL_END]

# QQQ returns
q <- as.numeric(qqq_p[cal])
rq <- c(0, q[-1] / q[-length(q)] - 1)
sma200_q <- as.numeric(stats::filter(q, rep(1/200, 200), sides = 1))
qqq_trend_up <- q >= sma200_q; qqq_trend_up[is.na(qqq_trend_up)] <- TRUE

# Risk-free & margin borrowing rates
irx <- readRDS(file.path(HIST, "data_^irx.rds"))
rf  <- as.numeric(zoo::na.locf(irx[, 4], na.rm = FALSE)[cal])
rf  <- zoo::na.locf(rf, na.rm = FALSE)
rf[is.na(rf)] <- 2
rf_rate <- rf / 100
rf_daily <- rf_rate / 252
margin_borrow_daily <- (rf_rate + MARGIN_SPREAD) / 252

# QLD returns (Actual from 2006, synthetic 2x before)
m_qld <- match(cal, as.Date(index(qld_p)))
qld_px <- as.numeric(qld_p)[m_qld]
qld_px <- zoo::na.locf(qld_px, na.rm = FALSE)
r_qld <- c(0, qld_px[-1] / qld_px[-length(qld_px)] - 1)
r_qld[is.na(r_qld)] <- 2 * rq[is.na(r_qld)]

# TQQQ returns (Actual from 2010-02-11, synthetic 3x - expense - borrow drag before)
m_tqqq <- match(cal, as.Date(index(tqqq_p)))
tqqq_px <- as.numeric(tqqq_p)[m_tqqq]
tqqq_px <- zoo::na.locf(tqqq_px, na.rm = FALSE)
r_tqqq <- c(0, tqqq_px[-1] / tqqq_px[-length(tqqq_px)] - 1)

# Synthetic pre-2010 TQQQ: 3x daily return minus 0.86% expense ratio minus 2x risk-free rate financing
synth_tqqq <- 3 * rq - (0.0086 / 252) - 2 * (rf_daily + 0.0050 / 252)
r_tqqq[is.na(r_tqqq) | cal < as.Date("2010-02-11")] <- synth_tqqq[is.na(r_tqqq) | cal < as.Date("2010-02-11")]

# GLD returns
m_gld <- match(cal, as.Date(index(gld_p)))
gld_px <- as.numeric(gld_p)[m_gld]
gld_px <- zoo::na.locf(gld_px, na.rm = FALSE)
r_gld <- c(0, gld_px[-1] / gld_px[-length(gld_px)] - 1); r_gld[is.na(r_gld)] <- 0

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
roll_vol_20  <- rollapply(rq, width = 20, FUN = function(x) sd(x) * sqrt(252), align = "right", fill = 0.20)

ev <- which(cal >= EVAL_START)
dates <- cal[ev]
month_end <- c(format(cal[-1], "%m") != format(cal[-length(cal)], "%m"), TRUE)

sim_engine <- function(core_asset = c("QQQ", "QLD", "TQQQ"),
                       use_core_trend = TRUE,
                       portfolio_margin = 1.0,
                       margin_trend_gate = TRUE,
                       core_pct = 0.30,
                       top_n = 5,
                       vol_target = 0.22,
                       disaster_stop = 0.20,
                       label = "Test") {
  core_asset <- match.arg(core_asset)
  sat_pct <- 1 - core_pct
  
  eq <- 1.0; peak_eq <- 1.0
  out_ret <- numeric(length(ev))
  turn_tot <- 0
  
  curr_core_w  <- 0.0
  curr_sat_w   <- rep(0.0, ncol(P)); names(curr_sat_w) <- colnames(P)
  curr_def_w   <- 0.0 # GLD
  
  entry_prices <- rep(NA_real_, ncol(P)); names(entry_prices) <- colnames(P)
  stopped_out  <- rep(FALSE, ncol(P)); names(stopped_out) <- colnames(P)
  
  prev_eq <- eq
  
  for (k in seq_along(ev)) {
    t <- ev[k]
    r_core <- if (core_asset == "TQQQ") r_tqqq[t] else if (core_asset == "QLD") r_qld[t] else rq[t]
    r_sat  <- R[t, ]
    r_def  <- r_gld[t]
    
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
      scale_mult <- min(1.0, vol_target / max(0.05, realized_v))
      
      active_margin <- portfolio_margin
      if (portfolio_margin > 1.0 && margin_trend_gate && !isTRUE(qqq_trend_up[s])) {
        active_margin <- 1.0 # De-lever to 0% debt during bear regimes
      }
      
      core_active <- TRUE
      if (use_core_trend && !isTRUE(qqq_trend_up[s])) {
        core_active <- FALSE
      }
      
      tgt_core_w <- if (core_active) core_pct * scale_mult * active_margin else 0.0
      
      # Top-5 RS Selection
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
          tgt_sat_w[top_syms] <- (sat_pct * scale_mult * active_margin) / length(top_syms)
        }
        
        new_entrants <- setdiff(top_syms, names(curr_sat_w)[curr_sat_w > 0])
        for (ne in new_entrants) {
          entry_prices[ne] <- P[t, ne]
          stopped_out[ne]  <- FALSE
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

cat("Simulating Models (2006-2026)...\n")

runs <- list(
  "QQQ Buy & Hold"                           = rq[ev],
  "Option 1: QQQ Core (No Margin)"           = sim_engine(core_asset = "QQQ",  use_core_trend = TRUE, portfolio_margin = 1.0, margin_trend_gate = FALSE, label = "Option 1: QQQ Cash"),
  "Option 3: QLD Core (No Margin / IRA)"     = sim_engine(core_asset = "QLD",  use_core_trend = TRUE, portfolio_margin = 1.0, margin_trend_gate = FALSE, label = "Option 3: QLD Cash"),
  "Option 6: QLD Core + 1.5x Margin >200SMA" = sim_engine(core_asset = "QLD",  use_core_trend = TRUE, portfolio_margin = 1.5, margin_trend_gate = TRUE,  label = "Option 6: QLD + 1.5x Margin"),
  "Option 7: TQQQ Core (No Margin / IRA)"    = sim_engine(core_asset = "TQQQ", use_core_trend = TRUE, portfolio_margin = 1.0, margin_trend_gate = FALSE, label = "Option 7: TQQQ Cash"),
  "Option 8: TQQQ Core + 1.5x Margin >200SMA"= sim_engine(core_asset = "TQQQ", use_core_trend = TRUE, portfolio_margin = 1.5, margin_trend_gate = TRUE,  label = "Option 8: TQQQ + 1.5x Margin")
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
  
  m2008 <- format(dates, "%Y") == "2008"; r2008 <- (prod(1 + r[m2008]) - 1) * 100
  m2020 <- format(dates, "%Y") == "2020"; r2020 <- (prod(1 + r[m2020]) - 1) * 100
  m2022 <- format(dates, "%Y") == "2022"; r2022 <- (prod(1 + r[m2022]) - 1) * 100
  
  c(CAGR = cagr, Terminal_8k = term, MaxDD = mdd, Vol = vol,
    Sharpe = sharpe, Sortino = sortino, Calmar = calmar,
    Ret_2008 = r2008, Ret_2020 = r2020, Ret_2022 = r2022)
}

tbl <- t(sapply(runs, calc_metrics))
cat("\n========================================================================================================================\n")
cat("                       20-YEAR EMPIRICAL PERFORMANCE SCORECARD (2006 - 2026)\n")
cat("========================================================================================================================\n")
print(round(tbl, 2))
cat("========================================================================================================================\n")

# Generate High Resolution Comparison Chart
png(file.path(ART, "tqqq_core_margin_comparison.png"), width = 1600, height = 900, res = 120)
par(mfrow = c(2, 1), mar = c(3.5, 4.5, 2.5, 1.5))

# Top: Log Equity Curves
cum_mat <- sapply(runs, function(r) cumprod(1 + r) * 8000)
cols <- c("#7f8c8d", "#2980b9", "#27ae60", "#d35400", "#8e44ad", "#c0392b")
ltys <- c(2, 1, 1, 1, 1, 1)
lwds <- c(2, 2, 2, 2.5, 2, 3)

matplot(dates, cum_mat, type = "l", lty = ltys, lwd = lwds, col = cols, log = "y",
        main = "20-Year Growth of $8,000: TQQQ Core + 1.5x Margin vs QLD & QQQ (2006-2026)",
        ylab = "Portfolio Value (Log Scale, $)", xlab = "", yaxt = "n")
grid(col = "gray85")

ticks <- c(8000, 25000, 100000, 500000, 2000000, 10000000, 50000000)
labels <- c("$8k", "$25k", "$100k", "$500k", "$2M", "$10M", "$50M")
axis(2, at = ticks, labels = labels, las = 2)

legend("topleft", legend = rownames(tbl), col = cols, lty = ltys, lwd = lwds, bty = "n", cex = 0.85)

# Bottom: Drawdown Curves
dd_mat <- sapply(runs, function(r) {
  cr <- cumprod(1 + r)
  (cr / cummax(cr) - 1) * 100
})

matplot(dates, dd_mat, type = "l", lty = ltys, lwd = lwds, col = cols,
        main = "Underwater Drawdown Comparison (2006-2026)",
        ylab = "Drawdown (%)", xlab = "Year", ylim = c(-85, 0))
grid(col = "gray85")
abline(h = -50, col = "red", lty = 3)

dev.off()
cat("Chart saved to:", file.path(ART, "tqqq_core_margin_comparison.png"), "\n")
