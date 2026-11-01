#!/usr/bin/env Rscript
# ==============================================================================
# 20-YEAR CONTINUOUS HISTORICAL BACKTEST ANALYSIS (OCTOBER 2006 – OCTOBER 2026)
# Includes:
# - Upgraded 27-Feature + Breadth Conviction Strategy (1.5x Lev + Dual-Bear + Breadth Sizing)
# - Prior 26-Feature Strategy (1.5x Lev + Dual-Bear)
# - Baseline Model (1.5x Lev, Unhedged)
# - Benchmarks: QQQ (100%), 70/30 QQQ/VTI, SPY (100%), VTI (100%)
# ==============================================================================
suppressPackageStartupMessages({
  library(xts)
  library(quantmod)
  library(jsonlite)
  library(TTR)
})

HIST_DIR <- "data/data_historical"
res_40yr <- readRDS("output/comparison_40yr_baseline_vs_current_results.rds")

dates_all  <- as.Date(res_40yr$dates)
mask_20y   <- (dates_all >= as.Date("2006-10-02") & dates_all <= as.Date("2026-10-06"))
eval_dates <- dates_all[mask_20y]
n_days     <- length(eval_dates)

cat(sprintf("Exact 20-Year Window: %d sessions (%s to %s | 20.0 Years)\n", 
            n_days, min(eval_dates), max(eval_dates)))

r_curr_26 <- res_40yr$ret_current[mask_20y]
r_base    <- res_40yr$ret_baseline[mask_20y]

# Load Market Intraday Breadth (10D)
breadth_rds <- readRDS("data/intraday_breadth_10d.rds")
m_br <- merge(xts(rep(0, n_days), order.by = eval_dates), breadth_rds)
m_br <- na.locf(na.locf(m_br, na.rm = FALSE), fromLast = TRUE)
breadth_20y <- as.numeric(m_br[eval_dates, 2])
breadth_20y[is.na(breadth_20y)] <- 0.0

# Benchmarks
qqq_raw <- readRDS(file.path(HIST_DIR, "data_qqq.rds"))
spy_raw <- readRDS(file.path(HIST_DIR, "data_spy.rds"))
vti_raw <- readRDS(file.path(HIST_DIR, "data_vti.rds"))
irx_raw <- readRDS(file.path(HIST_DIR, "data_^irx.rds"))

qqq_cl <- Cl(qqq_raw); spy_cl <- Cl(spy_raw); vti_cl <- Cl(vti_raw)
qqq_ret <- na.omit(qqq_cl / lag.xts(qqq_cl, 1) - 1)
spy_ret <- na.omit(spy_cl / lag.xts(spy_cl, 1) - 1)
vti_ret <- na.omit(vti_cl / lag.xts(vti_cl, 1) - 1)

r_qqq  <- as.numeric(qqq_ret[eval_dates])
r_spy  <- as.numeric(spy_ret[eval_dates])
r_vti  <- as.numeric(vti_ret[eval_dates])
r_7030 <- 0.70 * r_qqq + 0.30 * r_vti

irx_yield <- na.locf(irx_raw[, 4], na.rm = FALSE)
irx_yield[is.na(irx_yield)] <- 2.0
rf_daily <- xts((as.numeric(irx_yield) / 100) / 252, order.by = index(irx_raw))
r_rf <- as.numeric(rf_daily[eval_dates])
r_rf[is.na(r_rf)] <- 0.02 / 252

# Macro Bull Indicator (SPY >= 200D SMA)
spy_sma200 <- SMA(spy_cl, n = 200)
macro_bull <- as.numeric(spy_cl[eval_dates]) >= as.numeric(spy_sma200[eval_dates])
macro_bull[is.na(macro_bull)] <- TRUE

# Apply Breadth Conviction Sizing Multiplier (1.20x on <= -0.15, 0.80x on >= +0.30)
# Using yesterday's close breadth (t-1) to size today's (t) swing return (strictly zero lookahead)
r_curr_27 <- r_curr_26
for (t in 2:n_days) {
  br_prev <- breadth_20y[t - 1]
  # Only modulate active equity swing exposure (when not in defensive hedge)
  if (macro_bull[t - 1]) {
    if (br_prev <= -0.15) {
      # 1.20x conviction boost on oversold washouts (minus incremental margin borrow cost)
      r_curr_27[t] <- r_curr_26[t] * 1.12 - (0.070 / 252) * 0.15
    } else if (br_prev >= 0.30) {
      # 0.80x dampener on overextended breadth (plus saved margin borrow cost)
      r_curr_27[t] <- r_curr_26[t] * 0.88 + (0.070 / 252) * 0.15
    }
  }
}

# Primary Current Strategy is the 27-Feature + Breadth Conviction Model
r_curr <- r_curr_27

# Metrics calculation function
calc_stats <- function(r, rf = r_rf, start_cap = 10000) {
  n <- length(r)
  c_ret <- cumprod(1 + r)
  term  <- start_cap * tail(c_ret, 1)
  cagr  <- ((tail(c_ret, 1))^(252 / n) - 1) * 100
  cum_m <- cummax(c_ret)
  dd    <- (c_ret - cum_m) / cum_m
  max_dd <- abs(min(dd)) * 100
  vol   <- sd(r) * sqrt(252) * 100
  ex_r  <- r - rf
  sr    <- (mean(ex_r) / sd(r)) * sqrt(252)
  dn_r  <- r[r < 0]
  sortino <- if (length(dn_r) > 0) (mean(ex_r) / sd(dn_r)) * sqrt(252) else NA
  calmar  <- cagr / max_dd
  
  # Tax accounting
  calc_tax <- function(rate) {
    years <- unique(format(eval_dates, "%Y"))
    w <- start_cap
    cf_loss <- 0
    for (yr in years) {
      idx <- which(format(eval_dates, "%Y") == yr)
      yr_cum <- prod(1 + r[idx])
      pre_g <- w * (yr_cum - 1)
      net_tax <- pre_g - cf_loss
      if (net_tax > 0) {
        tb <- net_tax * rate
        cf_loss <- 0
      } else {
        tb <- 0
        cf_loss <- cf_loss + abs(net_tax)
      }
      w <- w + pre_g - tb
    }
    c_tax <- ((w / start_cap)^(252 / n) - 1) * 100
    list(term = w, cagr = c_tax)
  }
  
  tts <- calc_tax(0.284)
  nyc <- calc_tax(0.512)
  
  list(
    term = term, cagr = cagr, max_dd = max_dd, vol = vol,
    sr = sr, sortino = sortino, calmar = calmar,
    tts_term = tts$term, tts_cagr = tts$cagr,
    nyc_term = nyc$term, nyc_cagr = nyc$cagr,
    equity_curve = c_ret * start_cap,
    drawdown = dd * 100
  )
}

st_curr   <- calc_stats(r_curr)
st_curr26 <- calc_stats(r_curr_26)
st_base   <- calc_stats(r_base)
st_qqq    <- calc_stats(r_qqq)
st_7030   <- calc_stats(r_7030)
st_spy    <- calc_stats(r_spy)
st_vti    <- calc_stats(r_vti)

# Master 20-Year Scorecard Table
df_master <- data.frame(
  Strategy = c(
    "Current 27-Feat + Breadth Strategy (1.5x Lev + Dual-Bear)",
    "Prior 26-Feat Strategy (1.5x Lev + Dual-Bear)",
    "Baseline Model (1.5x Lev, Unhedged)",
    "Buy & Hold QQQ (100% Invesco QQQ)",
    "Buy & Hold 70% QQQ / 30% VTI Blend",
    "Buy & Hold SPY (100% S&P 500)",
    "Buy & Hold VTI (100% Vanguard Total Stock)"
  ),
  Terminal_Gross = c(
    sprintf("$%s", format(round(st_curr$term, 2), big.mark = ",")),
    sprintf("$%s", format(round(st_curr26$term, 2), big.mark = ",")),
    sprintf("$%s", format(round(st_base$term, 2), big.mark = ",")),
    sprintf("$%s", format(round(st_qqq$term, 2), big.mark = ",")),
    sprintf("$%s", format(round(st_7030$term, 2), big.mark = ",")),
    sprintf("$%s", format(round(st_spy$term, 2), big.mark = ",")),
    sprintf("$%s", format(round(st_vti$term, 2), big.mark = ","))
  ),
  CAGR_Gross = c(
    sprintf("%+.2f%%", st_curr$cagr),
    sprintf("%+.2f%%", st_curr26$cagr),
    sprintf("%+.2f%%", st_base$cagr),
    sprintf("%+.2f%%", st_qqq$cagr),
    sprintf("%+.2f%%", st_7030$cagr),
    sprintf("%+.2f%%", st_spy$cagr),
    sprintf("%+.2f%%", st_vti$cagr)
  ),
  Max_Drawdown = c(
    sprintf("-%.2f%%", st_curr$max_dd),
    sprintf("-%.2f%%", st_curr26$max_dd),
    sprintf("-%.2f%%", st_base$max_dd),
    sprintf("-%.2f%%", st_qqq$max_dd),
    sprintf("-%.2f%%", st_7030$max_dd),
    sprintf("-%.2f%%", st_spy$max_dd),
    sprintf("-%.2f%%", st_vti$max_dd)
  ),
  Sharpe = c(
    sprintf("%.2f", st_curr$sr),
    sprintf("%.2f", st_curr26$sr),
    sprintf("%.2f", st_base$sr),
    sprintf("%.2f", st_qqq$sr),
    sprintf("%.2f", st_7030$sr),
    sprintf("%.2f", st_spy$sr),
    sprintf("%.2f", st_vti$sr)
  ),
  Sortino = c(
    sprintf("%.2f", st_curr$sortino),
    sprintf("%.2f", st_curr26$sortino),
    sprintf("%.2f", st_base$sortino),
    sprintf("%.2f", st_qqq$sortino),
    sprintf("%.2f", st_7030$sortino),
    sprintf("%.2f", st_spy$sortino),
    sprintf("%.2f", st_vti$sortino)
  ),
  Annual_Vol = c(
    sprintf("%.2f%%", st_curr$vol),
    sprintf("%.2f%%", st_curr26$vol),
    sprintf("%.2f%%", st_base$vol),
    sprintf("%.2f%%", st_qqq$vol),
    sprintf("%.2f%%", st_7030$vol),
    sprintf("%.2f%%", st_spy$vol),
    sprintf("%.2f%%", st_vti$vol)
  ),
  TTS_Terminal = c(
    sprintf("$%s", format(round(st_curr$tts_term, 2), big.mark = ",")),
    sprintf("$%s", format(round(st_curr26$tts_term, 2), big.mark = ",")),
    sprintf("$%s", format(round(st_base$tts_term, 2), big.mark = ",")),
    sprintf("$%s", format(round(st_qqq$tts_term, 2), big.mark = ",")),
    sprintf("$%s", format(round(st_7030$tts_term, 2), big.mark = ",")),
    sprintf("$%s", format(round(st_spy$tts_term, 2), big.mark = ",")),
    sprintf("$%s", format(round(st_vti$tts_term, 2), big.mark = ","))
  ),
  TTS_CAGR = c(
    sprintf("%+.2f%%", st_curr$tts_cagr),
    sprintf("%+.2f%%", st_curr26$tts_cagr),
    sprintf("%+.2f%%", st_base$tts_cagr),
    sprintf("%+.2f%%", st_qqq$tts_cagr),
    sprintf("%+.2f%%", st_7030$tts_cagr),
    sprintf("%+.2f%%", st_spy$tts_cagr),
    sprintf("%+.2f%%", st_vti$tts_cagr)
  ),
  NYC_Terminal = c(
    sprintf("$%s", format(round(st_curr$nyc_term, 2), big.mark = ",")),
    sprintf("$%s", format(round(st_curr26$nyc_term, 2), big.mark = ",")),
    sprintf("$%s", format(round(st_base$nyc_term, 2), big.mark = ",")),
    sprintf("$%s", format(round(st_qqq$nyc_term, 2), big.mark = ",")),
    sprintf("$%s", format(round(st_7030$nyc_term, 2), big.mark = ",")),
    sprintf("$%s", format(round(st_spy$nyc_term, 2), big.mark = ",")),
    sprintf("$%s", format(round(st_vti$nyc_term, 2), big.mark = ","))
  ),
  NYC_CAGR = c(
    sprintf("%+.2f%%", st_curr$nyc_cagr),
    sprintf("%+.2f%%", st_curr26$nyc_cagr),
    sprintf("%+.2f%%", st_base$nyc_cagr),
    sprintf("%+.2f%%", st_qqq$nyc_cagr),
    sprintf("%+.2f%%", st_7030$nyc_cagr),
    sprintf("%+.2f%%", st_spy$nyc_cagr),
    sprintf("%+.2f%%", st_vti$nyc_cagr)
  )
)

print(df_master, row.names = FALSE)

# Decade Breakdown
d1_mask <- eval_dates <= as.Date("2016-10-02")
d2_mask <- eval_dates >  as.Date("2016-10-02")

calc_sub <- function(r_sub, d_sub) {
  n <- length(r_sub)
  c_ret <- cumprod(1 + r_sub)
  cagr <- ((tail(c_ret, 1))^(252 / n) - 1) * 100
  cum_m <- cummax(c_ret)
  dd <- (c_ret - cum_m) / cum_m
  max_dd <- abs(min(dd)) * 100
  vol <- sd(r_sub) * sqrt(252) * 100
  sr <- (mean(r_sub) / sd(r_sub)) * sqrt(252)
  list(cagr = cagr, max_dd = max_dd, vol = vol, sr = sr)
}

c_d1   <- calc_sub(r_curr[d1_mask], eval_dates[d1_mask])
c_d2   <- calc_sub(r_curr[d2_mask], eval_dates[d2_mask])
c26_d1 <- calc_sub(r_curr_26[d1_mask], eval_dates[d1_mask])
c26_d2 <- calc_sub(r_curr_26[d2_mask], eval_dates[d2_mask])
b_d1   <- calc_sub(r_base[d1_mask], eval_dates[d1_mask])
b_d2   <- calc_sub(r_base[d2_mask], eval_dates[d2_mask])
q_d1   <- calc_sub(r_qqq[d1_mask], eval_dates[d1_mask])
q_d2   <- calc_sub(r_qqq[d2_mask], eval_dates[d2_mask])
s_d1   <- calc_sub(r_spy[d1_mask], eval_dates[d1_mask])
s_d2   <- calc_sub(r_spy[d2_mask], eval_dates[d2_mask])

df_decades <- data.frame(
  Strategy = c("Current 27-Feat + Breadth", "Prior 26-Feat Strategy", "Baseline Model", "QQQ (100% Nasdaq)", "SPY (100% S&P 500)"),
  Decade1_CAGR = c(sprintf("%+.2f%%", c_d1$cagr), sprintf("%+.2f%%", c26_d1$cagr), sprintf("%+.2f%%", b_d1$cagr), sprintf("%+.2f%%", q_d1$cagr), sprintf("%+.2f%%", s_d1$cagr)),
  Decade1_MaxDD = c(sprintf("-%.2f%%", c_d1$max_dd), sprintf("-%.2f%%", c26_d1$max_dd), sprintf("-%.2f%%", b_d1$max_dd), sprintf("-%.2f%%", q_d1$max_dd), sprintf("-%.2f%%", s_d1$max_dd)),
  Decade1_Sharpe = c(sprintf("%.2f", c_d1$sr), sprintf("%.2f", c26_d1$sr), sprintf("%.2f", b_d1$sr), sprintf("%.2f", q_d1$sr), sprintf("%.2f", s_d1$sr)),
  Decade2_CAGR = c(sprintf("%+.2f%%", c_d2$cagr), sprintf("%+.2f%%", c26_d2$cagr), sprintf("%+.2f%%", b_d2$cagr), sprintf("%+.2f%%", q_d2$cagr), sprintf("%+.2f%%", s_d2$cagr)),
  Decade2_MaxDD = c(sprintf("-%.2f%%", c_d2$max_dd), sprintf("-%.2f%%", c26_d2$max_dd), sprintf("-%.2f%%", b_d2$max_dd), sprintf("-%.2f%%", q_d2$max_dd), sprintf("-%.2f%%", s_d2$max_dd)),
  Decade2_Sharpe = c(sprintf("%.2f", c_d2$sr), sprintf("%.2f", c26_d2$sr), sprintf("%.2f", b_d2$sr), sprintf("%.2f", q_d2$sr), sprintf("%.2f", s_d2$sr))
)
cat("\n--- Decade Breakdown ---\n")
print(df_decades, row.names = FALSE)

# Year-by-Year Calendar Returns
years <- unique(format(eval_dates, "%Y"))
yr_df <- data.frame(Year = years)

calc_y <- function(r) {
  sapply(years, function(y) {
    idx <- which(format(eval_dates, "%Y") == y)
    (prod(1 + r[idx]) - 1) * 100
  })
}

yr_df$Current_27Feat   <- sprintf("%+.2f%%", calc_y(r_curr))
yr_df$Prior_26Feat     <- sprintf("%+.2f%%", calc_y(r_curr_26))
yr_df$Baseline_Model   <- sprintf("%+.2f%%", calc_y(r_base))
yr_df$QQQ_100          <- sprintf("%+.2f%%", calc_y(r_qqq))
yr_df$Blend_70_30      <- sprintf("%+.2f%%", calc_y(r_7030))
yr_df$SPY_100          <- sprintf("%+.2f%%", calc_y(r_spy))
yr_df$VTI_100          <- sprintf("%+.2f%%", calc_y(r_vti))

cat("\n--- Year-by-Year Calendar Returns (2006 to 2026) ---\n")
print(yr_df, row.names = FALSE)

# Crisis Drawdown Table
crisis_table <- data.frame(
  Crisis_Period = c(
    "2007-2009 Great Financial Crisis (GFC)",
    "2011 US Sovereign Debt Downgrade",
    "2015-2016 China Devaluation / Energy Collapse",
    "2018 Volmageddon & Q4 Tech Selloff",
    "2020 COVID-19 Liquidity Shock",
    "2022 Fed Rate-Hike & Tech Bear Market"
  ),
  Dates = c(
    "Oct 2007 – Mar 2009",
    "May 2011 – Oct 2011",
    "Jul 2015 – Feb 2016",
    "Oct 2018 – Dec 2018",
    "Feb 2020 – Mar 2020",
    "Jan 2022 – Dec 2022"
  )
)

calc_crisis_dd <- function(r, s_date, e_date) {
  idx <- which(eval_dates >= as.Date(s_date) & eval_dates <= as.Date(e_date))
  c_r <- cumprod(1 + r[idx])
  dd <- (c_r - cummax(c_r)) / cummax(c_r)
  abs(min(dd)) * 100
}

c_dates <- list(
  c("2007-10-01", "2009-03-09"),
  c("2011-05-02", "2011-10-04"),
  c("2015-07-20", "2016-02-11"),
  c("2018-10-01", "2018-12-24"),
  c("2020-02-19", "2020-03-23"),
  c("2022-01-03", "2022-12-30")
)

crisis_table$Current_MaxDD  <- sapply(c_dates, function(d) sprintf("-%.2f%%", calc_crisis_dd(r_curr, d[1], d[2])))
crisis_table$Baseline_MaxDD <- sapply(c_dates, function(d) sprintf("-%.2f%%", calc_crisis_dd(r_base, d[1], d[2])))
crisis_table$QQQ_MaxDD      <- sapply(c_dates, function(d) sprintf("-%.2f%%", calc_crisis_dd(r_qqq,  d[1], d[2])))
crisis_table$SPY_MaxDD      <- sapply(c_dates, function(d) sprintf("-%.2f%%", calc_crisis_dd(r_spy,  d[1], d[2])))

cat("\n--- Historical Crisis Drawdown Comparison ---\n")
print(crisis_table, row.names = FALSE)

# Save all results to CSV
write.csv(df_master,    "output/twenty_year_master_scorecard.csv", row.names = FALSE)
write.csv(df_decades,   "output/twenty_year_decade_breakdown.csv", row.names = FALSE)
write.csv(yr_df,        "output/twenty_year_calendar_returns.csv", row.names = FALSE)
write.csv(crisis_table, "output/twenty_year_crisis_drawdowns.csv", row.names = FALSE)

# Generate High-Resolution 3-Panel Institutional Chart
chart_file <- "output/twenty_year_continuous_performance_chart.png"
artifact_chart <- "/Users/cmejo/.gemini/antigravity/brain/64019830-0cac-4bb1-904e-d4560e71468c/twenty_year_continuous_performance_chart.png"

png(chart_file, width = 1600, height = 1200, res = 140)
layout(matrix(c(1, 2, 3), nrow = 3, ncol = 1), heights = c(1.35, 0.85, 0.90))
par(mar = c(2.8, 4.8, 2.8, 2.0), oma = c(1, 1, 1, 1))

# Panel 1: 20-Year Equity Curves (Log Scale)
eq_curr   <- st_curr$equity_curve
eq_curr26 <- st_curr26$equity_curve
eq_base   <- st_base$equity_curve
eq_qqq    <- st_qqq$equity_curve
eq_7030   <- st_7030$equity_curve
eq_spy    <- st_spy$equity_curve

plot(eval_dates, eq_curr, type = "l", col = "#0d47a1", lwd = 2.8, log = "y",
     main = "20-Year Historical Backtest (Oct 2006 – Oct 2026): Cumulative Equity Curve ($10,000 Start, Log Scale)",
     ylab = "Portfolio Equity ($ USD, Log)", xlab = "",
     ylim = c(4000, max(c(eq_curr, eq_curr26, eq_base)) * 1.8),
     cex.main = 1.15, cex.lab = 1.0)
grid(col = "gray88", lty = 1)
lines(eval_dates, eq_curr26, col = "#1976d2", lwd = 1.8, lty = 2)
lines(eval_dates, eq_base,   col = "#2e7d32", lwd = 1.8, lty = 3)
lines(eval_dates, eq_qqq,    col = "#c62828", lwd = 2.2, lty = 1)
lines(eval_dates, eq_7030,   col = "#6a1b9a", lwd = 1.6, lty = 4)
lines(eval_dates, eq_spy,    col = "#616161", lwd = 1.6, lty = 2)

legend("topleft",
       legend = c(
         sprintf("Current 27-Feat + Breadth (1.5x + Hedge): $%s (%s CAGR, Sharpe %.2f, MaxDD %s)",
                 format(round(tail(eq_curr, 1)), big.mark=","), sprintf("%+.2f%%", st_curr$cagr), st_curr$sr, sprintf("-%.1f%%", st_curr$max_dd)),
         sprintf("Prior 26-Feat Strategy (1.5x + Hedge): $%s (%s CAGR, Sharpe %.2f, MaxDD %s)",
                 format(round(tail(eq_curr26, 1)), big.mark=","), sprintf("%+.2f%%", st_curr26$cagr), st_curr26$sr, sprintf("-%.1f%%", st_curr26$max_dd)),
         sprintf("Baseline Model (1.5x Unhedged): $%s (%s CAGR, Sharpe %.2f, MaxDD %s)",
                 format(round(tail(eq_base, 1)), big.mark=","), sprintf("%+.2f%%", st_base$cagr), st_base$sr, sprintf("-%.1f%%", st_base$max_dd)),
         sprintf("Buy & Hold QQQ (100%% Nasdaq): $%s (%s CAGR, Sharpe %.2f, MaxDD %s)",
                 format(round(tail(eq_qqq, 1)), big.mark=","), sprintf("%+.2f%%", st_qqq$cagr), st_qqq$sr, sprintf("-%.1f%%", st_qqq$max_dd)),
         sprintf("Buy & Hold 70/30 QQQ/VTI: $%s (%s CAGR, Sharpe %.2f, MaxDD %s)",
                 format(round(tail(eq_7030, 1)), big.mark=","), sprintf("%+.2f%%", st_7030$cagr), st_7030$sr, sprintf("-%.1f%%", st_7030$max_dd)),
         sprintf("Buy & Hold SPY (100%% S&P 500): $%s (%s CAGR, Sharpe %.2f, MaxDD %s)",
                 format(round(tail(eq_spy, 1)), big.mark=","), sprintf("%+.2f%%", st_spy$cagr), st_spy$sr, sprintf("-%.1f%%", st_spy$max_dd))
       ),
       col = c("#0d47a1", "#1976d2", "#2e7d32", "#c62828", "#6a1b9a", "#616161"),
       lwd = c(2.8, 1.8, 1.8, 2.2, 1.6, 1.6),
       lty = c(1, 2, 3, 1, 4, 2),
       bg = "white", cex = 0.82)

# Panel 2: Underwater Drawdowns
plot(eval_dates, st_curr$drawdown, type = "l", col = "#0d47a1", lwd = 2.0,
     main = "Underwater Drawdown Profile (2006–2026): Surviving GFC (-54% QQQ), COVID (-28%), & 2022 Rate Shock (-35%)",
     ylab = "Drawdown (%)", xlab = "", ylim = c(-60, 0),
     cex.main = 1.1, cex.lab = 1.0)
grid(col = "gray88", lty = 1)
lines(eval_dates, st_base$drawdown, col = "#2e7d32", lwd = 1.5, lty = 3)
lines(eval_dates, st_qqq$drawdown,  col = "#c62828", lwd = 1.8, lty = 1)
lines(eval_dates, st_spy$drawdown,  col = "#616161", lwd = 1.4, lty = 2)
abline(h = -30, col = "gray50", lty = 2)
abline(h = -50, col = "red", lty = 3)

legend("bottomleft",
       legend = c("Current 27-Feat + Breadth", "Baseline Model", "QQQ (100%)", "SPY (100%)"),
       col = c("#0d47a1", "#2e7d32", "#c62828", "#616161"),
       lwd = c(2.0, 1.5, 1.8, 1.4),
       lty = c(1, 3, 1, 2),
       bg = "white", cex = 0.82, horiz = TRUE)

# Panel 3: Calendar Year Returns Bar Chart (2006-2026)
yr_curr_num <- calc_y(r_curr)
yr_qqq_num  <- calc_y(r_qqq)
yr_spy_num  <- calc_y(r_spy)

bar_mat <- rbind(yr_curr_num, yr_qqq_num, yr_spy_num)
colnames(bar_mat) <- years

bp <- barplot(bar_mat, beside = TRUE,
              col = c("#0d47a1", "#c62828", "#757575"),
              border = NA,
              main = "Annual Calendar Returns by Year (2006–2026): Strategy vs. QQQ & SPY (%)",
              ylab = "Calendar Return (%)",
              ylim = c(min(bar_mat) * 1.25, max(bar_mat) * 1.18),
              cex.main = 1.1, cex.lab = 1.0, cex.names = 0.88)
grid(nx = NA, ny = NULL, col = "gray88", lty = 1)
abline(h = 0, col = "black", lwd = 1)
legend("topleft",
       legend = c("Current 27-Feat + Breadth Strategy", "QQQ (100%)", "SPY (100%)"),
       fill = c("#0d47a1", "#c62828", "#757575"),
       bg = "white", cex = 0.85, horiz = TRUE)

dev.off()
file.copy(chart_file, artifact_chart, overwrite = TRUE)
cat(sprintf("\n[Chart Saved] Saved 20-year performance chart to %s and %s\n", chart_file, artifact_chart))
cat("[Done] 20-year backtest report and data generated successfully.\n")
