#!/usr/bin/env Rscript
# ==============================================================================
# scratch/test_tax_audit_3_10_20yr.R
# Comprehensive 3-Year, 10-Year, and 20-Year Tax-Adjusted Backtests
# Starting Capital: $10,000
#
# Regimes Evaluated:
#   1. Model: Trader Tax Status (TTS Sec. 475(f) S-Corp) -> 28.4%
#   2. Model: Taxable NYC Resident (Fed + State + City)  -> 51.2%
#   3. Model: Taxable NYS Resident (Fed + State)         -> 47.7%
#   4. Benchmark: QQQ Buy & Hold (NYC Taxable)           -> 34.1% LTCG / Div
#   5. Benchmark: QQQ Buy & Hold (NYS Taxable)           -> 30.7% LTCG / Div
#   6. Model & QQQ: Tax-Advantaged (IRA / 401k / Act 60) -> 0.0%
#
# Metrics Computed:
#   - Terminal Wealth on $10,000
#   - After-Tax CAGR (%)
#   - Total Taxes Paid ($)
#   - Maximum Drawdown (%, including Baseline QQQ MaxDD)
# ==============================================================================

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(jsonlite)
})

# Load equity data generated from the 20-year discrete simulation
df <- read.csv("factsheet/data/ultimate_equity.csv")
df$date <- as.Date(df$date)

n_rows <- nrow(df)
ret_ultimate <- c(0, diff(df$ultimate) / df$ultimate[1:(n_rows - 1)])
ret_qqq      <- c(0, diff(df$qqq) / df$qqq[1:(n_rows - 1)])

# Tax Rates
RATE_TTS       <- 0.284   # TTS Sec. 475(f) Mark-to-Market S-Corp
RATE_NYC_ORD   <- 0.512   # NYC Resident Ordinary / Short-Term Capital Gains
RATE_NYS_ORD   <- 0.4765  # NYS Resident (Non-NYC) Ordinary / Short-Term Capital Gains

RATE_NYC_LTCG  <- 0.341   # NYC Resident Long-Term Capital Gains & Qualified Dividends
RATE_NYS_LTCG  <- 0.3065  # NYS Resident Long-Term Capital Gains & Qualified Dividends
DIV_YIELD_QQQ  <- 0.0070  # ~0.70% Annual Dividend Yield for QQQ

# 1. Active Strategy Tax Compounding Engine (Yearly Mark-to-Market with Loss Carryforward)
sim_active_tax <- function(daily_rets, dates, tax_rate, starting_cap = 10000) {
  n <- length(daily_rets)
  equity_curve <- numeric(n)
  years <- as.integer(format(dates, "%Y"))
  unique_years <- unique(years)
  
  cap_curr <- starting_cap
  loss_carryforward <- 0.0
  total_tax_paid <- 0.0
  
  for (yr in unique_years) {
    yr_mask <- (years == yr)
    yr_indices <- which(yr_mask)
    if (length(yr_indices) == 0) next
    
    yr_cap_start <- cap_curr
    for (i in yr_indices) {
      cap_curr <- cap_curr * (1 + daily_rets[i])
      equity_curve[i] <- cap_curr
    }
    yr_cap_end <- cap_curr
    net_gain <- yr_cap_end - yr_cap_start
    
    if (tax_rate > 0) {
      if (net_gain > 0) {
        taxable_gain <- max(0, net_gain - loss_carryforward)
        loss_carryforward <- max(0, loss_carryforward - net_gain)
        tax_due <- taxable_gain * tax_rate
        cap_curr <- cap_curr - tax_due
        total_tax_paid <- total_tax_paid + tax_due
      } else {
        loss_carryforward <- loss_carryforward + abs(net_gain)
      }
    }
    equity_curve[tail(yr_indices, 1)] <- cap_curr
  }
  
  cagr <- ((cap_curr / starting_cap)^(252 / n) - 1) * 100
  cum_max <- cummax(equity_curve)
  dd <- (equity_curve - cum_max) / cum_max
  max_dd <- abs(min(dd)) * 100
  
  list(
    terminal_wealth = cap_curr,
    cagr            = cagr,
    max_dd          = max_dd,
    total_tax_paid  = total_tax_paid,
    equity_curve    = equity_curve
  )
}

# 2. Buy & Hold Benchmark Tax Engine (Annual Dividend Tax + Terminal LTCG Liquidation Tax)
sim_benchmark_tax <- function(daily_rets, dates, tax_rate_ltcg, div_yield = DIV_YIELD_QQQ, starting_cap = 10000) {
  n <- length(daily_rets)
  equity_curve <- numeric(n)
  years <- as.integer(format(dates, "%Y"))
  unique_years <- unique(years)
  
  cap_curr <- starting_cap
  total_div_tax_paid <- 0.0
  
  for (yr in unique_years) {
    yr_mask <- (years == yr)
    yr_indices <- which(yr_mask)
    if (length(yr_indices) == 0) next
    
    for (i in yr_indices) {
      cap_curr <- cap_curr * (1 + daily_rets[i])
      equity_curve[i] <- cap_curr
    }
    
    # Pay annual tax on dividends distributed during the year
    if (tax_rate_ltcg > 0) {
      annual_div <- cap_curr * div_yield
      div_tax <- annual_div * tax_rate_ltcg
      cap_curr <- cap_curr - div_tax
      total_div_tax_paid <- total_div_tax_paid + div_tax
      equity_curve[tail(yr_indices, 1)] <- cap_curr
    }
  }
  
  # Calculate pre-liquidation Drawdown
  cum_max <- cummax(equity_curve)
  dd <- (equity_curve - cum_max) / cum_max
  max_dd <- abs(min(dd)) * 100
  
  # Terminal Liquidation: Pay LTCG tax on total net unrealized gain
  total_gain <- max(0, cap_curr - starting_cap)
  ltcg_tax <- if (tax_rate_ltcg > 0) total_gain * tax_rate_ltcg else 0.0
  final_wealth <- cap_curr - ltcg_tax
  total_tax_paid <- total_div_tax_paid + ltcg_tax
  
  cagr <- ((final_wealth / starting_cap)^(252 / n) - 1) * 100
  
  list(
    terminal_wealth = final_wealth,
    cagr            = cagr,
    max_dd          = max_dd,
    total_tax_paid  = total_tax_paid,
    equity_curve    = equity_curve
  )
}

# Define the 3 evaluation windows:
windows <- list(
  "3-Year (2023 - 2026)" = list(start = as.Date("2023-10-03"), end = as.Date("2026-10-06")),
  "10-Year (2016 - 2026)" = list(start = as.Date("2016-10-03"), end = as.Date("2026-10-06")),
  "20-Year (2007 - 2026)" = list(start = as.Date("2007-01-03"), end = as.Date("2026-10-06"))
)

all_results <- list()

for (w_name in names(windows)) {
  w <- windows[[w_name]]
  sub_idx <- which(df$date >= w$start & df$date <= w$end)
  sub_dates <- df$date[sub_idx]
  sub_u <- ret_ultimate[sub_idx]
  sub_q <- ret_qqq[sub_idx]
  
  # (1) Model: Trader Tax Status (TTS Sec. 475(f) S-Corp @ 28.4%)
  r1 <- sim_active_tax(sub_u, sub_dates, tax_rate = RATE_TTS, starting_cap = 10000)
  
  # (2) Model: Taxable NYC Resident (Fed + State + City @ 51.2%)
  r2 <- sim_active_tax(sub_u, sub_dates, tax_rate = RATE_NYC_ORD, starting_cap = 10000)
  
  # (3) Model: Taxable NYS Resident (Fed + State @ 47.7%)
  r3 <- sim_active_tax(sub_u, sub_dates, tax_rate = RATE_NYS_ORD, starting_cap = 10000)
  
  # (4) Benchmark: QQQ Buy & Hold (NYC Taxable @ 34.1% LTCG / Div)
  r4 <- sim_benchmark_tax(sub_q, sub_dates, tax_rate_ltcg = RATE_NYC_LTCG, starting_cap = 10000)
  
  # (5) Benchmark: QQQ Buy & Hold (NYS Taxable @ 30.7% LTCG / Div)
  r5 <- sim_benchmark_tax(sub_q, sub_dates, tax_rate_ltcg = RATE_NYS_LTCG, starting_cap = 10000)
  
  # (6a) Model: Tax-Advantaged (IRA / 401k / PR Act 60 @ 0.0%)
  r6_mod <- sim_active_tax(sub_u, sub_dates, tax_rate = 0.0, starting_cap = 10000)
  
  # (6b) Benchmark: QQQ Buy & Hold (Tax-Advantaged @ 0.0%)
  r6_qqq <- sim_benchmark_tax(sub_q, sub_dates, tax_rate_ltcg = 0.0, starting_cap = 10000)
  
  df_window <- data.frame(
    Window = w_name,
    Regime = c(
      "(1) Model: Trader Tax Status (TTS Sec. 475(f) S-Corp)",
      "(2) Model: Taxable NYC Resident (Fed + State + City)",
      "(3) Model: Taxable NYS Resident (Fed + State)",
      "(4) Benchmark: QQQ Buy & Hold (NYC Taxable)",
      "(5) Benchmark: QQQ Buy & Hold (NYS Taxable)",
      "(6a) Model: Tax-Advantaged (IRA / 401k / Act 60)",
      "(6b) Benchmark: QQQ Buy & Hold (Tax-Advantaged / IRA)"
    ),
    Tax_Rate = c("28.4% (Ord/MTM)", "51.2% (Ord/STCG)", "47.7% (Ord/STCG)", "34.1% (LTCG/Div)", "30.7% (LTCG/Div)", "0.0% (Tax-Free)", "0.0% (Tax-Free)"),
    Initial_Wealth = "$10,000",
    Terminal_Wealth = c(r1$terminal_wealth, r2$terminal_wealth, r3$terminal_wealth, r4$terminal_wealth, r5$terminal_wealth, r6_mod$terminal_wealth, r6_qqq$terminal_wealth),
    CAGR = c(r1$cagr, r2$cagr, r3$cagr, r4$cagr, r5$cagr, r6_mod$cagr, r6_qqq$cagr),
    Max_Drawdown = c(r1$max_dd, r2$max_dd, r3$max_dd, r4$max_dd, r5$max_dd, r6_mod$max_dd, r6_qqq$max_dd),
    Total_Tax_Paid = c(r1$total_tax_paid, r2$total_tax_paid, r3$total_tax_paid, r4$total_tax_paid, r5$total_tax_paid, 0, 0),
    stringsAsFactors = FALSE
  )
  
  all_results[[w_name]] <- df_window
}

final_df <- do.call(rbind, all_results)

# Format for pretty display
fmt_usd <- function(x) {
  ifelse(x >= 1e9, sprintf("$%.2fB", x / 1e9),
  ifelse(x >= 1e6, sprintf("$%.2fM", x / 1e6),
  ifelse(x >= 1e3, sprintf("$%sk", formatC(round(x / 1e3, 1), format = "f", digits = 1, big.mark = ",")),
         sprintf("$%.2f", x))))
}

display_df <- final_df
display_df$Terminal_Wealth <- fmt_usd(display_df$Terminal_Wealth)
display_df$Total_Tax_Paid  <- fmt_usd(display_df$Total_Tax_Paid)
display_df$CAGR            <- sprintf("%+.2f%%", display_df$CAGR)
display_df$Max_Drawdown    <- sprintf("-%.2f%%", display_df$Max_Drawdown)

cat("\n========================================================================================================================\n")
cat("                       COMPREHENSIVE AFTER-TAX AUDIT SCORECARD (3-YR, 10-YR, 20-YR)                                    \n")
cat("========================================================================================================================\n")
print(display_df[, c("Window", "Regime", "Tax_Rate", "Terminal_Wealth", "CAGR", "Max_Drawdown", "Total_Tax_Paid")], row.names = FALSE)
cat("========================================================================================================================\n\n")

write.csv(final_df, "scratch/scorecard_tax_audit_3_10_20yr.csv", row.names = FALSE)

# Generate Visual Comparison Chart Artifact
ART <- "/Users/cmejo/.gemini/antigravity/brain/64019830-0cac-4bb1-904e-d4560e71468c"
chart_path <- file.path(ART, "tax_audit_3_10_20yr_comparison.png")
png(chart_path, width = 1600, height = 900, res = 120)
par(mfrow = c(1, 3), mar = c(5, 14, 4, 1))

reg_labels <- c(
  "Model: Tax-Adv (0%)",
  "Model: TTS S-Corp (28.4%)",
  "Model: NYS Taxable (47.7%)",
  "Model: NYC Taxable (51.2%)",
  "QQQ: Tax-Adv (0%)",
  "QQQ: NYS Taxable (30.7%)",
  "QQQ: NYC Taxable (34.1%)"
)

cols <- c("#0B3D91", "#1557bf", "#4285f4", "#ea4335", "#34a853", "#80868b", "#5f6368")

for (w_name in names(windows)) {
  sub_d <- final_df[final_df$Window == w_name, ]
  # Order: Model Tax-Adv, Model TTS, Model NYS, Model NYC, QQQ Tax-Adv, QQQ NYS, QQQ NYC
  ord <- c(6, 1, 3, 2, 7, 5, 4)
  cagrs <- sub_d$CAGR[ord]
  
  barplot(rev(cagrs), horiz = TRUE, las = 1, col = rev(cols),
          names.arg = rev(reg_labels),
          main = sprintf("%s After-Tax CAGR", w_name),
          xlab = "After-Tax CAGR (%)", xlim = c(0, max(cagrs) * 1.15))
  grid(col = "gray85")
}
dev.off()
cat("Chart saved to:", chart_path, "\n")
