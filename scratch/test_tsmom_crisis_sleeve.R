source("R/02_trend_lmMA.R")
source("R/10_option_d_features.R")
source("R/11_option_d_engine.R")

# Load broad universe
src <- readLines("scratch/compare_symbol_lists.R")
idx <- grep("^lists <- c", src)
eval(parse(text = src[1:(idx - 1)]))
invisible(set_universe(read_list("symbols_broad.txt")))

D <- prepare_option_d_data(list(cal = cal, syms = syms, P = P, R = R, MEM = MEM, q = q, rq = rq,
                                r_gld = r_gld, rf_daily = rf_daily, VOL63 = VOL63, vol20 = vol20,
                                month_end = month_end))

# Load Crisis ETFs
etf_df <- read.csv("data/tsmom_crisis_etfs.csv", stringsAsFactors = FALSE)
etf_df$Date <- as.Date(etf_df$Date)
etf_df <- etf_df[order(etf_df$Date), ]

# Align Crisis ETFs to D$cal
etf_tickers <- c("TLT", "IEF", "GLD", "UUP", "DBC") # SGOV/BIL will be cash
etf_P <- matrix(NA_real_, nrow = length(D$cal), ncol = length(etf_tickers),
                dimnames = list(as.character(D$cal), etf_tickers))

for (t in etf_tickers) {
  m <- match(D$cal, etf_df$Date)
  etf_P[, t] <- etf_df[[t]][m]
  # Fill NAs backwards for inception if needed, or leave NA
  etf_P[, t] <- zoo::na.locf(etf_P[, t], na.rm = FALSE)
}

# Calculate daily simple returns for ETFs
etf_R <- matrix(0, nrow = nrow(etf_P), ncol = ncol(etf_P), dimnames = dimnames(etf_P))
etf_R[-1, ] <- etf_P[-1, ] / etf_P[-nrow(etf_P), ] - 1
etf_R[is.na(etf_R) | is.infinite(etf_R)] <- 0

# Function to compute TSMOM sleeve returns
# long_short: if TRUE, short when 12m return < 0. If FALSE, 0 (cash) when 12m return < 0.
calc_tsmom_sleeve <- function(long_short = TRUE, target_vol = 0.12) {
  n_days <- length(D$cal)
  tsmom_ret <- rep(0, n_days)
  
  # Trailing 252d return and 63d vol
  for (t in 253:n_days) {
    # Check which ETFs have valid 252d history
    valid <- which(!is.na(etf_P[t - 1, ]) & !is.na(etf_P[t - 252, ]) & etf_P[t - 252, ] > 0)
    if (length(valid) == 0) {
      tsmom_ret[t] <- D$rf_daily[t]
      next
    }
    
    ret_12m <- etf_P[t - 1, valid] / etf_P[t - 252, valid] - 1
    # 63d realized vol
    sub_R <- etf_R[(t - 63):(t - 1), valid, drop = FALSE]
    vol_63 <- apply(sub_R, 2, stats::sd) * sqrt(252)
    vol_63[is.na(vol_63) | vol_63 < 0.04] <- 0.04 # floor at 4%
    
    if (long_short) {
      sig <- sign(ret_12m)
    } else {
      sig <- ifelse(ret_12m > 0, 1, 0)
    }
    
    if (all(sig == 0)) {
      tsmom_ret[t] <- D$rf_daily[t]
      next
    }
    
    # Equal risk weighting: w_i = (sig / vol) / sum(abs(sig / vol))
    raw_w <- sig / vol_63
    w <- raw_w / sum(abs(raw_w)) # normalized to 100% gross risk budget
    
    # Sleeve return on day t
    # For long positions: r. For short positions: -r. Cash earns rf_daily
    gross_inv <- sum(abs(w))
    day_r <- sum(w * etf_R[t, valid]) + max(0, 1 - gross_inv) * D$rf_daily[t]
    tsmom_ret[t] <- day_r
  }
  tsmom_ret
}

tsmom_ls  <- calc_tsmom_sleeve(long_short = TRUE)
tsmom_lo  <- calc_tsmom_sleeve(long_short = FALSE)

# ------------------------------------------------------------------------------
# Evaluate Bear Regimes Specifically
# ------------------------------------------------------------------------------
bear_days <- which(!D$benchmark_bull)
cat(sprintf("Total trading days: %d | Bear regime days (QQQ < 200 SMA): %d (%.1f%%)\n\n",
            length(D$cal), length(bear_days), length(bear_days) / length(D$cal) * 100))

# 1. Measure Bear Sleeve Returns standalone across different crisis episodes:
episodes <- list(
  "Full History Bear Days (2007-2026)" = bear_days,
  "2008 GFC (Oct 2007 - Mar 2009)"     = which(D$cal >= as.Date("2007-10-01") & D$cal <= as.Date("2009-03-31")),
  "2020 COVID (Feb 2020 - May 2020)"   = which(D$cal >= as.Date("2020-02-19") & D$cal <= as.Date("2020-05-31")),
  "2022 Fed Rate Hikes (Jan - Dec 2022)"= which(D$cal >= as.Date("2022-01-01") & D$cal <= as.Date("2022-12-31"))
)

cat("========================================================================================\n")
cat("            CRISIS EPISODE PERFORMANCE: STANDALONE DEFENSIVE SLEEVES                    \n")
cat("========================================================================================\n")
cat(sprintf("%-36s | %-12s | %-12s | %-12s | %-12s\n",
            "Episode", "QQQ Ret", "GLD Ret", "TSMOM Long/Short", "TSMOM Long-Only"))
cat("----------------------------------------------------------------------------------------\n")

for (ep_name in names(episodes)) {
  idx <- episodes[[ep_name]]
  qqq_cum   <- (prod(1 + D$rq[idx]) - 1) * 100
  gld_cum   <- (prod(1 + D$r_gld[idx]) - 1) * 100
  tls_cum   <- (prod(1 + tsmom_ls[idx]) - 1) * 100
  tlo_cum   <- (prod(1 + tsmom_lo[idx]) - 1) * 100
  cat(sprintf("%-36s | %+10.1f%% | %+10.1f%% | %+15.1f%% | %+14.1f%%\n",
              ep_name, qqq_cum, gld_cum, tls_cum, tlo_cum))
}
cat("========================================================================================\n\n")

# Save daily sleeve series for full portfolio integration
saveRDS(list(tsmom_ls = tsmom_ls, tsmom_lo = tsmom_lo), "scratch/tsmom_sleeve_returns.rds")
print("Saved scratch/tsmom_sleeve_returns.rds")
