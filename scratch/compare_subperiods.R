suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
})

source("scratch/compare_three_fngd_sqqq_allocations.R")

horizons <- list(
  "1-Month (21d)"   = 21,
  "6-Month (126d)"  = 126,
  "1-Year (252d)"   = 252,
  "2-Year (504d)"   = 504,
  "3-Year (756d)"   = 756,
  "5-Year (1260d)"  = 1260,
  "10-Year (2520d)" = 2520
)

cat("\n========================================================================================\n")
cat(" MULTI-HORIZON COMPARISON ($10k STARTING, PURE 15-DAY TIME STOP, -12% CIRCUIT BREAKER)\n")
cat("========================================================================================\n\n")

sub_rows <- list()
for (h_name in names(horizons)) {
  n_h <- horizons[[h_name]]
  sub_dates <- tail(eval_dates, n_h)
  
  for (a_name in names(allocs)) {
    k <- sprintf("Pure 15-Day Time Stop | %s", a_name)
    eq_full <- equity_curves[[k]]
    eq_sub <- tail(eq_full, n_h)
    eq_sub_norm <- eq_sub / eq_sub[1]
    
    fin_val <- 10000 * tail(eq_sub_norm, 1)
    cagr_val <- (tail(eq_sub_norm, 1))^(252 / n_h) - 1
    mdd_val <- min((eq_sub_norm - cummax(eq_sub_norm)) / cummax(eq_sub_norm))
    
    sub_rows[[paste(h_name, a_name)]] <- data.frame(
      Horizon = h_name,
      Allocation = a_name,
      Final_Val = fin_val,
      CAGR = cagr_val * 100,
      MaxDD = mdd_val * 100,
      stringsAsFactors = FALSE
    )
  }
}

df_sub <- do.call(rbind, sub_rows)
hdr <- sprintf("%-18s | %-22s | %16s | %10s | %10s",
               "Horizon", "Allocation", "Terminal Value", "Ann. Return", "Max DD")
cat(hdr, "\n", paste(rep("-", nchar(hdr)), collapse=""), "\n")

for (i in 1:nrow(df_sub)) {
  val_str <- if (df_sub$Final_Val[i] >= 1e6) sprintf("$%.2fM", df_sub$Final_Val[i] / 1e6) else sprintf("$%.2f", df_sub$Final_Val[i])
  cat(sprintf("%-18s | %-22s | %16s | %+9.2f%% | %9.2f%%\n",
              df_sub$Horizon[i], df_sub$Allocation[i], val_str, df_sub$CAGR[i], df_sub$MaxDD[i]))
}

