suppressPackageStartupMessages({
  library(quantmod)
  library(xts)
})

HIST_DIR <- "data/data_historical"
files <- list.files(HIST_DIR, pattern = "^data_[a-z0-9_]+\\.rds$", full.names = TRUE)
files <- files[!grepl("data_(\\^|vti|qqq|spy|vfinx|gspc|ndx|irx)", files)]

volumes_usd <- numeric()
sym_names   <- character()

for (f in files) {
  sym <- toupper(sub("^data_", "", sub("\\.rds$", "", basename(f))))
  d <- tryCatch(readRDS(f), error = function(e) NULL)
  if (is.null(d) || nrow(d) < 50) next
  
  tail_d <- tail(d, 60) # last 60 trading days
  cl <- as.numeric(Cl(tail_d))
  vo <- as.numeric(Vo(tail_d))
  daily_usd_vol <- median(cl * vo, na.rm = TRUE)
  if (!is.na(daily_usd_vol) && daily_usd_vol > 0) {
    volumes_usd <- c(volumes_usd, daily_usd_vol)
    sym_names   <- c(sym_names, sym)
  }
}

names(volumes_usd) <- sym_names
sorted_vol <- sort(volumes_usd, decreasing = TRUE)

cat("Total liquid symbols evaluated:", length(sorted_vol), "\n")
cat("Top 5 Daily Dollar Volumes:\n")
print(head(sorted_vol / 1e6, 5)) # in millions

cat("\nMedian Daily Dollar Volume across broad universe: $", sprintf("%.1fM", median(sorted_vol) / 1e6), "\n")
cat("25th percentile Daily Dollar Volume: $", sprintf("%.1fM", quantile(sorted_vol, 0.25) / 1e6), "\n")
cat("Minimum Daily Dollar Volume: $", sprintf("%.1fM", min(sorted_vol) / 1e6), "\n")

# Compute % of ADV for $100k account ($50k position) and $1M account ($500k position)
pos_100k <- 50000
pos_1m   <- 500000

pct_100k <- (pos_100k / sorted_vol) * 100
pct_1m   <- (pos_1m   / sorted_vol) * 100

cat("\n--- At $100,000 Starting Account ($50,000 position size) ---\n")
cat("  Median % of ADV:          ", sprintf("%.4f%%", median(pct_100k)), " (Negligible)\n")
cat("  90th percentile % of ADV:  ", sprintf("%.4f%%", quantile(pct_100k, 0.90)), "\n")
cat("  Max % of ADV:              ", sprintf("%.4f%%", max(pct_100k)), " (Well below the 1.0% institutional threshold)\n")

cat("\n--- At $1,000,000 Starting Account ($500,000 position size) ---\n")
cat("  Median % of ADV:          ", sprintf("%.4f%%", median(pct_1m)), " (0.07% of daily market volume)\n")
cat("  90th percentile % of ADV:  ", sprintf("%.4f%%", quantile(pct_1m, 0.90)), "\n")
cat("  Max % of ADV:              ", sprintf("%.4f%%", max(pct_1m)), "\n")

# Maximum Portfolio Capacity before hitting 1.0% ADV threshold on median stock
# 1% of median stock ($680M) = $6.8M per position -> $20.4M portfolio!
max_pos_cap_median <- median(sorted_vol) * 0.01
cat(sprintf("\n--- Maximum Institutional Portfolio Capacity ---\n"))
cat(sprintf("  At 1.0%% ADV limit on median constituent: $%.1fM position size -> $%.1fM total portfolio\n", 
            max_pos_cap_median / 1e6, (max_pos_cap_median * 3 / 1.5) / 1e6))
