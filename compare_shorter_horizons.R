scorecard_5y <- read.csv("output/low_max_pos_matrix_scorecard.csv", stringsAsFactors = FALSE)
horizons <- c("6 Months", "12 Months", "24 Months")
configs <- data.frame(
  mp = c(3, 4, 4, 5, 5),
  cap = c(3, 3, 4, 3, 4),
  stringsAsFactors = FALSE
)

cat("=== SHORTER HORIZONS COMPARISON (Starting $10,000 | Vince Sizing) ===\n")
cat(sprintf("%-16s | %-16s | %-16s | %-16s\n", "Configuration", "6 Months", "12 Months", "24 Months"))
cat("------------------------------------------------------------------------------------\n")

for (i in 1:nrow(configs)) {
  mp <- configs$mp[i]
  cap <- configs$cap[i]
  cfg_name <- sprintf("POS=%d, CAP=%d", mp, cap)
  
  vals <- sapply(horizons, function(h) {
    row <- scorecard_5y[scorecard_5y$Max_Positions == mp & scorecard_5y$Sector_Cap == cap & 
                        scorecard_5y$Sizing_Mode == "vince" & scorecard_5y$Horizon == h, ]
    if (nrow(row) > 0) sprintf("%s (DD:%s)", row$Terminal_Wealth, row$Max_Drawdown) else "N/A"
  })
  
  cat(sprintf("%-16s | %-16s | %-16s | %-16s\n", cfg_name, vals[1], vals[2], vals[3]))
}
