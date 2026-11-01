scorecard <- read.csv("output/low_max_pos_matrix_scorecard.csv", stringsAsFactors = FALSE)

# Let's inspect the exact rows for MAX_POS 2, 3, 4, 5 across caps and horizons
subset_df <- scorecard[, c("Horizon", "Max_Positions", "Sector_Cap", "Sizing_Mode", 
                           "Terminal_Wealth", "Sharpe_Ratio", "Max_Drawdown", "Wealth_Num")]

cat("=================== VINCE SIZING SUMMARY TABLE ===================\n")
vince_df <- subset_df[subset_df$Sizing_Mode == "vince", ]
vince_df <- vince_df[order(vince_df$Horizon, vince_df$Max_Positions, vince_df$Sector_Cap), ]

# Reshape into wide table by Horizon
horizons <- c("6 Months", "12 Months", "24 Months", "60 Months")
configs <- unique(vince_df[, c("Max_Positions", "Sector_Cap")])

cat(sprintf("%-16s | %-14s | %-14s | %-14s | %-14s\n", 
            "Config (P/Cap)", "6 Months", "12 Months", "24 Months", "60 Months"))
cat("----------------------------------------------------------------------------------------\n")
for (i in 1:nrow(configs)) {
  mp <- configs$Max_Positions[i]
  cap <- configs$Sector_Cap[i]
  cfg_lbl <- sprintf("POS=%d / CAP=%d", mp, cap)
  
  vals <- sapply(horizons, function(h) {
    row <- vince_df[vince_df$Max_Positions == mp & vince_df$Sector_Cap == cap & vince_df$Horizon == h, ]
    if (nrow(row) > 0) sprintf("$%8.0f (SR:%.1f|DD:%.0f%%)", row$Wealth_Num, as.numeric(row$Sharpe_Ratio), as.numeric(gsub("%", "", row$Max_Drawdown))) else "N/A"
  })
  
  cat(sprintf("%-16s | %-14s | %-14s | %-14s | %-14s\n", 
              cfg_lbl, vals[1], vals[2], vals[3], vals[4]))
}

cat("\n=================== EQUAL SIZING SUMMARY TABLE ===================\n")
equal_df <- subset_df[subset_df$Sizing_Mode == "equal", ]
cat(sprintf("%-16s | %-14s | %-14s | %-14s | %-14s\n", 
            "Config (P/Cap)", "6 Months", "12 Months", "24 Months", "60 Months"))
cat("----------------------------------------------------------------------------------------\n")
for (i in 1:nrow(configs)) {
  mp <- configs$Max_Positions[i]
  cap <- configs$Sector_Cap[i]
  cfg_lbl <- sprintf("POS=%d / CAP=%d", mp, cap)
  
  vals <- sapply(horizons, function(h) {
    row <- equal_df[equal_df$Max_Positions == mp & equal_df$Sector_Cap == cap & equal_df$Horizon == h, ]
    if (nrow(row) > 0) sprintf("$%8.0f (SR:%.1f|DD:%.0f%%)", row$Wealth_Num, as.numeric(row$Sharpe_Ratio), as.numeric(gsub("%", "", row$Max_Drawdown))) else "N/A"
  })
  
  cat(sprintf("%-16s | %-14s | %-14s | %-14s | %-14s\n", 
              cfg_lbl, vals[1], vals[2], vals[3], vals[4]))
}
