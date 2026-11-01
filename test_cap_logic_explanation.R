# Demonstrate why CAP=4 has no effect when MAX_POS=3
max_pos <- 3
sector_cap_3 <- 3
sector_cap_4 <- 4

cat("For MAX_POS = 3:\n")
cat(sprintf("Maximum possible positions in the entire portfolio: %d\n", max_pos))
cat(sprintf("If SECTOR_CAP = 3: Maximum stocks allowed from any single sector = min(%d, %d) = %d\n", sector_cap_3, max_pos, min(sector_cap_3, max_pos)))
cat(sprintf("If SECTOR_CAP = 4: Maximum stocks allowed from any single sector = min(%d, %d) = %d\n", sector_cap_4, max_pos, min(sector_cap_4, max_pos)))
cat("Conclusion: Because a 3-position portfolio can never hold 4 positions under any circumstances, SECTOR_CAP=4 is mathematically identical to SECTOR_CAP=3.\n")
