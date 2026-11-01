#!/usr/bin/env Rscript
# ==============================================================================
# scratch/precompute_market_breadth.R
# Precompute 10-day smoothed Intraday Breadth Ratio across universe
# ==============================================================================

suppressPackageStartupMessages({
  library(xts)
  library(quantmod)
  library(TTR)
})

HIST_DIR <- "data/data_historical"
if (!dir.exists(HIST_DIR)) {
  HIST_DIR <- "data/data_10yr"
}

stock_files <- list.files(HIST_DIR, pattern = "^data_[a-z0-9_]+\\.rds$", full.names = TRUE)
bench_patterns <- c("data_\\^", "data_vti", "data_qqq", "data_spy", "data_vfinx", "data_gspc", "data_ndx", "data_irx", "data_gld")
for (bp in bench_patterns) stock_files <- stock_files[!grepl(bp, stock_files)]

spy_file <- file.path(HIST_DIR, "data_spy.rds")
if (!file.exists(spy_file)) spy_file <- "data/data_spy.rds"
spy <- readRDS(spy_file)
all_dates <- as.Date(index(spy))
n_dates <- length(all_dates)

intraday_mat <- matrix(NA_real_, nrow = n_dates, ncol = length(stock_files))
rownames(intraday_mat) <- as.character(all_dates)
colnames(intraday_mat) <- basename(stock_files)

cat(sprintf("Computing intraday breadth across %d stocks over %d dates...\n", length(stock_files), n_dates))

for (k in seq_along(stock_files)) {
  f <- stock_files[k]
  d <- tryCatch(readRDS(f), error = function(e) NULL)
  if (is.null(d) || nrow(d) < 50) next
  
  stk_dates <- as.Date(index(d))
  m_idx <- match(all_dates, stk_dates)
  valid <- !is.na(m_idx)
  if (sum(valid) > 0) {
    op <- as.numeric(Op(d))[m_idx[valid]]
    cl <- as.numeric(Cl(d))[m_idx[valid]]
    diff_oc <- cl - op
    sign_oc <- ifelse(diff_oc > 0, 1, ifelse(diff_oc < 0, -1, 0))
    intraday_mat[valid, k] <- sign_oc
  }
}

n_adv <- rowSums(intraday_mat == 1, na.rm = TRUE)
n_dec <- rowSums(intraday_mat == -1, na.rm = TRUE)
n_tot <- rowSums(!is.na(intraday_mat), na.rm = TRUE)
n_tot[n_tot == 0] <- 1

net_adv_dec <- (n_adv - n_dec) / n_tot
breadth_xts <- xts(net_adv_dec, order.by = all_dates)
breadth_10d <- TTR::SMA(breadth_xts, n = 10)
breadth_10d[is.na(breadth_10d)] <- 0.0
colnames(breadth_10d) <- "IntradayBreadthRatio_10D"

saveRDS(breadth_10d, "data/intraday_breadth_10d.rds")
cat(sprintf("Saved data/intraday_breadth_10d.rds (%d bars, mean = %.4f, sd = %.4f, range = [%.3f, %.3f])\n",
            nrow(breadth_10d), mean(breadth_10d), sd(breadth_10d), min(breadth_10d), max(breadth_10d)))
