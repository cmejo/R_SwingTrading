suppressPackageStartupMessages({
  library(xts)
  library(quantmod)
  library(TTR)
})

# Compare AAPL with the correct realistic bracket vs the leaky bracket
obj <- readRDS("data/cache_10yr/aapl_10yr_wf.rds")
ohlcv <- readRDS("data/data_10yr/data_aapl.rds")
c_idx <- intersect(index(ohlcv), index(obj$pred_prob))

ohlcv_sub <- ohlcv[c_idx]
cl <- as.numeric(Cl(ohlcv_sub))
op <- as.numeric(Op(ohlcv_sub))
hi <- as.numeric(Hi(ohlcv_sub))
lo <- as.numeric(Lo(ohlcv_sub))
atr <- as.numeric(ATR(HLC(ohlcv_sub), n=14)$atr)
atr[is.na(atr)] <- cl[is.na(atr)] * 0.02
prob <- as.numeric(obj$pred_prob)
rs   <- as.numeric(obj$rs_spy)

nb <- length(c_idx)
correct_ret <- numeric(nb)
in_pos <- FALSE; entry_p <- 0; stop_p <- 0; t1_p <- 0; t2_p <- 0; days_h <- 0
t1_hit <- FALSE; t2_hit <- FALSE; pos_shares_pct <- 0; high_p <- 0

for (d in 2:nb) {
  cur_o <- op[d]; cur_h <- hi[d]; cur_l <- lo[d]; cur_c <- cl[d]; prev_c <- cl[d-1]; cur_atr <- atr[d]
  if (!in_pos && !is.na(prob[d-1]) && prob[d-1] >= 0.58 && !is.na(rs[d-1]) && rs[d-1] > 0) {
    in_pos <- TRUE
    entry_p <- ifelse(!is.na(cur_o) && cur_o > 0, cur_o, cur_c)
    high_p <- entry_p
    days_h <- 0; t1_hit <- FALSE; t2_hit <- FALSE; pos_shares_pct <- 1.0
    risk_1r <- 2.0 * (cur_atr / cur_c) * entry_p
    stop_p <- entry_p - risk_1r
    t1_p   <- entry_p + 1.5 * risk_1r
    t2_p   <- entry_p + 3.0 * risk_1r
    correct_ret[d] <- (cur_c - entry_p) / entry_p - 0.001
    next
  }
  if (in_pos) {
    days_h <- days_h + 1
    high_p <- max(high_p, cur_h)
    chand_stop <- high_p - 2.5 * cur_atr
    
    if (cur_l <= stop_p) {
      exit_p <- min(cur_o, stop_p)
      bar_r <- (exit_p - prev_c) / prev_c
      correct_ret[d] <- pos_shares_pct * bar_r - 0.001
      in_pos <- FALSE; pos_shares_pct <- 0
      next
    }
    if (!t1_hit && cur_h >= t1_p) {
      t1_hit <- TRUE; pos_shares_pct <- 0.50; stop_p <- max(entry_p, chand_stop)
    }
    if (t1_hit && !t2_hit && cur_h >= t2_p) {
      t2_hit <- TRUE; pos_shares_pct <- 0.25; stop_p <- max(stop_p, chand_stop)
    }
    if (t1_hit) stop_p <- max(stop_p, chand_stop)
    if (!t2_hit && days_h >= 7) {
      bar_r <- (cur_c - prev_c) / prev_c
      correct_ret[d] <- pos_shares_pct * bar_r - 0.001
      in_pos <- FALSE; pos_shares_pct <- 0
      next
    }
    bar_r <- (cur_c - prev_c) / prev_c
    correct_ret[d] <- pos_shares_pct * bar_r
  }
}

cat(sprintf("AAPL Leaky 10yr Wealth: $%.2f\n", 10000 * prod(1 + as.numeric(obj$bracket_ret))))
cat(sprintf("AAPL Realistic 10yr Wealth: $%.2f\n", 10000 * prod(1 + correct_ret)))
