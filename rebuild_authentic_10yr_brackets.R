suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(TTR)
  library(parallel)
})

CACHE_DIR_10Y <- "data/cache_10yr"
DATA_DIR_10Y  <- "data/data_10yr"
cache_files <- list.files(CACHE_DIR_10Y, pattern = "_10yr_wf\\.rds$", full.names = TRUE)

cat(sprintf("[RebuildBrackets] Rebuilding authentic bracket execution for %d cached 10-year files...\n", length(cache_files)))

process_bracket_file <- function(cf) {
  obj <- tryCatch(readRDS(cf), error = function(e) NULL)
  if (is.null(obj)) return(NULL)
  
  s <- obj$symbol
  df_file <- file.path(DATA_DIR_10Y, sprintf("data_%s.rds", tolower(gsub("[^A-Za-z0-9]", "_", s))))
  if (!file.exists(df_file)) return(NULL)
  
  ohlcv <- tryCatch(readRDS(df_file), error = function(e) NULL)
  if (is.null(ohlcv)) return(NULL)
  
  c_idx <- intersect(index(ohlcv), index(obj$pred_prob))
  if (length(c_idx) < 50) return(NULL)
  
  ohlcv_sub <- ohlcv[c_idx]
  cl <- as.numeric(Cl(ohlcv_sub))
  op <- as.numeric(Op(ohlcv_sub))
  hi <- as.numeric(Hi(ohlcv_sub))
  lo <- as.numeric(Lo(ohlcv_sub))
  
  atr <- as.numeric(ATR(HLC(ohlcv_sub), n = 14)$atr)
  atr[is.na(atr) | atr <= 0] <- cl[is.na(atr) | atr <= 0] * 0.02
  
  prob <- as.numeric(obj$pred_prob)[match(c_idx, index(obj$pred_prob))]
  rs   <- as.numeric(obj$rs_spy)[match(c_idx, index(obj$rs_spy))]
  
  nb <- length(c_idx)
  authentic_ret <- numeric(nb)
  
  in_pos <- FALSE
  entry_p <- 0; stop_p <- 0; t1_p <- 0; t2_p <- 0; high_p <- 0; days_h <- 0
  t1_hit <- FALSE; t2_hit <- FALSE; pos_shares_pct <- 0
  
  for (d in 2:nb) {
    cur_o   <- op[d]
    cur_h   <- hi[d]
    cur_l   <- lo[d]
    cur_c   <- cl[d]
    prev_c  <- cl[d - 1]
    cur_atr <- atr[d]
    
    # Check entry at bar open following a valid signal on d-1
    if (!in_pos && !is.na(prob[d - 1]) && prob[d - 1] >= 0.58 && !is.na(rs[d - 1]) && rs[d - 1] > 0) {
      in_pos  <- TRUE
      entry_p <- ifelse(!is.na(cur_o) && cur_o > 0, cur_o, cur_c)
      high_p  <- entry_p
      days_h  <- 0
      t1_hit  <- FALSE
      t2_hit  <- FALSE
      pos_shares_pct <- 1.0
      
      # 2.0x daily ATR risk bracket
      risk_1r <- 2.0 * cur_atr
      stop_p  <- entry_p - risk_1r
      t1_p    <- entry_p + 1.5 * risk_1r
      t2_p    <- entry_p + 3.0 * risk_1r
      
      # Entry day return with 10 bps slippage/friction
      authentic_ret[d] <- (cur_c - entry_p) / entry_p - (10 / 10000)
      next
    }
    
    if (in_pos) {
      days_h <- days_h + 1
      high_p <- max(high_p, cur_h)
      chand_stop <- high_p - 2.5 * cur_atr
      
      # 1. Stop loss hit
      if (cur_l <= stop_p) {
        exit_p <- min(cur_o, stop_p)
        bar_r  <- (exit_p - prev_c) / prev_c
        authentic_ret[d] <- pos_shares_pct * bar_r - (10 / 10000)
        in_pos <- FALSE
        pos_shares_pct <- 0
        next
      }
      
      # 2. Tier 1 target hit: exit 50%
      if (!t1_hit && cur_h >= t1_p) {
        t1_hit <- TRUE
        pos_shares_pct <- 0.50
        stop_p <- max(entry_p, chand_stop)
      }
      
      # 3. Tier 2 target hit: exit 25%, leave 25% runner
      if (t1_hit && !t2_hit && cur_h >= t2_p) {
        t2_hit <- TRUE
        pos_shares_pct <- 0.25
        stop_p <- max(stop_p, chand_stop)
      }
      
      if (t1_hit) stop_p <- max(stop_p, chand_stop)
      
      # 4. Max hold period (7 trading days) for non-runners
      if (!t2_hit && days_h >= 7) {
        bar_r <- (cur_c - prev_c) / prev_c
        authentic_ret[d] <- pos_shares_pct * bar_r - (10 / 10000)
        in_pos <- FALSE
        pos_shares_pct <- 0
        next
      }
      
      bar_r <- (cur_c - prev_c) / prev_c
      authentic_ret[d] <- pos_shares_pct * bar_r
    }
  }
  
  obj$bracket_ret <- xts(authentic_ret, order.by = c_idx)
  saveRDS(obj, cf)
  return(s)
}

res <- mclapply(cache_files, process_bracket_file, mc.cores = min(8, detectCores()))
cat(sprintf("[RebuildBrackets] Successfully rebuilt %d symbols with authentic friction-adjusted brackets.\n", length(unlist(res))))
