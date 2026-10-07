suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(TTR)
  library(parallel)
})

source("R/01_data_loader.R")
source("R/02_trend_lmMA.R")
source("R/03_volatility_garch.R")
source("R/04_feature_pipeline.R")
source("R/05_logistic_model.R")
source("R/08_metrics.R")

data_dir_10y <- "data/data_10yr"
cache_dir_10y <- "data/cache_10yr"
dir.create(cache_dir_10y, showWarnings = FALSE, recursive = TRUE)

broad_lines <- readLines("symbols_broad.txt", warn=FALSE)
broad_syms <- unique(toupper(trimws(unlist(strsplit(gsub("#.*", "", broad_lines), "[, \\t\\r\\n]+")))))
broad_syms <- broad_syms[broad_syms != ""]

spy_ohlcv <- readRDS(file.path(data_dir_10y, "data_spy.rds"))
qqq_ohlcv <- readRDS(file.path(data_dir_10y, "data_qqq.rds"))

process_symbol_10yr <- function(s) {
  cache_file <- file.path(cache_dir_10y, sprintf("%s_10yr_wf.rds", tolower(gsub("[^A-Za-z0-9]", "_", s))))
  if (file.exists(cache_file)) return(s)
  
  df_file <- file.path(data_dir_10y, sprintf("data_%s.rds", tolower(gsub("[^A-Za-z0-9]", "_", s))))
  if (!file.exists(df_file)) return(NULL)
  
  df <- tryCatch(readRDS(df_file), error = function(e) NULL)
  if (is.null(df) || nrow(df) < 150) return(NULL)
  
  c_idx <- intersect(index(df), index(spy_ohlcv))
  if (length(c_idx) < 150) return(NULL)
  
  df_sub  <- df[c_idx]
  spy_sub <- spy_ohlcv[c_idx]
  
  tryCatch({
    pipe_res <- build_feature_dataset(df_sub, benchmark_ohlcv = spy_sub, look_ahead = 7, use_garch = FALSE)
    df_raw <- pipe_res$model_data
    df_raw <- na.omit(df_raw)
    if (is.null(df_raw) || nrow(df_raw) < 100) return(NULL)
    
    # Target column is TargetBinary
    feature_cols <- setdiff(names(df_raw), c("Date", "TargetRet", "TargetBinary"))
    df_model <- data.frame(
      target = as.factor(df_raw$TargetBinary),
      df_raw[, feature_cols, drop = FALSE],
      stringsAsFactors = FALSE
    )
    
    n_bars <- nrow(df_model)
    preds <- numeric(n_bars)
    preds[1:100] <- 0.5
    
    fit_init <- suppressWarnings(glm(target ~ ., data = df_model[1:100, ], family = binomial))
    
    step <- 60
    for (start_i in seq(101, n_bars, by = step)) {
      end_i <- min(start_i + step - 1, n_bars)
      train_sub <- df_model[1:(start_i - 1), ]
      fit <- tryCatch(suppressWarnings(glm(target ~ ., data = train_sub, family = binomial)), error = function(e) fit_init)
      test_sub <- df_model[start_i:end_i, feature_cols, drop = FALSE]
      preds[start_i:end_i] <- tryCatch(suppressWarnings(predict(fit, newdata = test_sub, type = "response")), error = function(e) rep(0.5, nrow(test_sub)))
    }
    preds[is.na(preds)] <- 0.5
    pred_xts <- xts(preds, order.by = as.Date(df_raw$Date))
    
    # Relative Strength vs SPY (63-day)
    stock_cl <- Cl(df_sub)
    spy_cl   <- Cl(spy_sub)
    rs_spy   <- (stock_cl / lag.xts(stock_cl, 63)) - (spy_cl / lag.xts(spy_cl, 63))
    
    # ATR Vol
    atr_val <- as.numeric(ATR(HLC(df_sub), n = 14)$atr) / as.numeric(Cl(df_sub))
    atr_xts <- xts(atr_val, order.by = index(df_sub))
    
    # Bracket return simulation for hold_days = 7
    c_sub  <- as.numeric(Cl(df_sub))
    h_sub  <- as.numeric(Hi(df_sub))
    l_sub  <- as.numeric(Lo(df_sub))
    atr_abs <- as.numeric(ATR(HLC(df_sub), n = 14)$atr)
    atr_abs[is.na(atr_abs)] <- c_sub[is.na(atr_abs)] * 0.02
    
    nb <- length(c_sub)
    bracket_daily_ret <- numeric(nb)
    pos_active <- FALSE
    entry_p <- 0; stop_p <- 0; pt_p <- 0; bars_held <- 0; prev_c <- 0
    
    for (i in 1:nb) {
      curr_c <- c_sub[i]; curr_h <- h_sub[i]; curr_l <- l_sub[i]; curr_atr <- atr_abs[i]
      if (pos_active) {
        bars_held <- bars_held + 1
        is_stopped <- (curr_l <= stop_p)
        is_pt      <- (curr_h >= pt_p)
        is_time    <- (bars_held >= 7)
        
        if (is_stopped) {
          exit_p <- min(prev_c, stop_p)
          bracket_daily_ret[i] <- (exit_p / prev_c) - 1
          pos_active <- FALSE
        } else if (is_pt) {
          exit_p <- max(prev_c, pt_p)
          bracket_daily_ret[i] <- (exit_p / prev_c) - 1
          pos_active <- FALSE
        } else if (is_time) {
          bracket_daily_ret[i] <- (curr_c / prev_c) - 1
          pos_active <- FALSE
        } else {
          bracket_daily_ret[i] <- (curr_c / prev_c) - 1
          stop_p <- max(stop_p, curr_c - 1.5 * curr_atr)
          prev_c <- curr_c
        }
      } else {
        entry_p <- curr_c
        stop_p  <- entry_p - 1.5 * curr_atr
        pt_p    <- entry_p + 2.5 * curr_atr
        prev_c  <- entry_p
        bars_held <- 0
        pos_active <- TRUE
        bracket_daily_ret[i] <- 0
      }
    }
    bracket_xts <- xts(bracket_daily_ret, order.by = index(df_sub))
    
    saveRDS(list(
      symbol = s,
      pred_prob = pred_xts,
      rs_spy = rs_spy,
      garch_vol = atr_xts,
      bracket_ret = bracket_xts,
      dates = as.Date(index(df_sub))
    ), cache_file)
    return(s)
  }, error = function(e) {
    return(NULL)
  })
}

cat("[10Yr-WF] Precomputing walk-forward series for broad universe in parallel...\n")
num_cores <- min(8, detectCores())
res_wf <- mclapply(broad_syms, process_symbol_10yr, mc.cores = num_cores)
valid_symbols <- unlist(res_wf)
cat(sprintf("\n[10Yr-WF] Successfully prepared %d / %d symbols for 10-year backtest.\n", length(valid_symbols), length(broad_syms)))
