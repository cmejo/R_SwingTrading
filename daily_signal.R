#!/usr/bin/env Rscript
#' Multi-Stock Daily Signal Scanner & Execution Sizer ($10K Capital)
#'
#' Scans your watchlist, updates GARCH(1,1) and lmMA models for each stock,
#' ranks opportunities by P(Up), and allocates capital to the top candidates.
#'
#' Usage:
#'   Rscript daily_signal.R [--symbols=SNDK,NVDA,AAPL] [--capital=10000] [--max_pos=2]

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(TTR)
  library(glmnet)
  library(tseries)
  library(jsonlite)
})

source("R/01_data_loader.R")
source("R/02_trend_lmMA.R")
source("R/03_volatility_garch.R")
source("R/04_feature_pipeline.R")
source("R/05_logistic_model.R")

# Default Parameters
CAPITAL       <- 10000
MAX_POSITIONS <- 5     # Max concurrent swing positions to hold (e.g., 5 positions @ $2,000 each)
TARGET_VOL    <- 1.00  # High Growth Sizing (100% allocation of per-position capital)
FAST_N        <- 20
SLOW_N        <- 50
LOOK_AHEAD    <- 5
P_LONG        <- 0.58
P_SHORT       <- 0.42

# Read symbols & portfolio management CLI args
SYMBOLS <- NULL
SYMBOLS_FILE <- NULL
RECORD_BUY  <- NULL
RECORD_SELL <- NULL
RESET_PORT  <- FALSE

args <- commandArgs(trailingOnly = TRUE)
for (arg in args) {
  if (grepl("^--capital=", arg)) CAPITAL <- as.numeric(sub("^--capital=", "", arg))
  if (grepl("^--max_pos=", arg)) MAX_POSITIONS <- as.numeric(sub("^--max_pos=", "", arg))
  if (grepl("^--target_vol=", arg)) TARGET_VOL <- as.numeric(sub("^--target_vol=", "", arg))
  if (grepl("^--symbols_file=", arg)) SYMBOLS_FILE <- sub("^--symbols_file=", "", arg)
  if (grepl("^--symbol=", arg))  SYMBOLS <- strsplit(sub("^--symbol=", "", arg), "[, ]+")[[1]]
  if (grepl("^--symbols=", arg)) SYMBOLS <- strsplit(sub("^--symbols=", "", arg), "[, ]+")[[1]]
  if (grepl("^--record_buy=", arg)) RECORD_BUY <- sub("^--record_buy=", "", arg)
  if (grepl("^--record_sell=", arg)) RECORD_SELL <- sub("^--record_sell=", "", arg)
  if (arg == "--reset_portfolio") RESET_PORT <- TRUE
}

# Portfolio State Management
PORTFOLIO_FILE <- "portfolio.json"
if (!file.exists(PORTFOLIO_FILE) || RESET_PORT) {
  port_state <- list(total_capital = CAPITAL, positions = list())
  write(toJSON(port_state, auto_unbox = TRUE, pretty = TRUE), PORTFOLIO_FILE)
} else {
  port_state <- fromJSON(PORTFOLIO_FILE)
}

# Handle Buy Recording: e.g. --record_buy=SNDK,2,1816.57
if (!is.null(RECORD_BUY)) {
  parts <- strsplit(RECORD_BUY, "[,:]")[[1]]
  buy_sym <- toupper(parts[1])
  buy_qty <- as.numeric(parts[2])
  buy_px  <- as.numeric(parts[3])
  port_state$positions[[buy_sym]] <- list(
    symbol = buy_sym, shares = buy_qty, entry_price = buy_px, entry_date = as.character(Sys.Date())
  )
  write(toJSON(port_state, auto_unbox = TRUE, pretty = TRUE), PORTFOLIO_FILE)
  cat(sprintf("[Portfolio] Recorded BUY: %d shares of %s at $%.2f\n", buy_qty, buy_sym, buy_px))
}

# Handle Sell Recording: e.g. --record_sell=SNDK,1850.00
if (!is.null(RECORD_SELL)) {
  parts <- strsplit(RECORD_SELL, "[,:]")[[1]]
  sell_sym <- toupper(parts[1])
  if (sell_sym %in% names(port_state$positions)) {
    port_state$positions[[sell_sym]] <- NULL
    write(toJSON(port_state, auto_unbox = TRUE, pretty = TRUE), PORTFOLIO_FILE)
    cat(sprintf("[Portfolio] Recorded SELL / EXIT to CASH for %s\n", sell_sym))
  }
}

# Resolve symbols.txt path if not explicitly provided as a vector
if (is.null(SYMBOLS)) {
  target_file <- if (!is.null(SYMBOLS_FILE) && file.exists(SYMBOLS_FILE)) {
    SYMBOLS_FILE
  } else if (file.exists("symbols.txt")) {
    "symbols.txt"
  } else if (file.exists("/Volumes/2TB.ssd/_a Development/swingtrading/symbols.txt")) {
    "/Volumes/2TB.ssd/_a Development/swingtrading/symbols.txt"
  } else {
    NULL
  }
  
  if (!is.null(target_file)) {
    lines <- readLines(target_file, warn = FALSE)
    # Strip comments (#...) and empty lines
    lines <- gsub("#.*", "", lines)
    raw_tokens <- unlist(strsplit(lines, "[,\\s]+"))
    SYMBOLS <- unique(toupper(trimws(raw_tokens)))
    SYMBOLS <- SYMBOLS[SYMBOLS != ""]
    cat(sprintf("[Config] Loaded %d symbols from '%s': %s\n", 
                length(SYMBOLS), target_file, paste(SYMBOLS, collapse = ", ")))
  } else {
    SYMBOLS <- c("SNDK", "NVDA", "AAPL", "AMD", "MSFT")
    cat("[Config] No symbols.txt found. Using fallback universe: SNDK, NVDA, AAPL, AMD, MSFT\n")
  }
} else {
  SYMBOLS <- unique(toupper(trimws(SYMBOLS)))
  cat(sprintf("[Config] Using CLI provided symbols: %s\n", paste(SYMBOLS, collapse = ", ")))
}

cat("\n========================================================================================\n")
cat(sprintf(" MULTI-ASSET QUANTITATIVE SWING SCANNER | CAPITAL: $%.2f | MAX POSITIONS: %d\n", CAPITAL, MAX_POSITIONS))
cat(sprintf(" Watchlist (%d Symbols): %s\n", length(SYMBOLS), paste(SYMBOLS, collapse = ", ")))
cat("========================================================================================\n\n")

scan_results <- list()

for (sym in SYMBOLS) {
  cat(sprintf("[Scanning %s] Fetching data & updating models...\n", sym))
  
  res <- tryCatch({
    ohlcv <- load_stock_data(symbol = sym)
    price <- Cl(ohlcv)
    latest_date <- as.character(index(last(price)))
    latest_close <- as.numeric(last(price))
    
    # Feature engineering
    pipeline_out <- build_feature_dataset(
      ohlcv = ohlcv,
      fast_n = FAST_N,
      slow_n = SLOW_N,
      look_ahead = LOOK_AHEAD,
      use_garch = TRUE
    )
    
    df_model <- pipeline_out$model_data
    feat_names <- pipeline_out$feature_names
    
    # Train ElasticNet model
    X_train <- as.matrix(df_model[, feat_names])
    y_train <- df_model$TargetBinary
    set.seed(42)
    cv_fit <- cv.glmnet(X_train, y_train, alpha = 0.5, family = "binomial", type.measure = "deviance")
    
    # Today's features
    dual_lm <- calculate_dual_lmMA(price, fast_n = FAST_N, slow_n = SLOW_N)
    residuals <- price - dual_lm$fast_lm$fit
    resid_vol <- TTR::runSD(residuals, n = FAST_N)
    zscore <- residuals / resid_vol
    garch_out <- compute_garch_volatility(price)
    
    all_feats <- merge(
      SlopeFast = dual_lm$fast_lm$slope,
      SlopeSlow = dual_lm$slow_lm$slope,
      TrendQuality = dual_lm$fast_lm$r.squared,
      DistPct = dual_lm$dist_pct,
      ZScore = zscore,
      GARCH_Vol = garch_out$annualized_vol,
      GARCH_Shock = garch_out$shocks,
      GARCH_VolPct = garch_out$vol_percentile
    )
    
    latest_feats <- tail(na.omit(all_feats), 1)
    latest_feat_matrix <- matrix(as.numeric(latest_feats), nrow = 1)
    colnames(latest_feat_matrix) <- feat_names
    
    # Model forward probability
    pred_prob <- as.numeric(predict(cv_fit, newx = latest_feat_matrix, s = "lambda.min", type = "response"))
    
    # Volatility & Levels
    curr_ann_vol <- as.numeric(latest_feats$GARCH_Vol)
    curr_daily_vol <- curr_ann_vol / sqrt(252)
    stop_loss_price <- latest_close * (1 - 2.0 * curr_daily_vol)
    take_profit_price <- latest_close * (1 + 3.0 * curr_daily_vol)
    
    signal_status <- if (pred_prob >= P_LONG) "BUY" else if (pred_prob <= P_SHORT) "CASH" else "HOLD"
    
    data.frame(
      Symbol = sym,
      Date = latest_date,
      Close = latest_close,
      Prob_Up = pred_prob,
      Signal = signal_status,
      GARCH_AnnVol = curr_ann_vol,
      Daily_Vol = curr_daily_vol,
      Stop_Loss = stop_loss_price,
      Take_Profit = take_profit_price,
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    cat(sprintf("  -> Skipping %s: %s\n", sym, e$message))
    NULL
  })
  
  if (!is.null(res)) {
    scan_results[[length(scan_results) + 1]] <- res
  }
}

if (length(scan_results) == 0) {
  stop("No symbols successfully scanned.")
}

df_scan <- do.call(rbind, scan_results)

# Rank by Model Probability Descending
df_scan <- df_scan[order(-df_scan$Prob_Up), ]
rownames(df_scan) <- 1:nrow(df_scan)

# Formatted Leaderboard
cat("\n========================================================================================\n")
cat("               MULTI-ASSET SWING TRADING OPPORTUNITY LEADERBOARD\n")
cat("========================================================================================\n")

summary_table <- data.frame(
  Rank       = 1:nrow(df_scan),
  Symbol     = df_scan$Symbol,
  Price      = sprintf("$%.2f", df_scan$Close),
  P_Up       = sprintf("%.1f%%", df_scan$Prob_Up * 100),
  Signal     = df_scan$Signal,
  GARCH_Vol  = sprintf("%.1f%%", df_scan$GARCH_AnnVol * 100),
  Stop_Loss  = sprintf("$%.2f (-%.1f%%)", df_scan$Stop_Loss, 2.0 * df_scan$Daily_Vol * 100),
  Take_Profit= sprintf("$%.2f (+%.1f%%)", df_scan$Take_Profit, 3.0 * df_scan$Daily_Vol * 100)
)
print(summary_table, row.names = FALSE)

# ========================================================================================
# PORTFOLIO STATE & CASH MONITOR
# ========================================================================================
cat("\n========================================================================================\n")
cat("                        MY PORTFOLIO HOLDINGS & CASH MONITOR\n")
cat("========================================================================================\n")

held_syms <- names(port_state$positions)
n_held <- length(held_syms)
total_invested_val <- 0

if (n_held > 0) {
  cat(sprintf(" Active Open Positions (%d of %d slots used):\n\n", n_held, MAX_POSITIONS))
  
  for (h_sym in held_syms) {
    p_info <- port_state$positions[[h_sym]]
    cur_px <- if (h_sym %in% df_scan$Symbol) df_scan$Close[df_scan$Symbol == h_sym] else p_info$entry_price
    cur_sig <- if (h_sym %in% df_scan$Symbol) df_scan$Signal[df_scan$Symbol == h_sym] else "N/A"
    cur_prob <- if (h_sym %in% df_scan$Symbol) sprintf("%.1f%%", df_scan$Prob_Up[df_scan$Symbol == h_sym] * 100) else "N/A"
    
    pos_val <- p_info$shares * cur_px
    cost_val <- p_info$shares * p_info$entry_price
    unrealized_pnl <- pos_val - cost_val
    unrealized_pct <- (cur_px - p_info$entry_price) / p_info$entry_price * 100
    total_invested_val <- total_invested_val + pos_val
    
    cat(sprintf("  [%s] %d SHARES | Entry: $%.2f | Current: $%.2f | Value: $%.2f | P&L: %s$%.2f (%s%.2f%%)\n",
                h_sym, p_info$shares, p_info$entry_price, cur_px, pos_val,
                ifelse(unrealized_pnl >= 0, "+", "-"), abs(unrealized_pnl),
                ifelse(unrealized_pct >= 0, "+", "-"), abs(unrealized_pct)))
    
    if (cur_sig == "CASH") {
      cat(sprintf("    *** ACTION REQUIRED: Model signal flipped to CASH (P(Up): %s). SELL at Market Open! ***\n", cur_prob))
    } else {
      cat(sprintf("    ✓ Status: %s (P(Up): %s) -> MAINTAIN HOLDING (Brackets Active)\n", cur_sig, cur_prob))
    }
    cat("\n")
  }
} else {
  cat(" Currently 0 Open Positions. Portfolio is 100% in CASH.\n\n")
}

cash_available <- max(0, CAPITAL - total_invested_val)
empty_slots <- max(0, MAX_POSITIONS - n_held)

cat(sprintf(" TOTAL ACCOUNT VALUE:    $%.2f\n", total_invested_val + cash_available))
cat(sprintf(" INVESTED IN EQUITIES:   $%.2f (%.1f%%)\n", total_invested_val, total_invested_val / CAPITAL * 100))
cat(sprintf(" AVAILABLE CASH BALANCE: $%.2f (%.1f%%)\n", cash_available, cash_available / CAPITAL * 100))
cat(sprintf(" AVAILABLE CASH SLOTS:   %d of %d\n", empty_slots, MAX_POSITIONS))

# ========================================================================================
# ACTIONABLE CAPITAL DEPLOYMENT (ORDERS TO FILL EMPTY CASH SLOTS)
# ========================================================================================
cat("\n========================================================================================\n")
cat(sprintf("              RECOMMENDED ORDERS FOR EMPTY CASH SLOTS (%d AVAILABLE)\n", empty_slots))
cat("========================================================================================\n")

if (empty_slots > 0) {
  # Candidate buys excluding already held positions
  unowned_buys <- df_scan[df_scan$Signal == "BUY" & !(df_scan$Symbol %in% held_syms), ]
  n_actionable <- min(nrow(unowned_buys), empty_slots)
  
  if (n_actionable > 0) {
    slot_capital <- cash_available / empty_slots
    cat(sprintf(" Available Cash per Slot: $%.2f\n\n", slot_capital))
    
    for (i in 1:n_actionable) {
      row <- unowned_buys[i, ]
      shares <- floor(slot_capital / row$Close)
      invested <- shares * row$Close
      
      cat(sprintf("--- ORDER TICKET #%d: %s (P(Up): %.1f%%) ---\n", i, row$Symbol, row$Prob_Up * 100))
      cat(sprintf("  Action:              BUY %d SHARES at Market Open\n", shares))
      cat(sprintf("  Estimated Outlay:    $%.2f (Cash remaining in slot: $%.2f)\n", invested, slot_capital - invested))
      cat(sprintf("  GTC Stop-Loss:       $%.2f (-%.2f%%) [Risk: $%.2f]\n", 
                  row$Stop_Loss, 2.0 * row$Daily_Vol * 100, shares * (row$Close - row$Stop_Loss)))
      cat(sprintf("  GTC Take-Profit:     $%.2f (+%.2f%%) [Gain: $%.2f]\n", 
                  row$Take_Profit, 3.0 * row$Daily_Vol * 100, shares * (row$Take_Profit - row$Close)))
      cat(sprintf("  Reward / Risk Ratio: 1.50 : 1.0\n\n"))
    }
  } else {
    cat(" No unowned symbols currently meet BUY criteria (P(Up) >= 58%).\n")
    cat(" RECOMMENDATION: Retain available cash buffer in money market / cash.\n")
  }
} else {
  cat(" All portfolio slots are currently filled. No new purchases needed.\n")
}
cat("========================================================================================\n\n")
