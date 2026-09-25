#!/usr/bin/env Rscript
#' Multi-Stock Daily Signal Scanner & Execution Sizer ($10K Capital)
#'
#' Scans your watchlist, updates GARCH(1,1) and lmMA models for each stock,
#' applies Macro Market Gate (QQQ), Earnings Blackout filter, Weekly Trend Synergy,
#' and rolling Walk-Forward Retraining to generate actionable bracket order tickets.
#'
#' Usage:
#'   Rscript daily_signal.R [--symbols=SNDK,NVDA,AAPL] [--capital=10000] [--max_pos=5]

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
source("R/07_leverage_space.R")
source("R/portfolio_manager.R")

# Default Parameters
CAPITAL          <- 10000
MAX_POSITIONS    <- 5     # Max concurrent swing positions to hold (e.g., 5 positions @ $2,000 each)
TARGET_VOL       <- 1.00  # High Growth Sizing (100% allocation of per-position capital)
FAST_N           <- 20
SLOW_N           <- 50
LOOK_AHEAD       <- 5
P_LONG           <- 0.58
P_SHORT          <- 0.42
TRAIN_WINDOW     <- 500   # Rolling historical training window (bars) to prevent regime decay
MACRO_GATE       <- TRUE  # Top-down market regime filter (QQQ)
EARNINGS_DAYS    <- 7     # Disqualify stocks reporting earnings within N trading days (~10 calendar days)
SIZING_MODE      <- "vince" # Position sizing engine: "vince" (Leverage Space) or "equal"
SAFETY_FACTOR    <- 0.50    # Aggressive Safe f scaling factor for Ralph Vince Leverage Space model
VINCE_LOOKBACK   <- 120     # Lookback days for joint scenario return matrix
ALLOW_FRACTIONAL <- TRUE    # Allow fractional shares for exact risk budget allocation
PORTFOLIO_FILE   <- "portfolio.json"
IS_FRIDAY        <- (format(Sys.Date(), "%u") == "5") # Friday weekend exit check

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
  if (grepl("^--train_window=", arg)) TRAIN_WINDOW <- as.numeric(sub("^--train_window=", "", arg))
  if (grepl("^--macro_gate=", arg)) MACRO_GATE <- as.logical(sub("^--macro_gate=", "", arg))
  if (grepl("^--earnings_days=", arg)) EARNINGS_DAYS <- as.numeric(sub("^--earnings_days=", "", arg))
  if (grepl("^--sizing_mode=", arg)) SIZING_MODE <- tolower(sub("^--sizing_mode=", "", arg))
  if (grepl("^--safety_factor=", arg)) SAFETY_FACTOR <- as.numeric(sub("^--safety_factor=", "", arg))
  if (grepl("^--vince_lookback=", arg)) VINCE_LOOKBACK <- as.numeric(sub("^--vince_lookback=", "", arg))
  if (grepl("^--fractional=", arg)) ALLOW_FRACTIONAL <- as.logical(sub("^--fractional=", "", arg))
  if (grepl("^--portfolio_file=", arg)) PORTFOLIO_FILE <- sub("^--portfolio_file=", "", arg)
  if (grepl("^--is_friday=", arg)) IS_FRIDAY <- as.logical(sub("^--is_friday=", "", arg))
  if (grepl("^--symbols_file=", arg)) SYMBOLS_FILE <- sub("^--symbols_file=", "", arg)
  if (grepl("^--symbol=", arg))  SYMBOLS <- strsplit(sub("^--symbol=", "", arg), "[, ]+")[[1]]
  if (grepl("^--symbols=", arg)) SYMBOLS <- strsplit(sub("^--symbols=", "", arg), "[, ]+")[[1]]
  if (grepl("^--record_buy=", arg)) RECORD_BUY <- sub("^--record_buy=", "", arg)
  if (grepl("^--record_sell=", arg)) RECORD_SELL <- sub("^--record_sell=", "", arg)
  if (arg == "--reset_portfolio") RESET_PORT <- TRUE
}

# Portfolio State Initialization & Management
if (RESET_PORT) {
  port_state <- list(
    total_capital = CAPITAL,
    cash_balance = CAPITAL,
    max_positions = MAX_POSITIONS,
    last_updated = as.character(Sys.time()),
    positions = list(),
    closed_trades = list()
  )
  save_portfolio(port_state, PORTFOLIO_FILE)
  cat(sprintf("[PortfolioManager] Reset portfolio to $%.2f cash balance.\n", CAPITAL))
} else {
  port_state <- load_portfolio(PORTFOLIO_FILE, default_capital = CAPITAL, max_positions = MAX_POSITIONS)
}

# Handle Buy Recording: e.g. --record_buy=AMD:9.26:614.61:555.76:702.88
if (!is.null(RECORD_BUY)) {
  parts <- strsplit(RECORD_BUY, "[,:]")[[1]]
  if (length(parts) >= 3) {
    buy_sym <- toupper(parts[1])
    buy_qty <- as.numeric(parts[2])
    buy_px  <- as.numeric(parts[3])
    buy_stp <- if (length(parts) >= 4) as.numeric(parts[4]) else round(buy_px * 0.95, 2)
    buy_tgt <- if (length(parts) >= 5) as.numeric(parts[5]) else round(buy_px * 1.10, 2)
    port_state <- record_fill(port_state, buy_sym, buy_qty, buy_px, buy_stp, buy_tgt)
    save_portfolio(port_state, PORTFOLIO_FILE)
    cat(sprintf("[PortfolioManager] Recorded BUY: %.3f shares of %s at $%.2f (Stop: $%.2f, Target: $%.2f)\n",
                buy_qty, buy_sym, buy_px, buy_stp, buy_tgt))
  }
}

# Handle Sell Recording: e.g. --record_sell=AMD:635.00:TAKE_PROFIT
if (!is.null(RECORD_SELL)) {
  parts <- strsplit(RECORD_SELL, "[,:]")[[1]]
  sell_sym <- toupper(parts[1])
  sell_px  <- as.numeric(parts[2])
  sell_rsn <- if (length(parts) >= 3) parts[3] else "MANUAL_EXIT"
  port_state <- record_exit(port_state, sell_sym, sell_px, reason = sell_rsn)
  save_portfolio(port_state, PORTFOLIO_FILE)
  cat(sprintf("[PortfolioManager] Recorded SELL / EXIT for %s at $%.2f (Reason: %s)\n", sell_sym, sell_px, sell_rsn))
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
cat(" 1. MACRO MARKET & VOLATILITY REGIME GATE (QQQ + VIX DUAL FILTER)\n")
cat("========================================================================================\n")

macro_info <- tryCatch({
  qqq_ohlcv <- load_stock_data("QQQ")
  qqq_price <- Cl(qqq_ohlcv)
  qqq_lm <- lmMA(qqq_price, n = 50)
  latest_qqq_close <- as.numeric(last(qqq_price))
  latest_qqq_fit   <- as.numeric(last(qqq_lm$fit))
  latest_qqq_slope <- as.numeric(last(qqq_lm$slope))
  
  is_bullish <- (latest_qqq_close >= latest_qqq_fit) && (latest_qqq_slope >= 0)
  list(
    close = latest_qqq_close,
    fit = latest_qqq_fit,
    slope = latest_qqq_slope,
    is_bullish = is_bullish,
    regime = if (is_bullish) "BULLISH (Risk-On)" else "DEFENSIVE (Risk-Off)"
  )
}, error = function(e) {
  cat(sprintf("  -> Warning: Macro fetch failed (%s). Defaulting to Bullish.\n", e$message))
  list(close = 0, fit = 0, slope = 0, is_bullish = TRUE, regime = "BULLISH (Default)")
})

# Dynamic CBOE VIX Volatility Regime
vix_info <- tryCatch({
  vix_ohlcv <- load_stock_data("^VIX")
  vix_px <- as.numeric(last(Cl(vix_ohlcv)))
  v_reg <- if (vix_px < 20) "NORMAL (Low Volatility)" else if (vix_px <= 28) "ELEVATED (Caution)" else "CRISIS (Extreme Volatility)"
  list(close = vix_px, regime = v_reg)
}, error = function(e) {
  list(close = 16.5, regime = "NORMAL (Default)")
})

# Multi-Factor Macro Sizing Policy
if (MACRO_GATE && !macro_info$is_bullish) {
  EFFECTIVE_MAX_POS       <- min(2, MAX_POSITIONS)
  EFFECTIVE_P_LONG        <- 0.65
  EFFECTIVE_SAFETY_FACTOR <- min(SAFETY_FACTOR, 0.30)
  macro_note <- "QQQ Trend Deficit: Capping positions at 2, raising P(Up) >= 65%"
} else if (vix_info$close > 28) {
  EFFECTIVE_MAX_POS       <- min(1, MAX_POSITIONS)
  EFFECTIVE_P_LONG        <- 0.68
  EFFECTIVE_SAFETY_FACTOR <- 0.15
  macro_note <- "VIX Crisis (>28): Capital preservation mode, 1 position max, Safe f = 0.15"
} else if (vix_info$close >= 20) {
  EFFECTIVE_MAX_POS       <- min(3, MAX_POSITIONS)
  EFFECTIVE_P_LONG        <- 0.60
  EFFECTIVE_SAFETY_FACTOR <- min(SAFETY_FACTOR, 0.30)
  macro_note <- "VIX Elevated (20-28): Caution scaling to 3 positions, Safe f = 0.30"
} else {
  EFFECTIVE_MAX_POS       <- MAX_POSITIONS
  EFFECTIVE_P_LONG        <- P_LONG
  EFFECTIVE_SAFETY_FACTOR <- SAFETY_FACTOR
  macro_note <- "Full Risk-On Allocation: 5 positions, Safe f = 0.50, P(Up) >= 58.0%"
}

cat(sprintf(" [Macro Gate Status] %s | Volatility Regime: %s\n", macro_info$regime, vix_info$regime))
cat(sprintf("   QQQ Close: $%.2f | 50D lmMA Fit: $%.2f | Slope: %+.3f\n", 
            macro_info$close, macro_info$fit, macro_info$slope))
cat(sprintf("   CBOE VIX:  %.2f   | Volatility Regime: %s\n", vix_info$close, vix_info$regime))
cat(sprintf("   -> Dynamic Policy: Max Positions = %d | Safe f = %.2f | Min P(Up) >= %.1f%%\n\n", 
            EFFECTIVE_MAX_POS, EFFECTIVE_SAFETY_FACTOR, EFFECTIVE_P_LONG * 100))

cat("========================================================================================\n")
cat(sprintf(" 2. SCANNING WATCHLIST (%d ASSETS) WITH WALK-FORWARD RETRAINING\n", length(SYMBOLS)))
cat("========================================================================================\n")

scan_results <- list()

for (sym in SYMBOLS) {
  cat(sprintf("[Scanning %s] Fetching data, checking earnings & updating rolling model...\n", sym))
  
  res <- tryCatch({
    # Upcoming earnings date check
    earn_date <- get_upcoming_earnings_date(sym)
    days_to_earn <- if (!is.na(earn_date)) as.numeric(as.Date(earn_date) - Sys.Date()) else NA_real_
    is_earnings_blackout <- (!is.na(days_to_earn) && days_to_earn >= 0 && days_to_earn <= (EARNINGS_DAYS + 3))

    ohlcv <- load_stock_data(symbol = sym)
    price <- Cl(ohlcv)
    latest_date <- as.character(index(last(price)))
    latest_close <- as.numeric(last(price))
    
    # Feature engineering with weekly trend synergy
    pipeline_out <- build_feature_dataset(
      ohlcv = ohlcv,
      fast_n = FAST_N,
      slow_n = SLOW_N,
      look_ahead = LOOK_AHEAD,
      use_garch = TRUE
    )
    
    df_model <- pipeline_out$model_data
    feat_names <- pipeline_out$feature_names
    weekly_slope_pct <- pipeline_out$latest_weekly_slope_pct
    is_weekly_bullish <- (!is.na(weekly_slope_pct) && weekly_slope_pct > 0)
    
    # Automated Rolling Walk-Forward Retraining (last TRAIN_WINDOW observations)
    n_obs <- nrow(df_model)
    train_slice <- if (n_obs > TRAIN_WINDOW) tail(df_model, TRAIN_WINDOW) else df_model
    X_train <- as.matrix(train_slice[, feat_names])
    y_train <- train_slice$TargetBinary
    set.seed(42)
    cv_fit <- cv.glmnet(X_train, y_train, alpha = 0.5, family = "binomial", type.measure = "deviance")
    
    # Today's features
    dual_lm <- calculate_dual_lmMA(price, fast_n = FAST_N, slow_n = SLOW_N)
    residuals <- price - dual_lm$fast_lm$fit
    resid_vol <- TTR::runSD(residuals, n = FAST_N)
    zscore <- residuals / resid_vol
    garch_out <- compute_garch_volatility(price)
    
    feat_slope_weekly <- xts(rep(weekly_slope_pct, nrow(price)), order.by = index(price))
    
    # Volume features
    vol <- tryCatch(Vo(ohlcv), error = function(e) xts(rep(1, nrow(price)), order.by = index(price)))
    vol_ma <- TTR::runMean(vol, n = FAST_N)
    feat_vol_ratio <- vol / (vol_ma + 1e-6)
    
    obv <- tryCatch(TTR::OBV(price, vol), error = function(e) xts(rep(0, nrow(price)), order.by = index(price)))
    obv_lm <- calculate_dual_lmMA(obv, fast_n = FAST_N, slow_n = SLOW_N)$fast_lm
    obv_sd <- TTR::runSD(obv, n = FAST_N)
    feat_obv_slope <- obv_lm$slope / (obv_sd + 1e-6)

    all_feats <- merge(
      SlopeFast = dual_lm$fast_lm$slope,
      SlopeSlow = dual_lm$slow_lm$slope,
      SlopeWeeklyPct = feat_slope_weekly,
      TrendQuality = dual_lm$fast_lm$r.squared,
      DistPct = dual_lm$dist_pct,
      ZScore = zscore,
      GARCH_Vol = garch_out$annualized_vol,
      GARCH_Shock = garch_out$shocks,
      GARCH_VolPct = garch_out$vol_percentile,
      VolumeRatio = feat_vol_ratio,
      OBV_Slope = feat_obv_slope
    )
    
    latest_feats <- tail(na.omit(all_feats), 1)
    latest_feat_matrix <- matrix(as.numeric(latest_feats), nrow = 1)
    colnames(latest_feat_matrix) <- feat_names
    
    # Model forward probability
    pred_prob <- as.numeric(predict(cv_fit, newx = latest_feat_matrix, s = "lambda.min", type = "response"))
    
    # Volatility & Bracket Levels
    curr_ann_vol <- as.numeric(latest_feats$GARCH_Vol)
    curr_daily_vol <- curr_ann_vol / sqrt(252)
    stop_loss_price <- latest_close * (1 - 2.0 * curr_daily_vol)
    take_profit_price <- latest_close * (1 + 3.0 * curr_daily_vol)
    
    # Signal Assignment with Multi-Timeframe & Earnings Gate
    signal_status <- "HOLD"
    gate_note <- "Normal"
    
    if (pred_prob <= P_SHORT) {
      signal_status <- "CASH"
      gate_note <- "Bearish Model"
    } else if (pred_prob >= EFFECTIVE_P_LONG) {
      if (is_earnings_blackout) {
        signal_status <- "BLACKOUT"
        gate_note <- sprintf("Earnings in %dd (%s)", as.integer(days_to_earn), earn_date)
      } else if (!is_weekly_bullish) {
        signal_status <- "COUNTER_TREND"
        gate_note <- sprintf("Weekly Bear (%.2f%%)", weekly_slope_pct)
      } else {
        signal_status <- "BUY"
        gate_note <- "Synergy Confirmed"
      }
    } else {
      signal_status <- "HOLD"
      gate_note <- sprintf("Below %.0f%% Cutoff", EFFECTIVE_P_LONG * 100)
    }
    
    data.frame(
      Symbol = sym,
      Date = latest_date,
      Close = latest_close,
      Prob_Up = pred_prob,
      Signal = signal_status,
      Gate_Note = gate_note,
      Weekly_Slope = weekly_slope_pct,
      Earnings_Date = ifelse(is.na(earn_date), "None/ETF", earn_date),
      Days_To_Earn = ifelse(is.na(days_to_earn), -999, days_to_earn),
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
cat(sprintf(" Macro Gate: %s | Max Positions: %d | Buy Threshold: P(Up) >= %.1f%%\n",
            macro_info$regime, EFFECTIVE_MAX_POS, EFFECTIVE_P_LONG * 100))
cat("========================================================================================\n")

summary_table <- data.frame(
  Rank       = 1:nrow(df_scan),
  Symbol     = df_scan$Symbol,
  Price      = sprintf("$%.2f", df_scan$Close),
  P_Up       = sprintf("%.1f%%", df_scan$Prob_Up * 100),
  Signal     = df_scan$Signal,
  Weekly     = sprintf("%s%.1f%% [%s]", 
                       ifelse(df_scan$Weekly_Slope >= 0, "+", ""),
                       df_scan$Weekly_Slope, 
                       ifelse(df_scan$Weekly_Slope > 0, "BULL", "BEAR")),
  Earnings   = ifelse(df_scan$Days_To_Earn >= 0 & df_scan$Days_To_Earn <= 90, 
                      sprintf("%s (%dd)", df_scan$Earnings_Date, as.integer(df_scan$Days_To_Earn)),
                      df_scan$Earnings_Date),
  Filter_Note= df_scan$Gate_Note,
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

cur_px_all <- setNames(df_scan$Close, df_scan$Symbol)
sync_res <- sync_portfolio_with_market(
  port_state, 
  current_prices = cur_px_all, 
  current_date = Sys.Date(), 
  is_friday = IS_FRIDAY,
  model_scan_df = df_scan,
  macro_bullish = macro_info$is_bullish
)

if (sync_res$active_count > 0) {
  cat(sprintf(" Active Open Positions (%d of %d active slots used):\n\n", sync_res$active_count, EFFECTIVE_MAX_POS))
  print(sync_res$holdings_df[, c("Symbol", "Shares", "Entry_Price", "Current_Price", "Market_Value", "Unrealized_PnL", "Return_Pct", "Days_Held", "Action_Required")], row.names = FALSE)
  cat("\n")
} else {
  cat(" Currently 0 Open Positions. Portfolio is 100% in CASH.\n\n")
}

if (length(sync_res$alerts) > 0) {
  cat("----------------------------------------------------------------------------------------\n")
  cat(" ⚠️  PORTFOLIO ACTION ALERTS:\n")
  for (al in sync_res$alerts) {
    cat(sprintf("    * %s\n", al))
  }
  cat("----------------------------------------------------------------------------------------\n\n")
}

cash_available <- sync_res$cash_balance
empty_slots <- min(sync_res$available_slots, max(0, EFFECTIVE_MAX_POS - sync_res$active_count))

cat(sprintf(" TOTAL ACCOUNT VALUE:    $%.2f\n", sync_res$total_account_value))
cat(sprintf(" INVESTED IN EQUITIES:   $%.2f (%.1f%%)\n", sync_res$total_invested, sync_res$total_invested / max(1, sync_res$total_account_value) * 100))
cat(sprintf(" AVAILABLE CASH BALANCE: $%.2f (%.1f%%)\n", cash_available, cash_available / max(1, sync_res$total_account_value) * 100))
cat(sprintf(" AVAILABLE CASH SLOTS:   %d of %d (Regime Limit: %d)\n", empty_slots, EFFECTIVE_MAX_POS, EFFECTIVE_MAX_POS))

# ========================================================================================
# ACTIONABLE CAPITAL DEPLOYMENT (ORDERS TO FILL EMPTY CASH SLOTS)
# ========================================================================================
cat("\n========================================================================================\n")

if (isTRUE(IS_FRIDAY)) {
  cat("              FRIDAY 15:30 EVALUATION: CONDITIONAL WEEKEND HOLD REVIEW        \n")
  cat("========================================================================================\n")
  if (sync_res$active_count > 0) {
    holds <- sync_res$holdings_df[sync_res$holdings_df$Status == "WEEKEND_HOLD_APPROVED", ]
    sells <- sync_res$holdings_df[sync_res$holdings_df$Status != "WEEKEND_HOLD_APPROVED", ]
    
    if (nrow(holds) > 0) {
      cat(sprintf(" [APPROVED TO HOLD OVER WEEKEND] (%d Positions with Strong Momentum):\n", nrow(holds)))
      for (h_i in 1:nrow(holds)) {
        h_row <- holds[h_i, ]
        cat(sprintf("   ✓ %s: %s | Current: $%.2f | P&L: %s\n",
                    h_row$Symbol, h_row$Action_Required, h_row$Current_Price, h_row$Return_Pct))
      }
      cat("\n")
    }
    
    if (nrow(sells) > 0) {
      cat(sprintf(" [DEFENSIVE WEEKEND EXITS] (%d Positions to Liquidate Today):\n", nrow(sells)))
      for (s_i in 1:nrow(sells)) {
        s_row <- sells[s_i, ]
        cat(sprintf("   ⚠️ %s: %s | P&L: %s -> SELL AT MARKET BEFORE 16:00 EDT CLOSE!\n",
                    s_row$Symbol, s_row$Action_Required, s_row$Return_Pct))
      }
      cat("\n")
    }
  } else {
    cat(" Portfolio is 100% in cash. No open weekend exposure.\n\n")
  }
  cat(" -> NEXT WEEKLY BUYS: Fresh opportunities will be scanned and executed MONDAY at 14:00 EDT.\n")
  cat("========================================================================================\n\n")
} else {
  cat(sprintf("              RECOMMENDED ORDERS FOR EMPTY CASH SLOTS (%d AVAILABLE)\n", empty_slots))
  cat("========================================================================================\n")
  
  held_syms <- if (sync_res$active_count > 0) sync_res$holdings_df$Symbol else character(0)
  
  # Sector Taxonomy Mapping & Cluster Risk Defense (Max 2 positions per sector)
  SECTOR_MAP <- list(
    AMD   = "Semiconductors",
    NVDA  = "Semiconductors",
    MU    = "Semiconductors",
    SNDK  = "Semiconductors",
    SOXL  = "Semiconductors",
    MSFT  = "Software_MegaCap",
    AAPL  = "Software_MegaCap",
    META  = "Software_MegaCap",
    TSLA  = "Hardware_Tech",
    LITE  = "Hardware_Tech",
    IONQ  = "Hardware_Tech",
    SMHC  = "Hardware_Tech",
    KXIAY = "Hardware_Tech",
    QQQ   = "Index_ETF",
    QLD   = "Index_ETF"
  )
  get_sector <- function(s) if (s %in% names(SECTOR_MAP)) SECTOR_MAP[[s]] else "General_Tech"
  
  # Count existing sector exposure from currently held positions
  held_sectors <- list()
  for (hs in held_syms) {
    sec <- get_sector(hs)
    held_sectors[[sec]] <- if (is.null(held_sectors[[sec]])) 1 else held_sectors[[sec]] + 1
  }
  
  if (empty_slots > 0 && cash_available >= 50) {
    unowned_buys <- df_scan[df_scan$Signal == "BUY" & !(df_scan$Symbol %in% held_syms), ]
    
    # Apply Sector Concentration Filter (Max 2 positions per sector)
    filtered_candidates <- character()
    if (nrow(unowned_buys) > 0) {
      for (cand_i in 1:nrow(unowned_buys)) {
        cand_sym <- unowned_buys$Symbol[cand_i]
        cand_sec <- get_sector(cand_sym)
        c_count  <- if (is.null(held_sectors[[cand_sec]])) 0 else held_sectors[[cand_sec]]
        
        if (c_count < 2) {
          filtered_candidates <- c(filtered_candidates, cand_sym)
          held_sectors[[cand_sec]] <- c_count + 1
          if (length(filtered_candidates) >= empty_slots) break
        } else {
          cat(sprintf(" [Sector Defense] Skipping %s: Sector '%s' already at maximum cap (2 positions).\n",
                      cand_sym, cand_sec))
        }
      }
    }
    
    n_actionable <- length(filtered_candidates)
    
    if (n_actionable > 0) {
      candidate_syms <- filtered_candidates
      actionable_df <- unowned_buys[unowned_buys$Symbol %in% candidate_syms, ]
      vince_alloc <- NULL
      
      # Ralph Vince Leverage Space Model Sizing
      if (SIZING_MODE == "vince") {
        cat(sprintf(" Sizing Engine: Ralph Vince Leverage Space Model (Safe f = %.2f, %d-Day Scenarios, Fractional: %s)\n",
                    EFFECTIVE_SAFETY_FACTOR, VINCE_LOOKBACK, ifelse(ALLOW_FRACTIONAL, "YES (Exact)", "NO (Floor)")))
        tryCatch({
          joint_events <- build_joint_scenario_matrix(candidate_syms, lookback_days = VINCE_LOOKBACK)
          avail_syms <- intersect(candidate_syms, colnames(joint_events))
          if (length(avail_syms) >= 1) {
            sub_events <- joint_events[, avail_syms, drop = FALSE]
            vince_opt <- vince_optimal_f(sub_events, max_leverage = 1.0, safety_factor = EFFECTIVE_SAFETY_FACTOR)
            cur_px_map <- setNames(actionable_df$Close[match(avail_syms, actionable_df$Symbol)], avail_syms)
            vince_alloc <- vince_portfolio_allocation(vince_opt, total_cash = cash_available, current_prices = cur_px_map, allow_fractional = ALLOW_FRACTIONAL)
            cat(sprintf(" -> Optimized Portfolio GHPR: %.4f (Expected Geometric Growth: %+.2f%% / day)\n\n",
                        vince_opt$ghpr, vince_opt$expected_growth_pct))
          }
        }, error = function(e) {
          cat(sprintf(" -> Leverage Space note: %s. Using Equal-Weight slots.\n\n", e$message))
          vince_alloc <<- NULL
        })
      }
      
      slot_capital_equal <- cash_available / empty_slots
      if (is.null(vince_alloc)) {
        cat(sprintf(" Sizing Mode: Equal-Weight Cash Allocation ($%.2f per slot)\n\n", slot_capital_equal))
      }
      
      for (i in 1:n_actionable) {
        row <- actionable_df[actionable_df$Symbol == candidate_syms[i], ]
        c_sec <- get_sector(row$Symbol)
        
        if (!is.null(vince_alloc) && row$Symbol %in% vince_alloc$Symbol) {
          v_row <- vince_alloc[vince_alloc$Symbol == row$Symbol, ]
          shares <- v_row$Shares
          invested <- v_row$Actual_Outlay
          allocated_cash <- v_row$Dollar_Allocation
          opt_f_val <- v_row$Optimal_f
          safe_f_val <- v_row$Safe_f
          max_loss_val <- v_row$Max_Loss
          weight_pct <- v_row$Weight * 100
          sizing_note <- sprintf("Vince Optimal f: %.4f | Safe f: %.4f | Max Loss: %s",
                                 opt_f_val, safe_f_val, max_loss_val)
          alloc_note  <- sprintf("Target Allocation: %.1f%% ($%.2f) -> Actual Outlay: $%.2f (Cash Left: $%.2f)",
                                 weight_pct, allocated_cash, invested, v_row$Cash_Left)
        } else {
          shares <- if (isTRUE(ALLOW_FRACTIONAL)) round(slot_capital_equal / row$Close, 3) else floor(slot_capital_equal / row$Close)
          invested <- round(shares * row$Close, 2)
          sizing_note <- "Equal-Weight Slot Allocation"
          alloc_note  <- sprintf("Slot Budget: $%.2f -> Actual Outlay: $%.2f (Cash Left: $%.2f)",
                                 slot_capital_equal, invested, slot_capital_equal - invested)
        }
        
        shares_display <- if (isTRUE(ALLOW_FRACTIONAL) && (shares %% 1 != 0)) sprintf("%.3f", shares) else sprintf("%d", as.integer(shares))
        
        # Multi-Tier Bracket Pricing
        tier1_shares <- if (isTRUE(ALLOW_FRACTIONAL)) round(shares * 0.5, 3) else floor(shares * 0.5)
        tier2_shares <- round(shares - tier1_shares, 3)
        tier1_target <- round(row$Close * (1 + 3.0 * row$Daily_Vol), 2)  # +1.5R target
        tier2_target <- round(row$Close * (1 + 6.0 * row$Daily_Vol), 2)  # +3.0R runner target
        total_risk   <- round(shares * (row$Close - row$Stop_Loss), 2)
        tier1_gain   <- round(tier1_shares * (tier1_target - row$Close), 2)
        tier2_gain   <- round(tier2_shares * (tier2_target - row$Close), 2)
        
        cat(sprintf("--- ORDER TICKET #%d: %s (P(Up): %.1f%% | Sector: %s) ---\n", 
                    i, row$Symbol, row$Prob_Up * 100, c_sec))
        cat(sprintf("  Action:              BUY %s SHARES at Market (Monday Afternoon Execution)\n", shares_display))
        cat(sprintf("  Position Sizing:     %s\n", sizing_note))
        cat(sprintf("  Capital Allocation:  %s\n", alloc_note))
        cat(sprintf("  Multi-Timeframe:     Weekly Trend +%.2f%% [BULLISH SYNERGY]\n", row$Weekly_Slope))
        cat(sprintf("  Earnings Safe:       Next report %s (%d days out)\n", row$Earnings_Date, as.integer(row$Days_To_Earn)))
        cat(sprintf("  GTC Stop-Loss:       $%.2f (-%.2f%%) [Total Downside Risk: $%.2f]\n", 
                    row$Stop_Loss, 2.0 * row$Daily_Vol * 100, total_risk))
        cat("  Multi-Tier Bracket Exits:\n")
        cat(sprintf("    -> Tier 1 (50%% = %s shs): Target $%.2f (+%.2f%%, +1.5R) [Gain: $%.2f] -> Lock in profits & move stop to Breakeven $%.2f\n",
                    tier1_shares, tier1_target, 3.0 * row$Daily_Vol * 100, tier1_gain, row$Close))
        cat(sprintf("    -> Tier 2 (50%% = %s shs): Target $%.2f (+%.2f%%, +3.0R) [Gain: $%.2f] -> Momentum runner\n",
                    tier2_shares, tier2_target, 6.0 * row$Daily_Vol * 100, tier2_gain))
        cat(sprintf("  Reward / Risk Ratio: Tier 1: 1.50R | Tier 2: 3.00R (Combined Potential: +$%.2f vs -$%.2f Risk)\n\n",
                    tier1_gain + tier2_gain, total_risk))
      }
    } else {
      cat(sprintf(" No unowned symbols currently meet all BUY criteria (P(Up) >= %.1f%%, Weekly Bullish, Sector Cap).\n",
                  EFFECTIVE_P_LONG * 100))
      cat(" RECOMMENDATION: Retain available cash buffer in money market / cash.\n")
    }
  } else {
    cat(sprintf(" All %d active portfolio slots are currently filled or constrained by Macro Regime.\n", EFFECTIVE_MAX_POS))
    cat(" No new purchases needed today.\n")
  }
  cat("========================================================================================\n\n")
}
