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
source("R/send_alert.R")

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
LEVERAGE         <- 1.0     # Default leverage multiplier: 1.0 (cash only). Set >1.0 for margin.
MAX_PER_SECTOR   <- 2       # Maximum concurrent positions allowed in any single sector
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
  if (grepl("^--leverage=", arg)) LEVERAGE <- as.numeric(sub("^--leverage=", "", arg))
  if (grepl("^--margin_leverage=", arg)) LEVERAGE <- as.numeric(sub("^--margin_leverage=", "", arg))
  if (grepl("^--max_pos=", arg)) MAX_POSITIONS <- as.numeric(sub("^--max_pos=", "", arg))
  if (grepl("^--max_per_sector=", arg)) MAX_PER_SECTOR <- as.numeric(sub("^--max_per_sector=", "", arg))
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
    raw_tokens <- unlist(strsplit(lines, "[, \\t\\r\\n]+"))
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

# Benchmark (SPY) for Relative Strength
spy_ohlcv <- tryCatch({
  load_stock_data("SPY")
}, error = function(e) {
  qqq_ohlcv
})
spy_px <- as.numeric(last(Cl(spy_ohlcv)))

# Dynamic CBOE VIX Volatility Regime
vix_info <- tryCatch({
  vix_ohlcv <- load_stock_data("^VIX")
  vix_close <- suppressWarnings(na.omit(Cl(vix_ohlcv)))
  vix_px <- as.numeric(last(vix_close))
  v_reg <- if (vix_px < 20) "NORMAL (Low Volatility)" else if (vix_px <= 28) "ELEVATED (Caution)" else "CRISIS (Extreme Volatility)"
  list(close = vix_px, regime = v_reg)
}, error = function(e) {
  list(close = 16.5, regime = "NORMAL (Default)")
})

# Multi-Factor Macro Sizing Policy
# VIX Crisis must be checked FIRST since it's the most restrictive
if (vix_info$close > 28) {
  EFFECTIVE_MAX_POS       <- min(1, MAX_POSITIONS)
  EFFECTIVE_P_LONG        <- 0.68
  EFFECTIVE_SAFETY_FACTOR <- 0.15
  macro_note <- "VIX Crisis (>28): Capital preservation mode, 1 position max, Safe f = 0.15"
} else if (MACRO_GATE && !macro_info$is_bullish) {
  EFFECTIVE_MAX_POS       <- min(2, MAX_POSITIONS)
  EFFECTIVE_P_LONG        <- 0.65
  EFFECTIVE_SAFETY_FACTOR <- min(SAFETY_FACTOR, 0.30)
  macro_note <- "QQQ Trend Deficit: Capping positions at 2, raising P(Up) >= 65%"
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
cat(sprintf("   SPY Benchmark Close: $%.2f\n", spy_px))
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
    
    # Feature engineering with weekly trend synergy & benchmark relative strength
    pipeline_out <- build_feature_dataset(
      ohlcv = ohlcv,
      benchmark_ohlcv = spy_ohlcv,
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
    if (length(unique(y_train)) < 2) {
      stop(sprintf("Training slice for %s contains only 1 target class.", sym))
    }
    
    # Unified Model Trainer with Purged CV (L1), Platt Calibration (M1), and Feature Attribution (M2)
    latest_feat_matrix <- pipeline_out$latest_feature_matrix
    model_core <- train_swing_model(
      X_train       = X_train,
      y_train       = y_train,
      X_test        = latest_feat_matrix,
      feature_names = feat_names,
      alpha         = 0.5,
      p_long        = EFFECTIVE_P_LONG,
      p_short       = P_SHORT,
      calibrate     = TRUE,
      embargo_days  = 5
    )
    
    cv_fit            <- model_core$cv_fit
    pred_prob         <- model_core$pred_probs[1]
    is_intercept_only <- model_core$is_intercept_only
    feat_imp          <- model_core$feature_importance
    top_feature_str   <- if (!is_intercept_only) paste0(feat_imp$Feature[1], " (", feat_imp$Direction[1], ")") else "Base-Rate"

    # Use single source of truth for features and volatility from pipeline_out
    curr_ann_vol       <- pipeline_out$latest_ann_vol
    latest_rs_20       <- pipeline_out$latest_rs_20
    is_rs_leader       <- (!is.na(latest_rs_20) && latest_rs_20 >= 0)

    # 20-day Average Daily Volume & Dollar Volume (L3 Market Impact Guard)
    vol_series <- tryCatch(Vo(ohlcv), error = function(e) xts(rep(1e6, nrow(price)), order.by = index(price)))
    adv_20 <- as.numeric(tail(na.omit(TTR::runMean(vol_series, n = 20)), 1))
    if (is.na(adv_20) || adv_20 <= 0) adv_20 <- 1e6
    addv_20 <- adv_20 * latest_close
    is_illiquid <- (addv_20 < 5e6)  # Minimum $5M Average Daily Dollar Volume

    # 14-day ATR & Dynamic Chandelier Trailing Stop anchor
    atr_14 <- tryCatch({
      as.numeric(tail(TTR::ATR(HLC(ohlcv), n = 14)$atr, 1))
    }, error = function(e) 0.02 * latest_close)
    chandelier_stop <- round(latest_close - (2.5 * atr_14), 2)

    # Volatility & Bracket Levels (with sanity bounds enforced in pipeline)
    curr_daily_vol <- curr_ann_vol / sqrt(252)
    stop_loss_price <- round(latest_close * (1 - 2.0 * curr_daily_vol), 2)
    take_profit_price <- round(latest_close * (1 + 3.0 * curr_daily_vol), 2)

    # Signal Assignment with Multi-Timeframe, RS Leader, Liquidity & Earnings Gate
    signal_status <- "HOLD"
    gate_note <- "Normal"

    if (pred_prob <= P_SHORT) {
      signal_status <- "CASH"
      gate_note <- "Bearish Model"
    } else if (is_illiquid) {
      signal_status <- "ILLIQUID"
      gate_note <- sprintf("Low ADDV ($%.1fM < $5M)", addv_20 / 1e6)
    } else if (pred_prob >= EFFECTIVE_P_LONG) {
      if (is_earnings_blackout) {
        signal_status <- "BLACKOUT"
        gate_note <- sprintf("Earnings in %dd (%s)", as.integer(days_to_earn), earn_date)
      } else if (!is_weekly_bullish) {
        signal_status <- "COUNTER_TREND"
        gate_note <- sprintf("Weekly Bear (%.2f%%)", weekly_slope_pct)
      } else if (!is_rs_leader) {
        signal_status <- "RS_LAGGER"
        gate_note <- sprintf("RS Lagger (%+.1f%% vs SPY)", latest_rs_20 * 100)
      } else if (is_intercept_only) {
        signal_status <- "BUY"
        gate_note <- sprintf("Leader [BaseRateOnly 0/%d coefs]", length(feat_names))
      } else {
        signal_status <- "BUY"
        gate_note <- sprintf("Leader (%s)", top_feature_str)
      }
    } else {
      signal_status <- "HOLD"
      gate_note <- if (is_intercept_only) sprintf("BaseRateOnly (0/%d coefs)", length(feat_names)) else sprintf("Below %.0f%% Cutoff", EFFECTIVE_P_LONG * 100)
    }
    
    data.frame(
      Symbol = sym,
      Date = latest_date,
      Close = latest_close,
      Prob_Up = pred_prob,
      Signal = signal_status,
      Gate_Note = gate_note,
      Weekly_Slope = weekly_slope_pct,
      RS_20 = latest_rs_20,
      ATR_14 = atr_14,
      Chandelier_Stop = chandelier_stop,
      ADV_20 = adv_20,
      Top_Feature = top_feature_str,
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
  RS_SPY     = sprintf("%s%.1f%%", ifelse(df_scan$RS_20 >= 0, "+", ""), df_scan$RS_20 * 100),
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
port_state <- sync_res$updated_portfolio
save_portfolio(port_state, PORTFOLIO_FILE)

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
purchasing_power <- cash_available * LEVERAGE

cat(sprintf(" TOTAL ACCOUNT VALUE:    $%.2f\n", sync_res$total_account_value))
cat(sprintf(" PEAK EQUITY RECORD:     $%.2f (Drawdown: %+.2f%%)\n", sync_res$peak_equity, sync_res$drawdown_pct))
cat(sprintf(" INVESTED IN EQUITIES:   $%.2f (%.1f%%)\n", sync_res$total_invested, sync_res$total_invested / max(1, sync_res$total_account_value) * 100))
cat(sprintf(" AVAILABLE CASH BALANCE: $%.2f (%.1f%%)\n", cash_available, cash_available / max(1, sync_res$total_account_value) * 100))
if (LEVERAGE > 1.0) {
  cat(sprintf(" MARGIN LEVERAGE RATIO:  %.2fx (Active Buying Power: $%.2f)\n", LEVERAGE, purchasing_power))
}
cat(sprintf(" AVAILABLE CASH SLOTS:   %d of %d (Regime Limit: %d)\n", empty_slots, EFFECTIVE_MAX_POS, EFFECTIVE_MAX_POS))

# Account Drawdown Circuit Breaker Enforcement
if (isTRUE(sync_res$circuit_breaker_active)) {
  EFFECTIVE_SAFETY_FACTOR <- EFFECTIVE_SAFETY_FACTOR * 0.50
  cat(sprintf(" ⚠️  [CIRCUIT BREAKER ENGAGED] Account drawdown %+.2f%% <= -4.0%%! Throttling Safe f to %.2f\n", 
              sync_res$drawdown_pct, EFFECTIVE_SAFETY_FACTOR))
}

# Total Portfolio Heat (Max 5.0% Dollars at Risk across portfolio)
existing_dollar_risk <- if (sync_res$active_count > 0) {
  sum(sapply(port_state$positions, function(p) {
    pmax(0, as.numeric(p$shares) * (as.numeric(p$entry_price) - as.numeric(p$stop_loss)))
  }))
} else {
  0.0
}
max_portfolio_heat <- 0.05 * sync_res$total_account_value
avail_heat_budget  <- max(0, max_portfolio_heat - existing_dollar_risk)
cat(sprintf(" PORTFOLIO HEAT (RISK):  $%.2f of $%.2f max allowed (%.2f%% of equity)\n",
            existing_dollar_risk, max_portfolio_heat, (existing_dollar_risk / max(1, sync_res$total_account_value)) * 100))

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
  
  # Sector Taxonomy Mapping & Cluster Risk Defense
  SECTOR_MAP <- if (file.exists("sector_map.json")) {
    tryCatch(jsonlite::fromJSON("sector_map.json", simplifyDataFrame = FALSE), error = function(e) list())
  } else if (file.exists("/Volumes/2TB.ssd/_a Development/swingtrading/sector_map.json")) {
    tryCatch(jsonlite::fromJSON("/Volumes/2TB.ssd/_a Development/swingtrading/sector_map.json", simplifyDataFrame = FALSE), error = function(e) list())
  } else {
    list()
  }
  if (length(SECTOR_MAP) == 0) {
    SECTOR_MAP <- list(
      # Semiconductors & Semiconductor Equipment
      AMD   = "Semiconductors",
    NVDA  = "Semiconductors",
    TSM   = "Semiconductors",
    AVGO  = "Semiconductors",
    QCOM  = "Semiconductors",
    MU    = "Semiconductors",
    AMAT  = "Semiconductors",
    LRCX  = "Semiconductors",
    ARM   = "Semiconductors",
    SNDK  = "Semiconductors",
    SOXL  = "Semiconductors",
    SMH   = "Semiconductors",

    # Mega-Cap Tech & AI Platforms
    MSFT  = "MegaCap_Tech",
    AAPL  = "MegaCap_Tech",
    AMZN  = "MegaCap_Tech",
    GOOGL = "MegaCap_Tech",
    META  = "MegaCap_Tech",
    TSLA  = "MegaCap_Tech",

    # Enterprise Software, Cloud & SaaS
    CRM   = "Enterprise_Software",
    NOW   = "Enterprise_Software",
    ADBE  = "Enterprise_Software",
    INTU  = "Enterprise_Software",
    ORCL  = "Enterprise_Software",
    SNOW  = "Enterprise_Software",
    WDAY  = "Enterprise_Software",
    SHOP  = "Enterprise_Software",
    PLTR  = "Enterprise_Software",

    # Cybersecurity & Infrastructure
    PANW  = "Cybersecurity",
    CRWD  = "Cybersecurity",
    FTNT  = "Cybersecurity",
    NET   = "Cybersecurity",
    ANET  = "Cybersecurity",
    CSCO  = "Cybersecurity",

    # Hardware, Photonics & Quantum / Emerging Tech
    LITE  = "Hardware_Tech",
    IONQ  = "Hardware_Tech",
    SMHC  = "Hardware_Tech",
    KXIAY = "Hardware_Tech",
    DELL  = "Hardware_Tech",
    SMCI  = "Hardware_Tech",

    # Fintech, Payments & Digital Assets
    V     = "Fintech_Financials",
    MA    = "Fintech_Financials",
    PYPL  = "Fintech_Financials",
    SQ    = "Fintech_Financials",
    COIN  = "Fintech_Financials",
    MSTR  = "Fintech_Financials",

    # Energy Transition & High-Power Industrials
    CEG   = "Power_Industrial",
    VST   = "Power_Industrial",
    GE    = "Power_Industrial",

    # Benchmark & Leveraged Index ETFs
    QQQ   = "Index_ETF",
    QLD   = "Index_ETF",
    SPY   = "Index_ETF",
    XLK   = "Index_ETF"
  )
  }
  sector_map_dirty <- FALSE
  get_sector <- function(s) {
    s <- toupper(trimws(s))
    if (s %in% names(SECTOR_MAP)) {
      return(SECTOR_MAP[[s]])
    }
    # Dynamic Sector Lookup fallback (R1)
    auto_sec <- get_symbol_sector(s)
    SECTOR_MAP[[s]] <<- auto_sec
    sector_map_dirty <<- TRUE
    return(auto_sec)
  }
  
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
        
        if (c_count < MAX_PER_SECTOR) {
          filtered_candidates <- c(filtered_candidates, cand_sym)
          held_sectors[[cand_sec]] <- c_count + 1
          if (length(filtered_candidates) >= empty_slots) break
        } else {
          cat(sprintf(" [Sector Defense] Skipping %s: Sector '%s' already at maximum cap (%d positions).\n",
                      cand_sym, cand_sec, MAX_PER_SECTOR))
        }
      }
    }
    
    n_actionable <- length(filtered_candidates)
    
    if (n_actionable > 0) {
      candidate_syms <- filtered_candidates
      actionable_df <- unowned_buys[unowned_buys$Symbol %in% candidate_syms, ]
      vince_alloc <- NULL
      
      slot_capital_equal <- purchasing_power / empty_slots

      # Ralph Vince Leverage Space Model Sizing
      if (SIZING_MODE == "vince") {
        cat(sprintf(" Sizing Engine: Ralph Vince Leverage Space Model (Safe f = %.2f, %d-Day Scenarios, Fractional: %s, Leverage: %.2fx)\n",
                    EFFECTIVE_SAFETY_FACTOR, VINCE_LOOKBACK, ifelse(ALLOW_FRACTIONAL, "YES (Exact)", "NO (Floor)"), LEVERAGE))
        tryCatch({
          joint_events <- build_joint_scenario_matrix(candidate_syms, lookback_days = VINCE_LOOKBACK)
          avail_syms <- intersect(candidate_syms, colnames(joint_events))
          if (length(avail_syms) >= 1) {
            sub_events <- joint_events[, avail_syms, drop = FALSE]
            vince_opt <- vince_optimal_f(sub_events, max_leverage = LEVERAGE, safety_factor = EFFECTIVE_SAFETY_FACTOR)
            cur_px_map <- setNames(actionable_df$Close[match(avail_syms, actionable_df$Symbol)], avail_syms)
            
            # Cap deployable cash to slot budget across available symbols to prevent oversizing
            allocable_cash <- min(purchasing_power, slot_capital_equal * length(avail_syms))
            vince_alloc <- vince_portfolio_allocation(vince_opt, total_cash = allocable_cash, current_prices = cur_px_map, allow_fractional = ALLOW_FRACTIONAL)
            
            # Enforce max 1.25x equal-weight slot cap per individual position to preserve diversification
            max_single_alloc <- slot_capital_equal * 1.25
            if (any(vince_alloc$Dollar_Allocation > max_single_alloc)) {
              vince_alloc$Dollar_Allocation <- pmin(max_single_alloc, vince_alloc$Dollar_Allocation)
              vince_alloc$Weight <- vince_alloc$Dollar_Allocation / sum(vince_alloc$Dollar_Allocation)
              if (isTRUE(ALLOW_FRACTIONAL)) {
                vince_alloc$Shares <- round(vince_alloc$Dollar_Allocation / vince_alloc$Current_Price, 3)
              } else {
                vince_alloc$Shares <- floor(vince_alloc$Dollar_Allocation / vince_alloc$Current_Price)
              }
              vince_alloc$Actual_Outlay <- round(vince_alloc$Shares * vince_alloc$Current_Price, 2)
              vince_alloc$Cash_Left <- round(vince_alloc$Dollar_Allocation - vince_alloc$Actual_Outlay, 2)
            }
            cat(sprintf(" -> Optimized Portfolio GHPR: %.4f (Expected Geometric Growth: %+.2f%% / day | Allocable Cash: $%.2f)\n\n",
                        vince_opt$ghpr, vince_opt$expected_growth_pct, allocable_cash))
          }
        }, error = function(e) {
          cat(sprintf(" -> Leverage Space note: %s. Using Equal-Weight slots.\n\n", e$message))
          vince_alloc <<- NULL
        })
      }
      
      if (is.null(vince_alloc)) {
        cat(sprintf(" Sizing Mode: Equal-Weight Cash Allocation ($%.2f per slot with %.2fx leverage)\n\n", slot_capital_equal, LEVERAGE))
      }
      
      # Preliminary share sizing across candidates to evaluate total risk
      raw_shares_list <- numeric(n_actionable)
      cand_risk_list  <- numeric(n_actionable)
      for (i in 1:n_actionable) {
        c_row <- actionable_df[actionable_df$Symbol == candidate_syms[i], ]
        if (!is.null(vince_alloc) && c_row$Symbol %in% vince_alloc$Symbol) {
          raw_shares_list[i] <- vince_alloc$Shares[vince_alloc$Symbol == c_row$Symbol]
        } else {
          raw_shares_list[i] <- if (isTRUE(ALLOW_FRACTIONAL)) round(slot_capital_equal / c_row$Close, 3) else floor(slot_capital_equal / c_row$Close)
        }
        cand_risk_list[i] <- raw_shares_list[i] * (c_row$Close - c_row$Stop_Loss)
      }
      
      # Portfolio Heat Cap Scaling: Ensure total portfolio risk <= 5%
      proposed_risk_sum <- sum(cand_risk_list, na.rm = TRUE)
      heat_scale <- if (!is.na(proposed_risk_sum) && proposed_risk_sum > avail_heat_budget && proposed_risk_sum > 0) {
        cat(sprintf(" [Heat Defense] Proposed risk ($%.2f) exceeds available heat budget ($%.2f). Scaling position size by %.1f%%.\n\n",
                    proposed_risk_sum, avail_heat_budget, (avail_heat_budget / proposed_risk_sum) * 100))
        avail_heat_budget / proposed_risk_sum
      } else {
        1.0
      }
      
      alert_tickets <- character()
      
      for (i in 1:n_actionable) {
        row <- actionable_df[actionable_df$Symbol == candidate_syms[i], ]
        c_sec <- get_sector(row$Symbol)
        
        shares <- raw_shares_list[i] * heat_scale
        # Cap order size to 1.0% of 20-day ADV to eliminate market impact (L3)
        if (!is.null(row$ADV_20) && !is.na(row$ADV_20) && row$ADV_20 > 0) {
          max_adv_shs <- floor(row$ADV_20 * 0.01)
          if (max_adv_shs > 0 && shares > max_adv_shs) {
            cat(sprintf(" [Liquidity Defense] Capping %s shares from %.1f to %d (1.0%% of 20-day ADV %d shs).\n",
                        row$Symbol, shares, max_adv_shs, as.integer(row$ADV_20)))
            shares <- max_adv_shs
          }
        }
        if (isTRUE(ALLOW_FRACTIONAL)) {
          shares <- round(shares, 3)
        } else {
          shares <- floor(shares)
        }
        invested <- round(shares * row$Close, 2)
        
        if (!is.null(vince_alloc) && row$Symbol %in% vince_alloc$Symbol) {
          v_row <- vince_alloc[vince_alloc$Symbol == row$Symbol, ]
          weight_pct <- v_row$Weight * 100 * heat_scale
          sizing_note <- sprintf("Vince Optimal f: %.4f | Safe f: %.4f | Heat Scale: %.1f%%",
                                 v_row$Optimal_f, v_row$Safe_f, heat_scale * 100)
          alloc_note  <- sprintf("Target Allocation: %.1f%% ($%.2f) -> Actual Outlay: $%.2f",
                                 weight_pct, v_row$Dollar_Allocation * heat_scale, invested)
        } else {
          sizing_note <- sprintf("Equal-Weight Slot Allocation (Heat Scale: %.1f%%)", heat_scale * 100)
          alloc_note  <- sprintf("Slot Budget: $%.2f -> Actual Outlay: $%.2f", slot_capital_equal, invested)
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
        cat(sprintf("  Relative Strength:   %+.2f%% vs SPY Benchmark [MARKET LEADER]\n", row$RS_20 * 100))
        cat(sprintf("  Multi-Timeframe:     Weekly Trend +%.2f%% [BULLISH SYNERGY]\n", row$Weekly_Slope))
        cat(sprintf("  Earnings Safe:       Next report %s (%d days out)\n", row$Earnings_Date, as.integer(row$Days_To_Earn)))
        cat(sprintf("  GTC Stop-Loss:       $%.2f (-%.2f%%) [Total Downside Risk: $%.2f]\n", 
                    row$Stop_Loss, 2.0 * row$Daily_Vol * 100, total_risk))
        cat("  Multi-Tier Bracket Exits:\n")
        cat(sprintf("    -> Tier 1 (50%% = %s shs): Target $%.2f (+%.2f%%, +1.5R) [Gain: $%.2f] -> Lock in profits & move stop to Breakeven $%.2f\n",
                    tier1_shares, tier1_target, 3.0 * row$Daily_Vol * 100, tier1_gain, row$Close))
        cat(sprintf("    -> Tier 2 (50%% = %s shs): Target $%.2f (+%.2f%%, +3.0R) [Gain: $%.2f] -> Momentum runner\n",
                    tier2_shares, tier2_target, 6.0 * row$Daily_Vol * 100, tier2_gain))
        cat(sprintf("    -> Chandelier Trailing Stop: Initial trigger at $%.2f (Trails Highest High - 2.5 x ATR(14) $%.2f)\n",
                    row$Chandelier_Stop, row$ATR_14))
        cat(sprintf("  Reward / Risk Ratio: Tier 1: 1.50R | Tier 2: 3.00R (Combined Potential: +$%.2f vs -$%.2f Risk)\n\n",
                    tier1_gain + tier2_gain, total_risk))
        
        alert_tickets <- c(alert_tickets, sprintf(
          "• *%s* (%s): BUY %s shs @ ~$%.2f\n  - Stop: $%.2f | T1: $%.2f (+1.5R) | T2: $%.2f (+3.0R)\n  - Chandelier Stop: $%.2f | RS vs SPY: %+.1f%%",
          row$Symbol, c_sec, shares_display, row$Close, row$Stop_Loss, tier1_target, tier2_target, row$Chandelier_Stop, row$RS_20 * 100
        ))
      }
      
      # Dispatch Mobile Webhook Alert
      if (length(alert_tickets) > 0) {
        alert_body <- paste(
          c(sprintf("Macro Gate: %s | VIX: %.2f | Open Slots: %d\n", macro_info$regime, vix_info$close, empty_slots),
            "Actionable Order Tickets:",
            alert_tickets),
          collapse = "\n"
        )
        broadcast_alert(title = "📈 New Weekly Swing Signals", body = alert_body, level = "SUCCESS")
      }
    } else {
      cat(sprintf(" No unowned symbols currently meet all BUY criteria (P(Up) >= %.1f%%, Weekly Bullish, RS Leader, Sector Cap).\n",
                  EFFECTIVE_P_LONG * 100))
      cat(" RECOMMENDATION: Retain available cash buffer in money market / cash.\n")
    }
  } else {
    cat(sprintf(" All %d active portfolio slots are currently filled or constrained by Macro Regime.\n", EFFECTIVE_MAX_POS))
    cat(" No new purchases needed today.\n")
  }
  cat("========================================================================================\n\n")
}

if (exists("sector_map_dirty") && isTRUE(sector_map_dirty)) {
  tryCatch({
    jsonlite::write_json(SECTOR_MAP, "sector_map.json", pretty = TRUE, auto_unbox = TRUE)
    cat("[Sector Map] Automatically updated sector_map.json with newly resolved tickers.\n")
  }, error = function(e) NULL)
}

