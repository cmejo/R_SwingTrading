#!/usr/bin/env Rscript
#' Multi-Stock Daily Signal Scanner & Execution Sizer ($10K Capital)
#'
#' Scans your watchlist, updates GARCH(1,1) and lmMA models for each stock,
#' applies Macro Market Gate (QQQ), Earnings Blackout filter, Weekly Trend Synergy,
#' and rolling Walk-Forward Retraining to generate actionable bracket order tickets.
#'
#' Usage:
#'   Rscript daily_signal.R [--symbols=SNDK,NVDA,AAPL] [--capital=8230] [--max_pos=5]

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
CAPITAL          <- 8230
MAX_POSITIONS    <- 3     # Max concurrent swing positions to hold (e.g., 3 positions @ $3,333 each)
TARGET_VOL       <- 0.45  # Target Volatility Budget (0.45 = 45% annual vol, matches QQQ/tech growth)
FAST_N           <- 20
SLOW_N           <- 50
LOOK_AHEAD       <- 7
P_LONG           <- 0.58
P_SHORT          <- 0.42
TRAIN_WINDOW     <- 500   # Rolling historical training window (bars) to prevent regime decay
MACRO_GATE       <- TRUE  # Top-down market regime filter (QQQ)
EARNINGS_DAYS    <- 7     # Disqualify stocks reporting earnings within N trading days (~10 calendar days)
SIZING_MODE      <- "vince" # Position sizing engine: "vince" (Leverage Space) or "equal"
SAFETY_FACTOR    <- 0.50    # Aggressive Safe f scaling factor for Ralph Vince Leverage Space model
VINCE_LOOKBACK   <- 120     # Lookback days for joint scenario return matrix
ALLOW_FRACTIONAL <- TRUE    # Allow fractional shares for exact risk budget allocation
LEVERAGE         <- 1.5     # Default leverage multiplier: 1.5x (margin). Set 1.0 for cash only.
MAX_PER_SECTOR   <- 3       # Maximum concurrent positions allowed in any single sector
MAX_HEAT         <- 0.08    # Max portfolio heat budget (0.08 = 8.0% total dollars at risk across stops)
FORCE_REFRESH    <- FALSE   # Bypass cache and re-download latest market data
CIRCUIT_BREAKER  <- TRUE    # Account Drawdown Circuit Breaker (-12.0% throttle)
CIRCUIT_BREAKER_THRESH <- -12.0 # Circuit breaker threshold ceiling (default -12.0%)
RESET_PEAK_EQUITY <- FALSE  # Reset peak equity high-water mark to current balance
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
  if (grepl("^--max_heat=", arg)) MAX_HEAT <- as.numeric(sub("^--max_heat=", "", arg))
  if (grepl("^--heat_budget=", arg)) MAX_HEAT <- as.numeric(sub("^--heat_budget=", "", arg))
  if (grepl("^--target_vol=", arg)) TARGET_VOL <- as.numeric(sub("^--target_vol=", "", arg))
  if (grepl("^--train_window=", arg)) TRAIN_WINDOW <- as.numeric(sub("^--train_window=", "", arg))
  if (grepl("^--macro_gate=", arg)) MACRO_GATE <- as.logical(sub("^--macro_gate=", "", arg))
  if (grepl("^--earnings_days=", arg)) EARNINGS_DAYS <- as.numeric(sub("^--earnings_days=", "", arg))
  if (grepl("^--sizing_mode=", arg)) SIZING_MODE <- tolower(sub("^--sizing_mode=", "", arg))
  if (grepl("^--safety_factor=", arg)) SAFETY_FACTOR <- as.numeric(sub("^--safety_factor=", "", arg))
  if (grepl("^--vince_lookback=", arg)) VINCE_LOOKBACK <- as.numeric(sub("^--vince_lookback=", "", arg))
  if (grepl("^--fractional=", arg)) ALLOW_FRACTIONAL <- as.logical(sub("^--fractional=", "", arg))
  if (grepl("^--force_refresh=", arg)) FORCE_REFRESH <- as.logical(sub("^--force_refresh=", "", arg))
  if (arg == "--force_refresh" || arg == "--refresh") FORCE_REFRESH <- TRUE
  if (grepl("^--circuit_breaker=", arg)) CIRCUIT_BREAKER <- as.logical(sub("^--circuit_breaker=", "", arg))
  if (grepl("^--circuit_breaker_thresh", arg)) CIRCUIT_BREAKER_THRESH <- -abs(as.numeric(sub("^--circuit_breaker_thresh(old)?=", "", arg)))
  if (grepl("^--circuit_breaker_pct=", arg)) CIRCUIT_BREAKER_THRESH <- -abs(as.numeric(sub("^--circuit_breaker_pct=", "", arg)))
  if (arg == "--no_circuit_breaker" || arg == "--disable_circuit_breaker") CIRCUIT_BREAKER <- FALSE
  if (arg == "--reset_peak_equity" || arg == "--reset_drawdown") RESET_PEAK_EQUITY <- TRUE
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
  if (any(grepl("^--capital=", args))) {
    cli_cap <- as.numeric(sub("^--capital=", "", args[grep("^--capital=", args)[1]]))
    if (!is.na(cli_cap) && cli_cap > 0) {
      port_state$total_capital <- cli_cap
      pos_invested <- if (length(port_state$positions) > 0) {
        sum(sapply(port_state$positions, function(p) as.numeric(p$shares) * as.numeric(p$entry_price)))
      } else 0.0
      hedge_invested <- if (!is.null(port_state$hedge_positions) && length(port_state$hedge_positions) > 0) {
        sum(sapply(port_state$hedge_positions, function(p) as.numeric(p$shares) * as.numeric(p$entry_price)))
      } else 0.0
      port_state$cash_balance <- max(0, cli_cap - pos_invested - hedge_invested)
      port_state$peak_equity <- max(as.numeric(port_state$peak_equity), cli_cap)
      save_portfolio(port_state, PORTFOLIO_FILE)
      cat(sprintf("[PortfolioManager] Updated capital to $%.2f (Deployable cash: $%.2f).\n", cli_cap, port_state$cash_balance))
    }
  }
  if (RESET_PEAK_EQUITY) {
    port_state$peak_equity <- port_state$total_capital
    save_portfolio(port_state, PORTFOLIO_FILE)
    cat(sprintf("[PortfolioManager] Reset peak equity watermark to current capital: $%.2f (Drawdown reset to 0.0%%).\n", port_state$total_capital))
  }
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

# Resolve universe file path if not explicitly provided as a vector
if (is.null(SYMBOLS)) {
  target_file <- if (!is.null(SYMBOLS_FILE) && file.exists(SYMBOLS_FILE)) {
    SYMBOLS_FILE
  } else if (file.exists("symbols_broad.txt")) {
    "symbols_broad.txt"
  } else if (file.exists("/Volumes/2TB.ssd/_a Development/swingtrading/symbols_broad.txt")) {
    "/Volumes/2TB.ssd/_a Development/swingtrading/symbols_broad.txt"
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
    cat("[Config] No symbols file found. Using fallback universe: SNDK, NVDA, AAPL, AMD, MSFT\n")
  }
} else {
  SYMBOLS <- unique(toupper(trimws(SYMBOLS)))
  cat(sprintf("[Config] Using CLI provided symbols: %s\n", paste(SYMBOLS, collapse = ", ")))
}

cat("\n========================================================================================\n")
cat(" 1. MACRO MARKET & VOLATILITY REGIME GATE (QQQ + VIX DUAL FILTER)\n")
cat("========================================================================================\n")

qqq_ohlcv <- tryCatch(load_stock_data("QQQ", force_refresh = FORCE_REFRESH), error = function(e) NULL)
macro_info <- if (!is.null(qqq_ohlcv)) {
  tryCatch({
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
    cat(sprintf("  -> Warning: Macro calculation failed (%s). Defaulting to Bullish.\n", e$message))
    list(close = 0, fit = 0, slope = 0, is_bullish = TRUE, regime = "BULLISH (Default)")
  })
} else {
  cat("  -> Warning: Macro fetch failed. Defaulting to Bullish.\n")
  list(close = 0, fit = 0, slope = 0, is_bullish = TRUE, regime = "BULLISH (Default)")
}

# Benchmark (SPY) for Relative Strength
spy_ohlcv <- tryCatch({
  load_stock_data("SPY", force_refresh = FORCE_REFRESH)
}, error = function(e) {
  if (!is.null(qqq_ohlcv)) qqq_ohlcv else NULL
})
if (is.null(spy_ohlcv) && !is.null(qqq_ohlcv)) spy_ohlcv <- qqq_ohlcv
spy_px <- if (!is.null(spy_ohlcv)) as.numeric(last(Cl(spy_ohlcv))) else 0

# Gold (GLD) Trend Gate for Defensive Capital Rotation
gld_ohlcv <- tryCatch(load_stock_data("GLD", force_refresh = FORCE_REFRESH), error = function(e) NULL)
gld_info <- if (!is.null(gld_ohlcv)) {
  tryCatch({
    gld_price <- Cl(gld_ohlcv)
    gld_lm <- lmMA(gld_price, n = 50)
    latest_gld_close <- as.numeric(last(gld_price))
    latest_gld_fit   <- as.numeric(last(gld_lm$fit))
    latest_gld_slope <- as.numeric(last(gld_lm$slope))
    is_bullish <- (latest_gld_close >= latest_gld_fit) && (latest_gld_slope >= 0)
    list(
      close = latest_gld_close,
      fit = latest_gld_fit,
      slope = latest_gld_slope,
      is_bullish = is_bullish,
      regime = if (is_bullish) "BULLISH (Uptrend)" else "BEARISH (Downtrend)"
    )
  }, error = function(e) {
    list(close = 0, fit = 0, slope = 0, is_bullish = FALSE, regime = "UNKNOWN")
  })
} else {
  list(close = 0, fit = 0, slope = 0, is_bullish = FALSE, regime = "FETCH_FAILED")
}

# Defensive Inverse & Cash Anchor Quotes
fngd_ohlcv <- tryCatch(load_stock_data("FNGD", force_refresh = FORCE_REFRESH), error = function(e) NULL)
fngd_px <- if (!is.null(fngd_ohlcv)) as.numeric(last(Cl(fngd_ohlcv))) else 0

sqqq_ohlcv <- tryCatch(load_stock_data("SQQQ", force_refresh = FORCE_REFRESH), error = function(e) NULL)
sqqq_px <- if (!is.null(sqqq_ohlcv)) as.numeric(last(Cl(sqqq_ohlcv))) else 0

sgov_ohlcv <- tryCatch(load_stock_data("SGOV", force_refresh = FORCE_REFRESH), error = function(e) NULL)
sgov_px <- if (!is.null(sgov_ohlcv)) as.numeric(last(Cl(sgov_ohlcv))) else 0

# Dynamic CBOE VIX Volatility Regime & MS-GARCH Two-Regime Macro Gate
vix_info <- tryCatch({
  vix_ohlcv <- load_stock_data("^VIX", force_refresh = FORCE_REFRESH)
  vix_close <- suppressWarnings(na.omit(Cl(vix_ohlcv)))
  vix_px <- as.numeric(last(vix_close))
  v_reg <- if (vix_px < 20) "NORMAL (Low Volatility)" else if (vix_px <= 28) "ELEVATED (Caution)" else "CRISIS (Extreme Volatility)"
  list(close = vix_px, regime = v_reg)
}, error = function(e) {
  list(close = 16.5, regime = "NORMAL (Default)")
})

# Estimate 2-Regime MS-GARCH on SPY Benchmark
cat(" Estimating Two-Regime MS-GARCH Market Volatility on SPY...\n")
spy_msgarch <- tryCatch({
  if (!is.null(spy_ohlcv)) compute_msgarch_volatility(Cl(spy_ohlcv)) else NULL
}, error = function(e) {
  cat(sprintf("  -> Notice: SPY MS-GARCH estimate note: %s\n", e$message))
  NULL
})

spy_p_high_vol <- if (!is.null(spy_msgarch)) tail(as.numeric(spy_msgarch$prob_high_vol), 1) else 0.05
spy_ann_vol    <- if (!is.null(spy_msgarch)) tail(as.numeric(spy_msgarch$annualized_vol), 1) else 0.15
spy_regime_str <- if (spy_p_high_vol >= 0.50) sprintf("HIGH VOL (%.1f%% Prob)", spy_p_high_vol * 100) else sprintf("LOW VOL (%.1f%% Prob)", (1 - spy_p_high_vol) * 100)

DEFENSIVE_REGIME_ACTIVE <- FALSE

# Multi-Factor Macro Sizing Policy
# VIX Crisis or MS-GARCH Market High-Vol spike must be checked FIRST
if (vix_info$close > 28 || spy_p_high_vol >= 0.80) {
  EFFECTIVE_MAX_POS       <- min(1, MAX_POSITIONS)
  EFFECTIVE_P_LONG        <- 0.68
  DEFENSIVE_REGIME_ACTIVE <- TRUE
  EFFECTIVE_SAFETY_FACTOR <- 0.15
  macro_note <- sprintf("Crisis Volatility (VIX: %.1f, MS-GARCH P(High): %.1f%%): Capital preservation mode, 1 position max, Safe f = 0.15",
                        vix_info$close, spy_p_high_vol * 100)
} else if (MACRO_GATE && !macro_info$is_bullish) {
  EFFECTIVE_MAX_POS       <- min(2, MAX_POSITIONS)
  EFFECTIVE_P_LONG        <- 0.65
  DEFENSIVE_REGIME_ACTIVE <- TRUE
  EFFECTIVE_SAFETY_FACTOR <- min(SAFETY_FACTOR, 0.30)
  macro_note <- "QQQ Trend Deficit: Capping positions at 2, Asymmetric Hybrid Gate (60.0% for RS > +5.0%, 65.0% baseline)"
} else if (vix_info$close >= 20 || spy_p_high_vol >= 0.50) {
  EFFECTIVE_MAX_POS       <- min(3, MAX_POSITIONS)
  EFFECTIVE_P_LONG        <- 0.60
  DEFENSIVE_REGIME_ACTIVE <- FALSE
  EFFECTIVE_SAFETY_FACTOR <- min(SAFETY_FACTOR, 0.30)
  macro_note <- sprintf("Elevated Volatility (VIX: %.1f, MS-GARCH Regime: %s): Caution scaling to 3 positions, Safe f = 0.30",
                        vix_info$close, spy_regime_str)
} else {
  EFFECTIVE_MAX_POS       <- MAX_POSITIONS
  EFFECTIVE_P_LONG        <- P_LONG
  DEFENSIVE_REGIME_ACTIVE <- FALSE
  EFFECTIVE_SAFETY_FACTOR <- SAFETY_FACTOR
  macro_note <- "Full Risk-On Allocation: 5 positions, Safe f = 0.50, P(Up) >= 58.0%"
}

cat(sprintf(" [Macro Gate Status] %s | Volatility Regime: %s | MS-GARCH Market Regime: %s\n", 
            macro_info$regime, vix_info$regime, spy_regime_str))
cat(sprintf("   QQQ Close: $%.2f | 50D lmMA Fit: $%.2f | Slope: %+.3f (%s)\n", 
            macro_info$close, macro_info$fit, macro_info$slope, ifelse(macro_info$is_bullish, "Bullish", "Bearish")))
cat(sprintf("   GLD Gold Close: $%.2f | 50D lmMA Fit: $%.2f | Slope: %+.3f (%s)\n",
            gld_info$close, gld_info$fit, gld_info$slope, gld_info$regime))
cat(sprintf("   SPY Benchmark Close: $%.2f | MS-GARCH Ann Vol: %.1f%% | P(High Vol Regime): %.1f%%\n",
            spy_px, spy_ann_vol * 100, spy_p_high_vol * 100))
cat(sprintf("   CBOE VIX:  %.2f   | Volatility Regime: %s\n", vix_info$close, vix_info$regime))
cat(sprintf("   -> Dynamic Policy: Max Positions = %d | Safe f = %.2f | Min P(Up) >= %.1f%%%s\n\n", 
            EFFECTIVE_MAX_POS, EFFECTIVE_SAFETY_FACTOR, EFFECTIVE_P_LONG * 100,
            ifelse(isTRUE(DEFENSIVE_REGIME_ACTIVE), " (Asymmetric Hybrid Gate: 60.0% for RS > +5%)", "")))

# Sector Taxonomy Mapping & Cluster Risk Defense
SECTOR_MAP <- if (file.exists("sector_map.json")) {
  tryCatch(jsonlite::fromJSON("sector_map.json", simplifyDataFrame = FALSE), error = function(e) list())
} else if (file.exists("/Volumes/2TB.ssd/_a Development/swingtrading/sector_map.json")) {
  tryCatch(jsonlite::fromJSON("/Volumes/2TB.ssd/_a Development/swingtrading/sector_map.json", simplifyDataFrame = FALSE), error = function(e) list())
} else {
  list()
}

sector_etf_map <- list(
  Semiconductors      = "SMH",
  MegaCap_Tech        = "QQQ",
  Enterprise_Software = "XLK",
  Software_Cloud      = "XLK",
  Cybersecurity       = "XLK",
  Hardware_Tech       = "XLK",
  Fintech_Financials  = "XLF",
  Fintech_Crypto      = "XLF",
  Financials          = "XLF",
  Power_Industrial    = "XLI",
  Industrials         = "XLI",
  Energy_Power        = "XLE",
  Energy              = "XLE",
  HealthCare          = "XLV",
  Biotech             = "XLV",
  Materials           = "XLB",
  Communication       = "XLC",
  Consumer            = "XLY",
  Index_ETF           = "SPY"
)

sector_map_dirty <- FALSE
get_sector <- function(s) {
  s <- toupper(trimws(s))
  if (s %in% names(SECTOR_MAP)) {
    return(SECTOR_MAP[[s]])
  }
  auto_sec <- get_symbol_sector(s)
  SECTOR_MAP[[s]] <<- auto_sec
  sector_map_dirty <<- TRUE
  return(auto_sec)
}

loaded_sector_etfs <- list()

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

    # Sector ETF lookup for sector relative strength momentum spread (Feature 2)
    cand_sec <- get_sector(sym)
    sec_etf_sym <- if (!is.null(cand_sec) && !is.null(sector_etf_map[[cand_sec]])) sector_etf_map[[cand_sec]] else "XLK"
    if (is.null(loaded_sector_etfs[[sec_etf_sym]])) {
      loaded_sector_etfs[[sec_etf_sym]] <- tryCatch(load_stock_data(sec_etf_sym, force_refresh = FORCE_REFRESH), error = function(e) NULL)
    }
    sec_ohlcv <- loaded_sector_etfs[[sec_etf_sym]]

    ohlcv <- load_stock_data(symbol = sym, force_refresh = FORCE_REFRESH)
    price <- Cl(ohlcv)
    latest_date <- as.character(index(last(price)))
    latest_close <- as.numeric(last(price))
    
    # Feature engineering with weekly trend synergy, sector spread & ATR extension
    pipeline_out <- build_feature_dataset(
      ohlcv = ohlcv,
      benchmark_ohlcv = spy_ohlcv,
      sector_ohlcv = sec_ohlcv,
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
      embargo_days  = 7
    )
    
    cv_fit            <- model_core$cv_fit
    pred_prob         <- model_core$pred_probs[1]
    is_intercept_only <- model_core$is_intercept_only
    feat_imp          <- model_core$feature_importance
    top_feature_str   <- if (!is_intercept_only) paste0(feat_imp$Feature[1], " (", feat_imp$Direction[1], ")") else "Base-Rate"

    # Single source of truth for features and volatility from pipeline_out
    curr_ann_vol         <- pipeline_out$latest_ann_vol
    latest_prob_high_vol <- pipeline_out$latest_prob_high_vol
    latest_trend_quality <- if (!is.null(pipeline_out$latest_trend_quality)) pipeline_out$latest_trend_quality else 0.50
    latest_rs_20         <- pipeline_out$latest_rs_20
    latest_atr_ext       <- pipeline_out$latest_atr_ext
    latest_sec_spread    <- pipeline_out$latest_sector_spread
    latest_asym_vol      <- pipeline_out$latest_asym_vol
    is_rs_leader         <- (!is.na(latest_rs_20) && latest_rs_20 >= 0)
    is_overextended      <- (!is.na(latest_atr_ext) && latest_atr_ext > 3.0) # Feature 1: Mean-Reversion Guard

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

    # Asymmetric Hybrid Gate: In defensive regimes, reward top Relative Strength leaders (RS_20 >= +5.0%)
    # with an earlier momentum entry hurdle (60.0%), while enforcing strict 65.0% on neutral/lagging assets
    cand_p_long <- EFFECTIVE_P_LONG
    if (isTRUE(DEFENSIVE_REGIME_ACTIVE) && !is.na(latest_rs_20) && latest_rs_20 >= 0.05) {
      cand_p_long <- 0.60
    }

    # Signal Assignment with Multi-Timeframe, RS Leader, Extension Filter, Liquidity & Earnings Gate
    signal_status <- "HOLD"
    gate_note <- "Normal"

    if (pred_prob <= P_SHORT) {
      signal_status <- "CASH"
      gate_note <- "Bearish Model"
    } else if (is_illiquid) {
      signal_status <- "ILLIQUID"
      gate_note <- sprintf("Low ADDV ($%.1fM < $5M)", addv_20 / 1e6)
    } else if (pred_prob >= cand_p_long) {
      if (is_earnings_blackout) {
        signal_status <- "BLACKOUT"
        gate_note <- sprintf("Earnings in %dd (%s)", as.integer(days_to_earn), earn_date)
      } else if (!is_weekly_bullish) {
        signal_status <- "COUNTER_TREND"
        gate_note <- sprintf("Weekly Bear (%.2f%%)", weekly_slope_pct)
      } else if (!is_rs_leader) {
        signal_status <- "RS_LAGGER"
        gate_note <- sprintf("RS Lagger (%+.1f%% vs SPY)", latest_rs_20 * 100)
      } else if (is_overextended) {
        signal_status <- "OVEREXTENDED"
        gate_note <- sprintf("Overextended (%.1fx ATR above trend)", latest_atr_ext)
      } else if (is_intercept_only) {
        signal_status <- "HOLD"
        gate_note <- sprintf("No Model Signal (0/%d coefs)", length(feat_names))
      } else {
        signal_status <- "BUY"
        hybrid_tag <- if (cand_p_long < EFFECTIVE_P_LONG) " [Hybrid 60%]" else ""
        gate_note <- sprintf("Leader (%s)%s", top_feature_str, hybrid_tag)
      }
    } else {
      signal_status <- "HOLD"
      gate_note <- if (is_intercept_only) sprintf("BaseRateOnly (0/%d coefs)", length(feat_names)) else sprintf("Below %.0f%% Cutoff", cand_p_long * 100)
    }
    
    data.frame(
      Symbol = sym,
      Date = latest_date,
      Close = latest_close,
      Prob_Up = pred_prob,
      Req_P_Up = cand_p_long,
      Signal = signal_status,
      Gate_Note = gate_note,
      Weekly_Slope = weekly_slope_pct,
      Trend_Quality = latest_trend_quality,
      RS_20 = latest_rs_20,
      ATRExt = latest_atr_ext,
      Sector_Spread = latest_sec_spread,
      Asym_Vol = latest_asym_vol,
      Sector = cand_sec,
      ATR_14 = atr_14,
      Chandelier_Stop = chandelier_stop,
      ADV_20 = adv_20,
      Top_Feature = top_feature_str,
      Earnings_Date = ifelse(is.na(earn_date), "None/ETF", earn_date),
      Days_To_Earn = ifelse(is.na(days_to_earn), -999, days_to_earn),
      GARCH_AnnVol = curr_ann_vol,
      Prob_HighVol = latest_prob_high_vol,
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

if (sector_map_dirty) {
  tryCatch(jsonlite::write_json(SECTOR_MAP, "sector_map.json", pretty = TRUE, auto_unbox = TRUE), error = function(e) NULL)
}

df_scan <- do.call(rbind, scan_results)

# Cross-Sectional Quality Score incorporating Sector Momentum Tailwind and Asymmetric Volatility:
# Candidates with positive Sector Spread (sector ETF outperforming SPY) and positive Asymmetric Vol Ratio receive boost
sector_boost <- pmax(0, df_scan$Sector_Spread)
asym_boost   <- pmax(0, df_scan$Asym_Vol * 0.1)
df_scan$Quality_Score <- ifelse(df_scan$Signal == "BUY" & df_scan$Prob_Up >= df_scan$Req_P_Up & df_scan$RS_20 > 0,
                                df_scan$Prob_Up * (1 + df_scan$RS_20 + 0.5 * sector_boost + asym_boost),
                                0.0)
df_scan <- df_scan[order(-df_scan$Quality_Score, -df_scan$Prob_Up), ]
rownames(df_scan) <- 1:nrow(df_scan)

# Formatted Leaderboard
cat("\n========================================================================================\n")
cat("               MULTI-ASSET SWING TRADING OPPORTUNITY LEADERBOARD\n")
cat(sprintf(" Macro Gate: %s | Max Positions: %d | Buy Threshold: P(Up) >= %.1f%%%s\n",
            macro_info$regime, EFFECTIVE_MAX_POS, EFFECTIVE_P_LONG * 100,
            ifelse(isTRUE(DEFENSIVE_REGIME_ACTIVE), " (Hybrid: 60.0% for RS > +5.0%)", "")))
cat("========================================================================================\n")

summary_table <- data.frame(
  Rank       = 1:nrow(df_scan),
  Symbol     = df_scan$Symbol,
  Sector     = df_scan$Sector,
  Price      = sprintf("$%.2f", df_scan$Close),
  P_Up       = sprintf("%.1f%%", df_scan$Prob_Up * 100),
  Score      = sprintf("%.3f", df_scan$Quality_Score),
  Signal     = df_scan$Signal,
  RS_SPY     = sprintf("%s%.1f%%", ifelse(df_scan$RS_20 >= 0, "+", ""), df_scan$RS_20 * 100),
  ATR_Ext    = sprintf("%+.1fx", df_scan$ATRExt),
  Weekly     = sprintf("%s%.1f%% [%s]", 
                       ifelse(df_scan$Weekly_Slope >= 0, "+", ""),
                       df_scan$Weekly_Slope, 
                       ifelse(df_scan$Weekly_Slope > 0, "BULL", "BEAR")),
  Earnings   = ifelse(df_scan$Days_To_Earn >= 0 & df_scan$Days_To_Earn <= 90, 
                      sprintf("%s (%dd)", df_scan$Earnings_Date, as.integer(df_scan$Days_To_Earn)),
                      df_scan$Earnings_Date),
  Filter_Note= df_scan$Gate_Note,
  GARCH_Vol  = sprintf("%.1f%%", df_scan$GARCH_AnnVol * 100),
  P_HighVol  = sprintf("%.1f%%", df_scan$Prob_HighVol * 100),
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
if (macro_info$close > 0) cur_px_all["QQQ"] <- macro_info$close
if (gld_info$close > 0)   cur_px_all["GLD"] <- gld_info$close
if (fngd_px > 0)          cur_px_all["FNGD"] <- fngd_px
if (sqqq_px > 0)          cur_px_all["SQQQ"] <- sqqq_px
if (sgov_px > 0)          cur_px_all["SGOV"] <- sgov_px

sync_res <- sync_portfolio_with_market(
  port_state, 
  current_prices = cur_px_all, 
  current_date = Sys.Date(), 
  is_friday = IS_FRIDAY,
  model_scan_df = df_scan,
  macro_bullish = macro_info$is_bullish,
  enable_circuit_breaker = CIRCUIT_BREAKER,
  circuit_breaker_threshold = CIRCUIT_BREAKER_THRESH
)
port_state <- sync_res$updated_portfolio
save_portfolio(port_state, PORTFOLIO_FILE)

if (sync_res$active_count > 0) {
  cat(sprintf(" Active Open Swing Positions (%d of %d active slots used):\n\n", sync_res$active_count, EFFECTIVE_MAX_POS))
  print(sync_res$holdings_df[, c("Symbol", "Shares", "Entry_Price", "Current_Price", "Market_Value", "Unrealized_PnL", "Return_Pct", "Days_Held", "Action_Required")], row.names = FALSE)
  cat("\n")
} else {
  cat(" Currently 0 Open Swing Positions.\n\n")
}

if (!is.null(sync_res$hedge_df) && nrow(sync_res$hedge_df) > 0) {
  cat(" Active Cash-Park / Defensive Hedge Sleeve (0 swing slots consumed):\n\n")
  print(sync_res$hedge_df, row.names = FALSE)
  cat("\n")
}

if (length(sync_res$alerts) > 0) {
  cat("----------------------------------------------------------------------------------------\n")
  cat(" ⚠️  PORTFOLIO ACTION ALERTS:\n")
  for (al in sync_res$alerts) {
    cat(sprintf("    * %s\n", al))
  }
  cat("----------------------------------------------------------------------------------------\n\n")
}

hedge_invested_val <- if (!is.null(sync_res$hedge_invested)) sync_res$hedge_invested else 0.0
cash_available <- sync_res$cash_balance + hedge_invested_val
empty_slots <- min(sync_res$available_slots, max(0, EFFECTIVE_MAX_POS - sync_res$active_count))
purchasing_power <- cash_available * LEVERAGE

cat(sprintf(" TOTAL ACCOUNT VALUE:    $%.2f\n", sync_res$total_account_value))
cat(sprintf(" PEAK EQUITY RECORD:     $%.2f (Drawdown: %+.2f%%)\n", sync_res$peak_equity, sync_res$drawdown_pct))
cat(sprintf(" INVESTED IN SWINGS:     $%.2f (%.1f%%)\n", sync_res$total_invested, sync_res$total_invested / max(1, sync_res$total_account_value) * 100))
if (hedge_invested_val > 0) {
  cat(sprintf(" PARKED IN HEDGE/CASH:   $%.2f (%.1f%%)\n", hedge_invested_val, hedge_invested_val / max(1, sync_res$total_account_value) * 100))
}
cat(sprintf(" DEPLOYABLE CASH/HEDGE:  $%.2f (%.1f%%)\n", cash_available, cash_available / max(1, sync_res$total_account_value) * 100))
if (LEVERAGE > 1.0) {
  cat(sprintf(" MARGIN LEVERAGE RATIO:  %.2fx (Active Buying Power: $%.2f)\n", LEVERAGE, purchasing_power))
}
cat(sprintf(" AVAILABLE CASH SLOTS:   %d of %d (Regime Limit: %d)\n", empty_slots, EFFECTIVE_MAX_POS, EFFECTIVE_MAX_POS))

# Account Drawdown Circuit Breaker Enforcement
if (isTRUE(CIRCUIT_BREAKER) && isTRUE(sync_res$circuit_breaker_active)) {
  EFFECTIVE_SAFETY_FACTOR <- EFFECTIVE_SAFETY_FACTOR * 0.50
  cat(sprintf(" ⚠️  [CIRCUIT BREAKER ENGAGED] Account drawdown %+.2f%% <= %.1f%%! Throttling Safe f to %.2f\n", 
              sync_res$drawdown_pct, CIRCUIT_BREAKER_THRESH, EFFECTIVE_SAFETY_FACTOR))
} else if (!isTRUE(CIRCUIT_BREAKER)) {
  cat(sprintf(" ℹ️  [CIRCUIT BREAKER DISABLED] Full sizing active (Safe f = %.2f).\n", EFFECTIVE_SAFETY_FACTOR))
}

# Total Portfolio Heat (Max Dollars at Risk across portfolio)
existing_dollar_risk <- if (sync_res$active_count > 0) {
  sum(sapply(port_state$positions, function(p) {
    pmax(0, as.numeric(p$shares) * (as.numeric(p$entry_price) - as.numeric(p$stop_loss)))
  }))
} else {
  0.0
}
max_portfolio_heat <- MAX_HEAT * sync_res$total_account_value
avail_heat_budget  <- max(0, max_portfolio_heat - existing_dollar_risk)
cat(sprintf(" PORTFOLIO HEAT (RISK):  $%.2f of $%.2f max allowed (%.2f%% of equity, Cap: %.1f%%)\n",
            existing_dollar_risk, max_portfolio_heat, (existing_dollar_risk / max(1, sync_res$total_account_value)) * 100, MAX_HEAT * 100))

# ----------------------------------------------------------------------------------------
# Core-Satellite Cash Yield Allocation & Defensive Asset Deployment
# 1. Macro Bullish -> Park in QQQ
# 2. QQQ Bearish, Gold Bullish -> Park in GLD
# 3. Dual Bear -> 75% FNGD + 25% SQQQ with 15-Day Time Stop / -10% Max Loss -> SGOV
# Automatically emits SELL tickets when rotating regimes or funding swing setups.
# ----------------------------------------------------------------------------------------
deploy_unallocated_cash <- function(unalloc_cash, port_state, macro_info, gld_info,
                                    qqq_px, gld_px, fngd_px, sqqq_px, sgov_px,
                                    allow_fractional = TRUE, total_account_val = CAPITAL,
                                    swing_outlay = 0, raw_cash_balance = unalloc_cash) {
  has_existing_hedge <- (!is.null(port_state$hedge_positions) && length(port_state$hedge_positions) > 0)
  if (unalloc_cash < 100 && !has_existing_hedge) {
    return(list(tickets = character(), port_state = port_state))
  }
  
  cat("----------------------------------------------------------------------------------------\n")
  cat("       CORE-SATELLITE CASH ALLOCATION & DEFENSIVE ASSET DEPLOYMENT\n")
  cat("----------------------------------------------------------------------------------------\n")
  cat(sprintf(" Unallocated Cash:       $%.2f (%.1f%% of available capital)\n", 
              unalloc_cash, unalloc_cash / max(1, total_account_val) * 100))
  
  tickets <- character()
  today_str <- as.character(Sys.Date())
  px_lookup <- c(
    QQQ  = if (qqq_px > 0) qqq_px else 500.0,
    GLD  = if (gld_px > 0) gld_px else 250.0,
    FNGD = if (fngd_px > 0) fngd_px else 10.0,
    SQQQ = if (sqqq_px > 0) sqqq_px else 10.0,
    SGOV = if (sgov_px > 0) sgov_px else 100.0,
    BIL  = 91.5
  )
  
  # 1. Determine Target Defensive Regime & Update Hedge State
  target_syms <- character()
  if (isTRUE(macro_info$is_bullish)) {
    cat(sprintf(" Macro Gate Status:      %s (QQQ 50-day Trend is BULLISH)\n", macro_info$regime))
    port_state$hedge_state <- list(regime = "QQQ_BULL", entry_date = today_str, days_held = 0L, rotated_to_sgov = FALSE, rotation_reason = "")
    target_syms <- c("QQQ")
  } else if (isTRUE(gld_info$is_bullish)) {
    cat(sprintf(" Macro Gate Status:      %s | Gold Trend: BULLISH (GLD >= 50D lmMA)\n", macro_info$regime))
    port_state$hedge_state <- list(regime = "GLD_BULL", entry_date = today_str, days_held = 0L, rotated_to_sgov = FALSE, rotation_reason = "")
    target_syms <- c("GLD")
  } else {
    # DUAL BEAR REGIME: Both QQQ & GLD are Bearish
    cat(sprintf(" Macro Gate Status:      %s | Gold Trend: %s (DUAL BEAR ACTIVE)\n", 
                macro_info$regime, gld_info$regime))
    
    h_state <- port_state$hedge_state
    # Infer existing FNGD / SQQQ entry metadata from hedge_positions if present
    existing_fngd <- NULL
    existing_sqqq <- NULL
    if (has_existing_hedge) {
      for (hp in port_state$hedge_positions) {
        if (toupper(hp$symbol) == "FNGD") existing_fngd <- hp
        if (toupper(hp$symbol) == "SQQQ") existing_sqqq <- hp
      }
    }
    if (is.null(h_state) || is.null(h_state$regime) || h_state$regime != "DUAL_BEAR_HEDGE") {
      inferred_dt <- today_str
      if (!is.null(existing_fngd) && !is.null(existing_fngd$entry_date)) inferred_dt <- existing_fngd$entry_date
      else if (!is.null(existing_sqqq) && !is.null(existing_sqqq$entry_date)) inferred_dt <- existing_sqqq$entry_date
      
      inferred_fngd_px <- if (!is.null(existing_fngd) && as.numeric(existing_fngd$entry_price) > 0) as.numeric(existing_fngd$entry_price) else fngd_px
      inferred_sqqq_px <- if (!is.null(existing_sqqq) && as.numeric(existing_sqqq$entry_price) > 0) as.numeric(existing_sqqq$entry_price) else sqqq_px
      
      h_state <- list(
        regime = "DUAL_BEAR_HEDGE",
        entry_date = inferred_dt,
        days_held = 1L,
        entry_fngd_px = inferred_fngd_px,
        entry_sqqq_px = inferred_sqqq_px,
        peak_ratio = 1.0,
        rotated_to_sgov = FALSE,
        rotation_reason = ""
      )
    }
    
    ent_dt <- tryCatch(as.Date(h_state$entry_date), error = function(e) Sys.Date())
    cur_dt <- Sys.Date()
    if (cur_dt > ent_dt) {
      all_d <- seq(ent_dt + 1, cur_dt, by = "day")
      bus_days <- sum(!weekdays(all_d) %in% c("Saturday", "Sunday"))
      h_state$days_held <- max(1L, as.integer(bus_days) + 1L)
    }
    
    r_fngd <- if (!is.null(h_state$entry_fngd_px) && h_state$entry_fngd_px > 0 && fngd_px > 0) (fngd_px / h_state$entry_fngd_px) - 1 else 0
    r_sqqq <- if (!is.null(h_state$entry_sqqq_px) && h_state$entry_sqqq_px > 0 && sqqq_px > 0) (sqqq_px / h_state$entry_sqqq_px) - 1 else 0
    hedge_cum_pnl <- (0.75 * r_fngd) + (0.25 * r_sqqq)
    
    cur_ratio <- 1.0 + hedge_cum_pnl
    prev_peak <- if (!is.null(h_state$peak_ratio)) as.numeric(h_state$peak_ratio) else 1.0
    h_state$peak_ratio <- max(prev_peak, cur_ratio)
    
    # Safety Stops: 15-Day Time Stop OR -10.0% Max Loss Stop
    if (h_state$days_held > 15) {
      h_state$rotated_to_sgov <- TRUE
      h_state$rotation_reason <- sprintf("15-Day Time Stop Reached (%d trading days held)", h_state$days_held)
    } else if (hedge_cum_pnl <= -0.10) {
      h_state$rotated_to_sgov <- TRUE
      h_state$rotation_reason <- sprintf("-10.0%% Max Loss Triggered (Cumulative P&L: %+.1f%%)", hedge_cum_pnl * 100)
    }
    port_state$hedge_state <- h_state
    target_syms <- if (isTRUE(h_state$rotated_to_sgov)) c("SGOV") else c("FNGD", "SQQQ")
  }
  
  # 2. Check Existing Hedge / Cash-Park Positions for Regime Rotation or Swing Funding Liquidation
  kept_hedge_list <- list()
  kept_hedge_val  <- 0.0
  if (has_existing_hedge) {
    for (hp in port_state$hedge_positions) {
      hsym <- toupper(hp$symbol)
      hshs <- as.numeric(hp$shares)
      if (is.na(hshs) || hshs <= 0) next
      hpx <- if (!is.na(px_lookup[hsym])) as.numeric(px_lookup[hsym]) else as.numeric(hp$entry_price)
      
      if (!(hsym %in% target_syms)) {
        # Regime flipped (e.g. FNGD/SQQQ -> SGOV or GLD or QQQ): liquidate 100%
        shs_str <- if (isTRUE(allow_fractional) && (hshs %% 1 != 0)) sprintf("%.3f", hshs) else sprintf("%d", as.integer(hshs))
        cat(sprintf(" 🔄  REGIME ROTATION EXIT: Liquidating prior defensive position %s\n", hsym))
        cat(sprintf(" Actionable Ticket:      SELL %s shares of %s @ ~$%.2f (Regime Rotation Liquidation)\n",
                    shs_str, hsym, hpx))
        tickets <- c(tickets, sprintf("SELL %s shares of %s @ ~$%.2f (Regime Rotation Liquidation)", shs_str, hsym, hpx))
      } else {
        kept_hedge_list[[length(kept_hedge_list) + 1]] <- hp
        kept_hedge_val <- kept_hedge_val + round(hshs * hpx, 2)
      }
    }
  }
  
  # If new swing setups require cash currently parked in kept_hedge_list, emit trim SELL tickets
  cash_shortfall <- max(0, (kept_hedge_val - unalloc_cash))
  if (cash_shortfall >= 25 && length(kept_hedge_list) > 0) {
    rem_shortfall <- cash_shortfall
    updated_kept <- list()
    for (hp in kept_hedge_list) {
      hsym <- toupper(hp$symbol)
      hshs <- as.numeric(hp$shares)
      hpx  <- if (!is.na(px_lookup[hsym])) as.numeric(px_lookup[hsym]) else as.numeric(hp$entry_price)
      if (rem_shortfall > 0 && hpx > 0) {
        shs_sell <- if (isTRUE(allow_fractional)) min(hshs, round(rem_shortfall / hpx, 3)) else min(hshs, ceiling(rem_shortfall / hpx))
        if (shs_sell > 0) {
          shs_str <- if (isTRUE(allow_fractional) && (shs_sell %% 1 != 0)) sprintf("%.3f", shs_sell) else sprintf("%d", as.integer(shs_sell))
          cat(sprintf(" 💸  SWING FUNDING TRIM: Reclaiming $%.2f from cash-park ETF %s\n", round(shs_sell * hpx, 2), hsym))
          cat(sprintf(" Actionable Ticket:      SELL %s shares of %s @ ~$%.2f (Fund New Swing Setup)\n",
                      shs_str, hsym, hpx))
          tickets <- c(tickets, sprintf("SELL %s shares of %s @ ~$%.2f (Fund New Swing Setup)", shs_str, hsym, hpx))
          rem_shortfall <- max(0, rem_shortfall - round(shs_sell * hpx, 2))
          hshs <- round(hshs - shs_sell, 4)
        }
      }
      if (hshs > 0) {
        hp$shares <- hshs
        updated_kept[[length(updated_kept) + 1]] <- hp
      }
    }
    kept_hedge_list <- updated_kept
    kept_hedge_val  <- max(0, kept_hedge_val - cash_shortfall)
  }
  port_state$hedge_positions <- kept_hedge_list
  
  # 3. Deploy Net Unallocated Cash into Active Regime Target(s)
  net_deploy_cash <- max(0, unalloc_cash - kept_hedge_val)
  if (net_deploy_cash >= 100) {
    if (isTRUE(macro_info$is_bullish)) {
      qqq_shs <- if (isTRUE(allow_fractional)) round(net_deploy_cash / px_lookup["QQQ"], 3) else floor(net_deploy_cash / px_lookup["QQQ"])
      if (qqq_shs > 0) {
        cat(sprintf(" Actionable Ticket:      BUY %s shares of QQQ @ ~$%.2f ($%.2f outlay)\n",
                    ifelse(isTRUE(allow_fractional), sprintf("%.3f", qqq_shs), sprintf("%d", as.integer(qqq_shs))),
                    px_lookup["QQQ"], round(qqq_shs * px_lookup["QQQ"], 2)))
        cat(" Strategy Purpose:       Park idle cash in QQQ to capture market momentum and eliminate 0% cash drag.\n")
        cat("                         Automatically trimmed/liquidated when high-probability swing setups qualify.\n")
        tickets <- c(tickets, sprintf("BUY %s shares of QQQ @ ~$%.2f", qqq_shs, px_lookup["QQQ"]))
      }
    } else if (isTRUE(gld_info$is_bullish)) {
      gld_px_val <- px_lookup["GLD"]
      gld_shs <- if (isTRUE(allow_fractional)) round(net_deploy_cash / gld_px_val, 3) else floor(net_deploy_cash / gld_px_val)
      if (gld_shs > 0) {
        cat(sprintf(" Actionable Ticket:      BUY %s shares of GLD @ ~$%.2f ($%.2f outlay)\n",
                    ifelse(isTRUE(allow_fractional), sprintf("%.3f", gld_shs), sprintf("%d", as.integer(gld_shs))),
                    gld_px_val, round(gld_shs * gld_px_val, 2)))
        cat(" Strategy Purpose:       QQQ is in a trend deficit, but Gold is in a bull trend. Park cash in GLD.\n")
        cat("                         Captures non-correlated gold alpha while preserving purchasing power.\n")
        tickets <- c(tickets, sprintf("BUY %s shares of GLD @ ~$%.2f", gld_shs, gld_px_val))
      }
    } else if (isTRUE(port_state$hedge_state$rotated_to_sgov)) {
      sgov_px_val <- px_lookup["SGOV"]
      sgov_shs <- if (isTRUE(allow_fractional)) round(net_deploy_cash / sgov_px_val, 3) else floor(net_deploy_cash / sgov_px_val)
      if (sgov_shs > 0) {
        cat(sprintf(" 🛡️  HEDGE DEFENSE TRIGGERED: %s\n", port_state$hedge_state$rotation_reason))
        cat(sprintf(" Actionable Ticket:      BUY %s shares of SGOV @ ~$%.2f ($%.2f outlay)\n",
                    ifelse(isTRUE(allow_fractional), sprintf("%.3f", sgov_shs), sprintf("%d", as.integer(sgov_shs))),
                    sgov_px_val, round(sgov_shs * sgov_px_val, 2)))
        cat(" Strategy Purpose:       Safety stop triggered. Rotates 100% of defensive cash into risk-free SGOV yield\n")
        cat("                         to completely avoid late-stage bear market counter-trend squeezes and volatility decay.\n")
        tickets <- c(tickets, sprintf("BUY %s shares of SGOV @ ~$%.2f", sgov_shs, sgov_px_val))
      }
    } else {
      fngd_px_val <- px_lookup["FNGD"]
      sqqq_px_val <- px_lookup["SQQQ"]
      
      fngd_cash <- round(net_deploy_cash * 0.75, 2)
      sqqq_cash <- round(net_deploy_cash * 0.25, 2)
      fngd_shs  <- if (isTRUE(allow_fractional)) round(fngd_cash / fngd_px_val, 3) else floor(fngd_cash / fngd_px_val)
      sqqq_shs  <- if (isTRUE(allow_fractional)) round(sqqq_cash / sqqq_px_val, 3) else floor(sqqq_cash / sqqq_px_val)
      
      cat(sprintf(" ⚔️  DUAL BEAR HEDGE ACTIVE: Day %d of 15 (75%% FNGD + 25%% SQQQ | -10%% Max Loss Protection)\n", port_state$hedge_state$days_held))
      cat(sprintf(" Actionable Ticket:      BUY %s shares of FNGD @ ~$%.2f ($%.2f outlay)\n",
                  ifelse(isTRUE(allow_fractional), sprintf("%.3f", fngd_shs), sprintf("%d", as.integer(fngd_shs))),
                  fngd_px_val, round(fngd_shs * fngd_px_val, 2)))
      cat(sprintf(" Actionable Ticket:      BUY %s shares of SQQQ @ ~$%.2f ($%.2f outlay)\n",
                  ifelse(isTRUE(allow_fractional), sprintf("%.3f", sqqq_shs), sprintf("%d", as.integer(sqqq_shs))),
                  sqqq_px_val, round(sqqq_shs * sqqq_px_val, 2)))
      cat(" Strategy Purpose:       Captures explosive crash alpha during severe market selloffs.\n")
      cat("                         Protected by automatic 15-day time stop and -10.0% max loss circuit into SGOV.\n")
      tickets <- c(
        tickets,
        sprintf("BUY %s shares of FNGD @ ~$%.2f", fngd_shs, fngd_px_val),
        sprintf("BUY %s shares of SQQQ @ ~$%.2f", sqqq_shs, sqqq_px_val)
      )
    }
  } else if (kept_hedge_val > 0) {
    cat(sprintf(" ℹ️  Existing defensive sleeve ($%.2f in %s) already matches active regime.\n",
                kept_hedge_val, paste(target_syms, collapse = "+")))
  }
  
  port_state$hedge_positions <- kept_hedge_list
  cat("----------------------------------------------------------------------------------------\n\n")
  return(list(tickets = tickets, port_state = port_state))
}

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
  
  # Manage defensive cash & hedge sleeves on Friday if idle cash or hedge positions exist
  if (cash_available >= 100 || length(port_state$hedge_positions) > 0) {
    cash_plan <- deploy_unallocated_cash(
      unalloc_cash = cash_available,
      port_state = port_state,
      macro_info = macro_info,
      gld_info = gld_info,
      qqq_px = macro_info$close,
      gld_px = gld_info$close,
      fngd_px = fngd_px,
      sqqq_px = sqqq_px,
      sgov_px = sgov_px,
      allow_fractional = ALLOW_FRACTIONAL,
      total_account_val = sync_res$total_account_value,
      swing_outlay = 0,
      raw_cash_balance = sync_res$cash_balance
    )
    port_state <- cash_plan$port_state
    save_portfolio(port_state, PORTFOLIO_FILE)
    if (length(cash_plan$tickets) > 0) {
      alert_body <- paste(
        c(sprintf("Macro Gate: %s | VIX: %.2f | Friday Cash Allocation\n", macro_info$regime, vix_info$close),
          "Actionable Cash & Hedge Tickets:",
          cash_plan$tickets),
        collapse = "\n"
      )
      broadcast_alert(title = "🛡️ Friday Defensive Cash / Hedge Deployment", body = alert_body, level = "INFO")
    }
  }
  cat("========================================================================================\n\n")
} else {
  cat(sprintf("              RECOMMENDED ORDERS FOR EMPTY CASH SLOTS (%d AVAILABLE)\n", empty_slots))
  cat("========================================================================================\n")
  
  held_syms <- if (sync_res$active_count > 0) sync_res$holdings_df$Symbol else character(0)
  
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
    alert_tickets <- character()
    
    if (n_actionable > 0) {
      candidate_syms <- filtered_candidates
      actionable_df <- unowned_buys[unowned_buys$Symbol %in% candidate_syms, ]
      vince_alloc <- NULL
      
      slot_capital_equal <- purchasing_power / empty_slots

      # Unified Ralph Vince Leverage Space + Inverse GARCH Volatility Parity Sizing (Change #5)
      if (SIZING_MODE %in% c("vince", "vol_parity", "vince_vol_parity")) {
        cat(sprintf(" Sizing Engine: Blended Vince Safe f + Inverse-Volatility Parity (Safe f = %.2f, Target Vol = %.0f%%, Leverage: %.2fx)\n",
                    EFFECTIVE_SAFETY_FACTOR, TARGET_VOL * 100, LEVERAGE))
        tryCatch({
          joint_events <- build_joint_scenario_matrix(candidate_syms, lookback_days = VINCE_LOOKBACK)
          avail_syms <- intersect(candidate_syms, colnames(joint_events))
          if (length(avail_syms) >= 1) {
            sub_events <- joint_events[, avail_syms, drop = FALSE]
            vince_opt <- vince_optimal_f(sub_events, max_leverage = LEVERAGE, safety_factor = EFFECTIVE_SAFETY_FACTOR)
            cur_px_map <- setNames(actionable_df$Close[match(avail_syms, actionable_df$Symbol)], avail_syms)
            
            # Inverse GARCH Volatility Parity Weights (1 / GARCH_AnnVol)
            cand_vols <- pmax(0.15, pmin(1.20, actionable_df$GARCH_AnnVol[match(avail_syms, actionable_df$Symbol)]))
            inv_vol_raw <- 1.0 / cand_vols
            inv_vol_w <- inv_vol_raw / sum(inv_vol_raw)
            names(inv_vol_w) <- avail_syms
            
            # Blend 50% Ralph Vince Safe f weight + 50% Inverse Volatility Parity weight
            if (SIZING_MODE == "vol_parity") {
              vince_opt$weights <- inv_vol_w
            } else {
              vince_opt$weights <- 0.50 * vince_opt$weights[avail_syms] + 0.50 * inv_vol_w
              vince_opt$weights <- vince_opt$weights / sum(vince_opt$weights)
            }
            
            # Volatility-Parity Slot Scaling: scale allocable cash if average candidate volatility exceeds TARGET_VOL
            vol_scalars <- pmin(1.25, pmax(0.45, TARGET_VOL / cand_vols))
            allocable_cash <- min(purchasing_power, sum(slot_capital_equal * vol_scalars))
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
            cat(sprintf(" -> Optimized Portfolio GHPR: %.4f (Expected Geometric Growth: %+.2f%% / day | Vol-Adjusted Cash: $%.2f)\n\n",
                        vince_opt$ghpr, vince_opt$expected_growth_pct, allocable_cash))
          }
        }, error = function(e) {
          cat(sprintf(" -> Leverage Space note: %s. Using Inverse-Vol Equal-Risk slots.\n\n", e$message))
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
        
        # Multi-Tier Bracket Pricing (50% @ +1.5R, 25% @ +3.0R, 25% Chandelier Runner)
        tier1_shares  <- if (isTRUE(ALLOW_FRACTIONAL)) round(shares * 0.50, 3) else floor(shares * 0.50)
        tier2_shares  <- if (isTRUE(ALLOW_FRACTIONAL)) round(shares * 0.25, 3) else floor(shares * 0.25)
        runner_shares <- round(shares - tier1_shares - tier2_shares, 3)
        tier1_target  <- round(row$Close * (1 + 3.0 * row$Daily_Vol), 2)  # +1.5R target
        tier2_target  <- round(row$Close * (1 + 6.0 * row$Daily_Vol), 2)  # +3.0R target
        total_risk    <- round(shares * (row$Close - row$Stop_Loss), 2)
        tier1_gain    <- round(tier1_shares * (tier1_target - row$Close), 2)
        tier2_gain    <- round(tier2_shares * (tier2_target - row$Close), 2)
        
        cat(sprintf("--- ORDER TICKET #%d: %s (P(Up): %.1f%% | Quality Score: %.3f | Sector: %s) ---\n", 
                    i, row$Symbol, row$Prob_Up * 100, row$Quality_Score, c_sec))
        cat(sprintf("  Action:              BUY %s SHARES at Market (Order Execution)\n", shares_display))
        cat(sprintf("  Signal Price:        $%.2f (Scan Reference)\n", row$Close))
        cat(sprintf("  Position Sizing:     %s\n", sizing_note))
        cat(sprintf("  Capital Allocation:  %s\n", alloc_note))
        cat(sprintf("  Relative Strength:   %+.2f%% vs SPY Benchmark [MARKET LEADER]\n", row$RS_20 * 100))
        cat(sprintf("  Multi-Timeframe:     Weekly Trend +%.2f%% [BULLISH SYNERGY]\n", row$Weekly_Slope))
        cat(sprintf("  Earnings Safe:       Next report %s (%d days out)\n", row$Earnings_Date, as.integer(row$Days_To_Earn)))
        cat(sprintf("  GTC Stop-Loss:       $%.2f (-%.2f%%) [Total Downside Risk: $%.2f]\n", 
                    row$Stop_Loss, 2.0 * row$Daily_Vol * 100, total_risk))
        cat("  3-Tier Execution & Trend-Trailing Runner Plan:\n")
        cat(sprintf("    -> Tier 1 (50%% = %s shs): Target $%.2f (+1.5R) [Gain: $%.2f] -> Close 50%% & move stop to Breakeven $%.2f\n",
                    tier1_shares, tier1_target, tier1_gain, row$Close))
        cat(sprintf("    -> Tier 2 (25%% = %s shs): Target $%.2f (+3.0R) [Gain: $%.2f] -> Close 25%%\n",
                    tier2_shares, tier2_target, tier2_gain))
        runner_horizon <- if ((!is.na(row$Trend_Quality) && row$Trend_Quality > 0.60) || (!is.na(row$RS_20) && row$RS_20 >= 0.05)) 20L else 15L
        cat(sprintf("    -> Runner Lot (25%% = %s shs): Chandelier Trailing Stop (Initial: $%.2f, Trails Highest High - 2.5 x ATR(14) $%.2f)\n",
                    runner_shares, row$Chandelier_Stop, row$ATR_14))
        cat(sprintf("       * Adaptive Runner Horizon: %d trading days (R^2 = %.2f | Mark-Out Extended for High-R^2 / RS Leaders)!\n\n",
                    runner_horizon, ifelse(is.na(row$Trend_Quality), 0.50, row$Trend_Quality)))
        
        alert_tickets <- c(alert_tickets, sprintf(
          "• *%s* (%s): BUY %s shs @ ~$%.2f\n  - Stop: $%.2f | T1 (50%%): $%.2f (+1.5R) | T2 (25%%): $%.2f (+3.0R)\n  - Runner (25%%): Chandelier Stop $%.2f | RS vs SPY: %+.1f%%",
          row$Symbol, c_sec, shares_display, row$Close, row$Stop_Loss, tier1_target, tier2_target, row$Chandelier_Stop, row$RS_20 * 100
        ))
      }
      
      # Calculate total capital deployed into swing setups
      total_swing_outlay <- sum(sapply(1:n_actionable, function(idx) {
        shs <- raw_shares_list[idx] * heat_scale
        if (isTRUE(ALLOW_FRACTIONAL)) shs <- round(shs, 3) else shs <- floor(shs)
        shs * actionable_df$Close[actionable_df$Symbol == candidate_syms[idx]]
      }))
      

      
      unalloc_cash <- max(0, cash_available - total_swing_outlay)
      if (unalloc_cash >= 100 || length(port_state$hedge_positions) > 0) {
        cash_plan <- deploy_unallocated_cash(
          unalloc_cash = unalloc_cash,
          port_state = port_state,
          macro_info = macro_info,
          gld_info = gld_info,
          qqq_px = macro_info$close,
          gld_px = gld_info$close,
          fngd_px = fngd_px,
          sqqq_px = sqqq_px,
          sgov_px = sgov_px,
          allow_fractional = ALLOW_FRACTIONAL,
          total_account_val = sync_res$total_account_value,
          swing_outlay = total_swing_outlay,
          raw_cash_balance = sync_res$cash_balance
        )
        port_state <- cash_plan$port_state
        save_portfolio(port_state, PORTFOLIO_FILE)
        alert_tickets <- c(alert_tickets, cash_plan$tickets)
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
      cat(sprintf(" No unowned symbols currently meet all BUY criteria (P(Up) >= %.1f%%%s, Weekly Bullish, RS Leader, Sector Cap).\n",
                  EFFECTIVE_P_LONG * 100,
                  ifelse(isTRUE(DEFENSIVE_REGIME_ACTIVE), " [Hybrid RS Leader: 60.0%]", "")))
      
      # When 0 swing setups qualify, deploy 100% idle cash into dynamic cash / hedge engine
      if (cash_available >= 100 || length(port_state$hedge_positions) > 0) {
        cash_plan <- deploy_unallocated_cash(
          unalloc_cash = cash_available,
          port_state = port_state,
          macro_info = macro_info,
          gld_info = gld_info,
          qqq_px = macro_info$close,
          gld_px = gld_info$close,
          fngd_px = fngd_px,
          sqqq_px = sqqq_px,
          sgov_px = sgov_px,
          allow_fractional = ALLOW_FRACTIONAL,
          total_account_val = sync_res$total_account_value,
          swing_outlay = 0,
          raw_cash_balance = sync_res$cash_balance
        )
        port_state <- cash_plan$port_state
        save_portfolio(port_state, PORTFOLIO_FILE)
        if (length(cash_plan$tickets) > 0) {
          alert_tickets <- c(alert_tickets, cash_plan$tickets)
          alert_body <- paste(
            c(sprintf("Macro Gate: %s | VIX: %.2f | Open Slots: %d\n", macro_info$regime, vix_info$close, empty_slots),
              "Actionable Cash & Hedge Tickets:",
              alert_tickets),
            collapse = "\n"
          )
          broadcast_alert(title = "🛡️ Defensive Cash / Hedge Deployment", body = alert_body, level = "INFO")
        }
      }
    }
  } else {
    cat(sprintf(" All %d active portfolio slots are currently filled or constrained by Macro Regime.\n", EFFECTIVE_MAX_POS))
    cat(" No new swing purchases needed today.\n")
    
    # When all swing slots are filled/constrained, deploy any remaining unallocated cash or manage hedge sleeves
    if (cash_available >= 100 || length(port_state$hedge_positions) > 0) {
      cash_plan <- deploy_unallocated_cash(
        unalloc_cash = cash_available,
        port_state = port_state,
        macro_info = macro_info,
        gld_info = gld_info,
        qqq_px = macro_info$close,
        gld_px = gld_info$close,
        fngd_px = fngd_px,
        sqqq_px = sqqq_px,
        sgov_px = sgov_px,
        allow_fractional = ALLOW_FRACTIONAL,
        total_account_val = sync_res$total_account_value,
        swing_outlay = 0,
        raw_cash_balance = sync_res$cash_balance
      )
      port_state <- cash_plan$port_state
      save_portfolio(port_state, PORTFOLIO_FILE)
      if (length(cash_plan$tickets) > 0) {
        alert_body <- paste(
          c(sprintf("Macro Gate: %s | VIX: %.2f | Open Slots: %d\n", macro_info$regime, vix_info$close, empty_slots),
            "Actionable Cash & Hedge Tickets:",
            cash_plan$tickets),
          collapse = "\n"
        )
        broadcast_alert(title = "🛡️ Defensive Cash / Hedge Deployment", body = alert_body, level = "INFO")
      }
    }
  }
  cat("========================================================================================\n\n")
}

if (exists("sector_map_dirty") && isTRUE(sector_map_dirty)) {
  tryCatch({
    jsonlite::write_json(SECTOR_MAP, "sector_map.json", pretty = TRUE, auto_unbox = TRUE)
    cat("[Sector Map] Automatically updated sector_map.json with newly resolved tickers.\n")
  }, error = function(e) NULL)
}

