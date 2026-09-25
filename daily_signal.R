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

# Default Parameters
CAPITAL        <- 10000
MAX_POSITIONS  <- 5     # Max concurrent swing positions to hold (e.g., 5 positions @ $2,000 each)
TARGET_VOL     <- 1.00  # High Growth Sizing (100% allocation of per-position capital)
FAST_N         <- 20
SLOW_N         <- 50
LOOK_AHEAD     <- 5
P_LONG         <- 0.58
P_SHORT        <- 0.42
TRAIN_WINDOW   <- 500   # Rolling historical training window (bars) to prevent regime decay
MACRO_GATE     <- TRUE  # Top-down market regime filter (QQQ)
EARNINGS_DAYS  <- 7     # Disqualify stocks reporting earnings within N trading days (~10 calendar days)
SIZING_MODE    <- "vince" # Position sizing engine: "vince" (Leverage Space) or "equal"
SAFETY_FACTOR  <- 0.50    # Aggressive Safe f scaling factor for Ralph Vince Leverage Space model
VINCE_LOOKBACK <- 120     # Lookback days for joint scenario return matrix

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
cat(" 1. MACRO MARKET REGIME GATE (QQQ 50-DAY TREND FILTER)\n")
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

if (MACRO_GATE && !macro_info$is_bullish) {
  EFFECTIVE_MAX_POS <- min(2, MAX_POSITIONS)
  EFFECTIVE_P_LONG  <- 0.65
  cat(sprintf(" [Macro Gate Status] %s\n", macro_info$regime))
  cat(sprintf("   QQQ Close: $%.2f | 50D lmMA Fit: $%.2f | Slope: %.3f\n", 
              macro_info$close, macro_info$fit, macro_info$slope))
  cat(sprintf("   -> DEFENSIVE ADJUSTMENT: Max active positions scaled to %d (from %d)\n", 
              EFFECTIVE_MAX_POS, MAX_POSITIONS))
  cat(sprintf("   -> RAISING CONVICTION BAR: Required P(Up) >= %.1f%% (from %.1f%%)\n\n", 
              EFFECTIVE_P_LONG * 100, P_LONG * 100))
} else {
  EFFECTIVE_MAX_POS <- MAX_POSITIONS
  EFFECTIVE_P_LONG  <- P_LONG
  cat(sprintf(" [Macro Gate Status] %s\n", macro_info$regime))
  cat(sprintf("   QQQ Close: $%.2f | 50D lmMA Fit: $%.2f | Slope: +%.3f\n", 
              macro_info$close, macro_info$fit, macro_info$slope))
  cat(sprintf("   -> Full Risk-On Allocation: Max Positions = %d | P(Up) >= %.1f%%\n\n", 
              EFFECTIVE_MAX_POS, EFFECTIVE_P_LONG * 100))
}

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
    
    all_feats <- merge(
      SlopeFast = dual_lm$fast_lm$slope,
      SlopeSlow = dual_lm$slow_lm$slope,
      SlopeWeeklyPct = feat_slope_weekly,
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

held_syms <- names(port_state$positions)
n_held <- length(held_syms)
total_invested_val <- 0

if (n_held > 0) {
  cat(sprintf(" Active Open Positions (%d of %d active slots used):\n\n", n_held, EFFECTIVE_MAX_POS))
  
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
    
    # Check if earnings are approaching for held stock
    h_row <- df_scan[df_scan$Symbol == h_sym, ]
    if (nrow(h_row) > 0 && h_row$Days_To_Earn >= 0 && h_row$Days_To_Earn <= 10) {
      cat(sprintf("    ⚠️ EARNINGS WARNING: %s reports earnings in %d days (%s)! Consider tightening stop.\n",
                  h_sym, as.integer(h_row$Days_To_Earn), h_row$Earnings_Date))
    }
    
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
empty_slots <- max(0, EFFECTIVE_MAX_POS - n_held)

cat(sprintf(" TOTAL ACCOUNT VALUE:    $%.2f\n", total_invested_val + cash_available))
cat(sprintf(" INVESTED IN EQUITIES:   $%.2f (%.1f%%)\n", total_invested_val, total_invested_val / CAPITAL * 100))
cat(sprintf(" AVAILABLE CASH BALANCE: $%.2f (%.1f%%)\n", cash_available, cash_available / CAPITAL * 100))
cat(sprintf(" AVAILABLE CASH SLOTS:   %d of %d (Regime Limit: %d)\n", empty_slots, EFFECTIVE_MAX_POS, EFFECTIVE_MAX_POS))

# ========================================================================================
# ACTIONABLE CAPITAL DEPLOYMENT (ORDERS TO FILL EMPTY CASH SLOTS)
# ========================================================================================
cat("\n========================================================================================\n")
cat(sprintf("              RECOMMENDED ORDERS FOR EMPTY CASH SLOTS (%d AVAILABLE)\n", empty_slots))
cat("========================================================================================\n")

if (empty_slots > 0) {
  # Candidate buys excluding already held positions and passing all gates
  unowned_buys <- df_scan[df_scan$Signal == "BUY" & !(df_scan$Symbol %in% held_syms), ]
  n_actionable <- min(nrow(unowned_buys), empty_slots)
  
  if (n_actionable > 0) {
    candidate_syms <- unowned_buys$Symbol[1:n_actionable]
    vince_alloc <- NULL
    
    # Ralph Vince Leverage Space Model Sizing
    if (SIZING_MODE == "vince") {
      cat(sprintf(" Sizing Engine: Ralph Vince Leverage Space Model (Safe f = %.2f, %d-Day Scenarios)\n",
                  SAFETY_FACTOR, VINCE_LOOKBACK))
      tryCatch({
        joint_events <- build_joint_scenario_matrix(candidate_syms, lookback_days = VINCE_LOOKBACK)
        avail_syms <- intersect(candidate_syms, colnames(joint_events))
        if (length(avail_syms) >= 1) {
          sub_events <- joint_events[, avail_syms, drop = FALSE]
          vince_opt <- vince_optimal_f(sub_events, max_leverage = 1.0, safety_factor = SAFETY_FACTOR)
          cur_px_map <- setNames(unowned_buys$Close[match(avail_syms, unowned_buys$Symbol)], avail_syms)
          vince_alloc <- vince_portfolio_allocation(vince_opt, total_cash = cash_available, current_prices = cur_px_map)
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
      row <- unowned_buys[i, ]
      
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
        shares <- floor(slot_capital_equal / row$Close)
        invested <- shares * row$Close
        sizing_note <- "Equal-Weight Slot Allocation"
        alloc_note  <- sprintf("Slot Budget: $%.2f -> Actual Outlay: $%.2f (Cash Left: $%.2f)",
                               slot_capital_equal, invested, slot_capital_equal - invested)
      }
      
      cat(sprintf("--- ORDER TICKET #%d: %s (P(Up): %.1f%%) ---\n", i, row$Symbol, row$Prob_Up * 100))
      cat(sprintf("  Action:              BUY %d SHARES at Market Open\n", shares))
      cat(sprintf("  Position Sizing:     %s\n", sizing_note))
      cat(sprintf("  Capital Allocation:  %s\n", alloc_note))
      cat(sprintf("  Multi-Timeframe:     Weekly Trend +%.2f%% [BULLISH SYNERGY]\n", row$Weekly_Slope))
      cat(sprintf("  Earnings Safe:       Next report %s (%d days out)\n", row$Earnings_Date, as.integer(row$Days_To_Earn)))
      cat(sprintf("  GTC Stop-Loss:       $%.2f (-%.2f%%) [Risk: $%.2f]\n", 
                  row$Stop_Loss, 2.0 * row$Daily_Vol * 100, shares * (row$Close - row$Stop_Loss)))
      cat(sprintf("  GTC Take-Profit:     $%.2f (+%.2f%%) [Gain: $%.2f]\n", 
                  row$Take_Profit, 3.0 * row$Daily_Vol * 100, shares * (row$Take_Profit - row$Close)))
      cat(sprintf("  Reward / Risk Ratio: 1.50 : 1.0\n\n"))
    }
  } else {
    cat(sprintf(" No unowned symbols currently meet all BUY criteria (P(Up) >= %.1f%%, Weekly Bullish, No Earnings).\n",
                EFFECTIVE_P_LONG * 100))
    cat(" RECOMMENDATION: Retain available cash buffer in money market / cash.\n")
  }
} else {
  cat(sprintf(" All %d active portfolio slots are currently filled or constrained by Macro Regime.\n", EFFECTIVE_MAX_POS))
  cat(" No new purchases needed today.\n")
}
cat("========================================================================================\n\n")
