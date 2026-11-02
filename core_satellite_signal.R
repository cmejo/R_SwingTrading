#!/usr/bin/env Rscript
#' ==============================================================================
#' Triple-Engine Momentum & Relative Strength Execution System
#'
#' BACKTEST BASIS: 2007-2026, survivorship-free point-in-time Nasdaq-100 membership
#' (jmccarrell/n100tickers), dual RS + stop proceeds -> QLD. Earlier figures (55% CAGR)
#' came from a hindsight-selected list and are NOT achievable. See scratch/pit_feature_search.R.
#'
#' CONFIGURATION MODES SUPPORTED:
#' ------------------------------------------------------------------------------
#' OPTION 6 (ACTIVE DEFAULT): 1.5x Margin + QLD Core + Dual RS + GLD
#'   - PIT Backtest: ~35.8% CAGR | -62.3% MaxDD | 0.92 Sharpe (QQQ B&H: 16.6% | -53.4%)
#'   - Core Sleeve: QLD (2x QQQ ETF)
#'   - Satellite Sleeve: Top-5 Dual-Lookback RS (50% 126d + 50% 63d vs QQQ), universe = Nasdaq-100
#'   - Portfolio Margin: 1.5x gross exposure during confirmed bull regimes (QQQ >= 200 SMA)
#'   - Safety Gate: De-levers margin to 0% debt and exits QLD to GLD when QQQ < 200 SMA
#'   - Volatility Target: 22% Vol Target into Gold (GLD)
#'   - Stop proceeds: disaster-stopped cash is redeployed into QLD until the next rebalance
#'
#' OPTION 3 (COMMENTED OUT): Pure Cash / IRA (QLD Core + Dual RS, NO Margin)
#'   - PIT Backtest: ~27.4% CAGR | -51.6% MaxDD | 0.97 Sharpe
#'   - Zero margin debt, zero margin interest, zero margin call risk
#'   - Tradable in Cash, Margin, and IRA/401(k) retirement accounts
#'   - Core Sleeve: QLD (2x QQQ ETF) when QQQ >= 200 SMA; exits to GLD when < 200 SMA
#'   - Satellite Sleeve: Dual-Lookback RS (50% 126d + 50% 63d blend vs QQQ)
#'   - Volatility Target: 22% Vol Target into Gold (GLD)
#'
#' Usage:
#'   Rscript core_satellite_signal.R [--capital=8000] [--core_asset=QLD]
#'                                   [--portfolio_margin=1.5] [--vol_target=0.22]
#'                                   [--top_n=5] [--symbols_file=symbols_n100.txt]
#'                                   [--stop_to_core=TRUE] [--breadth_gate=FALSE]
#'                                   [--breadth_threshold=0.50]
#'                                   [--force_rebalance] [--reset]
#' ==============================================================================

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(TTR)
  library(jsonlite)
})

source("R/01_data_loader.R")

# ------------------------------------------------------------------------------
# 1. Parse Command Line Arguments & Configuration Presets
# ------------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)

CAPITAL               <- 8000    # Default capital base: $8,000
CORE_WEIGHT           <- 0.30    # 30% Core Sleeve
TOP_N                 <- 5       # 5 Satellite slots (14% each) - beat top 3 on every broad list (see scratch/compare_top3_vs_top5.R)
LOOKBACK_LONG         <- 126     # ~6 months intermediate trend anchor
LOOKBACK_SHORT        <- 63      # ~3 months quarterly momentum acceleration
WEIGHT_LONG           <- 0.50    # 50% weight on 126d RS
WEIGHT_SHORT          <- 0.50    # 50% weight on 63d RS
LOOKBACK_DAYS         <- LOOKBACK_LONG # Compatibility alias
DISASTER_STOP         <- 0.20    # 20% catastrophic max loss floor
STOP_PROCEEDS_TO_CORE <- TRUE    # (A) Redeploy stopped-out satellite cash into the Core asset (QLD)
                                 #     when QQQ >= 200 SMA, instead of idling in cash until month-end.
                                 #     PIT N100 backtest: +1.6 pts CAGR, improved in both 2007-16 and 2017-26.
USE_BREADTH_GATE      <- FALSE   # (B) OPTIONAL, OFF BY DEFAULT: drop margin to 1.0x when fewer than
BREADTH_THRESHOLD     <- 0.50    #     BREADTH_THRESHOLD of scanned stocks are above their 200 SMA.
VOL_CAP               <- 1.00    # (C) Speculative-spike filter: skip stocks whose 63-day realized volatility
                                 #     exceeds VOL_CAP (1.00 = 100% annualized, ~6.3%/day). Set NA to disable.
                                 #     Cuts MaxDD ~6 pts in backtest, but the 50% threshold is fragile
                                 #     (40% had no effect, 60% was worse). Needs a broad list (e.g. N100).
USE_CRISIS_MVP        <- TRUE    # (D) Crisis Minimum Variance Portfolio: When in crisis/bear territory
                                 #     (QQQ < 200 SMA), stay invested in equities but switch selection rules
                                 #     from Momentum to the Minimum Variance Portfolio (lowest-volatility stocks).
FORCE_REBALANCE       <- FALSE
RESET                 <- FALSE
SYMBOLS_FILE          <- "symbols_broad.txt" # Broad 170-name growth/momentum list (alt: symbols_n100.txt = PIT-tested universe)
PORTFOLIO_FILE        <- "portfolio_coresat.json"
BROKER_STATE_FILE     <- NULL    # Optional JSON file containing live broker equity, cash & positions
QUOTES_FILE           <- NULL    # Optional JSON file containing real-time quotes (e.g. from Schwab API)
ORDERS_OUT_FILE       <- NULL    # Optional JSON output destination for structured machine-readable orders

# ==============================================================================
# ACTIVE PRESET: OPTION 6 (Supercharged: 1.5x Margin + QLD Core + GLD Hedge)
# ==============================================================================
CORE_ASSET            <- "QLD"   # Core Sleeve holds QLD (2x QQQ ETF)
PORTFOLIO_MARGIN_LEV  <- 1.50    # 1.5x portfolio margin during bull markets
USE_MARGIN_TREND_GATE <- TRUE    # De-lever margin debt to 0% when QQQ < 200 SMA
USE_CORE_TREND        <- TRUE    # Exit QLD to GLD when QQQ < 200 SMA
VOL_TARGET            <- 0.22    # 22% Volatility Target
DEFENSIVE_ASSET       <- "GLD"   # Gold (GLD) flight-to-safety crisis hedge

# ==============================================================================
# OPTION 3 (COMMENTED OUT): Pure Cash / IRA Sweet Spot (QLD Core, NO Margin)
# To switch to Option 3 (Cash / IRA), uncomment the 6 lines below:
# ==============================================================================
# CORE_ASSET            <- "QLD"   # Core Sleeve holds QLD (2x QQQ ETF)
# PORTFOLIO_MARGIN_LEV  <- 1.00    # 1.0x cash account (zero margin debt)
# USE_MARGIN_TREND_GATE <- FALSE   # No margin to de-lever
# USE_CORE_TREND        <- TRUE    # Exit QLD to GLD when QQQ < 200 SMA
# VOL_TARGET            <- 0.22    # 22% Volatility Target
# DEFENSIVE_ASSET       <- "GLD"   # Gold (GLD) flight-to-safety crisis hedge
# ==============================================================================

# Parse CLI overrides
for (arg in args) {
  if (grepl("^--capital=", arg)) {
    CAPITAL <- as.numeric(sub("^--capital=", "", arg))
  } else if (grepl("^--core_asset=", arg)) {
    CORE_ASSET <- toupper(sub("^--core_asset=", "", arg))
  } else if (grepl("^--portfolio_margin=", arg)) {
    PORTFOLIO_MARGIN_LEV <- as.numeric(sub("^--portfolio_margin=", "", arg))
  } else if (grepl("^--margin_trend_gate=", arg)) {
    USE_MARGIN_TREND_GATE <- as.logical(sub("^--margin_trend_gate=", "", arg))
  } else if (grepl("^--core_weight=", arg)) {
    CORE_WEIGHT <- as.numeric(sub("^--core_weight=", "", arg))
  } else if (grepl("^--top_n=", arg)) {
    TOP_N <- as.integer(sub("^--top_n=", "", arg))
  } else if (grepl("^--lookback=", arg)) {
    LOOKBACK_LONG <- as.integer(sub("^--lookback=", "", arg))
    LOOKBACK_DAYS <- LOOKBACK_LONG
  } else if (grepl("^--lookback_long=", arg)) {
    LOOKBACK_LONG <- as.integer(sub("^--lookback_long=", "", arg))
    LOOKBACK_DAYS <- LOOKBACK_LONG
  } else if (grepl("^--lookback_short=", arg)) {
    LOOKBACK_SHORT <- as.integer(sub("^--lookback_short=", "", arg))
  } else if (grepl("^--stop_to_core=", arg)) {
    STOP_PROCEEDS_TO_CORE <- as.logical(sub("^--stop_to_core=", "", arg))
  } else if (grepl("^--breadth_gate=", arg)) {
    USE_BREADTH_GATE <- as.logical(sub("^--breadth_gate=", "", arg))
  } else if (grepl("^--vol_cap=", arg)) {
    VOL_CAP <- suppressWarnings(as.numeric(sub("^--vol_cap=", "", arg)))
  } else if (grepl("^--breadth_threshold=", arg)) {
    BREADTH_THRESHOLD <- as.numeric(sub("^--breadth_threshold=", "", arg))
  } else if (grepl("^--disaster_stop=", arg)) {
    DISASTER_STOP <- as.numeric(sub("^--disaster_stop=", "", arg))
  } else if (grepl("^--vol_target=", arg)) {
    VOL_TARGET <- as.numeric(sub("^--vol_target=", "", arg))
  } else if (grepl("^--defensive_asset=", arg)) {
    DEFENSIVE_ASSET <- toupper(sub("^--defensive_asset=", "", arg))
  } else if (grepl("^--use_core_trend=", arg)) {
    USE_CORE_TREND <- as.logical(sub("^--use_core_trend=", "", arg))
  } else if (arg == "--force_rebalance") {
    FORCE_REBALANCE <- TRUE
  } else if (arg == "--reset") {
    RESET <- TRUE
    FORCE_REBALANCE <- TRUE
  } else if (grepl("^--symbols_file=", arg)) {
    SYMBOLS_FILE <- sub("^--symbols_file=", "", arg)
  } else if (grepl("^--portfolio_file=", arg)) {
    PORTFOLIO_FILE <- sub("^--portfolio_file=", "", arg)
  } else if (grepl("^--broker_state=", arg)) {
    BROKER_STATE_FILE <- sub("^--broker_state=", "", arg)
  } else if (grepl("^--quotes_file=", arg)) {
    QUOTES_FILE <- sub("^--quotes_file=", "", arg)
  } else if (grepl("^--orders_out=", arg)) {
    ORDERS_OUT_FILE <- sub("^--orders_out=", "", arg)
  } else if (grepl("^--crisis_mvp=", arg)) {
    USE_CRISIS_MVP <- as.logical(sub("^--crisis_mvp=", "", arg))
  }
}

SATELLITE_WEIGHT <- max(0, 1.0 - CORE_WEIGHT)
SLOT_WEIGHT      <- if (TOP_N > 0) SATELLITE_WEIGHT / TOP_N else 0.0

# ------------------------------------------------------------------------------
# 2. Portfolio State Management
# ------------------------------------------------------------------------------
init_portfolio <- function(capital = CAPITAL) {
  list(
    total_capital        = capital,
    cash_balance         = capital,
    core_weight          = CORE_WEIGHT,
    satellite_weight     = SATELLITE_WEIGHT,
    core_asset           = CORE_ASSET,
    portfolio_margin_lev = PORTFOLIO_MARGIN_LEV,
    vol_target           = VOL_TARGET,
    defensive_asset      = DEFENSIVE_ASSET,
    use_core_trend       = USE_CORE_TREND,
    last_rebalance_date  = "",
    core_position        = NULL,
    defensive_position   = NULL,
    satellite_positions  = list(),
    closed_trades        = list()
  )
}

load_portfolio <- function(file_path = PORTFOLIO_FILE, capital = CAPITAL) {
  if (RESET) {
    cat(sprintf(" 🔄 PORTFOLIO RESET: Re-initializing fresh portfolio state with $%.2f capital.\n", capital))
    return(init_portfolio(capital))
  }
  if (file.exists(file_path)) {
    state <- tryCatch({
      fromJSON(file_path, simplifyVector = FALSE)
    }, error = function(e) {
      warning("Failed to parse ", file_path, "; initializing fresh portfolio.")
      init_portfolio(capital)
    })
    if (is.null(state$total_capital)) state$total_capital <- capital
    if (is.null(state$cash_balance)) state$cash_balance <- capital
    if (is.null(state$satellite_positions)) state$satellite_positions <- list()
    if (is.null(state$closed_trades)) state$closed_trades <- list()
    if (!is.null(state$sgov_position) && is.null(state$defensive_position)) {
      state$defensive_position <- state$sgov_position
      state$sgov_position <- NULL
    }
    return(state)
  }
  init_portfolio(capital)
}

save_portfolio <- function(state, file_path = PORTFOLIO_FILE) {
  state$last_updated <- as.character(Sys.time())
  write(toJSON(state, auto_unbox = TRUE, pretty = TRUE), file = file_path)
}

port_state <- load_portfolio(PORTFOLIO_FILE, CAPITAL)

`%||%` <- function(a, b) if (!is.null(a)) a else b

# Reconcile with live broker state if provided (e.g. from Schwab API sync)
if (!is.null(BROKER_STATE_FILE) && file.exists(BROKER_STATE_FILE)) {
  broker_state <- tryCatch(fromJSON(BROKER_STATE_FILE, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.null(broker_state)) {
    b_cap <- as.numeric(broker_state$total_capital %||% broker_state$equity %||% CAPITAL)
    b_cash <- as.numeric(broker_state$cash_balance %||% broker_state$cashAvailableForTrading %||% CAPITAL)
    if (!is.na(b_cap) && b_cap > 0) {
      CAPITAL <- b_cap
      port_state$total_capital <- b_cap
    }
    if (!is.na(b_cash)) {
      port_state$cash_balance <- b_cash
    }
    
    # Synchronize broker positions if available
    b_positions <- broker_state$positions
    if (!is.null(b_positions) && length(b_positions) > 0) {
      cat(sprintf(" 🔗 Reconciling %d live broker position(s) from %s...\n", length(b_positions), BROKER_STATE_FILE))
      b_core_pos <- NULL
      b_def_pos  <- NULL
      b_sat_pos  <- list()
      
      existing_sat_map <- list()
      for (p in port_state$satellite_positions) {
        if (!is.null(p$symbol)) existing_sat_map[[p$symbol]] <- p
      }
      
      for (bp in b_positions) {
        b_sym <- toupper(bp$symbol)
        b_shs <- as.numeric(bp$shares %||% bp$quantity %||% 0)
        b_px  <- as.numeric(bp$average_price %||% bp$averageCost %||% bp$current_price %||% 0)
        if (b_shs <= 0) next
        
        if (b_sym == CORE_ASSET || b_sym == "QQQ") {
          b_core_pos <- list(symbol = b_sym, shares = b_shs, entry_price = b_px, entry_date = as.character(Sys.Date()))
        } else if (b_sym == DEFENSIVE_ASSET || b_sym %in% c("GLD", "SGOV", "BIL")) {
          b_def_pos <- list(symbol = b_sym, shares = b_shs, entry_price = b_px, entry_date = as.character(Sys.Date()))
        } else {
          orig_p <- existing_sat_map[[b_sym]]
          entry_dt <- if (!is.null(orig_p$entry_date)) orig_p$entry_date else as.character(Sys.Date())
          entry_px <- if (!is.null(orig_p$entry_price)) as.numeric(orig_p$entry_price) else b_px
          b_sat_pos[[length(b_sat_pos) + 1]] <- list(
            symbol = b_sym, shares = b_shs, entry_price = entry_px,
            current_price = b_px, entry_date = entry_dt, market_value = round(b_shs * b_px, 2),
            unrealized_pnl = 0.0
          )
        }
      }
      if (!is.null(b_core_pos)) port_state$core_position <- b_core_pos
      if (!is.null(b_def_pos)) port_state$defensive_position <- b_def_pos
      if (length(b_sat_pos) > 0) port_state$satellite_positions <- b_sat_pos
    }
    cat(sprintf(" 🏦 Live Broker State Synchronized: Equity = $%.2f | Cash = $%.2f\n",
                port_state$total_capital, port_state$cash_balance))
  }
}

# Load fresh market quotes if provided (e.g. from Schwab real-time quotes API)
live_quotes_data <- NULL
if (!is.null(QUOTES_FILE) && file.exists(QUOTES_FILE)) {
  live_quotes_data <- tryCatch(fromJSON(QUOTES_FILE, simplifyVector = FALSE), error = function(e) {
    warning("Failed to parse quotes_file: ", e$message)
    NULL
  })
  if (!is.null(live_quotes_data)) {
    q_nodes <- live_quotes_data$quotes %||% live_quotes_data
    q_count <- if (is.list(q_nodes)) length(q_nodes) else 0
    cat(sprintf(" ⚡ Fresh real-time quotes loaded from: %s (%d symbol%s)\n",
                QUOTES_FILE, q_count, if (q_count == 1) "" else "s"))
  }
}

get_live_quote_price <- function(sym, lq) {
  if (is.null(lq)) return(NULL)
  sym <- toupper(trimws(sym))
  node <- lq[[sym]]
  if (is.null(node) && !is.null(lq$quotes)) node <- lq$quotes[[sym]]
  if (is.null(node)) return(NULL)
  if (is.numeric(node)) return(as.numeric(node))
  if (is.list(node)) {
    if (!is.null(node$price)) return(as.numeric(node$price))
    if (!is.null(node$lastPrice)) return(as.numeric(node$lastPrice))
    if (!is.null(node$quote$lastPrice)) return(as.numeric(node$quote$lastPrice))
    if (!is.null(node$close)) return(as.numeric(node$close))
  }
  return(NULL)
}

inject_live_quote <- function(ohlcv, sym, lq) {
  if (is.null(ohlcv) || nrow(ohlcv) == 0) return(ohlcv)
  px <- get_live_quote_price(sym, lq)
  if (is.null(px) || is.na(px) || px <= 0) return(ohlcv)
  
  n_rows <- nrow(ohlcv)
  last_dt <- as.Date(index(ohlcv[n_rows]))
  today_dt <- Sys.Date()
  
  cl_col <- grep("Close", colnames(ohlcv))[1]
  adj_col <- grep("Adjusted", colnames(ohlcv))[1]
  hi_col <- grep("High", colnames(ohlcv))[1]
  lo_col <- grep("Low", colnames(ohlcv))[1]
  if (is.na(cl_col)) cl_col <- ncol(ohlcv)
  
  if (last_dt == today_dt) {
    ohlcv[n_rows, cl_col] <- px
    if (!is.na(adj_col)) ohlcv[n_rows, adj_col] <- px
    if (!is.na(hi_col) && px > as.numeric(ohlcv[n_rows, hi_col])) ohlcv[n_rows, hi_col] <- px
    if (!is.na(lo_col) && px < as.numeric(ohlcv[n_rows, lo_col])) ohlcv[n_rows, lo_col] <- px
  } else if (today_dt > last_dt) {
    last_cl <- as.numeric(ohlcv[n_rows, cl_col])
    bar_mat <- matrix(c(last_cl, max(last_cl, px), min(last_cl, px), px, 0, px), nrow = 1)
    if (ncol(bar_mat) == ncol(ohlcv)) {
      new_row <- xts(bar_mat, order.by = today_dt)
      colnames(new_row) <- colnames(ohlcv)
      ohlcv <- rbind(ohlcv, new_row)
    }
  }
  return(ohlcv)
}

# ------------------------------------------------------------------------------
# 3. Load Market Data (QQQ, Core Asset, and Defensive Asset)
# ------------------------------------------------------------------------------
# Benchmark QQQ (Market Compass)
qqq_ohlcv <- tryCatch(load_stock_data("QQQ", force_refresh = FALSE), error = function(e) NULL)
qqq_ohlcv <- inject_live_quote(qqq_ohlcv, "QQQ", live_quotes_data)
if (is.null(qqq_ohlcv) || nrow(qqq_ohlcv) < max(LOOKBACK_LONG, 200)) {
  stop("FATAL: Could not load sufficient historical data for benchmark QQQ.")
}
qqq_close      <- as.numeric(Cl(qqq_ohlcv))
n_qqq          <- length(qqq_close)
qqq_cur_px     <- tail(qqq_close, 1)
qqq_ret_long   <- (qqq_cur_px / qqq_close[n_qqq - LOOKBACK_LONG]) - 1
qqq_ret_short  <- (qqq_cur_px / qqq_close[n_qqq - LOOKBACK_SHORT]) - 1
qqq_ret        <- qqq_ret_long # Compatibility alias

# QQQ Trend & Realized Volatility Diagnostics
qqq_sma200   <- mean(tail(qqq_close, 200))
qqq_trend_up <- qqq_cur_px >= qqq_sma200

# Rolling 20-day realized volatility of log returns
ret_20d      <- diff(log(tail(qqq_close, 21)))
realized_vol <- sd(ret_20d) * sqrt(252)
vol_scale    <- min(1.0, VOL_TARGET / max(0.05, realized_vol))

# Core Asset Data (QLD or QQQ)
core_ohlcv <- if (CORE_ASSET == "QQQ") qqq_ohlcv else tryCatch(load_stock_data(CORE_ASSET, force_refresh = FALSE), error = function(e) NULL)
core_ohlcv <- inject_live_quote(core_ohlcv, CORE_ASSET, live_quotes_data)
core_cur_px <- if (!is.null(core_ohlcv)) tail(as.numeric(Cl(core_ohlcv)), 1) else if (CORE_ASSET == "QLD") 82.59 else qqq_cur_px

# Defensive Asset Market Data (GLD or SGOV)
def_ohlcv  <- tryCatch(load_stock_data(DEFENSIVE_ASSET, force_refresh = FALSE), error = function(e) NULL)
def_ohlcv  <- inject_live_quote(def_ohlcv, DEFENSIVE_ASSET, live_quotes_data)
def_cur_px <- if (!is.null(def_ohlcv)) tail(as.numeric(Cl(def_ohlcv)), 1) else if (DEFENSIVE_ASSET == "GLD") 355.78 else 100.73

# Determine active margin leverage today
active_margin <- PORTFOLIO_MARGIN_LEV
if (PORTFOLIO_MARGIN_LEV > 1.0 && USE_MARGIN_TREND_GATE && !qqq_trend_up) {
  active_margin <- 1.0 # Bear market safety gate: de-lever margin debt to 0%
}

cat("\n========================================================================================\n")
if (PORTFOLIO_MARGIN_LEV > 1.0) {
  cat(sprintf("   TRIPLE-ENGINE SUPERCHARGED MODEL: 1.5x MARGIN + %s CORE + 22%% VT -> %s     \n", CORE_ASSET, DEFENSIVE_ASSET))
} else {
  cat(sprintf("   TRIPLE-ENGINE SWEET SPOT MODEL (CASH / IRA): %s CORE + 22%% VT -> %s        \n", CORE_ASSET, DEFENSIVE_ASSET))
}
cat("========================================================================================\n")
cat(sprintf(" Date:                   %s\n", Sys.Date()))
cat(sprintf(" Capital Base (Equity):  $%.2f\n", port_state$total_capital))
cat(sprintf(" Core Asset:             %s @ $%.2f (Target: %.1f%%)\n", CORE_ASSET, core_cur_px, CORE_WEIGHT * 100))
cat(sprintf(" Satellite Sleeve:       Top %d RS Stocks (Target: %.1f%% / %.1f%% per slot)\n",
            TOP_N, SATELLITE_WEIGHT * 100, SLOT_WEIGHT * 100))
cat(sprintf(" Portfolio Leverage:     Target: %.2fx | Active Today: %.2fx %s\n",
            PORTFOLIO_MARGIN_LEV, active_margin,
            if (active_margin < PORTFOLIO_MARGIN_LEV) "(DE-LEVERED TO 0% DEBT: QQQ < 200 SMA)" else "(BULL MARGIN ACTIVE)"))
cat(sprintf(" Volatility Targeting:   Target: %.1f%% | Realized (20d): %.1f%% | Vol Scale Factor: %.2fx\n",
            VOL_TARGET * 100, realized_vol * 100, vol_scale))
cat(sprintf(" Benchmark Status:       QQQ @ $%.2f | 200 SMA: $%.2f | Trend: %s\n",
            qqq_cur_px, qqq_sma200, if (qqq_trend_up) "BULLISH (>= 200 SMA)" else "BEARISH (< 200 SMA)"))
cat(sprintf(" Defensive Reserve:      %s @ $%.2f (Inflation/Crisis Flight-to-Safety)\n",
            DEFENSIVE_ASSET, def_cur_px))
cat(sprintf(" Disaster Stop Floor:    -%.1f%% on individual satellite positions\n", DISASTER_STOP * 100))
cat("========================================================================================\n\n")

# ------------------------------------------------------------------------------
# 4. Check Rebalance Condition
# ------------------------------------------------------------------------------
today <- Sys.Date()
today_str <- as.character(today)
last_reb_str <- if (!is.null(port_state$last_rebalance_date)) port_state$last_rebalance_date else ""

is_new_month <- FALSE
if (nzchar(last_reb_str)) {
  last_reb_date <- tryCatch(as.Date(last_reb_str), error = function(e) as.Date("2000-01-01"))
  is_new_month <- format(today, "%Y-%m") != format(last_reb_date, "%Y-%m")
} else {
  is_new_month <- TRUE
}

is_uninvested <- (length(port_state$satellite_positions) == 0 && is.null(port_state$core_position))
should_rebalance <- FORCE_REBALANCE || is_new_month || is_uninvested

# ------------------------------------------------------------------------------
# 5. Universe Relative Strength Scan
# ------------------------------------------------------------------------------
EXCLUDE_ETFS <- c("QQQ", "QLD", "TQQQ", "SQQQ", "SPY", "VTI", "SOXL", "SOXS", "FNGD", 
                  "FNGU", "SGOV", "BIL", "GLD", "SMH", "XLK", "^VIX", "^IRX",
                  # Leveraged single-stock / sector ETFs (excluded in backtests too)
                  "AMUU", "NVDU", "PLTU", "SNDQ", "SMHC", "SKHY", "NVDL", "TSLL", "CONL", "MSTU")

sym_lines <- if (file.exists(SYMBOLS_FILE)) readLines(SYMBOLS_FILE) else character()
sym_lines <- trimws(sub("#.*$", "", sym_lines))   # strip full-line and inline "# ..." comments
raw_syms <- unique(toupper(trimws(unlist(strsplit(sym_lines[nzchar(sym_lines)], "[,[:space:]]+")))))
raw_syms <- raw_syms[nzchar(raw_syms)]
scan_syms <- setdiff(raw_syms, EXCLUDE_ETFS)

cat(sprintf(" Scanning %d candidate stocks for Dual-Lookback RS (50%% %dd + 50%% %dd) vs QQQ...\n",
            length(scan_syms), LOOKBACK_LONG, LOOKBACK_SHORT))

rs_results <- list()
for (sym in scan_syms) {
  d <- tryCatch(load_stock_data(sym, force_refresh = FALSE), error = function(e) NULL)
  d <- inject_live_quote(d, sym, live_quotes_data)
  if (is.null(d) || nrow(d) < LOOKBACK_LONG + 10) next
  # Data-quality guard: skip tickers with zero/negative/missing closes in the lookback
  # window (e.g. thin OTC listings such as SSNLF report long runs of 0.00 prices)
  cl_chk <- tail(as.numeric(Cl(d)), max(200, LOOKBACK_LONG + 1))
  if (any(is.na(cl_chk)) || any(cl_chk <= 0)) next
  
  cl <- as.numeric(Cl(d))
  n <- length(cl)
  cur_px <- cl[n]
  lb_long_px  <- cl[n - LOOKBACK_LONG]
  lb_short_px <- cl[n - LOOKBACK_SHORT]
  
  # Trend check: 200 SMA
  sma200 <- if (n >= 200) mean(cl[(n - 199):n]) else mean(cl)
  trend_up <- cur_px >= sma200
  
  ret_long  <- (cur_px / lb_long_px) - 1
  ret_short <- (cur_px / lb_short_px) - 1
  rs_long   <- ret_long - qqq_ret_long
  rs_short  <- ret_short - qqq_ret_short
  rs_comp   <- (WEIGHT_LONG * rs_long) + (WEIGHT_SHORT * rs_short)
  vol_63d   <- sd(diff(log(tail(cl, 64)))) * sqrt(252)
  
  rs_results[[sym]] <- data.frame(
    Symbol       = sym,
    Close        = cur_px,
    SMA200       = sma200,
    Trend_Up     = trend_up,
    Ret_126D     = ret_long,
    Ret_63D      = ret_short,
    RS_126D      = rs_long,
    RS_63D       = rs_short,
    RS_Composite = rs_comp,
    Vol_63D      = vol_63d,
    stringsAsFactors = FALSE
  )
}

if (length(rs_results) == 0) {
  stop("FATAL: No candidate stocks had sufficient data for ranking.")
}

rs_df <- do.call(rbind, rs_results)

# (B) Optional market-breadth gate: % of scanned universe above its own 200 SMA
breadth_pct <- mean(rs_df$Trend_Up, na.rm = TRUE)
if (USE_BREADTH_GATE && active_margin > 1.0 && breadth_pct < BREADTH_THRESHOLD) {
  active_margin <- 1.0
  cat(sprintf("\n 🛡️ BREADTH GATE: only %.0f%% of %d scanned stocks above 200 SMA (< %.0f%%). Margin reduced to 1.00x.\n",
              breadth_pct * 100, nrow(rs_df), BREADTH_THRESHOLD * 100))
} else {
  cat(sprintf("\n Market Breadth: %.0f%% of %d scanned stocks above 200 SMA%s\n", breadth_pct * 100, nrow(rs_df),
              if (USE_BREADTH_GATE) sprintf(" (gate ON, threshold %.0f%%)", BREADTH_THRESHOLD * 100) else " (gate OFF)"))
}
rs_qualified <- rs_df[rs_df$Trend_Up, ]
if (nrow(rs_qualified) < TOP_N) rs_qualified <- rs_df

# (C) Speculative-spike filter: drop extreme-volatility names (only if enough names remain)
if (!is.na(VOL_CAP)) {
  vc_ok <- rs_qualified[!is.na(rs_qualified$Vol_63D) & rs_qualified$Vol_63D <= VOL_CAP, ]
  dropped <- setdiff(rs_qualified$Symbol, vc_ok$Symbol)
  if (nrow(vc_ok) >= TOP_N) {
    rs_qualified <- vc_ok
    if (length(dropped) > 0) {
      dd <- rs_df[rs_df$Symbol %in% dropped, ]
      dd <- dd[order(-dd$RS_Composite), ]
      cat(sprintf("\n Volatility cap (%.0f%%) excluded %d high-volatility candidate(s): %s\n", VOL_CAP * 100, nrow(dd),
                  paste(sprintf("%s (%.0f%%)", head(dd$Symbol, 8), head(dd$Vol_63D, 8) * 100), collapse = ", ")))
    }
  }
}

# Check regime: Bull (QQQ >= 200 SMA) vs Crisis / Serious Bear (QQQ < 200 SMA)
is_crisis_regime <- !qqq_trend_up

if (USE_CRISIS_MVP && is_crisis_regime) {
  cat("\n========================================================================================\n")
  cat(" 🛡️ CRISIS / BEAR REGIME ACTIVE: QQQ < 200-day SMA                                      \n")
  cat(" CHANGING RULES: REBALANCING AROUND MINIMUM VARIANCE EQUITIES PORTFOLIO (MVP)           \n")
  cat(" Philosophy: Stay 100% invested in equities, shifting selection from Momentum to Min-Var!\n")
  cat("========================================================================================\n")
  
  # When trend is down and volatility is elevated, shift rules from Momentum to Minimum Variance
  mv_pool <- rs_df[!is.na(rs_df$Vol_63D) & rs_df$Vol_63D > 0, ]
  mv_pool <- mv_pool[order(mv_pool$Vol_63D), ]
  top_picks <- head(mv_pool, TOP_N)
  
  cat(sprintf("\n Top %d Minimum Variance Defensive Stocks (Lowest 63-day Realized Volatility):\n", TOP_N))
  cat("--------------------------------------------------------------------------------------------------------\n")
  print_df <- head(mv_pool[, c("Symbol", "Close", "Trend_Up", "Ret_126D", "Ret_63D", "RS_126D", "RS_63D", "RS_Composite", "Vol_63D")], 8)
  print_df$Ret_126D     <- sprintf("%+.1f%%", print_df$Ret_126D * 100)
  print_df$Ret_63D      <- sprintf("%+.1f%%", print_df$Ret_63D * 100)
  print_df$RS_126D      <- sprintf("%+.1f%%", print_df$RS_126D * 100)
  print_df$RS_63D       <- sprintf("%+.1f%%", print_df$RS_63D * 100)
  print_df$RS_Composite <- sprintf("%+.1f%%", print_df$RS_Composite * 100)
  print_df$Vol_63D      <- sprintf("%.0f%%", print_df$Vol_63D * 100)
  print(print_df, row.names = FALSE)
  cat("--------------------------------------------------------------------------------------------------------\n\n")
} else {
  rs_qualified <- rs_qualified[order(-rs_qualified$RS_Composite), ]
  top_picks <- head(rs_qualified, TOP_N)
  
  cat("\n Top Candidate Relative Strength Leaders (Dual-Lookback: 50% 126d + 50% 63d | >= 200-day SMA):\n")
  cat("--------------------------------------------------------------------------------------------------------\n")
  print_df <- head(rs_qualified[, c("Symbol", "Close", "Trend_Up", "Ret_126D", "Ret_63D", "RS_126D", "RS_63D", "RS_Composite", "Vol_63D")], 8)
  print_df$Ret_126D     <- sprintf("%+.1f%%", print_df$Ret_126D * 100)
  print_df$Ret_63D      <- sprintf("%+.1f%%", print_df$Ret_63D * 100)
  print_df$RS_126D      <- sprintf("%+.1f%%", print_df$RS_126D * 100)
  print_df$RS_63D       <- sprintf("%+.1f%%", print_df$RS_63D * 100)
  print_df$RS_Composite <- sprintf("%+.1f%%", print_df$RS_Composite * 100)
  print_df$Vol_63D      <- sprintf("%.0f%%", print_df$Vol_63D * 100)
  print(print_df, row.names = FALSE)
  cat("--------------------------------------------------------------------------------------------------------\n\n")
}

# ------------------------------------------------------------------------------
# 6. Mark Current Portfolio Value & Process Disaster Stops
# ------------------------------------------------------------------------------
cur_prices <- list(QQQ = qqq_cur_px, QLD = core_cur_px, GLD = def_cur_px, SGOV = 100.73)
cur_prices[[CORE_ASSET]]      <- core_cur_px
cur_prices[[DEFENSIVE_ASSET]] <- def_cur_px

for (row_idx in seq_len(nrow(rs_df))) {
  cur_prices[[rs_df$Symbol[row_idx]]] <- rs_df$Close[row_idx]
}

# Override/supplement with any live quotes loaded from quotes_file
if (!is.null(live_quotes_data)) {
  q_nodes <- live_quotes_data$quotes %||% live_quotes_data
  if (is.list(q_nodes)) {
    for (sym_q in names(q_nodes)) {
      px_q <- get_live_quote_price(sym_q, live_quotes_data)
      if (!is.null(px_q) && px_q > 0) cur_prices[[sym_q]] <- px_q
    }
  }
}

# Value Core Position
core_val <- 0
core_pos <- port_state$core_position
if (!is.null(core_pos) && as.numeric(core_pos$shares) > 0) {
  sym_core <- if (!is.null(core_pos$symbol)) core_pos$symbol else CORE_ASSET
  core_px  <- if (!is.null(cur_prices[[sym_core]])) cur_prices[[sym_core]] else as.numeric(core_pos$entry_price)
  core_val <- round(as.numeric(core_pos$shares) * core_px, 2)
}

# Value Defensive Position (GLD or legacy SGOV)
def_val <- 0
def_pos <- port_state$defensive_position
if (!is.null(def_pos) && as.numeric(def_pos$shares) > 0) {
  sym_def <- if (!is.null(def_pos$symbol)) def_pos$symbol else DEFENSIVE_ASSET
  def_px  <- if (!is.null(cur_prices[[sym_def]])) cur_prices[[sym_def]] else as.numeric(def_pos$entry_price)
  def_val <- round(as.numeric(def_pos$shares) * def_px, 2)
}

# Value & Audit Satellite Positions
sat_val <- 0
kept_sat_positions <- list()
disaster_tickets   <- character()
order_records      <- list()

if (!is.null(port_state$satellite_positions) && length(port_state$satellite_positions) > 0) {
  cat(" Current Satellite Holdings Status & Disaster Stop Watchdog:\n")
  cat("----------------------------------------------------------------------------------------\n")
  for (pos in port_state$satellite_positions) {
    sym <- pos$symbol
    shs <- as.numeric(pos$shares)
    entry_px <- as.numeric(pos$entry_price)
    cur_px <- if (!is.null(cur_prices[[sym]])) cur_prices[[sym]] else entry_px
    mv <- round(shs * cur_px, 2)
    pnl_pct <- (cur_px / entry_px) - 1
    pnl_dlr <- mv - (shs * entry_px)
    
    # Check disaster stop (-20% catastrophic floor)
    if (pnl_pct <= -DISASTER_STOP) {
      cat(sprintf(" 🚨 DISASTER STOP TRIGGERED: %s is down %+.1f%% (Entry: $%.2f | Now: $%.2f)\n",
                  sym, pnl_pct * 100, entry_px, cur_px))
      ticket <- sprintf("SELL %s shares of %s @ ~$%.2f (Disaster Stop Floor Triggered: %+.1f%%)",
                        round(shs, 3), sym, cur_px, pnl_pct * 100)
      disaster_tickets <- c(disaster_tickets, ticket)
      order_records[[length(order_records) + 1]] <- list(
        symbol = sym, side = "SELL", shares = round(shs, 3),
        shares_whole = as.integer(round(shs)), estimated_price = round(cur_px, 2),
        estimated_outlay = round(shs * cur_px, 2), order_type = "MARKET",
        reason = "DISASTER_STOP", stop_loss_price = NULL
      )
      
      port_state$cash_balance <- port_state$cash_balance + mv
      port_state$closed_trades[[length(port_state$closed_trades) + 1]] <- list(
        symbol = sym, shares = shs, entry_price = entry_px, exit_price = cur_px,
        entry_date = pos$entry_date, exit_date = today_str,
        pnl_dollar = round(pnl_dlr, 2), pnl_pct = round(pnl_pct * 100, 2),
        reason = "DISASTER_STOP"
      )
    } else {
      cat(sprintf(" %-6s: %8.3f shs @ ~$%-7.2f | MV: $%8.2f | P&L: %-+7.1f%% ($%+.2f)\n",
                  sym, shs, cur_px, mv, pnl_pct * 100, pnl_dlr))
      sat_val <- sat_val + mv
      pos$current_price <- cur_px
      pos$market_value  <- mv
      pos$unrealized_pnl <- round(pnl_dlr, 2)
      kept_sat_positions[[length(kept_sat_positions) + 1]] <- pos
    }
  }
  cat("----------------------------------------------------------------------------------------\n\n")
} else {
  cat(" Currently 0 active satellite holdings.\n\n")
}

port_state$satellite_positions <- kept_sat_positions

# (A) Redeploy disaster-stop proceeds into the Core asset (QLD) between rebalances.
#     Skipped on rebalance days (the rebalance redeploys everything anyway) and in
#     bear regimes (QQQ < 200 SMA), where the proceeds stay in cash until month-end.
if (STOP_PROCEEDS_TO_CORE && length(disaster_tickets) > 0 && !should_rebalance &&
    (!USE_CORE_TREND || qqq_trend_up)) {
  stop_proceeds <- sum(sapply(port_state$closed_trades, function(tr)
    if (identical(tr$exit_date, today_str) && identical(tr$reason, "DISASTER_STOP"))
      as.numeric(tr$shares) * as.numeric(tr$exit_price) else 0))
  if (stop_proceeds >= 30) {
    buy_shs <- round(stop_proceeds / core_cur_px, 3)
    disaster_tickets <- c(disaster_tickets, sprintf(
      "BUY %s shares of %s @ ~$%.2f ($%.2f outlay - Redeploy stop-loss proceeds into Core until next rebalance)",
      buy_shs, CORE_ASSET, core_cur_px, round(buy_shs * core_cur_px, 2)))
    order_records[[length(order_records) + 1]] <- list(
      symbol = CORE_ASSET, side = "BUY", shares = round(buy_shs, 3),
      shares_whole = as.integer(floor(buy_shs)), estimated_price = round(core_cur_px, 2),
      estimated_outlay = round(buy_shs * core_cur_px, 2), order_type = "MARKET",
      reason = "REDEPLOY_STOP_PROCEEDS_TO_CORE", stop_loss_price = NULL
    )
    prev_shs <- if (!is.null(port_state$core_position$shares)) as.numeric(port_state$core_position$shares) else 0
    prev_px  <- if (!is.null(port_state$core_position$entry_price)) as.numeric(port_state$core_position$entry_price) else core_cur_px
    new_shs  <- prev_shs + buy_shs
    port_state$core_position <- list(
      symbol = CORE_ASSET, shares = new_shs,
      entry_price = round((prev_shs * prev_px + buy_shs * core_cur_px) / new_shs, 4),
      entry_date = if (!is.null(port_state$core_position$entry_date)) port_state$core_position$entry_date else today_str)
    port_state$cash_balance <- port_state$cash_balance - round(buy_shs * core_cur_px, 2)
    core_val <- round(new_shs * core_cur_px, 2)
  }
}

total_equity <- round(port_state$cash_balance + core_val + def_val + sat_val, 2)
port_state$total_capital <- total_equity

gross_exposure   <- core_val + def_val + sat_val
margin_debt      <- max(0, gross_exposure - total_equity)
unallocated_cash <- max(0, total_equity - gross_exposure)

cat("========================================================================================\n")
cat("                               PORTFOLIO BALANCE SHEET                                  \n")
cat("========================================================================================\n")
cat(sprintf(" Total Portfolio Equity (Net Worth): $%.2f\n", total_equity))
cat(sprintf(" Gross Long Market Value:            $%.2f (%5.1f%% of equity)\n",
            gross_exposure, (gross_exposure / max(1, total_equity)) * 100))
cat(sprintf(" Borrowed Margin Loan (Debt):        $%.2f (%5.1f%% leverage)\n",
            margin_debt, (margin_debt / max(1, total_equity)) * 100))
cat("----------------------------------------------------------------------------------------\n")
cat(sprintf(" Core %-4s Sleeve:                  $%.2f (%5.1f%% of equity)\n",
            CORE_ASSET, core_val, (core_val / max(1, total_equity)) * 100))
cat(sprintf(" Satellite Sleeve (%d Stocks):        $%.2f (%5.1f%% of equity)\n", TOP_N,
            sat_val, (sat_val / max(1, total_equity)) * 100))
cat(sprintf(" Defensive %-4s Sleeve:             $%.2f (%5.1f%% of equity)\n",
            DEFENSIVE_ASSET, def_val, (def_val / max(1, total_equity)) * 100))
cat(sprintf(" Unallocated Free Cash:              $%.2f (%5.1f%% of equity)\n",
            unallocated_cash, (unallocated_cash / max(1, total_equity)) * 100))
cat("========================================================================================\n\n")

# ------------------------------------------------------------------------------
# 7. Rebalancing Engine (Monthly Scheduled or Forced)
# ------------------------------------------------------------------------------
rebalance_tickets <- character()

if (should_rebalance) {
  cat("----------------------------------------------------------------------------------------\n")
  cat(sprintf(" 🔄 REBALANCE ACTIVE (Trigger: %s)\n",
              if (FORCE_REBALANCE) "FORCED CLI / SUPERCHARGED LEVERAGE UPGRADE"
              else if (is_uninvested) "INITIAL PORTFOLIO DEPLOYMENT"
              else "NEW MONTH ENTRY"))
  cat("----------------------------------------------------------------------------------------\n")
  
  # Engine 1: Core Trend & Bear Market Check
  core_active <- TRUE
  if (USE_CORE_TREND && !qqq_trend_up) {
    core_active <- FALSE
    cat(sprintf(" 🛡️ BEAR MARKET REGIME: QQQ < 200 SMA. Core %s exiting to 100%% %s / Cash.\n",
                CORE_ASSET, DEFENSIVE_ASSET))
  }
  
  # Target dollar amounts:
  base_core_cash <- if (core_active) total_equity * CORE_WEIGHT * vol_scale else 0.0
  base_sat_cash  <- total_equity * SATELLITE_WEIGHT * vol_scale
  
  target_core_cash <- base_core_cash * active_margin
  target_sat_cash  <- base_sat_cash * active_margin
  target_slot_cash <- target_sat_cash / TOP_N
  target_sat_syms  <- top_picks$Symbol
  
  # Defensive Target Allocation: absorbs unallocated buffer + core when core inactive
  base_equity_w   <- (if (core_active) CORE_WEIGHT else 0.0) * vol_scale + (SATELLITE_WEIGHT * vol_scale)
  target_def_cash <- max(0, total_equity * (1.0 - base_equity_w))
  
  cat(sprintf(" Target Allocations: Core %s: $%.2f (%.1f%%) | Satellites (%d): $%.2f (%.1f%%) | Defensive %s: $%.2f (%.1f%%)\n\n",
              CORE_ASSET, target_core_cash, (target_core_cash / total_equity) * 100, TOP_N,
              target_sat_cash, (target_sat_cash / total_equity) * 100,
              DEFENSIVE_ASSET, target_def_cash, (target_def_cash / total_equity) * 100))
  
  # A. Liquidate Satellite Positions That Dropped Out of Top-N
  updated_sat_list <- list()
  if (length(port_state$satellite_positions) > 0) {
    for (pos in port_state$satellite_positions) {
      sym <- pos$symbol
      shs <- as.numeric(pos$shares)
      px  <- if (!is.null(cur_prices[[sym]])) cur_prices[[sym]] else as.numeric(pos$entry_price)
      mv  <- round(shs * px, 2)
      
      if (!(sym %in% target_sat_syms)) {
        cat(sprintf(" ✂️  ROTATION EXIT: %s dropped out of Top-%d RS\n", sym, TOP_N))
        ticket <- sprintf("SELL %s shares of %s @ ~$%.2f (Monthly RS Rotation Out)", round(shs, 3), sym, px)
        rebalance_tickets <- c(rebalance_tickets, ticket)
        order_records[[length(order_records) + 1]] <- list(
          symbol = sym, side = "SELL", shares = round(shs, 3),
          shares_whole = as.integer(round(shs)), estimated_price = round(px, 2),
          estimated_outlay = round(shs * px, 2), order_type = "MARKET",
          reason = "MONTHLY_ROTATION", stop_loss_price = NULL
        )
        
        port_state$cash_balance <- port_state$cash_balance + mv
        port_state$closed_trades[[length(port_state$closed_trades) + 1]] <- list(
          symbol = sym, shares = shs, entry_price = as.numeric(pos$entry_price),
          exit_price = px, entry_date = pos$entry_date, exit_date = today_str,
          pnl_dollar = round(mv - (shs * as.numeric(pos$entry_price)), 2),
          pnl_pct = round(((px / as.numeric(pos$entry_price)) - 1) * 100, 2),
          reason = "MONTHLY_ROTATION"
        )
      } else {
        updated_sat_list[[length(updated_sat_list) + 1]] <- pos
      }
    }
  }
  port_state$satellite_positions <- updated_sat_list
  
  # B. Core Position Adjustment (Liquidate old QQQ if switching to QLD)
  current_core_sym <- if (!is.null(port_state$core_position$symbol)) port_state$core_position$symbol else ""
  current_core_shs <- if (!is.null(port_state$core_position$shares)) as.numeric(port_state$core_position$shares) else 0
  
  if (current_core_sym != CORE_ASSET && current_core_shs > 0) {
    old_core_px <- if (!is.null(cur_prices[[current_core_sym]])) cur_prices[[current_core_sym]] else as.numeric(port_state$core_position$entry_price)
    old_core_mv <- round(current_core_shs * old_core_px, 2)
    ticket <- sprintf("SELL %s shares of %s @ ~$%.2f ($%.2f proceeds - Switch Core to %s)",
                      current_core_shs, current_core_sym, old_core_px, old_core_mv, CORE_ASSET)
    rebalance_tickets <- c(rebalance_tickets, ticket)
    order_records[[length(order_records) + 1]] <- list(
      symbol = current_core_sym, side = "SELL", shares = round(current_core_shs, 3),
      shares_whole = as.integer(round(current_core_shs)), estimated_price = round(old_core_px, 2),
      estimated_outlay = round(old_core_mv, 2), order_type = "MARKET",
      reason = sprintf("SWITCH_CORE_TO_%s", CORE_ASSET), stop_loss_price = NULL
    )
    port_state$cash_balance <- port_state$cash_balance + old_core_mv
    port_state$core_position <- NULL
    current_core_shs <- 0
  }
  
  target_core_shs <- round(target_core_cash / core_cur_px, 3)
  core_delta_shs  <- target_core_shs - current_core_shs
  core_delta_cash <- round(core_delta_shs * core_cur_px, 2)
  
  if (abs(core_delta_cash) >= 30) {
    if (core_delta_cash > 0) {
      buy_shs <- round(core_delta_cash / core_cur_px, 3)
      ticket <- sprintf("BUY %s shares of %s @ ~$%.2f ($%.2f outlay - Align Core Target to %.1f%%)",
                        buy_shs, CORE_ASSET, core_cur_px, round(buy_shs * core_cur_px, 2),
                        (target_core_cash / total_equity) * 100)
      rebalance_tickets <- c(rebalance_tickets, ticket)
      order_records[[length(order_records) + 1]] <- list(
        symbol = CORE_ASSET, side = "BUY", shares = round(buy_shs, 3),
        shares_whole = as.integer(floor(buy_shs)), estimated_price = round(core_cur_px, 2),
        estimated_outlay = round(buy_shs * core_cur_px, 2), order_type = "MARKET",
        reason = "ALIGN_CORE_TARGET", stop_loss_price = NULL
      )
      port_state$core_position <- list(
        symbol = CORE_ASSET,
        shares = current_core_shs + buy_shs,
        entry_price = core_cur_px,
        entry_date = today_str
      )
      port_state$cash_balance <- port_state$cash_balance - round(buy_shs * core_cur_px, 2)
    } else {
      trim_shs <- abs(core_delta_shs)
      ticket <- sprintf("SELL %s shares of %s @ ~$%.2f ($%.2f proceeds - Trim Core Target to %.1f%%)",
                        trim_shs, CORE_ASSET, core_cur_px, abs(core_delta_cash),
                        (target_core_cash / total_equity) * 100)
      rebalance_tickets <- c(rebalance_tickets, ticket)
      order_records[[length(order_records) + 1]] <- list(
        symbol = CORE_ASSET, side = "SELL", shares = round(trim_shs, 3),
        shares_whole = as.integer(round(trim_shs)), estimated_price = round(core_cur_px, 2),
        estimated_outlay = abs(core_delta_cash), order_type = "MARKET",
        reason = "TRIM_CORE_TARGET", stop_loss_price = NULL
      )
      port_state$core_position$shares <- max(0, current_core_shs - trim_shs)
      port_state$cash_balance <- port_state$cash_balance + abs(core_delta_cash)
    }
  }
  
  # C. Buy / Top-up Top-N Satellite Positions
  existing_sat_syms <- sapply(port_state$satellite_positions, function(p) p$symbol)
  for (sym in target_sat_syms) {
    px <- cur_prices[[sym]]
    if (is.null(px) || px <= 0) next
    
    existing_idx <- which(existing_sat_syms == sym)
    if (length(existing_idx) == 0) {
      shs_to_buy <- round(target_slot_cash / px, 3)
      outlay <- round(shs_to_buy * px, 2)
      ticket_desc <- if (USE_CRISIS_MVP && is_crisis_regime) sprintf("Top-%d Minimum Variance Defensive Entrant", TOP_N) else sprintf("Top-%d RS Satellite Entrant", TOP_N)
      ticket <- sprintf("BUY %s shares of %s @ ~$%.2f ($%.2f outlay - %s)",
                        shs_to_buy, sym, px, outlay, ticket_desc)
      rebalance_tickets <- c(rebalance_tickets, ticket)
      order_records[[length(order_records) + 1]] <- list(
        symbol = sym, side = "BUY", shares = round(shs_to_buy, 3),
        shares_whole = as.integer(floor(shs_to_buy)), estimated_price = round(px, 2),
        estimated_outlay = round(outlay, 2), order_type = "MARKET",
        reason = if (USE_CRISIS_MVP && is_crisis_regime) sprintf("TOP_%d_MIN_VARIANCE_DEFENSIVE_ENTRANT", TOP_N) else sprintf("TOP_%d_RS_SATELLITE_ENTRANT", TOP_N),
        stop_loss_price = round(px * (1.0 - DISASTER_STOP), 2)
      )
      
      port_state$satellite_positions[[length(port_state$satellite_positions) + 1]] <- list(
        symbol = sym,
        shares = shs_to_buy,
        entry_price = px,
        current_price = px,
        entry_date = today_str,
        market_value = outlay,
        unrealized_pnl = 0.0
      )
      port_state$cash_balance <- port_state$cash_balance - outlay
    } else {
      existing_pos <- port_state$satellite_positions[[existing_idx]]
      curr_shs <- as.numeric(existing_pos$shares)
      curr_mv  <- curr_shs * px
      drift_dlr <- target_slot_cash - curr_mv
      if (abs(drift_dlr) >= (target_slot_cash * 0.15)) {
        delta_shs <- round(drift_dlr / px, 3)
        if (delta_shs > 0) {
          ticket <- sprintf("BUY %s shares of %s @ ~$%.2f ($%.2f outlay - Top up Satellite Slot to %.1f%%)",
                            delta_shs, sym, px, round(delta_shs * px, 2), (target_slot_cash / total_equity) * 100)
          rebalance_tickets <- c(rebalance_tickets, ticket)
          order_records[[length(order_records) + 1]] <- list(
            symbol = sym, side = "BUY", shares = round(delta_shs, 3),
            shares_whole = as.integer(floor(delta_shs)), estimated_price = round(px, 2),
            estimated_outlay = round(delta_shs * px, 2), order_type = "MARKET",
            reason = "TOP_UP_SATELLITE_SLOT",
            stop_loss_price = round(px * (1.0 - DISASTER_STOP), 2)
          )
          port_state$satellite_positions[[existing_idx]]$shares <- curr_shs + delta_shs
          port_state$cash_balance <- port_state$cash_balance - round(delta_shs * px, 2)
        } else {
          ticket <- sprintf("SELL %s shares of %s @ ~$%.2f ($%.2f proceeds - Trim Satellite Slot to %.1f%%)",
                            abs(delta_shs), sym, px, abs(round(delta_shs * px, 2)), (target_slot_cash / total_equity) * 100)
          rebalance_tickets <- c(rebalance_tickets, ticket)
          order_records[[length(order_records) + 1]] <- list(
            symbol = sym, side = "SELL", shares = round(abs(delta_shs), 3),
            shares_whole = as.integer(round(abs(delta_shs))), estimated_price = round(px, 2),
            estimated_outlay = abs(round(delta_shs * px, 2)), order_type = "MARKET",
            reason = "TRIM_SATELLITE_SLOT", stop_loss_price = NULL
          )
          port_state$satellite_positions[[existing_idx]]$shares <- max(0, curr_shs - abs(delta_shs))
          port_state$cash_balance <- port_state$cash_balance + abs(round(delta_shs * px, 2))
        }
      }
    }
  }
  
  # D. Defensive Reserve Adjustment (Liquidate SGOV if switching to GLD)
  curr_def_sym <- if (!is.null(port_state$defensive_position$symbol)) port_state$defensive_position$symbol else ""
  curr_def_shs <- if (!is.null(port_state$defensive_position$shares)) as.numeric(port_state$defensive_position$shares) else 0
  
  if (curr_def_sym != DEFENSIVE_ASSET && curr_def_shs > 0) {
    old_px <- if (!is.null(cur_prices[[curr_def_sym]])) cur_prices[[curr_def_sym]] else as.numeric(port_state$defensive_position$entry_price)
    old_mv <- round(curr_def_shs * old_px, 2)
    ticket <- sprintf("SELL %s shares of %s @ ~$%.2f ($%.2f proceeds - Switch Defense to %s)",
                      curr_def_shs, curr_def_sym, old_px, old_mv, DEFENSIVE_ASSET)
    rebalance_tickets <- c(rebalance_tickets, ticket)
    order_records[[length(order_records) + 1]] <- list(
      symbol = curr_def_sym, side = "SELL", shares = round(curr_def_shs, 3),
      shares_whole = as.integer(round(curr_def_shs)), estimated_price = round(old_px, 2),
      estimated_outlay = round(old_mv, 2), order_type = "MARKET",
      reason = sprintf("SWITCH_DEFENSE_TO_%s", DEFENSIVE_ASSET), stop_loss_price = NULL
    )
    port_state$cash_balance <- port_state$cash_balance + old_mv
    port_state$defensive_position <- NULL
    curr_def_shs <- 0
  }
  
  target_def_shs  <- round(target_def_cash / def_cur_px, 3)
  def_delta_shs   <- target_def_shs - curr_def_shs
  def_delta_cash  <- round(def_delta_shs * def_cur_px, 2)
  
  if (abs(def_delta_cash) >= 30) {
    if (def_delta_cash > 0) {
      ticket <- sprintf("BUY %s shares of %s @ ~$%.2f ($%.2f outlay - Park Vol-Target Buffer in Gold)",
                        def_delta_shs, DEFENSIVE_ASSET, def_cur_px, def_delta_cash)
      rebalance_tickets <- c(rebalance_tickets, ticket)
      order_records[[length(order_records) + 1]] <- list(
        symbol = DEFENSIVE_ASSET, side = "BUY", shares = round(def_delta_shs, 3),
        shares_whole = as.integer(floor(def_delta_shs)), estimated_price = round(def_cur_px, 2),
        estimated_outlay = round(def_delta_cash, 2), order_type = "MARKET",
        reason = "PARK_VOL_TARGET_BUFFER", stop_loss_price = NULL
      )
      port_state$defensive_position <- list(
        symbol = DEFENSIVE_ASSET,
        shares = curr_def_shs + def_delta_shs,
        entry_price = def_cur_px,
        entry_date = today_str
      )
      port_state$cash_balance <- port_state$cash_balance - def_delta_cash
    } else {
      trim_def <- abs(def_delta_shs)
      ticket <- sprintf("SELL %s shares of %s @ ~$%.2f ($%.2f proceeds - Trim Defense for Equity)",
                        trim_def, DEFENSIVE_ASSET, def_cur_px, abs(def_delta_cash))
      rebalance_tickets <- c(rebalance_tickets, ticket)
      order_records[[length(order_records) + 1]] <- list(
        symbol = DEFENSIVE_ASSET, side = "SELL", shares = round(trim_def, 3),
        shares_whole = as.integer(round(trim_def)), estimated_price = round(def_cur_px, 2),
        estimated_outlay = round(abs(def_delta_cash), 2), order_type = "MARKET",
        reason = "TRIM_DEFENSE_FOR_EQUITY", stop_loss_price = NULL
      )
      port_state$defensive_position$shares <- max(0, curr_def_shs - trim_def)
      port_state$cash_balance <- port_state$cash_balance + abs(def_delta_cash)
    }
  }
  
  port_state$core_weight          <- CORE_WEIGHT
  port_state$satellite_weight     <- SATELLITE_WEIGHT
  port_state$core_asset           <- CORE_ASSET
  port_state$portfolio_margin_lev <- PORTFOLIO_MARGIN_LEV
  port_state$vol_target           <- VOL_TARGET
  port_state$defensive_asset      <- DEFENSIVE_ASSET
  port_state$realized_vol_20d     <- round(realized_vol, 4)
  port_state$vol_scale            <- round(vol_scale, 4)
  port_state$last_rebalance_date  <- today_str
  save_portfolio(port_state, PORTFOLIO_FILE)
  cat(" Rebalance calculation complete and state updated.\n\n")
} else {
  cat(sprintf(" ℹ️  Holdings monitored. Next scheduled rebalance on first trading day of %s.\n\n",
              format(seq(today, by = "month", length.out = 2)[2], "%B %Y")))
}

# ------------------------------------------------------------------------------
# 8. Output Actionable Order Tickets & Structured JSON Orders
# ------------------------------------------------------------------------------
all_tickets <- c(disaster_tickets, rebalance_tickets)

cat("========================================================================================\n")
cat("                                ACTIONABLE ORDER TICKETS                                \n")
cat("========================================================================================\n")
if (length(all_tickets) > 0) {
  for (i in seq_along(all_tickets)) {
    cat(sprintf(" [%d] %s\n", i, all_tickets[i]))
  }
} else {
  cat(" No trading actions required today. Portfolio is in balance and all risk metrics healthy.\n")
}
cat("========================================================================================\n\n")

if (!is.null(ORDERS_OUT_FILE) && nzchar(ORDERS_OUT_FILE)) {
  dir.create(dirname(ORDERS_OUT_FILE), showWarnings = FALSE, recursive = TRUE)
  write(toJSON(order_records, pretty = TRUE, auto_unbox = TRUE), file = ORDERS_OUT_FILE)
  cat(sprintf(" 📋 Structured machine-readable orders written to: %s (%d order%s)\n\n",
              ORDERS_OUT_FILE, length(order_records), if (length(order_records) == 1) "" else "s"))
}

save_portfolio(port_state, PORTFOLIO_FILE)
invisible(port_state)
