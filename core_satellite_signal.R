#!/usr/bin/env Rscript
#' ==============================================================================
#' Triple-Engine Momentum & Relative Strength Execution System (30/70 SGOV/GLD-VT)
#'
#' Architecture (Solution 3: The Combined Upgrade):
#'   * Engine 1 - Core Trend & Bull Leverage (30% Baseline):
#'       - Bull Market (QQQ >= 200 SMA & Realized Vol <= 20%): 1.25x QQQ leverage
#'       - Bear Market (QQQ < 200 SMA): 100% Core capital parked in Defensive Reserve (0x QQQ)
#'   * Engine 2 - Satellite Momentum Sleeve (70% Baseline / 5 slots @ 14%):
#'       - Top-5 Relative Strength stocks (126-day excess return over QQQ)
#'       - Eligibility: Stock price >= 200-day SMA (no falling knives)
#'       - Unconstrained upside: No artificial profit caps (+2R/+4R removed)
#'       - Risk Floor: -20% disaster stop-loss floor on individual positions
#'   * Engine 3 - Volatility Targeting to Gold (22% Target Vol -> GLD):
#'       - Rolling 20-day realized volatility of QQQ (sigma_20d)
#'       - Gross exposure scaled by min(1.0, 22% / sigma_20d)
#'       - Unallocated capital parked in Gold (GLD) for real compounding & crisis flight-to-safety
#'
#' Usage:
#'   Rscript core_satellite_signal.R [--capital=8000] [--core_weight=0.30] [--top_n=5]
#'                                   [--vol_target=0.22] [--defensive_asset=GLD]
#'                                   [--core_bull_leverage=1.25] [--use_core_trend=TRUE]
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
# 1. Parse Command Line Arguments
# ------------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)

CAPITAL            <- 8000    # Default capital base: $8,000
CORE_WEIGHT        <- 0.30    # 30% Core Sleeve
TOP_N              <- 5       # 5 Satellite slots (14% each)
LOOKBACK_DAYS      <- 126     # ~6 months for relative strength
DISASTER_STOP      <- 0.20    # 20% catastrophic max loss floor
VOL_TARGET         <- 0.22    # 22% annualized realized volatility target
DEFENSIVE_ASSET    <- "GLD"   # Gold (GLD) as crisis hedge
CORE_BULL_LEVERAGE <- 1.25    # 1.25x bull leverage when QQQ > 200 SMA and vol <= 22%
USE_CORE_TREND     <- TRUE    # Park Core in Defensive Asset when QQQ < 200 SMA
FORCE_REBALANCE    <- FALSE
RESET              <- FALSE
SYMBOLS_FILE       <- "symbols.txt"
PORTFOLIO_FILE     <- "portfolio_coresat.json"

for (arg in args) {
  if (grepl("^--capital=", arg)) {
    CAPITAL <- as.numeric(sub("^--capital=", "", arg))
  } else if (grepl("^--core_weight=", arg)) {
    CORE_WEIGHT <- as.numeric(sub("^--core_weight=", "", arg))
  } else if (grepl("^--top_n=", arg)) {
    TOP_N <- as.integer(sub("^--top_n=", "", arg))
  } else if (grepl("^--lookback=", arg)) {
    LOOKBACK_DAYS <- as.integer(sub("^--lookback=", "", arg))
  } else if (grepl("^--disaster_stop=", arg)) {
    DISASTER_STOP <- as.numeric(sub("^--disaster_stop=", "", arg))
  } else if (grepl("^--vol_target=", arg)) {
    VOL_TARGET <- as.numeric(sub("^--vol_target=", "", arg))
  } else if (grepl("^--defensive_asset=", arg)) {
    DEFENSIVE_ASSET <- toupper(sub("^--defensive_asset=", "", arg))
  } else if (grepl("^--core_bull_leverage=", arg)) {
    CORE_BULL_LEVERAGE <- as.numeric(sub("^--core_bull_leverage=", "", arg))
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
  }
}

SATELLITE_WEIGHT <- max(0, 1.0 - CORE_WEIGHT)
SLOT_WEIGHT      <- if (TOP_N > 0) SATELLITE_WEIGHT / TOP_N else 0.0

# ------------------------------------------------------------------------------
# 2. Portfolio State Management
# ------------------------------------------------------------------------------
init_portfolio <- function(capital = CAPITAL) {
  list(
    total_capital       = capital,
    cash_balance        = capital,
    core_weight         = CORE_WEIGHT,
    satellite_weight    = SATELLITE_WEIGHT,
    vol_target          = VOL_TARGET,
    defensive_asset     = DEFENSIVE_ASSET,
    core_bull_leverage  = CORE_BULL_LEVERAGE,
    use_core_trend      = USE_CORE_TREND,
    last_rebalance_date = "",
    core_position       = NULL,
    defensive_position  = NULL,
    satellite_positions = list(),
    closed_trades       = list()
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
    # Migrate legacy sgov_position if present
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

# ------------------------------------------------------------------------------
# 3. Load Market Data & Benchmark (QQQ & Defensive Asset)
# ------------------------------------------------------------------------------
qqq_ohlcv <- tryCatch(load_stock_data("QQQ", force_refresh = FALSE), error = function(e) NULL)
if (is.null(qqq_ohlcv) || nrow(qqq_ohlcv) < max(LOOKBACK_DAYS, 200)) {
  stop("FATAL: Could not load sufficient historical data for benchmark QQQ.")
}
qqq_close  <- as.numeric(Cl(qqq_ohlcv))
n_qqq      <- length(qqq_close)
qqq_cur_px <- tail(qqq_close, 1)
qqq_lb_px  <- qqq_close[n_qqq - LOOKBACK_DAYS]
qqq_ret    <- (qqq_cur_px / qqq_lb_px) - 1

# QQQ Trend & Realized Volatility Diagnostics
qqq_sma200 <- mean(tail(qqq_close, 200))
qqq_trend_up <- qqq_cur_px >= qqq_sma200

# Rolling 20-day realized volatility of log returns
ret_20d      <- diff(log(tail(qqq_close, 21)))
realized_vol <- sd(ret_20d) * sqrt(252)
vol_scale    <- min(1.0, VOL_TARGET / max(0.05, realized_vol))

# Defensive Asset Market Data (GLD or SGOV)
def_ohlcv <- tryCatch(load_stock_data(DEFENSIVE_ASSET, force_refresh = FALSE), error = function(e) NULL)
def_cur_px <- if (!is.null(def_ohlcv)) tail(as.numeric(Cl(def_ohlcv)), 1) else if (DEFENSIVE_ASSET == "GLD") 355.78 else 100.73

cat("\n========================================================================================\n")
cat(sprintf("     TRIPLE-ENGINE MOMENTUM SYSTEM (30/70 + %.0f%% VT -> %s)       \n", VOL_TARGET * 100, DEFENSIVE_ASSET))
cat("========================================================================================\n")
cat(sprintf(" Date:                   %s\n", Sys.Date()))
cat(sprintf(" Capital Base:           $%.2f\n", port_state$total_capital))
cat(sprintf(" Target Weights:         Core (QQQ): %.1f%% | Satellite (Top %d): %.1f%% (%.1f%%/slot)\n",
            CORE_WEIGHT * 100, TOP_N, SATELLITE_WEIGHT * 100, SLOT_WEIGHT * 100))
cat(sprintf(" Volatility Targeting:   Target: %.1f%% | Realized (20d): %.1f%% | Vol Scale Factor: %.2fx\n",
            VOL_TARGET * 100, realized_vol * 100, vol_scale))
cat(sprintf(" Active Exposure:        Equities: %.1f%% ($%.2f) | Defensive %s: %.1f%% ($%.2f)\n",
            (CORE_WEIGHT + SATELLITE_WEIGHT) * vol_scale * 100,
            port_state$total_capital * (CORE_WEIGHT + SATELLITE_WEIGHT) * vol_scale,
            DEFENSIVE_ASSET, (1.0 - (CORE_WEIGHT + SATELLITE_WEIGHT) * vol_scale) * 100,
            port_state$total_capital * (1.0 - (CORE_WEIGHT + SATELLITE_WEIGHT) * vol_scale)))
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

should_rebalance <- FORCE_REBALANCE || is_new_month

# ------------------------------------------------------------------------------
# 5. Universe Relative Strength Scan
# ------------------------------------------------------------------------------
EXCLUDE_ETFS <- c("QQQ", "QLD", "TQQQ", "SQQQ", "SPY", "VTI", "SOXL", "SOXS", "FNGD", 
                  "FNGU", "SGOV", "BIL", "GLD", "SMH", "XLK", "^VIX", "^IRX")

sym_lines <- if (file.exists(SYMBOLS_FILE)) readLines(SYMBOLS_FILE) else character()
raw_syms <- unique(trimws(unlist(strsplit(sym_lines[!grepl("^#", sym_lines) & nzchar(sym_lines)], "[, ]+"))))
scan_syms <- setdiff(raw_syms, EXCLUDE_ETFS)

cat(sprintf(" Scanning %d candidate stocks for %d-day Relative Strength vs QQQ...\n", length(scan_syms), LOOKBACK_DAYS))

rs_results <- list()
for (sym in scan_syms) {
  d <- tryCatch(load_stock_data(sym, force_refresh = FALSE), error = function(e) NULL)
  if (is.null(d) || nrow(d) < LOOKBACK_DAYS + 10) next
  
  cl <- as.numeric(Cl(d))
  n <- length(cl)
  cur_px <- cl[n]
  lb_px  <- cl[n - LOOKBACK_DAYS]
  
  # Trend check: 200 SMA
  sma200 <- if (n >= 200) mean(cl[(n - 199):n]) else mean(cl)
  trend_up <- cur_px >= sma200
  
  ret_stock <- (cur_px / lb_px) - 1
  rs_vs_qqq <- ret_stock - qqq_ret
  
  rs_results[[sym]] <- data.frame(
    Symbol       = sym,
    Close        = cur_px,
    SMA200       = sma200,
    Trend_Up     = trend_up,
    Ret_126D     = ret_stock,
    RS_vs_QQQ    = rs_vs_qqq,
    stringsAsFactors = FALSE
  )
}

if (length(rs_results) == 0) {
  stop("FATAL: No candidate stocks had sufficient data for ranking.")
}

rs_df <- do.call(rbind, rs_results)
rs_qualified <- rs_df[rs_df$Trend_Up, ]
if (nrow(rs_qualified) < TOP_N) rs_qualified <- rs_df

rs_qualified <- rs_qualified[order(-rs_qualified$RS_vs_QQQ), ]
top_picks <- head(rs_qualified, TOP_N)

cat("\n Top Candidate Relative Strength Leaders (>= 200-day SMA):\n")
cat("----------------------------------------------------------------------------------------\n")
print(head(rs_qualified[, c("Symbol", "Close", "Trend_Up", "Ret_126D", "RS_vs_QQQ")], 8), row.names = FALSE)
cat("----------------------------------------------------------------------------------------\n\n")

# ------------------------------------------------------------------------------
# 6. Mark Current Portfolio Value & Process Disaster Stops
# ------------------------------------------------------------------------------
cur_prices <- list(QQQ = qqq_cur_px, GLD = def_cur_px, SGOV = 100.73)
cur_prices[[DEFENSIVE_ASSET]] <- def_cur_px

for (row_idx in seq_len(nrow(rs_df))) {
  cur_prices[[rs_df$Symbol[row_idx]]] <- rs_df$Close[row_idx]
}

# Value Core Position
core_val <- 0
core_pos <- port_state$core_position
if (!is.null(core_pos) && as.numeric(core_pos$shares) > 0) {
  core_px <- if (!is.null(cur_prices[["QQQ"]])) cur_prices[["QQQ"]] else as.numeric(core_pos$entry_price)
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

total_equity <- round(port_state$cash_balance + core_val + def_val + sat_val, 2)
port_state$total_capital <- total_equity

cat("========================================================================================\n")
cat("                               PORTFOLIO BALANCE SHEET                                  \n")
cat("========================================================================================\n")
cat(sprintf(" Total Portfolio Equity:  $%.2f\n", total_equity))
cat(sprintf(" Core QQQ Sleeve:         $%.2f (%5.1f%% of equity | Target: %4.1f%%)\n",
            core_val, (core_val / max(1, total_equity)) * 100, CORE_WEIGHT * 100))
cat(sprintf(" Satellite Sleeve:        $%.2f (%5.1f%% of equity | Target: %4.1f%%)\n",
            sat_val, (sat_val / max(1, total_equity)) * 100, SATELLITE_WEIGHT * 100))
cat(sprintf(" Defensive %s Sleeve:    $%.2f (%5.1f%% of equity)\n",
            if (!is.null(port_state$defensive_position$symbol)) port_state$defensive_position$symbol else DEFENSIVE_ASSET,
            def_val, (def_val / max(1, total_equity)) * 100))
cat(sprintf(" Unallocated Cash:        $%.2f (%5.1f%% of equity)\n",
            port_state$cash_balance, (port_state$cash_balance / max(1, total_equity)) * 100))
cat("========================================================================================\n\n")

# ------------------------------------------------------------------------------
# 7. Rebalancing Engine (Monthly Scheduled or Forced)
# ------------------------------------------------------------------------------
rebalance_tickets <- character()

if (should_rebalance) {
  cat("----------------------------------------------------------------------------------------\n")
  cat(sprintf(" 🔄 REBALANCE ACTIVE (Trigger: %s)\n",
              if (FORCE_REBALANCE) "FORCED CLI / SOLUTION 3 UPGRADE" else "NEW MONTH ENTRY"))
  cat("----------------------------------------------------------------------------------------\n")
  
  # Engine 1: Core Trend & Bull Leverage Sizing
  core_active <- TRUE
  if (USE_CORE_TREND && !qqq_trend_up) {
    core_active <- FALSE
    cat(sprintf(" 🛡️ BEAR MARKET REGIME: QQQ < 200 SMA. Core Sleeve exiting to 100%% %s / Cash.\n", DEFENSIVE_ASSET))
  }
  
  core_lev <- 1.0
  if (core_active && qqq_trend_up && realized_vol <= VOL_TARGET) {
    core_lev <- CORE_BULL_LEVERAGE
    cat(sprintf(" 🚀 BULL ENGINE ACTIVE: QQQ >= 200 SMA & Vol <= %.1f%%. Core Leverage = %.2fx.\n",
                VOL_TARGET * 100, core_lev))
  }
  
  target_core_cash <- if (core_active) total_equity * CORE_WEIGHT * vol_scale * core_lev else 0.0
  target_sat_cash  <- total_equity * SATELLITE_WEIGHT * vol_scale
  target_slot_cash <- target_sat_cash / TOP_N
  target_sat_syms  <- top_picks$Symbol
  
  # Defensive Target Allocation
  target_def_cash <- max(0, total_equity - (target_core_cash + (target_slot_cash * length(target_sat_syms))))
  
  cat(sprintf(" Target Allocations: Core QQQ: $%.2f (%.1f%%) | Satellites (5): $%.2f (%.1f%%) | Defensive %s: $%.2f (%.1f%%)\n\n",
              target_core_cash, (target_core_cash / total_equity) * 100,
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
  
  # B. Core QQQ Adjustment
  current_core_shs <- if (!is.null(port_state$core_position)) as.numeric(port_state$core_position$shares) else 0
  target_core_shs  <- round(target_core_cash / qqq_cur_px, 3)
  core_delta_shs   <- target_core_shs - current_core_shs
  core_delta_cash  <- round(core_delta_shs * qqq_cur_px, 2)
  
  if (abs(core_delta_cash) >= 30) {
    if (core_delta_cash > 0) {
      buy_shs <- round(core_delta_cash / qqq_cur_px, 3)
      ticket <- sprintf("BUY %s shares of QQQ @ ~$%.2f ($%.2f outlay - Align Core Target to %.1f%%)",
                        buy_shs, qqq_cur_px, round(buy_shs * qqq_cur_px, 2), (target_core_cash / total_equity) * 100)
      rebalance_tickets <- c(rebalance_tickets, ticket)
      port_state$core_position <- list(
        symbol = "QQQ",
        shares = current_core_shs + buy_shs,
        entry_price = qqq_cur_px,
        entry_date = today_str
      )
      port_state$cash_balance <- port_state$cash_balance - round(buy_shs * qqq_cur_px, 2)
    } else {
      trim_shs <- abs(core_delta_shs)
      ticket <- sprintf("SELL %s shares of QQQ @ ~$%.2f ($%.2f proceeds - Trim Core Target to %.1f%%)",
                        trim_shs, qqq_cur_px, abs(core_delta_cash), (target_core_cash / total_equity) * 100)
      rebalance_tickets <- c(rebalance_tickets, ticket)
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
      ticket <- sprintf("BUY %s shares of %s @ ~$%.2f ($%.2f outlay - Top-%d RS Satellite Entrant)",
                        shs_to_buy, sym, px, outlay, TOP_N)
      rebalance_tickets <- c(rebalance_tickets, ticket)
      
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
          port_state$satellite_positions[[existing_idx]]$shares <- curr_shs + delta_shs
          port_state$cash_balance <- port_state$cash_balance - round(delta_shs * px, 2)
        } else {
          ticket <- sprintf("SELL %s shares of %s @ ~$%.2f ($%.2f proceeds - Trim Satellite Slot to %.1f%%)",
                            abs(delta_shs), sym, px, abs(round(delta_shs * px, 2)), (target_slot_cash / total_equity) * 100)
          rebalance_tickets <- c(rebalance_tickets, ticket)
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
      port_state$defensive_position$shares <- max(0, curr_def_shs - trim_def)
      port_state$cash_balance <- port_state$cash_balance + abs(def_delta_cash)
    }
  }
  
  port_state$core_weight         <- CORE_WEIGHT
  port_state$satellite_weight    <- SATELLITE_WEIGHT
  port_state$vol_target          <- VOL_TARGET
  port_state$defensive_asset     <- DEFENSIVE_ASSET
  port_state$realized_vol_20d    <- round(realized_vol, 4)
  port_state$vol_scale           <- round(vol_scale, 4)
  port_state$last_rebalance_date <- today_str
  save_portfolio(port_state, PORTFOLIO_FILE)
  cat(" Rebalance calculation complete and state updated.\n\n")
} else {
  cat(sprintf(" ℹ️  Holdings monitored. Next scheduled rebalance on first trading day of %s.\n\n",
              format(seq(today, by = "month", length.out = 2)[2], "%B %Y")))
}

# ------------------------------------------------------------------------------
# 8. Output Actionable Order Tickets
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

save_portfolio(port_state, PORTFOLIO_FILE)
invisible(port_state)
