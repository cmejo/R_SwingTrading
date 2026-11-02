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
#'                                   [--top_n=5] [--max_per_industry=5]
#'                                   [--symbols_file=symbols_n100.txt]
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
source("R/02_trend_lmMA.R")
source("R/10_option_d_features.R")   # shared with R/11_option_d_engine.R (backtest)

# ------------------------------------------------------------------------------
# 1. Parse Command Line Arguments & Configuration Presets
# ------------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)

# Default capital base: $8,000
CAPITAL               <- 8000
EXPLICIT_CAPITAL      <- FALSE
CORE_WEIGHT           <- 0.00    # 100% Stock Model: 0% Core ETF (QQQ / QLD removed)
TOP_N                 <- 5       # 5 High-Conviction Stock slots (30% each at 1.5x margin)
LOOKBACK_LONG         <- 126     # ~6 months intermediate trend anchor
LOOKBACK_SHORT        <- 63      # ~3 months quarterly momentum acceleration
WEIGHT_LONG           <- 0.50    # 50% weight on 126d RS
WEIGHT_SHORT          <- 0.50    # 50% weight on 63d RS
LOOKBACK_DAYS         <- LOOKBACK_LONG # Compatibility alias
DISASTER_STOP         <- 0.25    # 25% catastrophic max loss floor (Option D: room for winners to compound)
STOP_PROCEEDS_TO_CORE <- FALSE   # (A) No Core ETF; stopped-out cash stays in cash to reduce margin debt
USE_BREADTH_GATE      <- FALSE   # (B) OPTIONAL, OFF BY DEFAULT: drop margin to 1.0x when fewer than
BREADTH_THRESHOLD     <- 0.50    #     BREADTH_THRESHOLD of scanned stocks are above their 200 SMA.
VOL_CAP               <- 1.00    # (C) Speculative-spike filter: skip stocks whose 63-day realized volatility
                                 #     exceeds VOL_CAP (1.00 = 100% annualized, ~6.3%/day). Set NA to disable.
USE_CRISIS_MVP        <- TRUE    # (D) Crisis Minimum Variance Portfolio: When in crisis/bear territory
                                 #     (trend predicted down and high volatility regime), stay invested
                                 #     in equities but switch selection rules from Momentum to Min-Var.
TREND_ENGINE          <- "lmMA_slope" # (E) Macro Trend Engine: "lmMA_slope" (low-lag regression conviction),
                                      #     alt: "sma200" (classic 200 SMA), "lmMA_fit" (price vs lmMA level)
LMMA_WINDOW           <- 50           # Rolling window for linear regression trend velocity (default: 50)
LMMA_RSQ_THRESHOLD    <- 0.15         # Statistical conviction threshold (R^2 > 0.15 confirms directional trend)
USE_EARNINGS_SHIELD   <- FALSE        # (F) Earnings Blackout Filter (DISABLED BY DEFAULT: Backtests prove holding through earnings delivers +58.7% CAGR vs +57.5% with blackout)
EARNINGS_BLACKOUT_DAYS<- 5            # Pre-earnings blackout window in trading days (default: 5 days)
EARNINGS_FILE         <- "data/historical_earnings_dates.json" # Historical & forward earnings dates database
USE_STOCK_TREND_GATE  <- FALSE        # (G) Candidate Stock Trend Gate (DISABLED BY DEFAULT: 10-Yr & 20-Yr backtests
                                      #     prove removing the 100-day lag of stock-level 200 SMA increases CAGR
                                      #     from 100.6% -> 102.8% (10yr) and 59.5% -> 60.6% (20yr, +$11.7M terminal wealth)).
                                      #     Macro regime is protected by QQQ lmMA(50); stocks rank purely on RS.
USE_LEVERAGE_TURBO    <- TRUE         # (H) Super-Bull Leverage Turbo: 1.60x Regulation T Turbo margin when QQQ > 50 SMA > 200 SMA & lmMA slope > 0
TURBO_MARGIN_LEV      <- 1.60         #     Turbo leverage level during confirmed multi-month institutional expansion (Choice 4: 1.60x Regulation T Compliant Turbo)
USE_STOP_REPLACEMENT  <- TRUE         # (I) Immediate Stop Replacement: Deploy disaster stop proceeds into Rank 6 immediately
USE_RETENTION_BUFFER  <- TRUE         # (J) Winner Retention Buffer: Retain existing holdings as long as they stay in Top 8
RETENTION_BUFFER_RANK <- 8            #     Hysteresis boundary: only rotate out if stock drops below Rank 8 or hits stop
USE_CONVEX_SIZING     <- FALSE        # (K) Convex Rank Sizing (30%, 25%, 20%, 15%, 10% on Ranks 1 to 5).
                                      #     Default: FALSE. Verified across 3, 10, and 20-Yr backtests: Equal 20% weighting
                                      #     delivers higher CAGR, higher wealth, and lower MaxDD due to lower single-stock volatility drag at Reg T margin.
USE_FIP               <- TRUE         # (FIP) Frog-in-the-Pan (FIP) Continuous Momentum (Da, Gurun, Warachka 2014)
                                      #       Default: TRUE. Rewards consistent, continuous positive return drift over trailing 63 days (70/30 blend).
                                      #       Expands 10-Yr CAGR by +12.4% (+196.1% vs +183.7%) and reduces annualized volatility.
# ---- Research features (ALL OFF BY DEFAULT; validate on the PIT universe with
# ---- scratch/backtest_option_d_pit.R before enabling in live trading) --------
USE_RISK_TARGET_LEV   <- FALSE        # (L) Feature 1: replace fixed 1.5x / 1.60x turbo with ex-ante portfolio-vol
SIGMA_TARGET          <- 0.30         #     target on the SELECTED book (EWMA cov) + fractional Kelly cap that uses
LEV_MAX               <- 2.00         #     the broker's actual margin rate. If L < 1, the undeployed (1 - L) goes to the
                                      #     defensive asset exactly as the 1 - vol_scale buffer does today.
MU_HAT                <- NA_real_     #     Expected arithmetic annual return of the UNLEVERED book. NA = no Kelly cap,
                                      #     in which case borrowing is capped at PORTFOLIO_MARGIN_LEV (1.5x).
BORROW_RATE           <- 0.12075      #     Schwab effective rate, debit < $25k, as of 2026-09-18. IBKR Pro ~0.054.
KELLY_FRAC            <- 0.50         #     Half-Kelly: mu_hat is estimated with large error.
USE_RESID_MOM         <- FALSE        # (M) Feature 2: rank on beta-adjusted residual momentum IR (Blitz, Huij &
RESID_EST_WINDOW      <- 504          #     Martens 2011) instead of raw return minus QQQ return. Beta uses up to 504d;
                                      #     names with >= 252d of history are scored, younger names rank last.
SKIP_DAYS             <- 0            # (N) Feature 3: exclude the most recent N trading days from formation
                                      #     (short-term reversal, Jegadeesh 1990). Applies to both RS variants.
USE_TAX_AWARE_ROTATION <- FALSE       # (O) Tax-Aware Rotation: 45d LTCG bridge, wash sale avoidance & HIFO lot ID
LTCG_BRIDGE_DAYS       <- 45          #     Hold winner past 365d if within 45d of 1-yr mark and rank <= LTCG_RANK_MAX
LTCG_RANK_MAX          <- 12          #     Extended retention rank for winners near LTCG qualification (saves ~15-17pp NYC tax)
AVOID_WASH_SALES       <- TRUE        #     Prevent wash sales: 30-day lockout on stocks exited at a loss with correlated substitute
TAX_LOT_METHOD         <- "HIFO"      #     Specific-lot identification: Highest-In, First-Out
MAX_PER_INDUSTRY       <- 5           # (P) Industry concentration cap: max stocks allowed per industry
                                      #     Default: 5 (unconstrained, backward compatible).
                                      #     Pass --max_per_industry=2 to activate the 2-stock industry cap (+17.5% 3Y CAGR alpha).
BEAR_SLEEVE            <- "crisis_mvp" # (Q) Crisis/Bear Regime Sleeve: "crisis_mvp" (default: 5 lowest-volatility equities),
                                       #     "gld_sgov" (Tactical Safe Haven: 100% GLD if uptrend, else 100% SGOV / Cash, +13.7% 10Y CAGR alpha),
                                       #     "cash" (100% SGOV / Cash). Pass --bear_sleeve=gld_sgov or --safe_haven_bear.
STOP_REALLOCATE        <- "rank6"      # (R) Stop-loss capital redeployment: "rank6" (default: buy Rank 6 replacement),
                                       #     "survivors" (reallocate stopped proceeds equally across surviving winners, +2.6% to +6.9% CAGR alpha).
                                       #     Pass --stop_reallocate=survivors.
SECTOR_MAP_FILE        <- "sector_map.json"
SECTOR_MAP             <- if (file.exists(SECTOR_MAP_FILE)) jsonlite::fromJSON(SECTOR_MAP_FILE) else list()
FORCE_REBALANCE        <- FALSE
RESET                 <- FALSE
SYMBOLS_FILE          <- "symbols_broad.txt" # Broad 170-name growth/momentum list (alt: symbols_n100.txt = PIT-tested universe)
PORTFOLIO_FILE        <- "portfolio_coresat.json"
BROKER_STATE_FILE     <- NULL    # Optional JSON file containing live broker equity, cash & positions
QUOTES_FILE           <- NULL    # Optional JSON file containing real-time quotes (e.g. from Schwab API)
ORDERS_OUT_FILE       <- NULL    # Optional JSON output destination for structured machine-readable orders

# ==============================================================================
# ACTIVE PRESET: 100% HIGH-CONVICTION STOCK MODEL (1.5x Margin, Top 5 Stocks)
# ==============================================================================
CORE_ASSET            <- "NONE"  # 100% Stock: No Core ETF (0% QQQ / QLD)
PORTFOLIO_MARGIN_LEV  <- 1.50    # 1.5x portfolio margin during bull markets
USE_MARGIN_TREND_GATE <- TRUE    # De-lever margin debt to 0% when QQQ < 200 SMA
USE_CORE_TREND        <- FALSE   # No core sleeve to rotate
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
    raw_cap <- sub("^--capital=", "", arg)
    if (tolower(raw_cap) %in% c("auto", "schwab", "broker", "all", "dynamic", "")) {
      EXPLICIT_CAPITAL <- FALSE
      cat(" 🌐 CAPITAL SIZING MODE: Automatic dynamic compounding (anchored to live broker equity).\n")
    } else {
      CAPITAL <- as.numeric(raw_cap)
      EXPLICIT_CAPITAL <- TRUE
    }
  } else if (arg == "--auto_capital") {
    EXPLICIT_CAPITAL <- FALSE
    cat(" 🌐 CAPITAL SIZING MODE: Automatic dynamic compounding (anchored to live broker equity).\n")
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
  } else if (grepl("^--trend_engine=", arg)) {
    TREND_ENGINE <- sub("^--trend_engine=", "", arg)
  } else if (grepl("^--lmma_window=", arg)) {
    LMMA_WINDOW <- as.integer(sub("^--lmma_window=", "", arg))
  } else if (grepl("^--lmma_rsq=", arg)) {
    LMMA_RSQ_THRESHOLD <- as.numeric(sub("^--lmma_rsq=", "", arg))
  } else if (grepl("^--earnings_shield=", arg)) {
    USE_EARNINGS_SHIELD <- as.logical(sub("^--earnings_shield=", "", arg))
  } else if (grepl("^--blackout_days=", arg)) {
    EARNINGS_BLACKOUT_DAYS <- as.integer(sub("^--blackout_days=", "", arg))
  } else if (grepl("^--earnings_file=", arg)) {
    EARNINGS_FILE <- sub("^--earnings_file=", "", arg)
  } else if (grepl("^--stock_trend_gate=", arg)) {
    USE_STOCK_TREND_GATE <- as.logical(sub("^--stock_trend_gate=", "", arg))
  } else if (grepl("^--leverage_turbo=", arg)) {
    USE_LEVERAGE_TURBO <- as.logical(sub("^--leverage_turbo=", "", arg))
  } else if (grepl("^--turbo_margin=", arg)) {
    TURBO_MARGIN_LEV <- as.numeric(sub("^--turbo_margin=", "", arg))
  } else if (grepl("^--stop_replacement=", arg)) {
    USE_STOP_REPLACEMENT <- as.logical(sub("^--stop_replacement=", "", arg))
  } else if (grepl("^--retention_buffer=", arg)) {
    USE_RETENTION_BUFFER <- as.logical(sub("^--retention_buffer=", "", arg))
  } else if (grepl("^--retention_rank=", arg)) {
    RETENTION_BUFFER_RANK <- as.integer(sub("^--retention_rank=", "", arg))
  } else if (grepl("^--convex_sizing=", arg)) {
    USE_CONVEX_SIZING <- as.logical(sub("^--convex_sizing=", "", arg))
  } else if (grepl("^--risk_target_lev=", arg)) {
    USE_RISK_TARGET_LEV <- as.logical(sub("^--risk_target_lev=", "", arg))
  } else if (grepl("^--sigma_target=", arg)) {
    SIGMA_TARGET <- as.numeric(sub("^--sigma_target=", "", arg))
  } else if (grepl("^--lev_max=", arg)) {
    LEV_MAX <- as.numeric(sub("^--lev_max=", "", arg))
  } else if (grepl("^--mu_hat=", arg)) {
    MU_HAT <- suppressWarnings(as.numeric(sub("^--mu_hat=", "", arg)))
  } else if (grepl("^--borrow_rate=", arg)) {
    BORROW_RATE <- as.numeric(sub("^--borrow_rate=", "", arg))
  } else if (grepl("^--kelly_frac=", arg)) {
    KELLY_FRAC <- as.numeric(sub("^--kelly_frac=", "", arg))
  } else if (grepl("^--resid_mom=", arg)) {
    USE_RESID_MOM <- as.logical(sub("^--resid_mom=", "", arg))
  } else if (grepl("^--resid_est_window=", arg)) {
    RESID_EST_WINDOW <- as.integer(sub("^--resid_est_window=", "", arg))
  } else if (grepl("^--skip_days=", arg)) {
    SKIP_DAYS <- as.integer(sub("^--skip_days=", "", arg))
  } else if (grepl("^--use_fip=", arg)) {
    USE_FIP <- as.logical(sub("^--use_fip=", "", arg))
  } else if (grepl("^--tax_aware_rotation=", arg)) {
    USE_TAX_AWARE_ROTATION <- as.logical(sub("^--tax_aware_rotation=", "", arg))
  } else if (grepl("^--tax_aware=", arg)) {
    USE_TAX_AWARE_ROTATION <- as.logical(sub("^--tax_aware=", "", arg))
  } else if (arg == "--tax_aware") {
    USE_TAX_AWARE_ROTATION <- TRUE
  } else if (grepl("^--ltcg_bridge_days=", arg)) {
    LTCG_BRIDGE_DAYS <- as.integer(sub("^--ltcg_bridge_days=", "", arg))
  } else if (grepl("^--ltcg_rank_max=", arg)) {
    LTCG_RANK_MAX <- as.integer(sub("^--ltcg_rank_max=", "", arg))
  } else if (grepl("^--avoid_wash_sales=", arg)) {
    AVOID_WASH_SALES <- as.logical(sub("^--avoid_wash_sales=", "", arg))
  } else if (grepl("^--tax_lot_method=", arg)) {
    TAX_LOT_METHOD <- toupper(sub("^--tax_lot_method=", "", arg))
  } else if (grepl("^--max_per_industry=", arg)) {
    MAX_PER_INDUSTRY <- as.integer(sub("^--max_per_industry=", "", arg))
  } else if (grepl("^--bear_sleeve=", arg)) {
    BEAR_SLEEVE <- tolower(sub("^--bear_sleeve=", "", arg))
  } else if (arg == "--safe_haven_bear" || grepl("^--safe_haven_bear=true", tolower(arg))) {
    BEAR_SLEEVE <- "gld_sgov"
  } else if (grepl("^--stop_reallocate=", arg)) {
    STOP_REALLOCATE <- tolower(sub("^--stop_reallocate=", "", arg))
  } else if (arg == "--stop_reallocate_survivors" || grepl("^--stop_reallocate_survivors=true", tolower(arg))) {
    STOP_REALLOCATE <- "survivors"
  }
}
if (USE_RESID_MOM && RESID_EST_WINDOW < LOOKBACK_LONG + SKIP_DAYS)
  stop("--resid_est_window must be >= lookback_long + skip_days")

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
broker_cap  <- NULL
broker_cash <- NULL

if (!is.null(BROKER_STATE_FILE) && file.exists(BROKER_STATE_FILE)) {
  broker_state <- tryCatch(fromJSON(BROKER_STATE_FILE, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.null(broker_state)) {
    is_sim <- isTRUE(broker_state$is_simulation)
    if (RESET && is_sim) {
      cat(sprintf(" 🔄 RESET active with simulated broker state: Re-initializing clean $%.2f cash balance and 0 positions.\n", CAPITAL))
      port_state$cash_balance  <- CAPITAL
      port_state$total_capital <- CAPITAL
      b_positions <- NULL
    } else {
      broker_cap  <- as.numeric(broker_state$total_capital %||% broker_state$liquidation_value %||% broker_state$equity %||% CAPITAL)
      broker_cash <- as.numeric(broker_state$cash_balance %||% broker_state$cashAvailableForTrading %||% CAPITAL)
      b_cap  <- broker_cap
      b_cash <- broker_cash
      if (!EXPLICIT_CAPITAL && !is.na(b_cap) && b_cap > 0) {
        CAPITAL <- b_cap
        port_state$total_capital <- b_cap
      } else if (EXPLICIT_CAPITAL) {
        port_state$total_capital <- CAPITAL
        cat(sprintf(" 🔒 EXPLICIT CAPITAL OVERRIDE: Constraining portfolio sizing to $%.2f (Broker equity: $%.2f)\n",
                    CAPITAL, if (!is.na(b_cap)) b_cap else CAPITAL))
      }
      if (!is.na(b_cash)) {
        port_state$cash_balance <- b_cash
      }
      b_positions <- broker_state$positions
    }
    
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
        
        if (b_sym == CORE_ASSET || b_sym %in% c("QLD", "QQQ", "TQQQ")) {
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
# Formation window ends SKIP_DAYS bars before today (SKIP_DAYS = 0 -> identical to prior behaviour)
qqq_end_px     <- qqq_close[n_qqq - SKIP_DAYS]
qqq_ret_long   <- (qqq_end_px / qqq_close[n_qqq - LOOKBACK_LONG]) - 1
qqq_ret_short  <- (qqq_end_px / qqq_close[n_qqq - LOOKBACK_SHORT]) - 1
qqq_ret        <- qqq_ret_long # Compatibility alias

# Linear Model Moving Average (lmMA) - Zero First-Order Phase Lag.
# MUST be computed before is_super_bull: previously qqq_lm_slope was referenced
# before assignment, which threw "object not found" whenever QQQ > SMA50 > SMA200
# (the && short-circuit hid the bug in every other regime) and aborted the run.
qqq_lm       <- lmMA(qqq_close, n = LMMA_WINDOW)
qqq_lm_fit   <- tail(as.numeric(qqq_lm$fit), 1)
qqq_lm_slope <- tail(as.numeric(qqq_lm$slope), 1)
qqq_lm_rsq   <- tail(as.numeric(qqq_lm$r.squared), 1)

# QQQ Trend & Realized Volatility Diagnostics
qqq_sma200   <- mean(tail(qqq_close, 200))
qqq_sma50    <- mean(tail(qqq_close, 50))
is_super_bull <- (qqq_cur_px > qqq_sma50 && qqq_sma50 > qqq_sma200 && qqq_lm_slope > 0)

# Trend Conviction Filter:
# Eliminates lag while avoiding whipsaws from naive price crossing.
# A crisis / bear regime is confirmed if BOTH slope is negative AND R^2 exceeds threshold.
is_confirmed_downtrend <- (qqq_lm_slope < 0 && qqq_lm_rsq > LMMA_RSQ_THRESHOLD)

qqq_trend_up <- if (TREND_ENGINE == "lmMA_slope") {
  !is_confirmed_downtrend
} else if (TREND_ENGINE == "lmMA_fit") {
  qqq_cur_px >= qqq_lm_fit
} else {
  qqq_cur_px >= qqq_sma200
}

# Rolling 20-day realized volatility of log returns
ret_20d      <- diff(log(tail(qqq_close, 21)))
realized_vol <- sd(ret_20d) * sqrt(252)
vol_scale    <- min(1.0, VOL_TARGET / max(0.05, realized_vol))

# Core Asset Data (QLD or QQQ or NONE)
core_ohlcv <- if (CORE_ASSET == "NONE" || CORE_WEIGHT == 0.0) NULL else if (CORE_ASSET == "QQQ") qqq_ohlcv else tryCatch(load_stock_data(CORE_ASSET, force_refresh = FALSE), error = function(e) NULL)
core_ohlcv <- inject_live_quote(core_ohlcv, CORE_ASSET, live_quotes_data)
core_cur_px <- if (!is.null(core_ohlcv)) tail(as.numeric(Cl(core_ohlcv)), 1) else if (CORE_ASSET == "QLD") 82.59 else 0.0

# Defensive Asset Market Data (GLD or SGOV)
def_ohlcv  <- tryCatch(load_stock_data(DEFENSIVE_ASSET, force_refresh = FALSE), error = function(e) NULL)
def_ohlcv  <- inject_live_quote(def_ohlcv, DEFENSIVE_ASSET, live_quotes_data)
def_cur_px <- if (!is.null(def_ohlcv)) tail(as.numeric(Cl(def_ohlcv)), 1) else if (DEFENSIVE_ASSET == "GLD") 355.78 else 100.73

# Determine active margin leverage today
active_margin <- PORTFOLIO_MARGIN_LEV
if (PORTFOLIO_MARGIN_LEV > 1.0 && USE_MARGIN_TREND_GATE && !qqq_trend_up) {
  active_margin <- 1.0 # Bear market safety gate: de-lever margin debt to 0%
} else if (USE_LEVERAGE_TURBO && is_super_bull) {
  active_margin <- TURBO_MARGIN_LEV # Super-bull leverage turbo: expand to 1.60x in golden trend alignment (Regulation T compliant)
}

cat("\n========================================================================================\n")
if (CORE_WEIGHT == 0.0) {
  cat(sprintf("        100%% HIGH-CONVICTION STOCK MOMENTUM MODEL: %.1fx MARGIN (TOP %d STOCKS)          \n",
              PORTFOLIO_MARGIN_LEV, TOP_N))
} else if (PORTFOLIO_MARGIN_LEV > 1.0) {
  cat(sprintf("   TRIPLE-ENGINE SUPERCHARGED MODEL: 1.5x MARGIN + %s CORE + 22%% VT -> %s     \n", CORE_ASSET, DEFENSIVE_ASSET))
} else {
  cat(sprintf("   TRIPLE-ENGINE SWEET SPOT MODEL (CASH / IRA): %s CORE + 22%% VT -> %s        \n", CORE_ASSET, DEFENSIVE_ASSET))
}
cat("========================================================================================\n")
cat(sprintf(" Date:                   %s\n", Sys.Date()))
cat(sprintf(" Capital Base (Equity):  $%.2f\n", port_state$total_capital))
if (CORE_WEIGHT > 0) {
  cat(sprintf(" Core Asset:             %s @ $%.2f (Target: %.1f%%)\n", CORE_ASSET, core_cur_px, CORE_WEIGHT * 100))
} else {
  cat(" Portfolio Composition:  100% Stock (0% QQQ / 0% QLD Core ETF)\n")
}
cat(sprintf(" Stock Allocation:       Top %d RS Stocks (Target: %.1f%% gross exposure / %.1f%% per slot with %.2fx margin)\n",
            TOP_N, SATELLITE_WEIGHT * 100 * active_margin, SLOT_WEIGHT * 100 * active_margin, active_margin))
cat(sprintf(" Portfolio Leverage:     Target: %.2fx | Active Today: %.2fx %s\n",
            PORTFOLIO_MARGIN_LEV, active_margin,
            if (active_margin < PORTFOLIO_MARGIN_LEV) "(DE-LEVERED TO 0% DEBT: QQQ < 200 SMA)"
            else if (active_margin > PORTFOLIO_MARGIN_LEV) sprintf("(SUPER-BULL LEVERAGE TURBO ACTIVE: %.2fx MARGIN)", active_margin)
            else "(BULL MARGIN ACTIVE)"))
cat(sprintf(" Volatility Targeting:   Target: %.1f%% | Realized (20d): %.1f%% | Vol Scale Factor: %.2fx\n",
            VOL_TARGET * 100, realized_vol * 100, vol_scale))
cat(sprintf(" Benchmark Status:       QQQ @ $%.2f | Trend Engine: %s (%s)\n",
            qqq_cur_px,
            if (TREND_ENGINE == "lmMA_slope") sprintf("lmMA(%d) Slope + R^2", LMMA_WINDOW)
            else if (TREND_ENGINE == "lmMA_fit") sprintf("lmMA(%d) Level", LMMA_WINDOW)
            else "200 SMA",
            if (qqq_trend_up) "BULLISH (UPTREND / CHOP BUFFER)" else "CRISIS / BEARISH"))
cat(sprintf(" lmMA Diagnostics:       Slope: %+.2f/day (%+.1f%% ann.) | R^2: %.3f | Fit: $%.2f | Signal: %s\n",
            qqq_lm_slope, (qqq_lm_slope * 252 / qqq_cur_px) * 100, qqq_lm_rsq, qqq_lm_fit,
            if (is_confirmed_downtrend) "CONFIRMED DOWNTREND (CRISIS MODE)"
            else if (qqq_lm_slope >= 0) "UPWARD DRIFT (BULLISH)"
            else "NEGATIVE SLOPE BUT LOW R^2 (WHIPSAW SHIELD: STAY INVESTED)"))
cat(sprintf(" 50 SMA Diagnostics:     50 SMA: $%.2f (Spread: %+.1f%% | Alignment: %s)\n",
            qqq_sma50, ((qqq_cur_px / qqq_sma50) - 1) * 100,
            if (qqq_sma50 > qqq_sma200) "50 > 200 SMA (GOLDEN ALIGNMENT)" else "50 <= 200 SMA"))
cat(sprintf(" 200 SMA Diagnostics:    200 SMA: $%.2f (Spread: %+.1f%% | Trend: %s)\n",
            qqq_sma200, ((qqq_cur_px / qqq_sma200) - 1) * 100,
            if (qqq_cur_px >= qqq_sma200) "BULLISH" else "BEARISH"))
cat(sprintf(" Defensive Reserve:      %s @ $%.2f (Inflation/Crisis Flight-to-Safety)\n",
            DEFENSIVE_ASSET, def_cur_px))
cat(sprintf(" Disaster Stop Floor:    -%.1f%% on individual stock positions\n", DISASTER_STOP * 100))
cat(sprintf(" Momentum Engine:        %s\n",
            if (USE_FIP) "Frog-in-the-Pan (FIP 63d, 70/30 weight) + Dual-Lookback RS [ACTIVE]"
            else "Standard Dual-Lookback RS (50% 126d + 50% 63d)"))
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
ret_cache  <- list()   # daily simple returns per scanned symbol (Feature 1 covariance)
qqq_ret_xts <- {
  qc <- Cl(qqq_ohlcv); r <- qc / stats::lag(qc, 1) - 1; colnames(r) <- "QQQ"; r[-1, ]
}
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
  end_px <- cl[n - SKIP_DAYS]            # Feature 3: formation ends SKIP_DAYS bars ago
  lb_long_px  <- cl[n - LOOKBACK_LONG]
  lb_short_px <- cl[n - LOOKBACK_SHORT]

  d_ret <- { dc <- Cl(d); r <- dc / stats::lag(dc, 1) - 1; colnames(r) <- sym; r[-1, ] }
  ret_cache[[sym]] <- d_ret
  rs_resid <- NA_real_
  if (USE_RESID_MOM) {
    mm <- stats::na.omit(merge(d_ret, qqq_ret_xts, join = "inner"))
    if (nrow(mm) >= 252) {
      mm <- tail(mm, RESID_EST_WINDOW)
      rs_resid <- residual_dual_score(as.numeric(mm[, 1]), as.numeric(mm[, 2]),
                                      est = RESID_EST_WINDOW, long = LOOKBACK_LONG,
                                      short = LOOKBACK_SHORT, w_long = WEIGHT_LONG,
                                      skip = SKIP_DAYS)
    }
  }
  
  # Trend check: 200 SMA
  sma200 <- if (n >= 200) mean(cl[(n - 199):n]) else mean(cl)
  trend_up_sma <- cur_px >= sma200
  
  # Linear Model Moving Average (lmMA) trend check for stock (50-day window)
  stk_lm <- if (n >= LMMA_WINDOW) lmMA(tail(cl, LMMA_WINDOW), n = LMMA_WINDOW) else NULL
  stk_slope <- if (!is.null(stk_lm)) tail(as.numeric(stk_lm$slope), 1) else 0.0
  stk_rsq   <- if (!is.null(stk_lm)) tail(as.numeric(stk_lm$r.squared), 1) else 0.0
  stk_down  <- (stk_slope < 0 && stk_rsq > LMMA_RSQ_THRESHOLD)
  
  trend_up <- if (TREND_ENGINE == "lmMA_slope") !stk_down else trend_up_sma
  
  ret_long  <- (end_px / lb_long_px) - 1
  ret_short <- (end_px / lb_short_px) - 1
  rs_long   <- ret_long - qqq_ret_long
  rs_short  <- ret_short - qqq_ret_short
  rs_comp   <- (WEIGHT_LONG * rs_long) + (WEIGHT_SHORT * rs_short)
  vol_63d   <- sd(diff(log(tail(cl, 64)))) * sqrt(252)
  fip_63d   <- calc_fip_ratio(cl, lookback = 63)
  
  rs_results[[sym]] <- data.frame(
    Symbol       = sym,
    Close        = cur_px,
    SMA200       = sma200,
    lmMA_Slope   = stk_slope,
    lmMA_R2      = stk_rsq,
    Trend_Up     = trend_up,
    Ret_126D     = ret_long,
    Ret_63D      = ret_short,
    RS_126D      = rs_long,
    RS_63D       = rs_short,
    RS_Composite = rs_comp,
    RS_Resid     = rs_resid,
    FIP_63D      = fip_63d,
    Vol_63D      = vol_63d,
    stringsAsFactors = FALSE
  )
}

if (length(rs_results) == 0) {
  stop("FATAL: No candidate stocks had sufficient data for ranking.")
}

rs_df <- do.call(rbind, rs_results)

# Ranking key: Feature 2 ranks on residual IR; FIP blends continuous drift (70/30).
# Names without enough history for the residual estimate rank last.
base_score <- if (USE_RESID_MOM) ifelse(is.na(rs_df$RS_Resid), -Inf, rs_df$RS_Resid) else rs_df$RS_Composite
rs_df$Rank_Score <- if (USE_FIP) {
  0.70 * base_score + 0.30 * (rs_df$FIP_63D - 0.50)
} else {
  base_score
}

if (USE_FIP || USE_RESID_MOM || SKIP_DAYS > 0) {
  cat(sprintf(" Ranking engine: %s%s%s\n",
              if (USE_RESID_MOM) sprintf("RESIDUAL momentum IR (beta vs QQQ, up to %dd estimation)", RESID_EST_WINDOW)
              else "Dual-lookback RS vs QQQ",
              if (USE_FIP) " + FIP Continuous Momentum (63d, 70/30 weight)" else "",
              if (SKIP_DAYS > 0) sprintf(" | skipping most recent %d day(s)", SKIP_DAYS) else ""))
}

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
if (USE_STOCK_TREND_GATE) {
  rs_qualified <- rs_df[rs_df$Trend_Up, ]
  if (nrow(rs_qualified) < TOP_N) rs_qualified <- rs_df
} else {
  # Unrestricted stock selection: Macro trend is protected by QQQ lmMA(50);
  # individual candidate stocks are selected unconstrained by RS rank (removes 100-day lag)
  rs_qualified <- rs_df
}

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

# (D) Option A Earnings Blackout Shield: Exclude candidate entrants reporting earnings within <=5 trading days
# Existing held positions continue holding through earnings protected by disaster stop
currently_held_syms <- sapply(port_state$satellite_positions, function(p) p$symbol)

if (USE_EARNINGS_SHIELD && file.exists(EARNINGS_FILE)) {
  earn_data <- tryCatch(jsonlite::fromJSON(EARNINGS_FILE), error = function(e) NULL)
  if (!is.null(earn_data)) {
    now_date <- as.Date(Sys.Date())
    earn_blocked <- character()
    next_earn_map <- list()
    
    for (sym in rs_qualified$Symbol) {
      # Option A Rule: Existing holdings are exempt (hold through protected by stops)
      if (sym %in% currently_held_syms) next
      
      ed_list <- earn_data[[sym]]
      if (!is.null(ed_list) && length(ed_list) > 0) {
        ed_dates <- as.Date(ed_list)
        upcoming <- ed_dates[ed_dates >= now_date]
        if (length(upcoming) > 0) {
          next_ed <- min(upcoming)
          days_to_earn <- as.numeric(next_ed - now_date)
          # Approximately 5 trading days corresponds to <= 7 calendar days
          if (days_to_earn <= round(EARNINGS_BLACKOUT_DAYS * (7/5))) {
            earn_blocked <- c(earn_blocked, sym)
            next_earn_map[[sym]] <- as.character(next_ed)
          }
        }
      }
    }
    
    if (length(earn_blocked) > 0) {
      eb_ok <- rs_qualified[!(rs_qualified$Symbol %in% earn_blocked), ]
      if (nrow(eb_ok) >= TOP_N) {
        rs_qualified <- eb_ok
        cat(sprintf("\n 🛡️ Option A Earnings Shield (<=%d days) bypassed %d candidate entrant(s): %s\n",
                    EARNINGS_BLACKOUT_DAYS, length(earn_blocked),
                    paste(sprintf("%s (Earn: %s)", earn_blocked, unlist(next_earn_map[earn_blocked])), collapse = ", ")))
      }
    }
  }
}

# Check regime: Bull (QQQ >= 200 SMA) vs Crisis / Serious Bear (QQQ < 200 SMA)
is_crisis_regime <- !qqq_trend_up

if (is_crisis_regime && BEAR_SLEEVE %in% c("gld_sgov", "cash")) {
  if (BEAR_SLEEVE == "gld_sgov") {
    gld_raw_ohlcv <- if (DEFENSIVE_ASSET == "GLD" && !is.null(def_ohlcv)) def_ohlcv else tryCatch(load_stock_data("GLD", force_refresh = FALSE), error = function(e) NULL)
    gld_raw_ohlcv <- inject_live_quote(gld_raw_ohlcv, "GLD", live_quotes_data)
    gld_sh <- evaluate_safe_haven_bear(if (!is.null(gld_raw_ohlcv)) Cl(gld_raw_ohlcv) else NULL, window = 50)
    DEFENSIVE_ASSET <- gld_sh$asset
    def_ohlcv <- if (DEFENSIVE_ASSET == "GLD") gld_raw_ohlcv else tryCatch(load_stock_data("SGOV", force_refresh = FALSE), error = function(e) NULL)
    def_ohlcv <- inject_live_quote(def_ohlcv, DEFENSIVE_ASSET, live_quotes_data)
    def_cur_px <- if (!is.null(def_ohlcv)) tail(as.numeric(Cl(def_ohlcv)), 1) else if (DEFENSIVE_ASSET == "GLD") 355.78 else 100.73
    
    cat("\n========================================================================================\n")
    cat(sprintf(" 🛡️ CRISIS / BEAR REGIME ACTIVE: %s Downtrend Confirmed \n",
                if (TREND_ENGINE == "lmMA_slope") sprintf("lmMA(%d) Slope (%.2f) + R^2 (%.2f)", LMMA_WINDOW, qqq_lm_slope, qqq_lm_rsq)
                else "QQQ < 200-day SMA"))
    cat(sprintf(" TACTICAL SAFE-HAVEN SLEEVE ACTIVE: %s \n", gld_sh$rationale))
    cat(sprintf(" Allocation Target: 100%% %s (0%% Equity Beta Exposure, +13.7%% 10Y CAGR Alpha)\n", DEFENSIVE_ASSET))
    cat("========================================================================================\n\n")
  } else {
    DEFENSIVE_ASSET <- "SGOV"
    def_ohlcv <- tryCatch(load_stock_data("SGOV", force_refresh = FALSE), error = function(e) NULL)
    def_ohlcv <- inject_live_quote(def_ohlcv, "SGOV", live_quotes_data)
    def_cur_px <- if (!is.null(def_ohlcv)) tail(as.numeric(Cl(def_ohlcv)), 1) else 100.73
    cat("\n========================================================================================\n")
    cat(" 🛡️ CRISIS / BEAR REGIME ACTIVE: Macro Downtrend Confirmed -> 100% Cash / SGOV Preservation\n")
    cat("========================================================================================\n\n")
  }
  top_picks <- rs_df[0, ]
} else if (USE_CRISIS_MVP && is_crisis_regime) {
  cat("\n========================================================================================\n")
  cat(sprintf(" 🛡️ CRISIS / BEAR REGIME ACTIVE: %s Downtrend Confirmed \n",
              if (TREND_ENGINE == "lmMA_slope") sprintf("lmMA(%d) Slope (%.2f) + R^2 (%.2f)", LMMA_WINDOW, qqq_lm_slope, qqq_lm_rsq)
              else "QQQ < 200-day SMA"))
  cat(" CHANGING RULES: REBALANCING AROUND MINIMUM VARIANCE EQUITIES PORTFOLIO (MVP)           \n")
  cat(" Philosophy: Stay 100% invested in equities, shifting selection from Momentum to Min-Var!\n")
  cat("========================================================================================\n")
  
  # When trend is down and volatility is elevated, shift rules from Momentum to Minimum Variance
  mv_pool <- rs_df[!is.na(rs_df$Vol_63D) & rs_df$Vol_63D > 0, ]
  mv_pool <- mv_pool[order(mv_pool$Vol_63D), ]
  top_picks <- head(mv_pool, TOP_N)
  
  cat(sprintf("\n Top %d Minimum Variance Defensive Stocks (Lowest 63-day Realized Volatility):\n", TOP_N))
  cat("--------------------------------------------------------------------------------------------------------\n")
  print_df <- head(mv_pool[, c("Symbol", "Close", "Trend_Up", "lmMA_Slope", "Ret_126D", "Ret_63D", "RS_Composite", "Vol_63D")], 8)
  print_df$lmMA_Slope   <- sprintf("%+.2f", print_df$lmMA_Slope)
  print_df$Ret_126D     <- sprintf("%+.1f%%", print_df$Ret_126D * 100)
  print_df$Ret_63D      <- sprintf("%+.1f%%", print_df$Ret_63D * 100)
  print_df$RS_Composite <- sprintf("%+.1f%%", print_df$RS_Composite * 100)
  print_df$Vol_63D      <- sprintf("%.0f%%", print_df$Vol_63D * 100)
  print(print_df, row.names = FALSE)
  cat("--------------------------------------------------------------------------------------------------------\n\n")
} else {
  rs_qualified <- rs_qualified[order(-rs_qualified$Rank_Score), ]
  
  cur_held_syms <- if (!is.null(port_state$satellite_positions)) {
    sapply(port_state$satellite_positions, function(p) p$symbol)
  } else character()
  
  if (USE_RETENTION_BUFFER && length(cur_held_syms) > 0) {
    buffer_cutoff <- min(nrow(rs_qualified), RETENTION_BUFFER_RANK)
    buffer_eligible <- rs_qualified$Symbol[1:buffer_cutoff]
    kept_buffer_syms <- intersect(cur_held_syms, buffer_eligible)
    
    # Optional Feature 6: Industry Concentration Limit on Retained Holdings
    if (MAX_PER_INDUSTRY < TOP_N && length(kept_buffer_syms) > 0) {
      kept_buffer_syms <- filter_by_industry_cap(kept_buffer_syms, MAX_PER_INDUSTRY, sector_map = SECTOR_MAP)
    }
    
    # Feature 5: Tax-Aware Winner Retention (LTCG Bridge)
    kept_tax_syms <- character()
    if (USE_TAX_AWARE_ROTATION && !is.null(port_state$satellite_positions)) {
      today_dt <- Sys.Date()
      for (pos in port_state$satellite_positions) {
        s <- pos$symbol
        if (s %in% kept_buffer_syms) next # already retained by standard buffer
        s_rank <- match(s, rs_qualified$Symbol)
        if (!is.na(s_rank) && is_tax_protected_winner(pos$entry_date, today_dt, pos$entry_price,
                                                      pos$current_price, s_rank,
                                                      bridge_days = LTCG_BRIDGE_DAYS, rank_max = LTCG_RANK_MAX)) {
          # Check industry cap before adding tax-protected winner
          s_ind <- get_stock_industry(s, SECTOR_MAP)
          held_inds <- sapply(c(kept_buffer_syms, kept_tax_syms), function(x) get_stock_industry(x, SECTOR_MAP))
          if (MAX_PER_INDUSTRY >= TOP_N || sum(held_inds == s_ind) < MAX_PER_INDUSTRY) {
            kept_tax_syms <- c(kept_tax_syms, s)
            days_h <- as.numeric(today_dt - as.Date(pos$entry_date))
            cat(sprintf("\n 🎯 TAX-AWARE ROTATION: Retained winner %s (#%d) held %d days across 365-day mark to secure LTCG status (saves ~15-17pp NYC/NYS tax)!\n",
                        s, s_rank, days_h))
          }
        }
      }
    }
    
    all_kept_syms <- unique(c(kept_buffer_syms, kept_tax_syms))
    slots_needed <- max(0, TOP_N - length(all_kept_syms))
    new_candidates <- setdiff(rs_qualified$Symbol, all_kept_syms)
    
    # Tax-Aware Wash Sale Avoidance: Lock out repurchase of stocks exited at a loss within 30 days
    if (USE_TAX_AWARE_ROTATION && AVOID_WASH_SALES && !is.null(port_state$closed_trades) && length(port_state$closed_trades) > 0) {
      recent_loss_syms <- character()
      today_dt <- Sys.Date()
      for (ct in port_state$closed_trades) {
        if (!is.null(ct$symbol) && is_wash_sale_risk(ct$symbol, list(ct), today_dt, window_days = 30)) {
          recent_loss_syms <- c(recent_loss_syms, ct$symbol)
        }
      }
      recent_loss_syms <- unique(recent_loss_syms)
      blocked_cands <- intersect(new_candidates, recent_loss_syms)
      if (length(blocked_cands) > 0) {
        clean_cands <- setdiff(new_candidates, recent_loss_syms)
        if (length(clean_cands) >= slots_needed) {
          cat(sprintf("\n 🛡️ WASH-SALE PREVENTED: Blocked repurchase of %s (sold at loss <=30d). Substituted with next ranked candidate(s): %s\n",
                      paste(blocked_cands, collapse = ", "),
                      paste(head(clean_cands, length(blocked_cands)), collapse = ", ")))
          new_candidates <- clean_cands
        }
      }
    }
    
    # Optional Feature 6: Industry Concentration Limit on New Additions
    if (MAX_PER_INDUSTRY < TOP_N) {
      new_candidates <- filter_by_industry_cap(new_candidates, MAX_PER_INDUSTRY, initial_held = all_kept_syms, sector_map = SECTOR_MAP)
    }
    
    new_picks_syms <- head(new_candidates, slots_needed)
    target_syms <- c(all_kept_syms, new_picks_syms)[1:min(TOP_N, length(c(all_kept_syms, new_picks_syms)))]
    top_picks <- rs_qualified[match(target_syms, rs_qualified$Symbol), ]
    
    if (length(kept_buffer_syms) > 0) {
      cat(sprintf("\n 🛡️ WINNER RETENTION BUFFER (Top %d): Retained %d existing winner(s) [ranks 1-%d]: %s\n",
                  RETENTION_BUFFER_RANK, length(kept_buffer_syms), RETENTION_BUFFER_RANK,
                  paste(sprintf("%s (#%d)", kept_buffer_syms, match(kept_buffer_syms, rs_qualified$Symbol)), collapse = ", ")))
    }
  } else {
    if (MAX_PER_INDUSTRY < TOP_N) {
      target_syms <- head(filter_by_industry_cap(rs_qualified$Symbol, MAX_PER_INDUSTRY, sector_map = SECTOR_MAP), TOP_N)
      top_picks <- rs_qualified[match(target_syms, rs_qualified$Symbol), ]
    } else {
      top_picks <- head(rs_qualified, TOP_N)
    }
  }
  
  if (MAX_PER_INDUSTRY < TOP_N) {
    pick_inds <- sapply(top_picks$Symbol, function(s) get_stock_industry(s, SECTOR_MAP))
    cat(sprintf("\n 🏢 INDUSTRY CONCENTRATION CAP: Max %d per Industry Active:\n", MAX_PER_INDUSTRY))
    for (ind_name in unique(pick_inds)) {
      ind_syms <- top_picks$Symbol[pick_inds == ind_name]
      cat(sprintf("    - %s (%d/%d): %s\n", ind_name, length(ind_syms), MAX_PER_INDUSTRY, paste(ind_syms, collapse = ", ")))
    }
  }
  
  cat(sprintf("\n Top Candidate Relative Strength Leaders (Dual-Lookback: 50%% 126d + 50%% 63d | %s):\n",
              if (!USE_STOCK_TREND_GATE) "Unrestricted RS Rank (No Stock Lag)"
              else if (TREND_ENGINE == "lmMA_slope") sprintf("lmMA(%d) Slope / Low-Lag Filter", LMMA_WINDOW)
              else ">= 200-day SMA"))
  cat("--------------------------------------------------------------------------------------------------------\n")
  print_df <- head(rs_qualified[, c("Symbol", "Close", "Trend_Up", "lmMA_Slope", "Ret_126D", "Ret_63D", "RS_Composite", "RS_Resid", "FIP_63D", "Rank_Score", "Vol_63D")], 8)
  print_df$lmMA_Slope   <- sprintf("%+.2f", print_df$lmMA_Slope)
  print_df$Ret_126D     <- sprintf("%+.1f%%", print_df$Ret_126D * 100)
  print_df$Ret_63D      <- sprintf("%+.1f%%", print_df$Ret_63D * 100)
  print_df$RS_Composite <- sprintf("%+.1f%%", print_df$RS_Composite * 100)
  print_df$RS_Resid     <- ifelse(is.na(print_df$RS_Resid), "-", sprintf("%+.2f", print_df$RS_Resid))
  print_df$FIP_63D      <- sprintf("%.1f%%", print_df$FIP_63D * 100)
  print_df$Rank_Score   <- sprintf("%+.2f", print_df$Rank_Score)
  print_df$Vol_63D      <- sprintf("%.0f%%", print_df$Vol_63D * 100)
  print(print_df, row.names = FALSE)
  cat("--------------------------------------------------------------------------------------------------------\n\n")
}

# (L) Feature 1: risk-targeted, fractional-Kelly leverage on the SELECTED book.
# Replaces the fixed 1.5x / 1.60x turbo and the QQQ-vol scale factor. Same function
# and conventions as R/11_option_d_engine.R (leverage_mode = "risk_kelly"):
# stock gross = vol_scale * active_margin = L, defensive asset = 1 - min(1, L).
if (USE_RISK_TARGET_LEV && nrow(top_picks) > 0) {
  pick_syms <- top_picks$Symbol[top_picks$Symbol %in% names(ret_cache)]
  rmat <- if (length(pick_syms) > 0) stats::na.omit(do.call(merge, c(unname(ret_cache[pick_syms]), list(join = "inner")))) else NULL
  sig_p <- if (!is.null(rmat) && nrow(rmat) >= 63) {
    portfolio_vol(rep(1 / ncol(rmat), ncol(rmat)), ewma_cov(as.matrix(tail(rmat, 126))))
  } else NA_real_
  lev <- risk_kelly_leverage(sig_p, sigma_target = SIGMA_TARGET, max_leverage = LEV_MAX,
                             bull = qqq_trend_up, mu_hat = MU_HAT, borrow_rate = BORROW_RATE,
                             kelly_fraction = KELLY_FRAC, no_mu_cap = PORTFOLIO_MARGIN_LEV)
  vol_scale     <- min(1.0, lev$L)
  active_margin <- if (vol_scale > 0) lev$L / vol_scale else 1.0
  cat("========================================================================================\n")
  cat(" FEATURE 1: RISK-TARGETED / FRACTIONAL-KELLY LEVERAGE (replaces fixed margin + turbo)\n")
  cat(sprintf(" Ex-ante book vol (EWMA, 126d): %s | Target: %.0f%% | L_risk: %.2fx | L_kelly: %s | Cap: %.2fx\n",
              if (is.na(sig_p)) "n/a (insufficient history)" else sprintf("%.1f%%", sig_p * 100),
              SIGMA_TARGET * 100, lev$L_risk,
              if (is.na(lev$L_kelly)) "off (no --mu_hat)" else sprintf("%.2fx", lev$L_kelly), LEV_MAX))
  cat(sprintf(" Borrow rate: %.2f%% | Kelly fraction: %.2f | ACTIVE GROSS: %.2fx (binding: %s)\n",
              BORROW_RATE * 100, KELLY_FRAC, lev$L, lev$binding))
  if (is.na(MU_HAT)) cat(sprintf(" WARNING: --mu_hat not set; borrowing capped at %.2fx (no Kelly cap).\n", PORTFOLIO_MARGIN_LEV))
  cat("========================================================================================\n\n")
}

# ------------------------------------------------------------------------------
# 6. Mark Current Portfolio Value & Process Disaster Stops
# ------------------------------------------------------------------------------
cur_prices <- list(QQQ = qqq_cur_px, QLD = 82.59, GLD = def_cur_px, SGOV = 100.73)
if (CORE_ASSET != "NONE" && core_cur_px > 0) cur_prices[[CORE_ASSET]] <- core_cur_px
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
if (!is.null(core_pos) && !is.null(core_pos$shares) && !is.na(core_pos$shares) && as.numeric(core_pos$shares) > 0) {
  sym_core <- if (!is.null(core_pos$symbol)) core_pos$symbol else CORE_ASSET
  core_px  <- if (!is.null(cur_prices[[sym_core]]) && cur_prices[[sym_core]] > 0) cur_prices[[sym_core]] else as.numeric(core_pos$entry_price)
  core_val <- round(as.numeric(core_pos$shares) * core_px, 2)
}

# Value Defensive Position (GLD or legacy SGOV)
def_val <- 0
def_pos <- port_state$defensive_position
if (!is.null(def_pos) && !is.null(def_pos$shares) && !is.na(def_pos$shares) && as.numeric(def_pos$shares) > 0) {
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
if (STOP_PROCEEDS_TO_CORE && CORE_WEIGHT > 0 && length(disaster_tickets) > 0 && !should_rebalance &&
    (!USE_CORE_TREND || qqq_trend_up)) {
  stop_proceeds <- sum(vapply(port_state$closed_trades, function(tr)
    if (identical(tr$exit_date, today_str) && identical(tr$reason, "DISASTER_STOP"))
      as.numeric(tr$shares) * as.numeric(tr$exit_price) else 0, numeric(1)))
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

# (B) Disaster Stop Capital Handling: Reallocate to Surviving Winners OR Replace with Rank 6
#     Eliminates 3-4 weeks of cash drag during bull regimes (+2.6% to +6.9% CAGR alpha)
if (USE_STOP_REPLACEMENT && length(disaster_tickets) > 0 && !should_rebalance && qqq_trend_up) {
  stopped_trades <- port_state$closed_trades[vapply(port_state$closed_trades, function(tr)
    identical(tr$exit_date, today_str) && identical(tr$reason, "DISASTER_STOP"), logical(1))]
  
  if (length(stopped_trades) > 0) {
    held_now <- sapply(port_state$satellite_positions, function(p) p$symbol)
    stopped_syms <- sapply(stopped_trades, function(tr) tr$symbol)
    
    if (STOP_REALLOCATE == "survivors" && length(port_state$satellite_positions) > 0) {
      total_stopped_proceeds <- sum(vapply(stopped_trades, function(tr)
        as.numeric(tr$shares) * as.numeric(tr$exit_price), numeric(1)))
      
      realloc_res <- reallocate_stop_to_survivors(total_stopped_proceeds, port_state$satellite_positions,
                                                  cur_prices = cur_prices, disaster_stop = DISASTER_STOP)
      if (length(realloc_res$order_records) > 0) {
        disaster_tickets <- c(disaster_tickets, realloc_res$tickets)
        order_records <- c(order_records, realloc_res$order_records)
        port_state$satellite_positions <- realloc_res$updated_positions
        outlay_sum <- sum(vapply(realloc_res$order_records, function(o) o$estimated_outlay, numeric(1)))
        port_state$cash_balance <- port_state$cash_balance - outlay_sum
        sat_val <- sum(vapply(port_state$satellite_positions, function(p) as.numeric(p$market_value), numeric(1)))
        cat(sprintf("\n 🚀 STOP PROCEEDS REALLOCATION TO SURVIVORS: Reallocated $%.2f across %d surviving winner(s)\n",
                    total_stopped_proceeds, length(port_state$satellite_positions)))
        for (rec in realloc_res$order_records) {
          cat(sprintf("    -> Added %s shs of %s ($%.2f outlay @ ~$%.2f)\n",
                      rec$shares, rec$symbol, rec$estimated_outlay, rec$estimated_price))
        }
      }
    } else {
      eligible_replacements <- setdiff(rs_qualified$Symbol, c(held_now, stopped_syms))
      if (MAX_PER_INDUSTRY < TOP_N) {
        eligible_replacements <- filter_by_industry_cap(eligible_replacements, MAX_PER_INDUSTRY, initial_held = held_now, sector_map = SECTOR_MAP)
      }
      
      for (tr in stopped_trades) {
        proceeds <- as.numeric(tr$shares) * as.numeric(tr$exit_price)
        if (proceeds < 50 || length(eligible_replacements) == 0) next
        
        rep_sym <- eligible_replacements[1]
        eligible_replacements <- eligible_replacements[-1]
        held_now <- c(held_now, rep_sym)
        if (MAX_PER_INDUSTRY < TOP_N && length(eligible_replacements) > 0) {
          eligible_replacements <- filter_by_industry_cap(eligible_replacements, MAX_PER_INDUSTRY, initial_held = held_now, sector_map = SECTOR_MAP)
        }
        rep_px <- if (!is.null(cur_prices[[rep_sym]])) cur_prices[[rep_sym]] else rs_qualified$Close[rs_qualified$Symbol == rep_sym][1]
        
        if (!is.null(rep_px) && rep_px > 0) {
          buy_shs <- round(proceeds / rep_px, 3)
          shs_whole <- as.integer(floor(buy_shs))
          if (buy_shs > 0) {
            outlay <- round(if (shs_whole >= 1) (shs_whole * rep_px) else (buy_shs * rep_px), 2)
            rep_rank <- match(rep_sym, rs_qualified$Symbol)
            ticket <- if (shs_whole >= 1) {
              sprintf("BUY %d shares (%.3f target) of %s @ ~$%.2f ($%.2f outlay - Immediate Stop Replacement into Rank #%d RS Leader)",
                      shs_whole, buy_shs, rep_sym, rep_px, outlay, rep_rank)
            } else {
              sprintf("BUY 0 shares (%.3f target < 1 share) of %s @ ~$%.2f ($%.2f outlay) [SKIPPED - Schwab requires whole shares]",
                      buy_shs, rep_sym, rep_px, round(buy_shs * rep_px, 2))
            }
            disaster_tickets <- c(disaster_tickets, ticket)
            order_records[[length(order_records) + 1]] <- list(
              symbol = rep_sym, side = "BUY", shares = round(buy_shs, 3),
              shares_whole = shs_whole, estimated_price = round(rep_px, 2),
              estimated_outlay = outlay, order_type = "MARKET",
              reason = "IMMEDIATE_STOP_REPLACEMENT_ENTRANT",
              stop_loss_price = round(rep_px * (1.0 - DISASTER_STOP), 2)
            )
            port_state$satellite_positions[[length(port_state$satellite_positions) + 1]] <- list(
              symbol = rep_sym,
              shares = if (shs_whole >= 1) shs_whole else buy_shs,
              entry_price = rep_px,
              current_price = rep_px,
              entry_date = today_str,
              market_value = outlay,
              unrealized_pnl = 0.0
            )
            port_state$cash_balance <- port_state$cash_balance - outlay
            sat_val <- sat_val + outlay
            cat(sprintf(" 🔄 IMMEDIATE STOP REPLACEMENT: Redeployed $%.2f from stopped %s into Rank #%d candidate %s (%d shs @ $%.2f)\n",
                        outlay, tr$symbol, rep_rank, rep_sym, shs_whole, rep_px))
          }
        }
      }
    }
  }
}

if (EXPLICIT_CAPITAL) {
  total_equity <- CAPITAL
} else if (!is.null(broker_cap) && !is.na(broker_cap) && broker_cap > 0) {
  total_equity <- broker_cap
} else {
  total_equity <- round(port_state$cash_balance + core_val + def_val + sat_val, 2)
}
port_state$total_capital <- total_equity

gross_exposure   <- round(core_val + def_val + sat_val, 2)
margin_debt      <- max(0, round(gross_exposure - total_equity, 2))
unallocated_cash <- max(0, round(total_equity - gross_exposure, 2))

# Maintain strict double-entry balance sheet accounting
port_state$cash_balance <- if (gross_exposure > total_equity) -margin_debt else unallocated_cash

cat("========================================================================================\n")
cat("                               PORTFOLIO BALANCE SHEET                                  \n")
cat("========================================================================================\n")
cat(sprintf(" Total Portfolio Equity (Net Worth): $%.2f\n", total_equity))
cat(sprintf(" Gross Long Market Value:            $%.2f (%5.1f%% of equity)\n",
            gross_exposure, (gross_exposure / max(1, total_equity)) * 100))
cat(sprintf(" Borrowed Margin Loan (Debt):        $%.2f (%5.1f%% leverage)\n",
            margin_debt, (margin_debt / max(1, total_equity)) * 100))
cat("----------------------------------------------------------------------------------------\n")
if (CORE_WEIGHT > 0 || core_val > 0) {
  cat(sprintf(" Core %-4s Sleeve:                  $%.2f (%5.1f%% of equity)\n",
              if (CORE_ASSET != "NONE") CORE_ASSET else "ETF", core_val, (core_val / max(1, total_equity)) * 100))
}
cat(sprintf(" Stock Positions (%d Stocks):        $%.2f (%5.1f%% of equity)\n", TOP_N,
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
  if (is_crisis_regime && BEAR_SLEEVE %in% c("gld_sgov", "cash")) {
    base_core_cash <- 0.0
    base_sat_cash  <- 0.0
    target_core_cash <- 0.0
    target_sat_cash  <- 0.0
    target_slot_cash <- 0.0
    target_sat_syms  <- character()
    target_def_cash  <- total_equity
  } else {
    base_core_cash <- if (core_active) total_equity * CORE_WEIGHT * vol_scale else 0.0
    base_sat_cash  <- total_equity * SATELLITE_WEIGHT * vol_scale
    
    target_core_cash <- base_core_cash * active_margin
    target_sat_cash  <- base_sat_cash * active_margin
    target_slot_cash <- target_sat_cash / TOP_N
    target_sat_syms  <- top_picks$Symbol

    # Exclude any symbols stopped out today from rebalance buy list
    # vapply (not sapply): with zero closed trades sapply returns list(), and
    # indexing a list by a list aborted every rebalance before the first exit.
    stopped_today <- vapply(port_state$closed_trades, function(tr)
      identical(tr$exit_date, today_str) && identical(tr$reason, "DISASTER_STOP"), logical(1))
    stopped_syms_today <- vapply(port_state$closed_trades[stopped_today], function(tr) tr$symbol, character(1))
    if (length(stopped_syms_today) > 0) {
      target_sat_syms <- setdiff(target_sat_syms, stopped_syms_today)
      needed <- TOP_N - length(target_sat_syms)
      if (needed > 0) {
        fill_cands <- setdiff(rs_qualified$Symbol, c(target_sat_syms, stopped_syms_today))
        target_sat_syms <- c(target_sat_syms, head(fill_cands, needed))
      }
    }
    
    # Defensive Target Allocation: absorbs unallocated buffer + core when core inactive
    base_equity_w   <- (if (core_active) CORE_WEIGHT else 0.0) * vol_scale + (SATELLITE_WEIGHT * vol_scale)
    target_def_cash <- max(0, total_equity * (1.0 - base_equity_w))
  }
  
  if (CORE_WEIGHT > 0) {
    cat(sprintf(" Target Allocations: Core %s: $%.2f (%.1f%%) | Stocks (%d): $%.2f (%.1f%%) | Defensive %s: $%.2f (%.1f%%)\n\n",
                CORE_ASSET, target_core_cash, (target_core_cash / total_equity) * 100, TOP_N,
                target_sat_cash, (target_sat_cash / total_equity) * 100,
                DEFENSIVE_ASSET, target_def_cash, (target_def_cash / total_equity) * 100))
  } else {
    cat(sprintf(" Target Allocations: 100%% Stocks (%d): $%.2f (%.1f%% gross exposure / %.1f%% per slot) | Defensive %s: $%.2f (%.1f%%)\n\n",
                TOP_N, target_sat_cash, (target_sat_cash / total_equity) * 100,
                (target_slot_cash / total_equity) * 100,
                DEFENSIVE_ASSET, target_def_cash, (target_def_cash / total_equity) * 100))
  }
  
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
        shs_whole <- as.integer(round(shs))
        proceeds  <- round(if (shs_whole >= 1) (shs_whole * px) else mv, 2)
        ticket <- if (shs_whole >= 1) {
          sprintf("SELL %d shares (%.3f held) of %s @ ~$%.2f ($%.2f proceeds - Monthly RS Rotation Out)",
                  shs_whole, shs, sym, px, proceeds)
        } else {
          sprintf("SELL 0 shares (%.3f held < 1 share) of %s @ ~$%.2f [SKIPPED - Schwab requires whole shares]",
                  shs, sym, px)
        }
        rebalance_tickets <- c(rebalance_tickets, ticket)
        order_records[[length(order_records) + 1]] <- list(
          symbol = sym, side = "SELL", shares = round(shs, 3),
          shares_whole = shs_whole, estimated_price = round(px, 2),
          estimated_outlay = proceeds, order_type = "MARKET",
          reason = "MONTHLY_ROTATION", stop_loss_price = NULL
        )
        
        port_state$cash_balance <- port_state$cash_balance + proceeds
        port_state$closed_trades[[length(port_state$closed_trades) + 1]] <- list(
          symbol = sym, shares = if (shs_whole >= 1) shs_whole else shs, entry_price = as.numeric(pos$entry_price),
          exit_price = px, entry_date = pos$entry_date, exit_date = today_str,
          pnl_dollar = round(proceeds - ((if (shs_whole >= 1) shs_whole else shs) * as.numeric(pos$entry_price)), 2),
          pnl_pct = round(((px / as.numeric(pos$entry_price)) - 1) * 100, 2),
          reason = "MONTHLY_ROTATION"
        )
      } else {
        updated_sat_list[[length(updated_sat_list) + 1]] <- pos
      }
    }
  }
  port_state$satellite_positions <- updated_sat_list
  
  # B. Core Position Adjustment (Liquidate Core if 100% Stock or Switching Core)
  current_core_sym <- if (!is.null(port_state$core_position$symbol)) port_state$core_position$symbol else ""
  current_core_shs <- if (!is.null(port_state$core_position$shares)) as.numeric(port_state$core_position$shares) else 0
  
  if (current_core_shs > 0 && (CORE_WEIGHT == 0.0 || current_core_sym != CORE_ASSET)) {
    old_core_px <- if (!is.null(cur_prices[[current_core_sym]]) && cur_prices[[current_core_sym]] > 0) cur_prices[[current_core_sym]] else as.numeric(port_state$core_position$entry_price)
    old_core_mv <- round(current_core_shs * old_core_px, 2)
    ticket_msg  <- if (CORE_WEIGHT == 0.0) "Liquidate Core Sleeve for 100% Stock Allocation" else sprintf("Switch Core to %s", CORE_ASSET)
    ticket <- sprintf("SELL %s shares of %s @ ~$%.2f ($%.2f proceeds - %s)",
                      current_core_shs, current_core_sym, old_core_px, old_core_mv, ticket_msg)
    rebalance_tickets <- c(rebalance_tickets, ticket)
    order_records[[length(order_records) + 1]] <- list(
      symbol = current_core_sym, side = "SELL", shares = round(current_core_shs, 3),
      shares_whole = as.integer(round(current_core_shs)), estimated_price = round(old_core_px, 2),
      estimated_outlay = round(old_core_mv, 2), order_type = "MARKET",
      reason = if (CORE_WEIGHT == 0.0) "LIQUIDATE_CORE_SLEEVE" else sprintf("SWITCH_CORE_TO_%s", CORE_ASSET),
      stop_loss_price = NULL
    )
    port_state$cash_balance <- port_state$cash_balance + old_core_mv
    port_state$core_position <- NULL
    current_core_shs <- 0
  }
  
  if (CORE_WEIGHT > 0 && core_cur_px > 0) {
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
  }
  
  # C. Buy / Top-up Top-N Satellite Positions
  existing_sat_syms <- sapply(port_state$satellite_positions, function(p) p$symbol)
  for (sym in target_sat_syms) {
    px <- cur_prices[[sym]]
    if (is.null(px) || px <= 0) next
    
    # Position sizing: Convex (30/25/20/15/10) vs Equal (20% each)
    rank_idx <- match(sym, target_sat_syms)
    slot_w <- if (USE_CONVEX_SIZING && length(target_sat_syms) == 5) {
      c(0.30, 0.25, 0.20, 0.15, 0.10)[rank_idx]
    } else {
      1.0 / length(target_sat_syms)
    }
    pos_target_cash <- target_sat_cash * slot_w
    
    existing_idx <- which(existing_sat_syms == sym)
    if (length(existing_idx) == 0) {
      shs_to_buy <- round(pos_target_cash / px, 3)
      shs_whole  <- as.integer(floor(shs_to_buy))
      outlay     <- round(shs_whole * px, 2)
      ticket_desc <- if (USE_CRISIS_MVP && is_crisis_regime) sprintf("Top-%d Minimum Variance Defensive Entrant", TOP_N) else sprintf("Top-%d RS Satellite Entrant (Rank #%d: %.1f%%)", TOP_N, rank_idx, slot_w * 100)
      ticket <- if (shs_whole >= 1) {
        sprintf("BUY %d shares (%.3f target) of %s @ ~$%.2f ($%.2f outlay - %s)",
                shs_whole, shs_to_buy, sym, px, outlay, ticket_desc)
      } else {
        sprintf("BUY 0 shares (%.3f target < 1 share) of %s @ ~$%.2f ($%.2f outlay - %s) [SKIPPED - Schwab requires whole shares]",
                shs_to_buy, sym, px, round(shs_to_buy * px, 2), ticket_desc)
      }
      rebalance_tickets <- c(rebalance_tickets, ticket)
      order_records[[length(order_records) + 1]] <- list(
        symbol = sym, side = "BUY", shares = round(shs_to_buy, 3),
        shares_whole = shs_whole, estimated_price = round(px, 2),
        estimated_outlay = round(if (shs_whole >= 1) outlay else (shs_to_buy * px), 2), order_type = "MARKET",
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
      drift_dlr <- pos_target_cash - curr_mv
      if (abs(drift_dlr) >= (pos_target_cash * 0.15)) {
        delta_shs <- round(drift_dlr / px, 3)
        shs_whole <- as.integer(floor(delta_shs))
        if (delta_shs > 0) {
          ticket <- if (shs_whole >= 1) {
            sprintf("BUY %d shares (%.3f target) of %s @ ~$%.2f ($%.2f outlay - Top up Satellite Slot to %.1f%%)",
                    shs_whole, delta_shs, sym, px, round(shs_whole * px, 2), (pos_target_cash / total_equity) * 100)
          } else {
            sprintf("BUY 0 shares (%.3f target < 1 share) of %s @ ~$%.2f ($%.2f outlay) [SKIPPED - Schwab requires whole shares]",
                    delta_shs, sym, px, round(delta_shs * px, 2))
          }
          rebalance_tickets <- c(rebalance_tickets, ticket)
          order_records[[length(order_records) + 1]] <- list(
            symbol = sym, side = "BUY", shares = round(delta_shs, 3),
            shares_whole = shs_whole, estimated_price = round(px, 2),
            estimated_outlay = round(if (shs_whole >= 1) (shs_whole * px) else (delta_shs * px), 2), order_type = "MARKET",
            reason = "TOP_UP_SATELLITE_SLOT",
            stop_loss_price = round(px * (1.0 - DISASTER_STOP), 2)
          )
          port_state$satellite_positions[[existing_idx]]$shares <- curr_shs + delta_shs
          port_state$cash_balance <- port_state$cash_balance - round(delta_shs * px, 2)
        } else {
          delta_abs <- abs(delta_shs)
          shs_whole <- as.integer(round(delta_abs))
          proceeds  <- round(shs_whole * px, 2)
          ticket <- if (shs_whole >= 1) {
            sprintf("SELL %d shares (%.3f target) of %s @ ~$%.2f ($%.2f proceeds - Trim Satellite Slot to %.1f%%)",
                    shs_whole, delta_abs, sym, px, proceeds, (pos_target_cash / total_equity) * 100)
          } else {
            sprintf("SELL 0 shares (%.3f target < 1 share) of %s @ ~$%.2f ($%.2f proceeds) [SKIPPED - Schwab requires whole shares]",
                    delta_abs, sym, px, round(delta_abs * px, 2))
          }
          rebalance_tickets <- c(rebalance_tickets, ticket)
          order_records[[length(order_records) + 1]] <- list(
            symbol = sym, side = "SELL", shares = round(delta_abs, 3),
            shares_whole = shs_whole, estimated_price = round(px, 2),
            estimated_outlay = round(if (shs_whole >= 1) proceeds else (delta_abs * px), 2), order_type = "MARKET",
            reason = "TRIM_SATELLITE_SLOT", stop_loss_price = NULL
          )
          port_state$satellite_positions[[existing_idx]]$shares <- max(0, curr_shs - (if (shs_whole >= 1) shs_whole else delta_abs))
          port_state$cash_balance <- port_state$cash_balance + (if (shs_whole >= 1) proceeds else abs(round(delta_shs * px, 2)))
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
  def_shs_whole   <- as.integer(floor(def_delta_shs))
  
  if (abs(def_delta_cash) >= 30) {
    if (def_delta_cash > 0) {
      ticket <- if (def_shs_whole >= 1) {
        sprintf("BUY %d shares (%.3f target) of %s @ ~$%.2f ($%.2f outlay - Park Vol-Target Buffer in Gold)",
                def_shs_whole, def_delta_shs, DEFENSIVE_ASSET, def_cur_px, round(def_shs_whole * def_cur_px, 2))
      } else {
        sprintf("BUY 0 shares (%.3f target < 1 share) of %s @ ~$%.2f ($%.2f outlay) [SKIPPED - Schwab requires whole shares]",
                def_delta_shs, DEFENSIVE_ASSET, def_cur_px, def_delta_cash)
      }
      rebalance_tickets <- c(rebalance_tickets, ticket)
      order_records[[length(order_records) + 1]] <- list(
        symbol = DEFENSIVE_ASSET, side = "BUY", shares = round(def_delta_shs, 3),
        shares_whole = def_shs_whole, estimated_price = round(def_cur_px, 2),
        estimated_outlay = round(if (def_shs_whole >= 1) (def_shs_whole * def_cur_px) else def_delta_cash, 2), order_type = "MARKET",
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
      trim_def  <- abs(def_delta_shs)
      shs_whole <- as.integer(round(trim_def))
      proceeds  <- round(shs_whole * def_cur_px, 2)
      ticket <- if (shs_whole >= 1) {
        sprintf("SELL %d shares (%.3f target) of %s @ ~$%.2f ($%.2f proceeds - Trim Defense for Equity)",
                shs_whole, trim_def, DEFENSIVE_ASSET, def_cur_px, proceeds)
      } else {
        sprintf("SELL 0 shares (%.3f target < 1 share) of %s @ ~$%.2f [SKIPPED - Schwab requires whole shares]",
                trim_def, DEFENSIVE_ASSET, def_cur_px)
      }
      rebalance_tickets <- c(rebalance_tickets, ticket)
      order_records[[length(order_records) + 1]] <- list(
        symbol = DEFENSIVE_ASSET, side = "SELL", shares = round(trim_def, 3),
        shares_whole = shs_whole, estimated_price = round(def_cur_px, 2),
        estimated_outlay = round(if (shs_whole >= 1) proceeds else abs(def_delta_cash), 2), order_type = "MARKET",
        reason = "TRIM_DEFENSE_FOR_EQUITY", stop_loss_price = NULL
      )
      port_state$defensive_position$shares <- max(0, curr_def_shs - (if (shs_whole >= 1) shs_whole else trim_def))
      port_state$cash_balance <- port_state$cash_balance + (if (shs_whole >= 1) proceeds else abs(def_delta_cash))
    }
  }
  
  port_state$core_weight          <- CORE_WEIGHT
  port_state$satellite_weight     <- SATELLITE_WEIGHT
  port_state$core_asset           <- CORE_ASSET
  port_state$portfolio_margin_lev <- PORTFOLIO_MARGIN_LEV
  port_state$vol_target           <- VOL_TARGET
  port_state$defensive_asset      <- DEFENSIVE_ASSET
  port_state$trend_engine         <- TREND_ENGINE
  port_state$lmma_window          <- LMMA_WINDOW
  port_state$lmma_rsq_threshold   <- LMMA_RSQ_THRESHOLD
  port_state$use_stock_trend_gate <- USE_STOCK_TREND_GATE
  port_state$use_leverage_turbo   <- USE_LEVERAGE_TURBO
  port_state$turbo_margin_lev     <- TURBO_MARGIN_LEV
  port_state$use_stop_replacement <- USE_STOP_REPLACEMENT
  port_state$use_retention_buffer <- USE_RETENTION_BUFFER
  port_state$retention_buffer_rank <- RETENTION_BUFFER_RANK
  port_state$use_tax_aware_rotation <- USE_TAX_AWARE_ROTATION
  port_state$ltcg_bridge_days     <- LTCG_BRIDGE_DAYS
  port_state$ltcg_rank_max        <- LTCG_RANK_MAX
  port_state$avoid_wash_sales     <- AVOID_WASH_SALES
  port_state$tax_lot_method       <- TAX_LOT_METHOD
  port_state$max_per_industry     <- MAX_PER_INDUSTRY
  port_state$bear_sleeve          <- BEAR_SLEEVE
  port_state$stop_reallocate      <- STOP_REALLOCATE
  port_state$use_convex_sizing    <- USE_CONVEX_SIZING
  port_state$use_fip              <- USE_FIP
  port_state$disaster_stop_pct    <- DISASTER_STOP
  port_state$active_margin_today  <- active_margin
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
