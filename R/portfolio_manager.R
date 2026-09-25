# ==============================================================================
# Portfolio State Manager & Position Lifecycle Engine
# Handles active portfolio tracking, P&L calculation, exit triggers,
# and Friday weekend risk mitigation.
# ==============================================================================

suppressPackageStartupMessages({
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    install.packages("jsonlite", repos = "https://cloud.r-project.org")
  }
  library(jsonlite)
})

#' Initialize or Load Portfolio State
#'
#' @param path File path to portfolio.json.
#' @param default_capital Initial account capital if creating new state.
#' @param max_positions Maximum concurrent positions.
#' @return Portfolio state list.
#' @export
load_portfolio <- function(path = "portfolio.json", default_capital = 10000, max_positions = 5) {
  if (file.exists(path)) {
    tryCatch({
      state <- jsonlite::fromJSON(path, simplifyDataFrame = FALSE)
      # Ensure numeric types
      state$total_capital  <- as.numeric(state$total_capital)
      state$cash_balance   <- as.numeric(state$cash_balance)
      state$max_positions  <- as.integer(state$max_positions)
      if (is.null(state$positions)) state$positions <- list()
      if (is.null(state$closed_trades)) state$closed_trades <- list()
      return(state)
    }, error = function(e) {
      warning(sprintf("[PortfolioManager] Error reading %s: %s. Reinitializing.", path, e$message))
    })
  }
  
  # Default initial state
  default_state <- list(
    total_capital = default_capital,
    cash_balance = default_capital,
    max_positions = max_positions,
    last_updated = as.character(Sys.time()),
    positions = list(),
    closed_trades = list()
  )
  save_portfolio(default_state, path)
  return(default_state)
}

#' Save Portfolio State to JSON File
#'
#' @param state Portfolio state list.
#' @param path File path to portfolio.json.
#' @export
save_portfolio <- function(state, path = "portfolio.json") {
  state$last_updated <- as.character(Sys.time())
  json_data <- jsonlite::toJSON(state, pretty = TRUE, auto_unbox = TRUE)
  writeLines(json_data, con = path)
}

#' Record Trade Entry (Fill)
#'
#' @param portfolio Portfolio state list.
#' @param symbol Stock ticker symbol.
#' @param shares Quantity of shares bought (can be fractional).
#' @param entry_price Purchase price per share.
#' @param stop_loss GTC stop-loss price.
#' @param take_profit GTC take-profit price.
#' @param entry_date Entry date string (YYYY-MM-DD). Defaults to Sys.Date().
#' @return Updated portfolio state list.
#' @export
record_fill <- function(portfolio, symbol, shares, entry_price, stop_loss, take_profit, entry_date = as.character(Sys.Date())) {
  shares      <- as.numeric(shares)
  entry_price <- as.numeric(entry_price)
  stop_loss   <- as.numeric(stop_loss)
  take_profit <- as.numeric(take_profit)
  cost_basis  <- round(shares * entry_price, 2)
  
  if (cost_basis > (portfolio$cash_balance + 0.01)) {
    warning(sprintf("[PortfolioManager] Insufficient cash for %s: requires $%.2f, available $%.2f",
                    symbol, cost_basis, portfolio$cash_balance))
  }
  
  # Deduct cash
  portfolio$cash_balance <- max(0, round(portfolio$cash_balance - cost_basis, 2))
  
  new_pos <- list(
    symbol = toupper(symbol),
    shares = shares,
    entry_price = entry_price,
    entry_date = entry_date,
    stop_loss = stop_loss,
    take_profit = take_profit,
    cost_basis = cost_basis,
    status = "OPEN"
  )
  
  # Replace existing position for same symbol or append
  existing_idx <- if (length(portfolio$positions) > 0) {
    which(sapply(portfolio$positions, function(p) p$symbol == toupper(symbol)))
  } else {
    integer(0)
  }
  if (length(existing_idx) > 0) {
    portfolio$positions[[existing_idx[1]]] <- new_pos
  } else {
    portfolio$positions[[length(portfolio$positions) + 1]] <- new_pos
  }
  
  return(portfolio)
}

#' Record Trade Exit (Close)
#'
#' @param portfolio Portfolio state list.
#' @param symbol Stock ticker symbol.
#' @param exit_price Sale price per share.
#' @param exit_date Exit date string (YYYY-MM-DD). Defaults to Sys.Date().
#' @param reason Exit trigger reason.
#' @return Updated portfolio state list.
#' @export
record_exit <- function(portfolio, symbol, exit_price, exit_date = as.character(Sys.Date()), reason = "MANUAL_EXIT") {
  symbol <- toupper(symbol)
  idx <- if (length(portfolio$positions) > 0) {
    which(sapply(portfolio$positions, function(p) p$symbol == symbol))
  } else {
    integer(0)
  }
  
  if (length(idx) == 0) {
    warning(sprintf("[PortfolioManager] No active position found for %s to exit.", symbol))
    return(portfolio)
  }
  
  pos <- portfolio$positions[[idx[1]]]
  exit_price <- as.numeric(exit_price)
  proceeds   <- round(pos$shares * exit_price, 2)
  pnl_dollar <- round(proceeds - pos$cost_basis, 2)
  pnl_pct    <- round((exit_price / pos$entry_price - 1) * 100, 2)
  
  # Return proceeds to cash
  portfolio$cash_balance <- round(portfolio$cash_balance + proceeds, 2)
  portfolio$total_capital <- round(portfolio$total_capital + pnl_dollar, 2)
  
  closed_trade <- list(
    symbol = pos$symbol,
    shares = pos$shares,
    entry_price = pos$entry_price,
    entry_date = pos$entry_date,
    exit_price = exit_price,
    exit_date = exit_date,
    pnl_dollar = pnl_dollar,
    pnl_pct = pnl_pct,
    reason = reason
  )
  
  portfolio$closed_trades[[length(portfolio$closed_trades) + 1]] <- closed_trade
  portfolio$positions[[idx[1]]] <- NULL # Remove open position
  
  return(portfolio)
}

#' Sync Portfolio Holdings with Live Market Prices & Check Triggers
#'
#' @param portfolio Portfolio state list.
#' @param current_prices Named numeric vector of latest closing / intraday prices.
#' @param current_date Date or string (YYYY-MM-DD). Defaults to Sys.Date().
#' @param is_friday Logical; if TRUE, triggers mandatory weekend risk exit.
#' @return List containing updated position summaries, total equity, available slots, and alerts.
#' @export
sync_portfolio_with_market <- function(portfolio, current_prices, current_date = Sys.Date(), is_friday = FALSE) {
  current_date <- as.Date(current_date)
  pos_list <- portfolio$positions
  
  pos_df <- data.frame(
    Symbol = character(),
    Shares = numeric(),
    Entry_Price = numeric(),
    Current_Price = numeric(),
    Market_Value = numeric(),
    Unrealized_PnL = numeric(),
    Return_Pct = character(),
    Days_Held = integer(),
    Stop_Loss = numeric(),
    Take_Profit = numeric(),
    Status = character(),
    Action_Required = character(),
    stringsAsFactors = FALSE
  )
  
  total_invested <- 0
  alerts <- character()
  
  if (length(pos_list) > 0) {
    for (i in seq_along(pos_list)) {
      p <- pos_list[[i]]
      sym <- p$symbol
      cp <- if (!is.na(current_prices[sym])) as.numeric(current_prices[sym]) else p$entry_price
      
      mv <- round(p$shares * cp, 2)
      total_invested <- total_invested + mv
      pnl <- round(mv - p$cost_basis, 2)
      pnl_pct <- (cp / p$entry_price - 1) * 100
      days_held <- as.integer(current_date - as.Date(p$entry_date))
      
      # Determine action trigger
      action <- "HOLD"
      status <- "ACTIVE"
      
      if (isTRUE(is_friday)) {
        action <- "SELL (FRIDAY WEEKEND RISK CLOSE)"
        status <- "WEEKEND_EXIT_TRIGGERED"
        alerts <- c(alerts, sprintf("[WEEKEND RISK EXIT] %s must be closed before 16:00 EDT Friday. P&L: %+.2f%%", sym, pnl_pct))
      } else if (cp <= p$stop_loss) {
        action <- "SELL (STOP-LOSS BREACHED)"
        status <- "STOP_TRIGGERED"
        alerts <- c(alerts, sprintf("[STOP-LOSS HIT] %s breached stop at $%.2f (Current: $%.2f). Exit immediately.", sym, p$stop_loss, cp))
      } else if (cp >= p$take_profit) {
        action <- "SELL (TAKE-PROFIT REACHED)"
        status <- "TARGET_TRIGGERED"
        alerts <- c(alerts, sprintf("[TARGET REACHED] %s hit take-profit target at $%.2f. Lock in gains!", sym, p$take_profit))
      } else if (days_held >= 5) {
        action <- "SELL (MAX 5-DAY TIME HORIZON)"
        status <- "TIME_EXIT_TRIGGERED"
        alerts <- c(alerts, sprintf("[TIME EXIT] %s held for %d trading days. Recycle capital into fresh setups.", sym, days_held))
      }
      
      pos_df <- rbind(pos_df, data.frame(
        Symbol = sym,
        Shares = p$shares,
        Entry_Price = p$entry_price,
        Current_Price = cp,
        Market_Value = mv,
        Unrealized_PnL = pnl,
        Return_Pct = sprintf("%+.2f%%", pnl_pct),
        Days_Held = days_held,
        Stop_Loss = p$stop_loss,
        Take_Profit = p$take_profit,
        Status = status,
        Action_Required = action,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  total_account_value <- round(portfolio$cash_balance + total_invested, 2)
  available_slots <- max(0, portfolio$max_positions - nrow(pos_df))
  
  return(list(
    holdings_df = pos_df,
    total_invested = total_invested,
    cash_balance = portfolio$cash_balance,
    total_account_value = total_account_value,
    available_slots = available_slots,
    active_count = nrow(pos_df),
    alerts = alerts
  ))
}
