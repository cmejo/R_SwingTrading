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
      state$peak_equity    <- if (!is.null(state$peak_equity)) as.numeric(state$peak_equity) else max(state$total_capital, state$cash_balance, default_capital)
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
    peak_equity = default_capital,
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
  
  # Accumulate into existing position for same symbol or append new position
  existing_idx <- if (length(portfolio$positions) > 0) {
    which(sapply(portfolio$positions, function(p) p$symbol == toupper(symbol)))
  } else {
    integer(0)
  }
  if (length(existing_idx) > 0) {
    old_pos <- portfolio$positions[[existing_idx[1]]]
    comb_shares <- round(as.numeric(old_pos$shares) + shares, 4)
    comb_cost   <- round(as.numeric(old_pos$cost_basis) + cost_basis, 2)
    avg_price   <- round(comb_cost / max(1e-6, comb_shares), 2)
    
    portfolio$positions[[existing_idx[1]]] <- list(
      symbol         = toupper(symbol),
      shares         = comb_shares,
      entry_price    = avg_price,
      entry_date     = old_pos$entry_date,
      stop_loss      = max(as.numeric(old_pos$stop_loss), stop_loss),
      take_profit    = take_profit,
      cost_basis     = comb_cost,
      highest_price  = max(if (!is.null(old_pos$highest_price)) as.numeric(old_pos$highest_price) else 0, avg_price),
      status         = "OPEN"
    )
  } else {
    new_pos$highest_price <- entry_price
    portfolio$positions[[length(portfolio$positions) + 1]] <- new_pos
  }
  
  return(portfolio)
}

#' Record Trade Exit (Close or Partial Scale-Out)
#'
#' @param portfolio Portfolio state list.
#' @param symbol Stock ticker symbol.
#' @param exit_price Sale price per share.
#' @param exit_date Exit date string (YYYY-MM-DD). Defaults to Sys.Date().
#' @param reason Exit trigger reason.
#' @param shares_to_exit Optional quantity of shares to sell (default: NULL = full position).
#' @return Updated portfolio state list.
#' @export
record_exit <- function(portfolio, symbol, exit_price, exit_date = as.character(Sys.Date()), 
                        reason = "MANUAL_EXIT", shares_to_exit = NULL) {
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
  total_pos_shares <- as.numeric(pos$shares)
  
  # Determine if full or partial scale-out
  close_qty <- if (is.null(shares_to_exit) || shares_to_exit >= total_pos_shares) {
    total_pos_shares
  } else {
    round(as.numeric(shares_to_exit), 4)
  }
  
  portion <- close_qty / max(1e-6, total_pos_shares)
  portion_cost <- round(as.numeric(pos$cost_basis) * portion, 2)
  proceeds     <- round(close_qty * exit_price, 2)
  pnl_dollar   <- round(proceeds - portion_cost, 2)
  pnl_pct      <- round((exit_price / as.numeric(pos$entry_price) - 1) * 100, 2)
  
  # Return proceeds to cash
  portfolio$cash_balance  <- round(portfolio$cash_balance + proceeds, 2)
  portfolio$total_capital <- round(portfolio$total_capital + pnl_dollar, 2)
  
  closed_trade <- list(
    symbol      = pos$symbol,
    shares      = close_qty,
    entry_price = pos$entry_price,
    entry_date  = pos$entry_date,
    exit_price  = exit_price,
    exit_date   = exit_date,
    pnl_dollar  = pnl_dollar,
    pnl_pct     = pnl_pct,
    reason      = reason
  )
  portfolio$closed_trades[[length(portfolio$closed_trades) + 1]] <- closed_trade
  
  if (close_qty >= total_pos_shares - 1e-4) {
    portfolio$positions[[idx[1]]] <- NULL # Full exit
  } else {
    # Partial scale-out: update remaining open position
    rem_shares <- round(total_pos_shares - close_qty, 4)
    rem_cost   <- round(as.numeric(pos$cost_basis) - portion_cost, 2)
    portfolio$positions[[idx[1]]]$shares <- rem_shares
    portfolio$positions[[idx[1]]]$cost_basis <- rem_cost
  }
  
  return(portfolio)
}

#' Sync Portfolio Holdings with Live Market Prices & Check Triggers
#'
#' @param portfolio Portfolio state list.
#' @param current_prices Named numeric vector of latest closing / intraday prices.
#' @param current_date Date or string (YYYY-MM-DD). Defaults to Sys.Date().
#' @param is_friday Logical; if TRUE, evaluates conditional weekend holding vs Friday liquidation.
#' @param model_scan_df Optional data.frame from daily_signal scanner with columns (Symbol, Prob_Up, Weekly_Slope, Days_To_Earn).
#' @param macro_bullish Logical; whether QQQ Macro Gate is Bullish (Risk-On).
#' @return List containing updated position summaries, total equity, available slots, and alerts.
#' @export
sync_portfolio_with_market <- function(portfolio, current_prices, current_date = Sys.Date(), 
                                       is_friday = FALSE, model_scan_df = NULL, macro_bullish = TRUE) {
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
    Suggested_Stop = numeric(),
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
      # Count actual trading days (exclude weekends, safely handle same-day entries)
      ent_dt <- as.Date(p$entry_date)
      cur_dt <- as.Date(current_date)
      days_held <- if (cur_dt <= ent_dt) {
        0L
      } else {
        all_days_seq <- seq(ent_dt + 1, cur_dt, by = "day")
        as.integer(sum(!weekdays(all_days_seq) %in% c("Saturday", "Sunday")))
      }
      
      suggested_stop <- p$stop_loss
      # Breakeven escalation rule: If position is up by >= 1R, raise stop to Entry Price
      risk_1r <- p$entry_price - p$stop_loss
      if (risk_1r > 0 && cp >= (p$entry_price + risk_1r)) {
        suggested_stop <- round(max(suggested_stop, p$entry_price), 2)
      }
      
      # ATR Chandelier Trailing Stop for Tier 2 runners
      # Persist highest_price so the trailing stop only ratchets upward
      prev_highest <- if (!is.null(p$highest_price)) as.numeric(p$highest_price) else as.numeric(p$entry_price)
      highest_seen <- max(prev_highest, cp)
      portfolio$positions[[i]]$highest_price <- highest_seen
      
      atr_val <- if (!is.null(p$atr)) p$atr else (p$entry_price * 0.025)
      chandelier_stop <- round(highest_seen - (2.5 * atr_val), 2)
      if (cp >= (p$entry_price + risk_1r) && chandelier_stop > suggested_stop) {
        suggested_stop <- chandelier_stop
      }
      
      # Determine action trigger
      action <- "HOLD"
      status <- "ACTIVE"
      
      # 1. Stop-Loss & Take-Profit Triggers (Immediate Execution)
      # Use suggested_stop (which includes breakeven escalation and chandelier ratchet)
      if (cp <= suggested_stop) {
        action <- sprintf("SELL (STOP BREACHED at $%.2f)", suggested_stop)
        status <- "STOP_TRIGGERED"
        alerts <- c(alerts, sprintf("[STOP-LOSS HIT] %s breached stop at $%.2f (Current: $%.2f). Exit immediately.", sym, suggested_stop, cp))
      } else if (cp >= p$take_profit) {
        action <- "SELL (TAKE-PROFIT REACHED)"
        status <- "TARGET_TRIGGERED"
        alerts <- c(alerts, sprintf("[TARGET REACHED] %s hit take-profit target at $%.2f. Lock in gains!", sym, p$take_profit))
      } else if (days_held >= 5) {
        action <- "SELL (MAX 5-DAY TIME HORIZON)"
        status <- "TIME_EXIT_TRIGGERED"
        alerts <- c(alerts, sprintf("[TIME EXIT] %s held for %d trading days. Recycle capital into fresh setups.", sym, days_held))
      } else if (isTRUE(is_friday)) {
        # 2. Conditional Weekend Holding Engine
        sym_row <- if (!is.null(model_scan_df) && sym %in% model_scan_df$Symbol) {
          model_scan_df[model_scan_df$Symbol == sym, ]
        } else {
          NULL
        }
        
        prob_up <- if (!is.null(sym_row) && length(sym_row$Prob_Up) > 0) as.numeric(sym_row$Prob_Up[1]) else NA
        weekly_slope <- if (!is.null(sym_row) && length(sym_row$Weekly_Slope) > 0) as.numeric(sym_row$Weekly_Slope[1]) else NA
        days_to_earn <- if (!is.null(sym_row) && length(sym_row$Days_To_Earn) > 0) as.numeric(sym_row$Days_To_Earn[1]) else 999
        
        pass_macro    <- isTRUE(macro_bullish)
        pass_prob     <- (!is.na(prob_up) && prob_up >= 0.55)
        pass_weekly   <- (!is.na(weekly_slope) && weekly_slope > 0)
        pass_earnings <- (is.na(days_to_earn) || days_to_earn < 0 || days_to_earn > 7)
        pass_buffer   <- (cp > p$stop_loss)
        
        if (pass_macro && pass_prob && pass_weekly && pass_earnings && pass_buffer) {
          status <- "WEEKEND_HOLD_APPROVED"
          if (suggested_stop > p$stop_loss) {
            action <- sprintf("HOLD OVER WEEKEND (Ratchet Stop to Breakeven $%.2f)", suggested_stop)
            alerts <- c(alerts, sprintf("[WEEKEND HOLD APPROVED] %s: Strong momentum (P(Up)=%.1f%%, Weekly +%.1f%%, P&L: %+.2f%%). Hold over weekend; raise stop to Breakeven $%.2f.",
                                        sym, prob_up * 100, weekly_slope, pnl_pct, suggested_stop))
          } else {
            action <- "HOLD OVER WEEKEND (Momentum Intact)"
            alerts <- c(alerts, sprintf("[WEEKEND HOLD APPROVED] %s: Meets all criteria (P(Up)=%.1f%%, Weekly +%.1f%%, P&L: %+.2f%%). Hold into next week.",
                                        sym, prob_up * 100, weekly_slope, pnl_pct))
          }
        } else {
          status <- "WEEKEND_EXIT_TRIGGERED"
          fail_reasons <- character()
          if (!pass_macro) fail_reasons <- c(fail_reasons, "Macro Risk-Off")
          if (!pass_prob) fail_reasons <- c(fail_reasons, sprintf("P(Up) %.1f%% < 55%%", ifelse(is.na(prob_up), 0, prob_up * 100)))
          if (!pass_weekly) fail_reasons <- c(fail_reasons, "Weekly Bearish Trend")
          if (!pass_earnings) fail_reasons <- c(fail_reasons, sprintf("Earnings in %dd", as.integer(days_to_earn)))
          if (!pass_buffer) fail_reasons <- c(fail_reasons, "At/Below Stop Loss")
          
          reason_str <- paste(fail_reasons, collapse = ", ")
          action <- sprintf("SELL BEFORE 16:00 (Weekend Risk: %s)", reason_str)
          alerts <- c(alerts, sprintf("[DEFENSIVE WEEKEND EXIT] %s must be closed before 16:00 Friday (%s). P&L: %+.2f%%",
                                      sym, reason_str, pnl_pct))
        }
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
        Suggested_Stop = suggested_stop,
        Take_Profit = p$take_profit,
        Status = status,
        Action_Required = action,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  total_account_value <- round(portfolio$cash_balance + total_invested, 2)
  peak_equity <- if (!is.null(portfolio$peak_equity)) max(as.numeric(portfolio$peak_equity), total_account_value) else total_account_value
  drawdown_pct <- round(((total_account_value - peak_equity) / max(1.0, peak_equity)) * 100, 2)
  circuit_breaker_active <- (drawdown_pct <= -4.0)
  
  if (circuit_breaker_active) {
    alerts <- c(alerts, sprintf("[CIRCUIT BREAKER ACTIVE] Account Drawdown %+.2f%% exceeds -4.0%% ceiling! Safe f throttled by 50%%.", drawdown_pct))
  }
  
  available_slots <- max(0, portfolio$max_positions - nrow(pos_df))
  
  # Update portfolio state attributes
  portfolio$total_capital <- total_account_value
  portfolio$peak_equity   <- peak_equity
  portfolio$last_updated  <- as.character(Sys.time())
  
  return(list(
    holdings_df            = pos_df,
    total_invested         = total_invested,
    cash_balance           = portfolio$cash_balance,
    total_account_value    = total_account_value,
    peak_equity            = peak_equity,
    drawdown_pct           = drawdown_pct,
    circuit_breaker_active = circuit_breaker_active,
    available_slots        = available_slots,
    active_count           = nrow(pos_df),
    alerts                 = alerts,
    updated_portfolio      = portfolio
  ))
}
