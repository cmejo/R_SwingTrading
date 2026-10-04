#!/usr/bin/env Rscript
# ==============================================================================
# CLI Trade Manager: Record fills, record exits, and monitor active portfolio
# Usage:
#   Rscript trade_manager.R --status
#   Rscript trade_manager.R --buy=AMD:9.26:614.61:555.76:702.88
#   Rscript trade_manager.R --sell=AMD:635.00
#   Rscript trade_manager.R --reset
# ==============================================================================

source("R/portfolio_manager.R")

args <- commandArgs(trailingOnly = TRUE)
state_file <- "portfolio.json"

for (arg in args) {
  if (startsWith(arg, "--portfolio_file=")) {
    state_file <- sub("^--portfolio_file=", "", arg)
  }
}

portfolio <- load_portfolio(state_file)

if (length(args) == 0 || "--status" %in% args) {
  cat("\n================================================================================\n")
  cat("                         ACTIVE PORTFOLIO & CASH STATUS                         \n")
  cat("================================================================================\n")
  cat(sprintf(" Total Starting Capital: $%.2f\n", portfolio$total_capital))
  cat(sprintf(" Available Cash Balance: $%.2f\n", portfolio$cash_balance))
  cat(sprintf(" Open Positions:         %d of %d allowed\n", length(portfolio$positions), portfolio$max_positions))
  cat(sprintf(" Last Updated:           %s\n", portfolio$last_updated))
  cat("--------------------------------------------------------------------------------\n")
  
  if (length(portfolio$positions) == 0) {
    cat(" No open positions. Portfolio is 100% in CASH.\n")
  } else {
    pos_df <- do.call(rbind, lapply(portfolio$positions, function(p) {
      data.frame(
        Symbol = p$symbol,
        Shares = p$shares,
        Entry_Price = sprintf("$%.2f", p$entry_price),
        Cost_Basis = sprintf("$%.2f", p$cost_basis),
        Stop_Loss = sprintf("$%.2f", p$stop_loss),
        Take_Profit = sprintf("$%.2f", p$take_profit),
        Entry_Date = p$entry_date,
        stringsAsFactors = FALSE
      )
    }))
    print(pos_df, row.names = FALSE)
  }
  
  if (length(portfolio$closed_trades) > 0) {
    cat("\n--------------------------------------------------------------------------------\n")
    cat(sprintf(" Closed Trades History (%d completed):\n", length(portfolio$closed_trades)))
    closed_df <- do.call(rbind, lapply(portfolio$closed_trades, function(ct) {
      data.frame(
        Symbol = ct$symbol,
        Shares = ct$shares,
        Entry = sprintf("$%.2f", ct$entry_price),
        Exit = sprintf("$%.2f", ct$exit_price),
        PnL_Dollar = sprintf("%+7.2f", ct$pnl_dollar),
        PnL_Pct = sprintf("%+6.2f%%", ct$pnl_pct),
        Reason = ct$reason,
        Exit_Date = ct$exit_date,
        stringsAsFactors = FALSE
      )
    }))
    print(closed_df, row.names = FALSE)
  }
  cat("================================================================================\n\n")
  quit(save = "no", status = 0)
}

for (arg in args) {
  if (startsWith(arg, "--buy=")) {
    val <- sub("^--buy=", "", arg)
    parts <- strsplit(val, "[,:]")[[1]]
    if (length(parts) < 3) {
      stop("Invalid format for --buy. Use: --buy=SYMBOL:SHARES:ENTRY_PRICE[:STOP_LOSS][:TAKE_PROFIT]")
    }
    sym <- parts[1]
    shs <- as.numeric(parts[2])
    ent <- as.numeric(parts[3])
    stp <- if (length(parts) >= 4) as.numeric(parts[4]) else round(ent * 0.95, 2)
    tgt <- if (length(parts) >= 5) as.numeric(parts[5]) else round(ent * 1.10, 2)
    
    portfolio <- record_fill(portfolio, sym, shs, ent, stp, tgt)
    save_portfolio(portfolio, state_file)
    cat(sprintf("[TradeManager] Recorded BUY for %s: %.3f shares @ $%.2f (Cost: $%.2f, Stop: $%.2f, Target: $%.2f)\n",
                toupper(sym), shs, ent, shs * ent, stp, tgt))
    cat(sprintf("[TradeManager] Remaining Cash: $%.2f\n", portfolio$cash_balance))
  }
  
  if (startsWith(arg, "--sell=")) {
    val <- sub("^--sell=", "", arg)
    parts <- strsplit(val, "[,:]")[[1]]
    sym <- parts[1]
    px  <- as.numeric(parts[2])
    reason <- if (length(parts) >= 3) parts[3] else "MANUAL_CLOSE"
    
    portfolio <- record_exit(portfolio, sym, px, reason = reason)
    save_portfolio(portfolio, state_file)
    cat(sprintf("[TradeManager] Recorded EXIT for %s @ $%.2f (Reason: %s)\n", toupper(sym), px, reason))
    cat(sprintf("[TradeManager] New Cash Balance: $%.2f | Total Capital: $%.2f\n",
                portfolio$cash_balance, portfolio$total_capital))
  }
  
  if (startsWith(arg, "--deposit=")) {
    dep <- as.numeric(sub("^--deposit=", "", arg))
    if (is.na(dep) || dep <= 0) stop("Invalid deposit amount.")
    portfolio$cash_balance <- round(portfolio$cash_balance + dep, 2)
    portfolio$total_capital <- round(portfolio$total_capital + dep, 2)
    portfolio$peak_equity <- max(if (!is.null(portfolio$peak_equity)) portfolio$peak_equity else 0, portfolio$total_capital)
    save_portfolio(portfolio, state_file)
    cat(sprintf("[TradeManager] Deposited $%.2f. New Cash Balance: $%.2f | Total Capital: $%.2f\n",
                dep, portfolio$cash_balance, portfolio$total_capital))
  }

  if (startsWith(arg, "--withdraw=")) {
    wth <- as.numeric(sub("^--withdraw=", "", arg))
    if (is.na(wth) || wth <= 0) stop("Invalid withdrawal amount.")
    if (wth > portfolio$cash_balance) {
      stop(sprintf("Cannot withdraw $%.2f. Available cash is only $%.2f.", wth, portfolio$cash_balance))
    }
    portfolio$cash_balance <- round(portfolio$cash_balance - wth, 2)
    portfolio$total_capital <- round(portfolio$total_capital - wth, 2)
    portfolio$peak_equity <- max(0, (if (!is.null(portfolio$peak_equity)) portfolio$peak_equity else portfolio$total_capital) - wth)
    save_portfolio(portfolio, state_file)
    cat(sprintf("[TradeManager] Withdrew $%.2f. New Cash Balance: $%.2f | Total Capital: $%.2f\n",
                wth, portfolio$cash_balance, portfolio$total_capital))
  }

  if (arg == "--reset") {
    portfolio <- list(
      total_capital = 10000,
      cash_balance = 10000,
      max_positions = 5,
      last_updated = as.character(Sys.time()),
      positions = list(),
      closed_trades = list()
    )
    save_portfolio(portfolio, state_file)
    cat("[TradeManager] Portfolio state reset to default ($10,000 cash, 0 positions).\n")
  }
}
