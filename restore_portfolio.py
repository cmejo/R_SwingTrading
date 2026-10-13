#!/usr/bin/env python3
"""
restore_portfolio.py - Restore Active Positions to portfolio.json

Reconstructs the active positions recorded from the model monitor:
  - QQQ:  3 shares @ $754.01 (Current: $751.37)
  - PANW: 9 shares @ $403.69 (Current: $418.59)
  - PENG: 27 shares @ $72.50 (Current: $74.96)
"""

import os
import json
import datetime

PORTFOLIO_FILE = "portfolio.json"

def restore():
    # Calculate entry date ~ 2 trading days ago
    today = datetime.date.today()
    # Go back 2-4 calendar days to account for weekend
    entry_date = (today - datetime.timedelta(days=4 if today.weekday() < 2 else 2)).strftime("%Y-%m-%d")

    positions = [
        {
            "symbol": "QQQ",
            "shares": 3,
            "initial_shares": 3,
            "entry_price": 754.01,
            "current_price": 751.37,
            "entry_date": entry_date,
            "stop_loss": 754.01,         # Breakeven stop ratcheted
            "take_profit": 785.00,
            "tier1_target": 754.01,       # Tier 1 reached (+1.5R)
            "tier2_target": 795.00,
            "tier1_hit": True,
            "tier2_hit": False,
            "is_runner": False,
            "highest_price": 755.00,
            "atr": 10.50,
            "cost_basis": 2262.03,
            "status": "OPEN"
        },
        {
            "symbol": "PANW",
            "shares": 9,
            "initial_shares": 9,
            "entry_price": 403.69,
            "current_price": 418.59,
            "entry_date": entry_date,
            "stop_loss": 387.54,
            "take_profit": 427.91,
            "tier1_target": 419.85,
            "tier2_target": 435.50,
            "tier1_hit": False,
            "tier2_hit": False,
            "is_runner": False,
            "highest_price": 418.59,
            "atr": 8.08,
            "cost_basis": 3633.21,
            "status": "OPEN"
        },
        {
            "symbol": "PENG",
            "shares": 27,
            "initial_shares": 27,
            "entry_price": 72.50,
            "current_price": 74.96,
            "entry_date": entry_date,
            "stop_loss": 68.15,
            "take_profit": 79.03,
            "tier1_target": 76.85,
            "tier2_target": 81.20,
            "tier1_hit": False,
            "tier2_hit": False,
            "is_runner": False,
            "highest_price": 74.96,
            "atr": 2.18,
            "cost_basis": 1957.50,
            "status": "OPEN"
        }
    ]

    total_invested = sum(p["shares"] * p["current_price"] for p in positions)
    total_cost = sum(p["cost_basis"] for p in positions)
    
    # Assuming baseline $10,000 account size
    cash_balance = round(10000.0 - total_cost + sum((p["shares"] * p["current_price"]) - p["cost_basis"] for p in positions), 2)
    total_capital = round(cash_balance + total_invested, 2)

    data = {
        "total_capital": total_capital,
        "cash_balance": max(0.0, cash_balance),
        "max_positions": 5,
        "last_updated": str(datetime.datetime.now()),
        "positions": positions,
        "closed_trades": [],
        "peak_equity": max(10000.0, total_capital)
    }

    with open(PORTFOLIO_FILE, "w") as f:
        json.dump(data, f, indent=2)

    print(f" Successfully restored 3 active positions into {PORTFOLIO_FILE}:")
    for p in positions:
        pnl = (p["current_price"] - p["entry_price"]) * p["shares"]
        ret = (p["current_price"] / p["entry_price"] - 1.0) * 100.0
        print(f"  - {p['symbol']:4s}: {p['shares']:2d} shs @ ${p['entry_price']:.2f} (Current: ${p['current_price']:.2f} | PnL: ${pnl:+.2f} [{ret:+.2f}%])")
    print(f"\nTotal Capital: ${total_capital:,.2f} | Cash: ${cash_balance:,.2f} | Invested: ${total_invested:,.2f}")

if __name__ == "__main__":
    restore()
