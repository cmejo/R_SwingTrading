#!/usr/bin/env python3
"""
Reset Evaluation Baseline Script
Safely archives historical testing records and resets the model evaluation
baseline to today's date with the current broker account liquidation balance.

Usage:
  python3 reset_evaluation.py --balance 9602.36
  python3 reset_evaluation.py --sync             # Uses live balance from Schwab
"""

import os
import sys
import json
import shutil
import datetime
import argparse


PORTFOLIO_FILE = "portfolio.json"
HISTORY_FILE = "data/equity_history.json"
DATA_DIR = "data"


def reset_evaluation(starting_balance: float = None, sync_broker: bool = False):
    os.makedirs(DATA_DIR, exist_ok=True)
    today_str = datetime.date.today().strftime("%Y-%m-%d")
    timestamp_str = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")

    # 1. Broker Sync if requested
    if sync_broker:
        print("[Reset Evaluation] Querying live account balance from Charles Schwab...")
        res = os.system("python3 execute_broker.py --broker=schwab --sync --dry_run=false")
        if res != 0:
            print("⚠️ Notice: Broker sync returned non-zero code. Falling back to portfolio.json / specified balance.")

    # 2. Load Portfolio
    if not os.path.exists(PORTFOLIO_FILE):
        print(f"❌ Error: {PORTFOLIO_FILE} not found.")
        sys.exit(1)

    with open(PORTFOLIO_FILE, "r") as f:
        port = json.load(f)

    # 3. Archive Existing Equity History
    if os.path.exists(HISTORY_FILE):
        hist_backup = os.path.join(DATA_DIR, f"equity_history_archive_{timestamp_str}.json")
        shutil.copy2(HISTORY_FILE, hist_backup)
        print(f"📦 Archived prior equity history to: {hist_backup}")

    # 4. Archive Old Closed Trades
    closed_trades = port.get("closed_trades", [])
    if closed_trades:
        trades_backup = os.path.join(DATA_DIR, f"closed_trades_archive_{timestamp_str}.json")
        with open(trades_backup, "w") as f:
            json.dump(closed_trades, f, indent=2)
        print(f"📦 Archived {len(closed_trades)} prior closed testing trades to: {trades_backup}")

    # 5. Determine New Baseline Balance
    if starting_balance is not None and starting_balance > 0:
        base_balance = float(starting_balance)
    else:
        # Calculate current equity from portfolio positions + cash
        cash_val = float(port.get("cash_balance", 0.0))
        all_positions = port.get("positions", []) + port.get("hedge_positions", [])
        invested_val = sum(float(p.get("shares", 0)) * float(p.get("current_price", p.get("entry_price", 0))) for p in all_positions)
        base_balance = round(cash_val + invested_val, 2)
        if base_balance <= 0:
            base_balance = float(port.get("total_capital", 10000.0))

    # 6. Update Portfolio Baseline
    port["total_capital"] = base_balance
    port["peak_equity"] = base_balance
    port["closed_trades"] = []  # Reset completed trades count to 0 for clean evaluation
    port["last_updated"] = str(datetime.datetime.now())

    with open(PORTFOLIO_FILE, "w") as f:
        json.dump(port, f, indent=2)
    print(f"✅ Reset portfolio.json starting capital & peak equity to: ${base_balance:,.2f}")

    # 7. Create Clean Equity History Baseline Starting Today
    cash_balance = float(port.get("cash_balance", 0.0))
    n_pos = len(port.get("positions", [])) + len(port.get("hedge_positions", []))
    unrealized = sum(float(p.get("shares", 0)) * (float(p.get("current_price", p.get("entry_price", 0))) - float(p.get("entry_price", 0))) for p in port.get("positions", []))

    new_history = [
        {
            "date": today_str,
            "equity": round(base_balance, 2),
            "cash": round(cash_balance, 2),
            "positions": n_pos,
            "unrealized_pnl": round(unrealized, 2),
            "timestamp": f"{today_str} {datetime.datetime.now().strftime('%H:%M:%S')}",
            "is_reset_baseline": True
        }
    ]

    with open(HISTORY_FILE, "w") as f:
        json.dump(new_history, f, indent=2)
    print(f"✅ Created fresh equity history baseline for {today_str} starting at ${base_balance:,.2f}")

    # 8. Regenerate Performance Dashboard & Charts
    print("\n[Dashboard] Regenerating performance dashboard and charts...")
    os.system("python3 track_performance.py")


def main():
    parser = argparse.ArgumentParser(description="Reset Strategy Evaluation Baseline")
    parser.add_argument("--balance", type=float, default=None, help="Current account balance in Charles Schwab (e.g. 9602.36)")
    parser.add_argument("--sync", action="store_true", help="Sync directly from Charles Schwab broker API")
    args = parser.parse_args()

    reset_evaluation(starting_balance=args.balance, sync_broker=args.sync)


if __name__ == "__main__":
    main()
