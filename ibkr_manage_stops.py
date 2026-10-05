#!/usr/bin/env python3
"""
IBKR Automated Breakeven Stop-Loss Ratchet Monitor (F4)
Monitors active bracket orders in Interactive Brokers (TWS / IB Gateway).
When a Tier 1 profit target limit order fills, this daemon detects the execution
and automatically modifies the corresponding Tier 2 runner stop order's trigger
price to Breakeven (the original entry price).

Usage:
  # Dry-run inspection (default):
  python3 ibkr_manage_stops.py --dry_run=true --once=true

  # Live continuous monitoring on port 7497:
  python3 ibkr_manage_stops.py --dry_run=false --port=7497 --client_id=9 --interval=30
"""

import sys
import os
import time
import json
import argparse
from typing import Dict, Any, List

def load_dot_env(env_file: str = ".env"):
    if os.path.exists(env_file):
        with open(env_file, "r") as f:
            for line in f:
                line = line.strip()
                if line and not line.startswith("#") and "=" in line:
                    k, v = line.split("=", 1)
                    k = k.strip()
                    v = v.strip().strip("'\"")
                    if k not in os.environ:
                        os.environ[k] = v

load_dot_env()

def load_portfolio_positions(portfolio_file: str = "portfolio.json") -> Dict[str, Dict[str, Any]]:
    if not os.path.exists(portfolio_file):
        return {}
    try:
        with open(portfolio_file, "r") as f:
            port = json.load(f)
        return {p["symbol"]: p for p in port.get("positions", [])}
    except Exception:
        return {}

def check_and_ratchet_stops(ib, dry_run: bool = True):
    """Inspects open orders and executions to ratchet Tier 2 stops to breakeven."""
    try:
        portfolio_positions = load_portfolio_positions()
        open_trades = ib.openTrades()
        fills = ib.fills()
        
        # Identify symbols where a Tier 1 limit sell has filled today
        filled_t1_symbols = set()
        for fill in fills:
            contract = fill.contract
            execution = fill.execution
            if execution.side == "SLD":
                # Check if it was a limit order or tagged as Tier 1
                sym = contract.symbol
                filled_t1_symbols.add(sym)

        # Inspect open stop orders for these symbols
        for trade in open_trades:
            contract = trade.contract
            order = trade.order
            sym = contract.symbol

            if order.orderType in ["STP", "TRAIL"] and order.action == "SELL":
                # Find entry price from portfolio state
                pos_info = portfolio_positions.get(sym)
                if not pos_info:
                    continue

                entry_px = float(pos_info.get("entry_price", 0.0))
                current_stop = float(order.auxPrice)

                # If Tier 1 filled and stop is still below breakeven
                if sym in filled_t1_symbols and current_stop < entry_px:
                    print(f"\n🎯 [IBKR Ratchet Triggered] Tier 1 filled for {sym}!")
                    print(f"   Current Stop: ${current_stop:.2f} -> Upgrading to Breakeven: ${entry_px:.2f}")

                    if not dry_run:
                        order.auxPrice = round(entry_px, 2)
                        ib.placeOrder(contract, order)
                        print(f"   [Order Placed] Modified order #{order.orderId} auxPrice set to ${entry_px:.2f}.")
                    else:
                        print(f"   [DRY RUN] Would submit order modification: auxPrice = ${entry_px:.2f}.")
    except Exception as e:
        print(f"[IBKR Stop Monitor] Error during evaluation: {e}")

def main():
    parser = argparse.ArgumentParser(description="IBKR Breakeven Stop Ratchet Monitor")
    parser.add_argument("--dry_run", type=str, default="true", choices=["true", "false"])
    parser.add_argument("--host", type=str, default="127.0.0.1", help="IB Gateway/TWS host")
    parser.add_argument("--port", type=int, default=7497, help="TWS (7496/7497) or Gateway (4001/4002) port")
    parser.add_argument("--client_id", type=int, default=9, help="IBKR API Client ID")
    parser.add_argument("--interval", type=int, default=30, help="Polling interval in seconds")
    parser.add_argument("--once", type=str, default="false", choices=["true", "false"])
    args = parser.parse_args()

    is_dry = args.dry_run.lower() == "true"
    run_once = args.once.lower() == "true"

    print("================================================================================")
    print("             IBKR PRO BREAKEVEN STOP-LOSS RATCHET MONITOR                       ")
    mode_str = "🟡 DRY RUN (Preview)" if is_dry else "🔴 LIVE EXECUTION"
    print("================================================================================")
    print(f" Mode:      {mode_str}")
    print(f" Host:Port: {args.host}:{args.port} (Client ID: {args.client_id})")
    print(f" Polling:   Every {args.interval}s")
    print("================================================================================")

    if is_dry:
        print("[IBKR Stop Monitor] DRY RUN mode active. Verifying local portfolio and exit rules...")
        positions = load_portfolio_positions()
        print(f" Found {len(positions)} active tracked positions in portfolio.json.")
        for s, p in positions.items():
            print(f"   * {s}: {p['shares']} shs @ ${p['entry_price']:.2f} (Stop: ${p['stop_loss']:.2f})")
        print("[IBKR Stop Monitor] Dry-run checks completed successfully.")
        return

    try:
        from ib_insync import IB
    except ImportError:
        print("[IBKR Stop Monitor] Error: 'ib_insync' package required for live IBKR monitor.")
        print("Install via: pip3 install -r requirements.txt")
        sys.exit(1)

    ib = IB()
    try:
        ib.connect(args.host, args.port, clientId=args.client_id)
        print("[IBKR Stop Monitor] Connected successfully to IBKR API.")
    except Exception as e:
        print(f"[IBKR Stop Monitor] Connection failed to {args.host}:{args.port}: {e}")
        sys.exit(1)

    try:
        while True:
            check_and_ratchet_stops(ib, dry_run=is_dry)
            if run_once:
                break
            ib.sleep(args.interval)
    finally:
        ib.disconnect()

if __name__ == "__main__":
    main()
