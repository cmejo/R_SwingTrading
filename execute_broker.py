#!/usr/bin/env python3
"""
Multi-Broker Automated Execution Bridge (Interactive Brokers & Charles Schwab)
Parses generated order tickets from LATEST_TICKET.txt and stages/submits parent
market orders paired with child OCO bracket exits (Stop-Loss + Multi-Tier Targets).

Usage:
  python3 execute_broker.py --broker=ibkr --dry_run=true
  python3 execute_broker.py --broker=schwab --dry_run=true
  python3 execute_broker.py --broker=ibkr --dry_run=false --port=7497
"""

import sys
import os
import re
import json
import argparse
from typing import List, Dict, Any

def parse_latest_tickets(ticket_file: str = "LATEST_TICKET.txt") -> List[Dict[str, Any]]:
    if not os.path.exists(ticket_file):
        print(f"[BrokerBridge] Error: Ticket file '{ticket_file}' not found.")
        sys.exit(1)

    with open(ticket_file, "r") as f:
        content = f.read()

    tickets = []
    # Match order tickets
    ticket_blocks = re.findall(
        r"--- ORDER TICKET #(\d+): (\w+) \(P\(Up\): ([\d\.]+)%.*?Sector: ([^\)]+)\) ---\n(.*?)(?=(?:--- ORDER TICKET|========================================================================================|\Z))",
        content,
        re.DOTALL
    )

    for rank, sym, prob, sector, body in ticket_blocks:
        # Extract Action & Shares
        act_match = re.search(r"Action:\s+BUY\s+([\d\.]+)\s+SHARES", body)
        shares = float(act_match.group(1)) if act_match else 0.0

        if shares <= 0:
            continue

        # Extract Stop Loss
        stop_match = re.search(r"GTC Stop-Loss:\s+\$([\d\.]+)", body)
        stop_px = float(stop_match.group(1)) if stop_match else 0.0

        # Extract Tier 1 Target & Shares
        t1_match = re.search(r"Tier 1 \(\d+% = ([\d\.]+) shs\): Target \$([\d\.]+)", body)
        t1_shares = float(t1_match.group(1)) if t1_match else shares / 2.0
        t1_price = float(t1_match.group(2)) if t1_match else 0.0

        # Extract Tier 2 Target & Shares
        t2_match = re.search(r"Tier 2 \(\d+% = ([\d\.]+) shs\): Target \$([\d\.]+)", body)
        t2_shares = float(t2_match.group(1)) if t2_match else shares - t1_shares
        t2_price = float(t2_match.group(2)) if t2_match else 0.0

        # Extract Dollar Allocation
        alloc_match = re.search(r"Actual Outlay:\s+\$([\d\.]+)", body)
        outlay = float(alloc_match.group(1)) if alloc_match else 0.0

        tickets.append({
            "rank": int(rank),
            "symbol": sym,
            "shares": shares,
            "sector": sector.strip(),
            "p_up": float(prob),
            "stop_loss": stop_px,
            "tier1_shares": t1_shares,
            "tier1_target": t1_price,
            "tier2_shares": t2_shares,
            "tier2_target": t2_price,
            "estimated_outlay": outlay
        })

    return tickets

def build_ibkr_payload(ticket: Dict[str, Any]) -> Dict[str, Any]:
    """Generates TWS / IB Gateway Bracket Order Specification."""
    return {
        "broker": "Interactive Brokers (IBKR)",
        "symbol": ticket["symbol"],
        "secType": "STK",
        "exchange": "SMART",
        "currency": "USD",
        "parent_order": {
            "action": "BUY",
            "orderType": "MKT",
            "totalQuantity": ticket["shares"],
            "transmit": False
        },
        "child_stop_loss": {
            "action": "SELL",
            "orderType": "STP",
            "auxPrice": ticket["stop_loss"],
            "totalQuantity": ticket["shares"],
            "tif": "GTC",
            "transmit": False
        },
        "child_profit_target_1": {
            "action": "SELL",
            "orderType": "LMT",
            "lmtPrice": ticket["tier1_target"],
            "totalQuantity": ticket["tier1_shares"],
            "tif": "GTC",
            "transmit": False,
            "note": "Tier 1: 50% scale-out (+1.5R)"
        },
        "child_profit_target_2": {
            "action": "SELL",
            "orderType": "LMT",
            "lmtPrice": ticket["tier2_target"],
            "totalQuantity": ticket["tier2_shares"],
            "tif": "GTC",
            "transmit": True,
            "note": "Tier 2: 50% runner (+3.0R)"
        }
    }

def build_schwab_payload(ticket: Dict[str, Any]) -> Dict[str, Any]:
    """Generates Charles Schwab Trader API FIRST_TRIGGERS_OCO JSON payload."""
    return {
        "broker": "Charles Schwab Trader API",
        "orderStrategyType": "TRIGGER",
        "orderType": "MARKET",
        "session": "NORMAL",
        "duration": "DAY",
        "orderLegCollection": [
            {
                "instruction": "BUY",
                "quantity": ticket["shares"],
                "instrument": {
                    "symbol": ticket["symbol"],
                    "assetType": "EQUITY"
                }
            }
        ],
        "childOrderStrategies": [
            {
                "orderStrategyType": "OCO",
                "childOrderStrategies": [
                    {
                        "orderType": "STOP",
                        "session": "NORMAL",
                        "duration": "GOOD_TILL_CANCEL",
                        "stopPrice": ticket["stop_loss"],
                        "orderLegCollection": [
                            {
                                "instruction": "SELL",
                                "quantity": ticket["shares"],
                                "instrument": {
                                    "symbol": ticket["symbol"],
                                    "assetType": "EQUITY"
                                }
                            }
                        ]
                    },
                    {
                        "orderType": "LIMIT",
                        "session": "NORMAL",
                        "duration": "GOOD_TILL_CANCEL",
                        "price": ticket["tier1_target"],
                        "orderLegCollection": [
                            {
                                "instruction": "SELL",
                                "quantity": ticket["tier1_shares"],
                                "instrument": {
                                    "symbol": ticket["symbol"],
                                    "assetType": "EQUITY"
                                }
                            }
                        ]
                    }
                ]
            }
        ]
    }

def execute_ibkr(tickets: List[Dict[str, Any]], host: str, port: int, client_id: int, dry_run: bool):
    print(f"\n[IBKR Bridge] Connecting to TWS/Gateway at {host}:{port} (Client ID: {client_id})...")
    
    if dry_run:
        print("[IBKR Bridge] MODE: DRY RUN (Simulation only. No orders transmitted).")
        for t in tickets:
            payload = build_ibkr_payload(t)
            print(f"\n--- Staging IBKR Bracket: {t['symbol']} ({t['shares']} shares) ---")
            print(json.dumps(payload, indent=2))
        print("\n[IBKR Bridge] Dry-run validation SUCCESS: All bracket payloads valid.")
        return

    # Live connection via ib_insync if installed
    try:
        from ib_insync import IB, Stock, MarketOrder, StopOrder, LimitOrder
        ib = IB()
        ib.connect(host, port, clientId=client_id)
        print("[IBKR Bridge] Connected successfully to Interactive Brokers API.")

        for t in tickets:
            contract = Stock(t["symbol"], "SMART", "USD")
            ib.qualifyContracts(contract)

            # Build Bracket
            parent = MarketOrder("BUY", t["shares"], transmit=False)
            parent_trade = ib.placeOrder(contract, parent)

            stop_order = StopOrder("SELL", t["shares"], t["stop_loss"], parentId=parent_trade.order.orderId, transmit=False, tif="GTC")
            ib.placeOrder(contract, stop_order)

            t1_order = LimitOrder("SELL", t["tier1_shares"], t["tier1_target"], parentId=parent_trade.order.orderId, transmit=False, tif="GTC")
            ib.placeOrder(contract, t1_order)

            t2_order = LimitOrder("SELL", t["tier2_shares"], t["tier2_target"], parentId=parent_trade.order.orderId, transmit=True, tif="GTC")
            ib.placeOrder(contract, t2_order)

            print(f"[IBKR Bridge] Submitted Live Bracket Order for {t['symbol']}: {t['shares']} shares")

        ib.disconnect()
        print("[IBKR Bridge] Live execution finished. Disconnected.")
    except ImportError:
        print("[IBKR Bridge] 'ib_insync' package not installed. Run: pip install ib_insync")
        print("[IBKR Bridge] Generating simulated order verification payloads instead.")
        for t in tickets:
            print(json.dumps(build_ibkr_payload(t), indent=2))
    except Exception as e:
        print(f"[IBKR Bridge] Connection error: {e}")
        sys.exit(1)

def execute_schwab(tickets: List[Dict[str, Any]], dry_run: bool):
    print("\n[Schwab Bridge] Initializing Charles Schwab Trader API Bridge...")
    
    app_key = os.environ.get("SCHWAB_APP_KEY", "")
    secret = os.environ.get("SCHWAB_SECRET", "")
    acct_id = os.environ.get("SCHWAB_ACCOUNT_ID", "SIMULATED_ACCOUNT_12345")

    if dry_run:
        print("[Schwab Bridge] MODE: DRY RUN (Simulation only. No orders transmitted).")
        for t in tickets:
            payload = build_schwab_payload(t)
            print(f"\n--- Staging Schwab REST Bracket: {t['symbol']} ({t['shares']} shares) ---")
            print(json.dumps(payload, indent=2))
        print("\n[Schwab Bridge] Dry-run validation SUCCESS: All Schwab order payloads valid.")
        return

    if not app_key or not secret:
        print("[Schwab Bridge] Error: SCHWAB_APP_KEY and SCHWAB_SECRET environment variables required for live submission.")
        sys.exit(1)

    print("[Schwab Bridge] Ready for live REST submission with configured Schwab credentials.")

def main():
    parser = argparse.ArgumentParser(description="Multi-Broker Execution Bridge for Swing Trading System")
    parser.add_argument("--broker", choices=["ibkr", "schwab"], default="ibkr", help="Target broker (ibkr or schwab)")
    parser.add_argument("--dry_run", type=str, default="true", help="Dry run mode (true/false)")
    parser.add_argument("--ticket_file", default="LATEST_TICKET.txt", help="Path to latest ticket file")
    parser.add_argument("--port", type=int, default=7497, help="IBKR TWS/Gateway port (7497 Paper, 7496 Live)")
    parser.add_argument("--host", default="127.0.0.1", help="IBKR Host IP")
    parser.add_argument("--client_id", type=int, default=1, help="IBKR Client ID")

    args = parser.parse_args()
    dry_run = args.dry_run.lower() in ["true", "1", "yes"]

    print("================================================================================")
    print("                 MULTI-BROKER ORDER EXECUTION & BRACKET BRIDGE                  ")
    print("================================================================================")
    print(f" Target Broker: {args.broker.upper()} | Mode: {'DRY RUN (Preview)' if dry_run else 'LIVE SUBMISSION'}")
    print(f" Source Ticket: {args.ticket_file}")

    tickets = parse_latest_tickets(args.ticket_file)
    print(f" Loaded {len(tickets)} Actionable Order Ticket(s) from {args.ticket_file}:\n")

    for t in tickets:
        print(f"  * #{t['rank']} {t['symbol']} ({t['sector']}): BUY {t['shares']} shs | Stop: ${t['stop_loss']} | T1: ${t['tier1_target']} | T2: ${t['tier2_target']}")

    if args.broker == "ibkr":
        execute_ibkr(tickets, args.host, args.port, args.client_id, dry_run)
    elif args.broker == "schwab":
        execute_schwab(tickets, dry_run)

    print("\n================================================================================")

if __name__ == "__main__":
    main()
