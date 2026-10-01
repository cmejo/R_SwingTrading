#!/usr/bin/env python3
"""
Interactive Brokers (IBKR Pro) Automated Order Execution Bridge
Parses generated order tickets from LATEST_TICKET.txt, connects to IB Gateway / TWS,
and transmits exchange-level First-Triggers-OCO Bracket Orders:
  1. Parent Order: BUY Market Order (transmit=False)
  2. Child Stop-Loss: GTC Stop Order (transmit=False)
  3. Child Target 1: GTC Limit Order for 50% shares @ +1.5R (transmit=False)
  4. Child Target 2: GTC Limit Order for 50% shares @ +3.0R (transmit=True)

Usage:
  # Dry-run preview:
  python3 execute_ibkr.py --dry_run=true

  # Live execution (connects to IB Gateway on port 4001 or TWS on port 7496/7497):
  python3 execute_ibkr.py --dry_run=false --port=7497 --client_id=1
"""

import sys
import os
import re
import json
import argparse
from typing import List, Dict, Any

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

def parse_latest_tickets(ticket_file: str = "LATEST_TICKET.txt") -> List[Dict[str, Any]]:
    if not os.path.exists(ticket_file):
        print(f"[IBKRBridge] Error: Ticket file '{ticket_file}' not found.")
        sys.exit(1)

    with open(ticket_file, "r") as f:
        content = f.read()

    tickets = []
    ticket_blocks = re.findall(
        r"--- ORDER TICKET #(\d+): (\w+) \(P\(Up\): ([\d\.]+)%.*?Sector: ([^\)]+)\) ---\n(.*?)(?=(?:--- ORDER TICKET|========================================================================================|\Z))",
        content,
        re.DOTALL
    )

    for rank, sym, prob, sector, body in ticket_blocks:
        act_match = re.search(r"Action:\s+BUY\s+([\d\.]+)\s+SHARES", body)
        shares = float(act_match.group(1)) if act_match else 0.0

        if shares <= 0:
            continue

        stop_match = re.search(r"GTC Stop-Loss:\s+\$([\d\.]+)", body)
        stop_px = float(stop_match.group(1)) if stop_match else 0.0

        t1_match = re.search(r"Tier 1 \(\d+% = ([\d\.]+) shs\): Target \$([\d\.]+)", body)
        t1_shares = float(t1_match.group(1)) if t1_match else shares / 2.0
        t1_price = float(t1_match.group(2)) if t1_match else 0.0

        t2_match = re.search(r"Tier 2 \(\d+% = ([\d\.]+) shs\): Target \$([\d\.]+)", body)
        t2_shares = float(t2_match.group(1)) if t2_match else shares - t1_shares
        t2_price = float(t2_match.group(2)) if t2_match else 0.0

        alloc_match = re.search(r"Actual Outlay:\s+\$([\d\.]+)", body)
        outlay = float(alloc_match.group(1)) if alloc_match else 0.0

        tickets.append({
            "rank": int(rank),
            "symbol": sym.upper(),
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

def build_ibkr_bracket_spec(t: Dict[str, Any]) -> Dict[str, Any]:
    return {
        "broker": "Interactive Brokers (IBKR Pro)",
        "symbol": t["symbol"],
        "exchange": "SMART",
        "currency": "USD",
        "parent_order": {
            "action": "BUY",
            "orderType": "MKT",
            "totalQuantity": t["shares"],
            "transmit": False
        },
        "child_stop_loss": {
            "action": "SELL",
            "orderType": "STP",
            "auxPrice": t["stop_loss"],
            "totalQuantity": t["shares"],
            "tif": "GTC",
            "transmit": False
        },
        "child_profit_target_1": {
            "action": "SELL",
            "orderType": "LMT",
            "lmtPrice": t["tier1_target"],
            "totalQuantity": t["tier1_shares"],
            "tif": "GTC",
            "transmit": False,
            "note": "Tier 1: 50% scale-out (+1.5R)"
        },
        "child_profit_target_2": {
            "action": "SELL",
            "orderType": "LMT",
            "lmtPrice": t["tier2_target"],
            "totalQuantity": t["tier2_shares"],
            "tif": "GTC",
            "transmit": True,
            "note": "Tier 2: 50% runner (+3.0R)"
        }
    }

def execute_ibkr(tickets: List[Dict[str, Any]], host: str, port: int, client_id: int, dry_run: bool = True):
    print("================================================================================")
    print("        INTERACTIVE BROKERS (IBKR PRO) AUTOMATED ORDER EXECUTION               ")
    mode_str = "🟡 DRY RUN (Preview)" if dry_run else "🔴 LIVE EXECUTION"
    print("================================================================================")
    print(f" Mode:         {mode_str}")
    print(f" Target Host:  {host}:{port} (ClientID: {client_id})")
    print(f" Orders Found: {len(tickets)}")
    print("================================================================================")

    if dry_run:
        print("\n[IBKRBridge] [DRY RUN] Generated TWS / IB Gateway Order Specifications:")
        for t in tickets:
            print(f"\n--- Bracket Order Specification for {t['symbol']} ({t['shares']} shares) ---")
            print(json.dumps(build_ibkr_bracket_spec(t), indent=2))
        print("\n[IBKRBridge] Dry run complete. No live orders submitted.")
        return

    try:
        from ib_insync import IB, Stock, MarketOrder, StopOrder, LimitOrder
    except ImportError:
        print("[IBKRBridge] Error: 'ib_insync' is not installed.")
        print("Install via: pip3 install ib_insync")
        sys.exit(1)

    ib = IB()
    try:
        print(f"[IBKRBridge] Connecting to IB Gateway / TWS at {host}:{port}...")
        ib.connect(host, port, clientId=client_id, timeout=10)
        print("[IBKRBridge] Connected successfully to Interactive Brokers API.")
    except Exception as e:
        print(f"[IBKRBridge] Connection Error: {e}")
        print("Ensure IB Gateway or TWS is running and 'Enable ActiveX and Socket Clients' is checked.")
        sys.exit(1)

    for t in tickets:
        sym = t["symbol"]
        shares = t["shares"]
        contract = Stock(sym, "SMART", "USD")
        ib.qualifyContracts(contract)

        print(f"\n[IBKRBridge] Transmitting Bracket Order for {sym} ({shares} shs)...")
        # 1. Parent Market Buy Order
        parent = MarketOrder("BUY", shares, transmit=False)
        parent_trade = ib.placeOrder(contract, parent)
        parent_id = parent_trade.order.orderId

        # 2. Child Stop Loss
        stop_order = StopOrder("SELL", shares, t["stop_loss"], parentId=parent_id, transmit=False, tif="GTC")
        ib.placeOrder(contract, stop_order)

        # 3. Child Profit Target 1 (50% scale-out)
        t1_order = LimitOrder("SELL", t["tier1_shares"], t["tier1_target"], parentId=parent_id, transmit=False, tif="GTC")
        ib.placeOrder(contract, t1_order)

        # 4. Child Profit Target 2 (50% runner) - transmit=True arms the entire bracket!
        t2_order = LimitOrder("SELL", t["tier2_shares"], t["tier2_target"], parentId=parent_id, transmit=True, tif="GTC")
        ib.placeOrder(contract, t2_order)

        print(f"[IBKRBridge] -> Bracket Armed for {sym}: Parent ID {parent_id} | Stop: ${t['stop_loss']:.2f} | T1: ${t['tier1_target']:.2f} | T2: ${t['tier2_target']:.2f}")

    ib.sleep(2)
    ib.disconnect()
    print("\n[IBKRBridge] Live execution finished. Disconnected from IB Gateway.")

def main():
    parser = argparse.ArgumentParser(description="Interactive Brokers Automated Bracket Order Bridge")
    parser.add_argument("--dry_run", type=str, default="true", choices=["true", "false"])
    parser.add_argument("--host", type=str, default=os.environ.get("IBKR_HOST", "127.0.0.1"))
    parser.add_argument("--port", type=int, default=int(os.environ.get("IBKR_PORT", 7497)))
    parser.add_argument("--client_id", type=int, default=int(os.environ.get("IBKR_CLIENT_ID", 1)))
    parser.add_argument("--ticket_file", type=str, default="LATEST_TICKET.txt")
    args = parser.parse_args()

    is_dry = args.dry_run.lower() == "true"
    tickets = parse_latest_tickets(args.ticket_file)

    if not tickets:
        print("[IBKRBridge] No actionable BUY tickets found in ticket file.")
        return

    execute_ibkr(tickets, host=args.host, port=args.port, client_id=args.client_id, dry_run=is_dry)

if __name__ == "__main__":
    main()
