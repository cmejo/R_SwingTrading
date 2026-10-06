#!/usr/bin/env python3
"""
Robinhood Automated Order Execution Bridge
Parses generated order tickets from LATEST_TICKET.txt, authenticates with Robinhood
via robin_stocks, and submits entry orders with capital/share sizing.
Maintains active positions in 'robinhood_positions.json' for the background exit poller.

Usage:
  python3 execute_robinhood.py --dry_run=true
  python3 execute_robinhood.py --dry_run=false
"""

import sys
import os
import re
import json
import argparse
from datetime import datetime, date
from typing import List, Dict, Any

POSITIONS_FILE = "robinhood_positions.json"
PORTFOLIO_FILE = "portfolio.json"

def sync_entry_to_portfolio_json(symbol: str, shares: float, entry_price: float, stop_loss: float, take_profit: float, entry_date: str, portfolio_file: str = "portfolio.json"):
    """Synchronizes filled Robinhood buy order into portfolio.json."""
    if not os.path.exists(portfolio_file) or os.path.getsize(portfolio_file) == 0:
        port = {
            "total_capital": 10000.0,
            "cash_balance": 10000.0,
            "max_positions": 5,
            "peak_equity": 10000.0,
            "last_updated": str(datetime.now()),
            "positions": [],
            "closed_trades": []
        }
    else:
        try:
            with open(portfolio_file, "r") as f:
                port = json.load(f)
        except Exception:
            return

    cost_basis = round(shares * entry_price, 2)
    port["cash_balance"] = max(0.0, round(float(port.get("cash_balance", 10000.0)) - cost_basis, 2))
    port["last_updated"] = str(datetime.now())

    matched = [p for p in port.get("positions", []) if p.get("symbol") == symbol]
    if matched:
        p = matched[0]
        comb_shares = round(float(p.get("shares", 0.0)) + shares, 4)
        comb_cost = round(float(p.get("cost_basis", 0.0)) + cost_basis, 2)
        avg_price = round(comb_cost / max(1e-6, comb_shares), 2)
        p["shares"] = comb_shares
        p["entry_price"] = avg_price
        p["cost_basis"] = comb_cost
        p["stop_loss"] = max(float(p.get("stop_loss", 0.0)), stop_loss)
        p["take_profit"] = take_profit
        p["highest_price"] = max(float(p.get("highest_price", 0.0)), avg_price)
    else:
        port.setdefault("positions", []).append({
            "symbol": symbol,
            "shares": shares,
            "entry_price": round(entry_price, 2),
            "entry_date": entry_date,
            "stop_loss": round(stop_loss, 2),
            "take_profit": round(take_profit, 2),
            "cost_basis": cost_basis,
            "highest_price": round(entry_price, 2),
            "status": "OPEN"
        })

    try:
        with open(portfolio_file, "w") as f:
            json.dump(port, f, indent=2)
        print(f"[RobinhoodBridge] Synced {symbol} fill to {portfolio_file} ({shares} shs @ ${entry_price:.2f}).")
    except Exception as e:
        print(f"[RobinhoodBridge] Warning: Failed to write to {portfolio_file}: {e}")


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
        print(f"[RobinhoodBridge] Error: Ticket file '{ticket_file}' not found.")
        sys.exit(1)

    with open(ticket_file, "r") as f:
        content = f.read()

    tickets = []
    ticket_blocks = re.findall(
        r"--- ORDER TICKET #(\d+): ([A-Za-z0-9\.\-]+) \(P\(Up\): ([\d\.]+)%.*?Sector: ([^\)]+)\) ---\n(.*?)(?=(?:--- ORDER TICKET|========================================================================================|\Z))",
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

def load_robinhood_positions() -> Dict[str, Any]:
    if os.path.exists(POSITIONS_FILE):
        try:
            with open(POSITIONS_FILE, "r") as f:
                return json.load(f)
        except Exception:
            return {"active_positions": {}}
    return {"active_positions": {}}

def save_robinhood_positions(data: Dict[str, Any]):
    with open(POSITIONS_FILE, "w") as f:
        json.dump(data, f, indent=2)

def login_robinhood():
    try:
        import robin_stocks.robinhood as rh
    except ImportError:
        print("[RobinhoodBridge] Error: 'robin_stocks' is not installed.")
        print("Install via: pip3 install robin_stocks pyotp")
        sys.exit(1)

    username = os.environ.get("ROBINHOOD_USERNAME")
    password = os.environ.get("ROBINHOOD_PASSWORD")
    totp_secret = os.environ.get("ROBINHOOD_TOTP_KEY")

    if not username or not password:
        print("[RobinhoodBridge] Error: ROBINHOOD_USERNAME and ROBINHOOD_PASSWORD must be set in .env")
        sys.exit(1)

    mfa_code = None
    if totp_secret:
        try:
            import pyotp
            totp = pyotp.TOTP(totp_secret)
            mfa_code = totp.now()
        except ImportError:
            print("[RobinhoodBridge] Note: 'pyotp' not installed. If MFA is required, run: pip3 install pyotp")

    login_res = rh.login(
        username=username,
        password=password,
        mfa_code=mfa_code,
        expiresIn=86400,
        by_sms=True if not mfa_code else False
    )
    return rh, login_res

def wait_for_fill(rh, order_id: str, timeout_sec: int = 60, poll_sec: int = 3) -> Dict[str, Any]:
    """Poll Robinhood until the order is filled, rejected, or the timeout passes.
    Returns {'state', 'filled_qty', 'avg_price'}."""
    import time
    deadline = time.time() + timeout_sec
    info = {}
    while time.time() < deadline:
        try:
            info = rh.orders.get_stock_order_info(order_id) or {}
        except Exception as e:
            print(f"   [WARNING] Could not fetch order status: {e}")
            info = {}
        state = info.get("state", "unknown")
        if state in ("filled", "rejected", "cancelled", "failed"):
            break
        time.sleep(poll_sec)
    filled_qty = float(info.get("cumulative_quantity") or 0.0)
    avg_px = info.get("average_price")
    return {
        "state": info.get("state", "unknown"),
        "filled_qty": filled_qty,
        "avg_price": float(avg_px) if avg_px not in (None, "") else None,
    }


def execute_buys(tickets: List[Dict[str, Any]], dry_run: bool = True):
    print(f"\n[RobinhoodBridge] Processing {len(tickets)} Actionable Order Tickets (Dry Run = {dry_run})...")
    positions_data = load_robinhood_positions()
    active_positions = positions_data.get("active_positions", {})

    rh = None
    if not dry_run:
        rh, _ = login_robinhood()
        profile = rh.profiles.load_account_profile() or {}
        raw_bp = profile.get("buying_power")
        if raw_bp in (None, ""):
            raw_bp = profile.get("portfolio_cash", 0.0)
        buying_power = float(raw_bp or 0.0)
        print(f"[RobinhoodBridge] Logged in. Total Available Buying Power: ${buying_power:,.2f}")

    for t in tickets:
        sym = t["symbol"]
        shares = t["shares"]
        outlay = t["estimated_outlay"]

        if sym in active_positions:
            print(f"[RobinhoodBridge] Skipping {sym}: Already an open position in {POSITIONS_FILE}.")
            continue

        print(f"\n-> Staging Buy: {sym} | Shares: {shares} | Target Outlay: ${outlay:.2f}")
        print(f"   Stop-Loss: ${t['stop_loss']:.2f} | T1 Target: ${t['tier1_target']:.2f} | T2 Target: ${t['tier2_target']:.2f}")

        filled_shares = shares
        entry_px = outlay / shares if shares > 0 else 0.0

        if dry_run:
            print(f"   [DRY RUN] Would submit market buy order for {shares} shares of {sym} on Robinhood.")
        else:
            try:
                # Submit Market Buy order by shares
                order = rh.orders.order_buy_market_by_quantity(
                    symbol=sym,
                    quantity=shares,
                    timeInForce="gfd"
                )
            except Exception as e:
                print(f"   [ERROR] Failed to execute order on Robinhood: {e}")
                continue

            if not order or "id" not in order or order.get("state") in ("rejected", "failed", "cancelled"):
                print(f"   [ERROR] Order rejected or not accepted: {order}. Not tracking {sym}.")
                continue

            print(f"   [SUBMITTED] Order ID: {order['id']} | State: {order.get('state')}. Waiting for fill...")
            fill = wait_for_fill(rh, order["id"])
            if fill["filled_qty"] <= 0:
                print(f"   [WARNING] {sym} order not filled (state: {fill['state']}). Not tracking it. "
                      f"Check the Robinhood app and cancel or record it manually.")
                continue
            if fill["filled_qty"] < shares:
                print(f"   [WARNING] Partial fill: {fill['filled_qty']} of {shares} shares (state: {fill['state']}).")
            filled_shares = fill["filled_qty"]
            if fill["avg_price"]:
                entry_px = fill["avg_price"]
            print(f"   [FILLED] {filled_shares} shares of {sym} @ ${entry_px:.2f}")

        # Scale tier sizes to what was actually filled
        t1_shares = round(filled_shares / 2.0, 4)
        t2_shares = round(filled_shares - t1_shares, 4)

        today_date_str = str(date.today())
        # Register position for local background stop/target monitoring (Fix B2)
        active_positions[sym] = {
            "symbol": sym,
            "shares": filled_shares,
            "entry_date": today_date_str,
            "entry_estimated_price": entry_px,
            "stop_loss": t["stop_loss"],
            "tier1_shares": t1_shares,
            "tier1_target": t["tier1_target"],
            "tier1_executed": False,
            "tier2_shares": t2_shares,
            "tier2_target": t["tier2_target"],
            "tier2_executed": False,
            "breakeven_stop_active": False
        }

        if not dry_run:
            sync_entry_to_portfolio_json(
                symbol=sym,
                shares=filled_shares,
                entry_price=entry_px,
                stop_loss=t["stop_loss"],
                take_profit=t["tier2_target"],
                entry_date=today_date_str
            )

    if not dry_run:
        positions_data["active_positions"] = active_positions
        save_robinhood_positions(positions_data)
        print(f"\n[RobinhoodBridge] Updated active positions tracked in '{POSITIONS_FILE}'.")
        rh.logout()

def main():
    parser = argparse.ArgumentParser(description="Robinhood Automated Order Bridge")
    parser.add_argument("--dry_run", type=str, default="true", choices=["true", "false"])
    parser.add_argument("--ticket_file", type=str, default="LATEST_TICKET.txt")
    args = parser.parse_args()

    is_dry = args.dry_run.lower() == "true"
    tickets = parse_latest_tickets(args.ticket_file)

    if not tickets:
        print("[RobinhoodBridge] No actionable BUY tickets found in ticket file.")
        return

    execute_buys(tickets, dry_run=is_dry)

if __name__ == "__main__":
    main()
