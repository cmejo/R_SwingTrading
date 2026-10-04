#!/usr/bin/env python3
"""
Robinhood Live Exit & Stop-Loss Monitoring Daemon
Because Robinhood does NOT support native exchange-level OCO bracket orders,
this background daemon polls real-time stock prices during market hours.
When price breaches a Stop-Loss or reaches a Profit Target, it automatically
submits the corresponding market sell order to protect capital and lock in gains.

Usage:
  # Run once (check current prices and exit if triggered):
  python3 poll_robinhood_exits.py --once=true --dry_run=true

  # Run as a continuous background daemon (checks every 30s):
  python3 poll_robinhood_exits.py --interval=30 --dry_run=false
"""

import sys
import os
import time
import json
import argparse
from datetime import datetime, timezone
from zoneinfo import ZoneInfo
from typing import Dict, Any

POSITIONS_FILE = "robinhood_positions.json"

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

def is_market_open() -> bool:
    """Checks whether US Stock Market is open (Mon-Fri 9:30 AM - 4:00 PM Eastern)."""
    now_et = datetime.now(ZoneInfo("America/New_York"))
    if now_et.weekday() >= 5: # Saturday or Sunday
        return False
    market_open = now_et.replace(hour=9, minute=30, second=0, microsecond=0)
    market_close = now_et.replace(hour=16, minute=0, second=0, microsecond=0)
    return market_open <= now_et <= market_close

def send_alert(message: str):
    """Sends optional notification via Discord or Telegram webhook."""
    discord_url = os.environ.get("DISCORD_WEBHOOK_URL")
    if discord_url:
        try:
            import requests
            requests.post(discord_url, json={"content": message}, timeout=5)
        except Exception as e:
            print(f"[Alert] Discord webhook error: {e}")

    tg_token = os.environ.get("TELEGRAM_BOT_TOKEN")
    tg_chat = os.environ.get("TELEGRAM_CHAT_ID")
    if tg_token and tg_chat:
        try:
            import requests
            url = f"https://api.telegram.org/bot{tg_token}/sendMessage"
            requests.post(url, json={"chat_id": tg_chat, "text": message}, timeout=5)
        except Exception as e:
            print(f"[Alert] Telegram error: {e}")

def load_positions() -> Dict[str, Any]:
    if os.path.exists(POSITIONS_FILE):
        try:
            with open(POSITIONS_FILE, "r") as f:
                return json.load(f)
        except Exception as e:
            print(f"[ExitPoller] Warning: Error reading {POSITIONS_FILE}: {e}")
    return {"active_positions": {}}

def save_positions(data: Dict[str, Any]):
    with open(POSITIONS_FILE, "w") as f:
        json.dump(data, f, indent=2)

def login_robinhood():
    try:
        import robin_stocks.robinhood as rh
    except ImportError:
        print("[ExitPoller] Error: 'robin_stocks' is not installed.")
        print("Install via: pip3 install robin_stocks pyotp")
        sys.exit(1)

    username = os.environ.get("ROBINHOOD_USERNAME")
    password = os.environ.get("ROBINHOOD_PASSWORD")
    totp_secret = os.environ.get("ROBINHOOD_TOTP_KEY")

    if not username or not password:
        print("[ExitPoller] Error: ROBINHOOD_USERNAME and ROBINHOOD_PASSWORD must be set in .env")
        sys.exit(1)

    mfa_code = None
    if totp_secret:
        try:
            import pyotp
            totp = pyotp.TOTP(totp_secret)
            mfa_code = totp.now()
        except ImportError:
            pass

    rh.login(username=username, password=password, mfa_code=mfa_code, expiresIn=86400, by_sms=False)
    return rh

def evaluate_exits(rh, dry_run: bool = True):
    pos_data = load_positions()
    active_positions = pos_data.get("active_positions", {})

    if not active_positions:
        return

    syms = list(active_positions.keys())
    prices = {}

    if rh is not None:
        try:
            quotes = rh.stocks.get_latest_price(syms)
            for s, p in zip(syms, quotes):
                if p is not None:
                    prices[s] = float(p)
        except Exception as e:
            print(f"[ExitPoller] Error fetching quotes: {e}")
            return
    else:
        # Fallback in dry-run without login
        print(f"[ExitPoller] [DRY RUN] Simulating price check for {syms}...")
        return

    timestamp = datetime.now(ZoneInfo("America/New_York")).strftime("%H:%M:%S ET")
    to_delete = []

    for sym, pos in active_positions.items():
        if sym not in prices:
            continue

        curr_px = prices[sym]
        stop_loss = pos["stop_loss"]
        t1_target = pos["tier1_target"]
        t2_target = pos["tier2_target"]
        shares = pos["shares"]

        # 1. Check Stop Loss Breach
        if curr_px <= stop_loss:
            msg = f"🚨 [ROBINHOOD STOP-LOSS TRIGGERED] {sym} price ${curr_px:.2f} <= Stop ${stop_loss:.2f}! Selling all {shares} shares."
            print(f"[{timestamp}] {msg}")
            send_alert(msg)

            order_ok = False
            if not dry_run:
                try:
                    res = rh.orders.order_sell_market_by_quantity(sym, quantity=shares, timeInForce="gfd")
                    if res and "id" in res and res.get("state") not in ["rejected", "failed", "cancelled"]:
                        print(f"[{timestamp}] -> Market Sell Submitted: {res.get('id')}")
                        order_ok = True
                    else:
                        print(f"[{timestamp}] -> Sell order rejected or failed: {res}")
                except Exception as e:
                    print(f"[{timestamp}] -> Error placing sell order: {e}")
            else:
                print(f"[{timestamp}] -> [DRY RUN] Would execute market sell for {shares} shares of {sym}.")
                order_ok = True

            if order_ok:
                to_delete.append(sym)
            continue

        # 2. Check Tier 2 Profit Target (Full Exit of everything still held).
        #    Also fires on a gap past both targets before Tier 1 executed.
        if curr_px >= t2_target:
            rem_shares = shares
            msg = f"🎯 [ROBINHOOD TARGET 2 REACHED] {sym} reached ${curr_px:.2f} >= Target ${t2_target:.2f}! Selling remaining {rem_shares} shares."
            print(f"[{timestamp}] {msg}")
            send_alert(msg)

            order_ok = False
            if not dry_run:
                try:
                    res = rh.orders.order_sell_market_by_quantity(sym, quantity=rem_shares, timeInForce="gfd")
                    if res and "id" in res and res.get("state") not in ["rejected", "failed", "cancelled"]:
                        print(f"[{timestamp}] -> Tier 2 Sell Submitted: {res.get('id')}")
                        order_ok = True
                    else:
                        print(f"[{timestamp}] -> Tier 2 sell rejected: {res}")
                except Exception as e:
                    print(f"[{timestamp}] -> Error placing sell order: {e}")
            else:
                print(f"[{timestamp}] -> [DRY RUN] Would execute market sell for {rem_shares} shares.")
                order_ok = True

            if order_ok:
                to_delete.append(sym)
            continue

        # 3. Check Tier 1 Profit Target (Partial Scale-Out + Breakeven Stop)
        if curr_px >= t1_target and not pos.get("tier1_executed", False):
            t1_shares = pos.get("tier1_shares", shares / 2.0)
            entry_px = pos.get("entry_estimated_price", stop_loss)
            msg = f"💰 [ROBINHOOD TARGET 1 HIT] {sym} reached ${curr_px:.2f} >= ${t1_target:.2f}! Selling {t1_shares} shares & raising stop to Breakeven (${entry_px:.2f})."
            print(f"[{timestamp}] {msg}")
            send_alert(msg)

            order_ok = False
            if not dry_run:
                try:
                    res = rh.orders.order_sell_market_by_quantity(sym, quantity=t1_shares, timeInForce="gfd")
                    if res and "id" in res and res.get("state") not in ["rejected", "failed", "cancelled"]:
                        print(f"[{timestamp}] -> Tier 1 Sell Submitted: {res.get('id')}")
                        order_ok = True
                    else:
                        print(f"[{timestamp}] -> Tier 1 sell rejected: {res}")
                except Exception as e:
                    print(f"[{timestamp}] -> Error placing sell order: {e}")
            else:
                print(f"[{timestamp}] -> [DRY RUN] Would execute market sell for {t1_shares} shares.")
                order_ok = True

            # Only update position state if sell was confirmed
            if order_ok:
                pos["tier1_executed"] = True
                pos["shares"] = round(shares - t1_shares, 4)
                pos["stop_loss"] = entry_px # Breakeven Stop
                pos["breakeven_stop_active"] = True

    # Cleanup closed positions
    for sym in to_delete:
        del active_positions[sym]

    if not dry_run and (to_delete or any(pos.get("tier1_executed") for pos in active_positions.values())):
        pos_data["active_positions"] = active_positions
        save_positions(pos_data)

def main():
    parser = argparse.ArgumentParser(description="Robinhood Background Exit Poller")
    parser.add_argument("--dry_run", type=str, default="true", choices=["true", "false"])
    parser.add_argument("--interval", type=int, default=30, help="Polling interval in seconds")
    parser.add_argument("--once", type=str, default="false", choices=["true", "false"])
    parser.add_argument("--force", action="store_true", help="Force execution even if market is closed")
    args = parser.parse_args()

    is_dry = args.dry_run.lower() == "true"
    run_once = args.once.lower() == "true"

    print("================================================================================")
    print("           ROBINHOOD REAL-TIME EXIT & STOP-LOSS POLLING DAEMON                  ")
    mode_str = "🟡 DRY RUN (Preview)" if is_dry else "🔴 LIVE EXECUTION"
    print("================================================================================")
    print(f" Mode:         {mode_str}")
    print(f" Polling:      Every {args.interval}s")
    print(f" Tracked File: {POSITIONS_FILE}")
    print("================================================================================")

    rh = None
    if not is_dry:
        rh = login_robinhood()
        print("[ExitPoller] Authenticated successfully with Robinhood API.")

    while True:
        if not args.force and not is_market_open():
            now_str = datetime.now(ZoneInfo("America/New_York")).strftime("%Y-%m-%d %H:%M:%S ET")
            print(f"[{now_str}] US Market is currently CLOSED. Pausing polling until open...")
            if run_once:
                break
            time.sleep(60)
            continue

        evaluate_exits(rh, dry_run=is_dry)

        if run_once:
            break

        time.sleep(args.interval)

if __name__ == "__main__":
    main()
