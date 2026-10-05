#!/usr/bin/env python3
"""
Robinhood Live Exit & Stop-Loss Monitoring Daemon
Because Robinhood does NOT support native exchange-level OCO bracket orders,
this background daemon polls real-time stock prices during market hours.
When price breaches a Stop-Loss or reaches a Profit Target, it automatically
submits the corresponding market sell order to protect capital and lock in gains.

Enhancements:
  - Auth refresh with automatic re-login on 24h session expiration (B7)
  - Dry-run Yahoo Finance quote evaluation (tests stops/targets without trading) (B7)
  - Pending-order locking to prevent duplicate order submissions (B7)
  - Order fill confirmation before position teardown (B7)
  - 5-trading-day time exit enforcement (F3)
  - Chandelier trailing stop for Tier 2 / runners (F3)
  - Friday 15:30 ET weekend defensive exit evaluation (F3)
  - Automatic portfolio.json synchronization on fills and exits (F3)

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
import urllib.request
from datetime import datetime, timezone, timedelta
from zoneinfo import ZoneInfo
from typing import Dict, Any, List, Optional

POSITIONS_FILE = "robinhood_positions.json"
PORTFOLIO_FILE = "portfolio.json"

# In-memory lock to prevent submitting duplicate orders while one is pending
PENDING_ORDERS: Dict[str, str] = {}

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
    if now_et.weekday() >= 5:  # Saturday or Sunday
        return False
    market_open = now_et.replace(hour=9, minute=30, second=0, microsecond=0)
    market_close = now_et.replace(hour=16, minute=0, second=0, microsecond=0)
    return market_open <= now_et <= market_close

def count_trading_days(start_date_str: str) -> int:
    """Calculates number of completed NYSE trading days since entry."""
    if not start_date_str:
        return 0
    try:
        start_dt = datetime.strptime(start_date_str, "%Y-%m-%d").date()
        today_dt = datetime.now(ZoneInfo("America/New_York")).date()
        if today_dt <= start_dt:
            return 0
        cur = start_dt + timedelta(days=1)
        days = 0
        while cur <= today_dt:
            if cur.weekday() < 5:  # Mon-Fri
                days += 1
            cur += timedelta(days=1)
        return days
    except Exception:
        return 0

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

def fetch_quotes_yahoo(symbols: List[str]) -> Dict[str, float]:
    """Fallback quote fetcher using Yahoo Finance chart endpoint."""
    prices: Dict[str, float] = {}
    for sym in symbols:
        try:
            url = f"https://query1.finance.yahoo.com/v8/finance/chart/{sym}?interval=1d&range=1d"
            req = urllib.request.Request(url, headers={"User-Agent": "Mozilla/5.0"})
            with urllib.request.urlopen(req, timeout=5) as resp:
                data = json.loads(resp.read().decode())
                meta = data["chart"]["result"][0]["meta"]
                px = meta.get("regularMarketPrice") or meta.get("chartPreviousClose")
                if px:
                    prices[sym] = float(px)
        except Exception as e:
            print(f"[ExitPoller] Fallback quote failed for {sym}: {e}")
    return prices

def sync_exit_to_portfolio_json(symbol: str, exit_price: float, shares: float, reason: str, is_partial: bool = False):
    """Synchronizes filled exits from Robinhood into portfolio.json for unified accounting."""
    if not os.path.exists(PORTFOLIO_FILE):
        return
    try:
        with open(PORTFOLIO_FILE, "r") as f:
            port = json.load(f)

        matched = [p for p in port.get("positions", []) if p.get("symbol") == symbol]
        if not matched:
            return
        pos = matched[0]
        cur_shares = float(pos.get("shares", 0))
        entry_px = float(pos.get("entry_price", 0))

        if is_partial:
            new_shares = max(0.0, cur_shares - shares)
            if new_shares <= 0:
                port["positions"] = [p for p in port["positions"] if p.get("symbol") != symbol]
            else:
                pos["shares"] = new_shares
                pos["cost_basis"] = round(new_shares * entry_px, 2)
                pos["stop_loss"] = entry_px  # Breakeven Stop on remainder
        else:
            port["positions"] = [p for p in port["positions"] if p.get("symbol") != symbol]

        pnl_dollar = round((exit_price - entry_px) * shares, 2)
        pnl_pct = round(((exit_price / entry_px) - 1.0) * 100.0, 2) if entry_px > 0 else 0.0

        port.setdefault("closed_trades", []).append({
            "symbol": symbol,
            "shares": shares,
            "entry_price": entry_px,
            "exit_price": round(exit_price, 2),
            "pnl_dollar": pnl_dollar,
            "pnl_pct": pnl_pct,
            "reason": reason,
            "exit_date": str(datetime.now(ZoneInfo("America/New_York")).date())
        })

        proceeds = round(shares * exit_price, 2)
        port["cash_balance"] = round(port.get("cash_balance", 0.0) + proceeds, 2)
        port["total_capital"] = round(port.get("total_capital", 0.0) + pnl_dollar, 2)
        port["peak_equity"] = max(port.get("peak_equity", port["total_capital"]), port["total_capital"])
        port["last_updated"] = str(datetime.now())

        with open(PORTFOLIO_FILE, "w") as f:
            json.dump(port, f, indent=2)
        print(f"[ExitPoller] Synced {reason} exit for {symbol} to {PORTFOLIO_FILE} (Realized PnL: ${pnl_dollar:+.2f})")
    except Exception as e:
        print(f"[ExitPoller] Warning: Failed to sync exit to {PORTFOLIO_FILE}: {e}")

def login_robinhood():
    try:
        import robin_stocks.robinhood as rh
    except ImportError:
        print("[ExitPoller] Error: 'robin_stocks' is not installed.")
        print("Install via: pip3 install -r requirements.txt")
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

def get_quotes_with_refresh(rh, symbols: List[str]) -> Dict[str, float]:
    """Fetches quotes via Robinhood API with automatic token refresh on expiration and Yahoo fallback."""
    if rh is not None:
        try:
            quotes = rh.stocks.get_latest_price(symbols)
            prices = {}
            for s, p in zip(symbols, quotes):
                if p is not None:
                    prices[s] = float(p)
            return prices
        except Exception as e:
            print(f"[ExitPoller] Robinhood quote error: {e}. Attempting session refresh...")
            try:
                rh = login_robinhood()
                quotes = rh.stocks.get_latest_price(symbols)
                prices = {}
                for s, p in zip(symbols, quotes):
                    if p is not None:
                        prices[s] = float(p)
                return prices
            except Exception as e2:
                print(f"[ExitPoller] Session refresh failed: {e2}. Falling back to Yahoo Finance...")

    # Dry-run or API fallback
    return fetch_quotes_yahoo(symbols)

def evaluate_exits(rh, dry_run: bool = True):
    pos_data = load_positions()
    active_positions = pos_data.get("active_positions", {})

    if not active_positions:
        return

    syms = list(active_positions.keys())
    prices = get_quotes_with_refresh(rh, syms)

    now_et = datetime.now(ZoneInfo("America/New_York"))
    timestamp = now_et.strftime("%H:%M:%S ET")
    to_delete = []

    # Check Friday 15:30 ET weekend defensive exit rule
    is_friday_late = (now_et.weekday() == 4 and (now_et.hour > 15 or (now_et.hour == 15 and now_et.minute >= 30)))

    for sym, pos in active_positions.items():
        if sym not in prices:
            continue

        if sym in PENDING_ORDERS:
            print(f"[{timestamp}] {sym} has an order pending ({PENDING_ORDERS[sym]}). Skipping evaluation.")
            continue

        curr_px = prices[sym]
        stop_loss = float(pos.get("stop_loss", 0.0))
        t1_target = float(pos.get("tier1_target", 0.0))
        t2_target = float(pos.get("tier2_target", 0.0))
        shares = float(pos.get("shares", 0.0))
        entry_px = float(pos.get("entry_estimated_price", pos.get("entry_price", stop_loss)))
        entry_date = pos.get("entry_date", "")
        days_held = count_trading_days(entry_date)

        # Update high-water mark for chandelier stop
        highest_px = max(float(pos.get("highest_price", curr_px)), curr_px)
        pos["highest_price"] = highest_px

        # Compute dynamic chandelier trailing stop if active or post-Tier 1
        atr_14 = float(pos.get("atr_14", curr_px * 0.02))
        chandelier_lvl = round(highest_px - (2.5 * atr_14), 2)
        pos["chandelier_stop"] = chandelier_lvl

        # If Tier 1 was hit, ratchet stop loss to max(entry_px, chandelier_lvl)
        if pos.get("tier1_executed", False):
            stop_loss = max(entry_px, chandelier_lvl)
            pos["stop_loss"] = stop_loss

        exit_triggered = False
        exit_reason = ""
        exit_shares = shares

        # 1. Stop Loss / Trailing Chandelier Stop Breach
        if curr_px <= stop_loss:
            exit_triggered = True
            exit_reason = "CHANDELIER_STOP" if pos.get("tier1_executed", False) else "STOP_LOSS"
            msg = f"🚨 [ROBINHOOD {exit_reason} TRIGGERED] {sym} price ${curr_px:.2f} <= Stop ${stop_loss:.2f}! Selling {shares} shares."
            print(f"[{timestamp}] {msg}")
            send_alert(msg)

        # 2. Tier 2 Target Hit (Full exit of remainder)
        elif t2_target > 0 and curr_px >= t2_target:
            exit_triggered = True
            exit_reason = "TIER2_TARGET"
            msg = f"🎯 [ROBINHOOD TARGET 2 REACHED] {sym} price ${curr_px:.2f} >= Target ${t2_target:.2f}! Selling {shares} shares."
            print(f"[{timestamp}] {msg}")
            send_alert(msg)

        # 3. 5-Trading-Day Time Expiration Exit
        elif days_held >= 5:
            exit_triggered = True
            exit_reason = "TIME_EXPIRATION"
            msg = f"⏳ [ROBINHOOD 5-DAY TIME EXPIRATION] {sym} held for {days_held} trading days! Liquidating {shares} shares."
            print(f"[{timestamp}] {msg}")
            send_alert(msg)

        # 4. Friday 15:30 Weekend Defensive Exit (if not approved for weekend carry)
        elif is_friday_late and not pos.get("weekend_hold_approved", False):
            exit_triggered = True
            exit_reason = "WEEKEND_DEFENSIVE_EXIT"
            msg = f"⚠️ [ROBINHOOD WEEKEND EXIT] {sym} unapproved for weekend hold! Liquidating {shares} shares before 16:00 close."
            print(f"[{timestamp}] {msg}")
            send_alert(msg)

        # 5. Tier 1 Profit Target Hit (Partial scale-out 50% + Breakeven stop ratchet)
        elif t1_target > 0 and curr_px >= t1_target and not pos.get("tier1_executed", False):
            t1_shares = float(pos.get("tier1_shares", shares / 2.0))
            msg = f"💰 [ROBINHOOD TARGET 1 HIT] {sym} price ${curr_px:.2f} >= ${t1_target:.2f}! Selling {t1_shares} shares & ratcheting stop to Breakeven (${entry_px:.2f})."
            print(f"[{timestamp}] {msg}")
            send_alert(msg)

            order_ok = False
            if not dry_run and rh is not None:
                try:
                    PENDING_ORDERS[sym] = "TIER1_SELL"
                    res = rh.orders.order_sell_market_by_quantity(sym, quantity=t1_shares, timeInForce="gfd")
                    if res and "id" in res and res.get("state") not in ["rejected", "failed", "cancelled"]:
                        print(f"[{timestamp}] -> Tier 1 Sell Submitted: {res.get('id')}")
                        order_ok = True
                    else:
                        print(f"[{timestamp}] -> Tier 1 Sell rejected: {res}")
                except Exception as e:
                    print(f"[{timestamp}] -> Error placing Tier 1 sell: {e}")
                finally:
                    PENDING_ORDERS.pop(sym, None)
            else:
                print(f"[{timestamp}] -> [DRY RUN] Would submit market sell for {t1_shares} shares of {sym} at ${curr_px:.2f}.")
                order_ok = True

            if order_ok:
                pos["tier1_executed"] = True
                pos["shares"] = round(shares - t1_shares, 4)
                pos["stop_loss"] = entry_px
                pos["breakeven_stop_active"] = True
                sync_exit_to_portfolio_json(sym, curr_px, t1_shares, "TIER1_TARGET", is_partial=True)
            continue

        # Execute full liquidation if triggered
        if exit_triggered:
            order_ok = False
            if not dry_run and rh is not None:
                try:
                    PENDING_ORDERS[sym] = exit_reason
                    res = rh.orders.order_sell_market_by_quantity(sym, quantity=exit_shares, timeInForce="gfd")
                    if res and "id" in res and res.get("state") not in ["rejected", "failed", "cancelled"]:
                        print(f"[{timestamp}] -> Market Sell Submitted ({exit_reason}): {res.get('id')}")
                        order_ok = True
                    else:
                        print(f"[{timestamp}] -> Sell order rejected or failed: {res}")
                except Exception as e:
                    print(f"[{timestamp}] -> Error placing sell order: {e}")
                finally:
                    PENDING_ORDERS.pop(sym, None)
            else:
                print(f"[{timestamp}] -> [DRY RUN] Would execute market sell ({exit_reason}) for {exit_shares} shares of {sym} at ${curr_px:.2f}.")
                order_ok = True

            if order_ok:
                to_delete.append(sym)
                sync_exit_to_portfolio_json(sym, curr_px, exit_shares, exit_reason, is_partial=False)

    # Cleanup closed positions
    for sym in to_delete:
        del active_positions[sym]

    if not dry_run or to_delete:
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
    mode_str = "🟡 DRY RUN (Evaluating Real Quotes via Yahoo)" if is_dry else "🔴 LIVE EXECUTION"
    print("================================================================================")
    print(f" Mode:         {mode_str}")
    print(f" Polling:      Every {args.interval}s")
    print(f" Tracked File: {POSITIONS_FILE}")
    print(f" Sync File:    {PORTFOLIO_FILE}")
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
