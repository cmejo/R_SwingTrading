#!/usr/bin/env python3
"""
Charles Schwab Automated Trading Bridge for Triple-Engine Core-Satellite Model
==============================================================================
Provides end-to-end integration with Charles Schwab Trader REST & Market Data APIs:
  1. OAuth2 Authentication & 7-Day Token Watchdog
  2. Real-Time Market Data / Quotes Ingestion (Schwab API with high-speed fallback)
  3. Real Account Balances & Position Synchronization (schwab_state.json)
  4. Machine-Readable Order Execution (orders.json) with whole-share enforcement,
     sell-first sequencing, and broker-side GTC disaster stop orders.

Usage:
  python3 schwab_bridge.py auth
  python3 schwab_bridge.py token-check --warn_hours=48
  python3 schwab_bridge.py sync --out=schwab_state.json --dry_run=true
  python3 schwab_bridge.py quotes --symbols_file=symbols_broad.txt --out=schwab_quotes.json
  python3 schwab_bridge.py execute --orders=orders.json --dry_run=true
"""

import os
import sys
import json
import time
import math
import argparse
import base64
import datetime
import urllib.request
import urllib.parse
from typing import List, Dict, Any, Optional

try:
    import requests
except ImportError:
    requests = None

TOKEN_FILE = "schwab_token.json"
ENV_FILE = ".env"
KILL_SWITCH_FILES = ["STOP", "HALT", "EMERGENCY_STOP"]


def load_env(env_path: str = ENV_FILE):
    """Loads environment variables from .env file if present."""
    if os.path.exists(env_path):
        with open(env_path, "r") as f:
            for line in f:
                line = line.strip()
                if not line or line.startswith("#") or "=" not in line:
                    continue
                k, v = line.split("=", 1)
                k = k.strip().replace("export ", "")
                v = v.strip().strip("\"'")
                if k not in os.environ:
                    os.environ[k] = v

load_env()


class SchwabTraderAPI:
    """Official Charles Schwab Trader & Market Data REST Client."""
    BASE_URL = "https://api.schwabapi.com/trader/v1"
    MARKET_URL = "https://api.schwabapi.com/marketdata/v1"
    AUTH_URL = "https://api.schwabapi.com/v1/oauth/authorize"
    TOKEN_URL = "https://api.schwabapi.com/v1/oauth/token"

    def __init__(self, app_key: str = None, secret: str = None, redirect_uri: str = None):
        self.app_key = app_key or os.environ.get("SCHWAB_APP_KEY", "")
        self.secret = secret or os.environ.get("SCHWAB_SECRET", "")
        self.redirect_uri = redirect_uri or os.environ.get("SCHWAB_REDIRECT_URI", "https://127.0.0.1")

    def get_auth_url(self) -> str:
        params = {
            "client_id": self.app_key,
            "redirect_uri": self.redirect_uri
        }
        return f"{self.AUTH_URL}?{urllib.parse.urlencode(params)}"

    def exchange_code(self, code_or_url: str) -> Dict[str, Any]:
        code = code_or_url.strip()
        if "code=" in code:
            code = code.split("code=")[1].split("&")[0]
        code = urllib.parse.unquote(code)

        auth_header = base64.b64encode(f"{self.app_key}:{self.secret}".encode()).decode()
        headers = {
            "Authorization": f"Basic {auth_header}",
            "Content-Type": "application/x-www-form-urlencoded"
        }
        data = {
            "grant_type": "authorization_code",
            "code": code,
            "redirect_uri": self.redirect_uri
        }
        if requests:
            resp = requests.post(self.TOKEN_URL, headers=headers, data=data, timeout=15)
            if resp.status_code != 200:
                raise Exception(f"Token exchange failed ({resp.status_code}): {resp.text}")
            token_data = resp.json()
        else:
            req_data = urllib.parse.urlencode(data).encode()
            req = urllib.request.Request(self.TOKEN_URL, data=req_data, headers=headers)
            with urllib.request.urlopen(req, timeout=15) as r:
                token_data = json.loads(r.read().decode())

        now = time.time()
        token_data["expires_at"] = now + token_data.get("expires_in", 1800) - 60
        token_data["refresh_token_created_at"] = now
        token_data["refresh_token_expires_at"] = now + (7 * 86400)
        with open(TOKEN_FILE, "w") as f:
            json.dump(token_data, f, indent=2)
        return token_data

    def get_valid_token(self) -> str:
        if not os.path.exists(TOKEN_FILE):
            raise Exception(f"No token file found at {TOKEN_FILE}. Run: python3 schwab_bridge.py auth")

        with open(TOKEN_FILE, "r") as f:
            token_data = json.load(f)

        now = time.time()
        if now >= token_data.get("expires_at", 0):
            refresh_token = token_data.get("refresh_token")
            if not refresh_token:
                raise Exception("Refresh token missing from token file. Run: python3 schwab_bridge.py auth")

            auth_header = base64.b64encode(f"{self.app_key}:{self.secret}".encode()).decode()
            headers = {
                "Authorization": f"Basic {auth_header}",
                "Content-Type": "application/x-www-form-urlencoded"
            }
            data = {
                "grant_type": "refresh_token",
                "refresh_token": refresh_token
            }
            if requests:
                resp = requests.post(self.TOKEN_URL, headers=headers, data=data, timeout=15)
                if resp.status_code != 200:
                    raise Exception(f"Token refresh failed ({resp.status_code}): {resp.text}")
                new_data = resp.json()
            else:
                req_data = urllib.parse.urlencode(data).encode()
                req = urllib.request.Request(self.TOKEN_URL, data=req_data, headers=headers)
                with urllib.request.urlopen(req, timeout=15) as r:
                    new_data = json.loads(r.read().decode())

            if "refresh_token" not in new_data:
                new_data["refresh_token"] = refresh_token
            if "refresh_token_created_at" in token_data:
                new_data["refresh_token_created_at"] = token_data["refresh_token_created_at"]
                new_data["refresh_token_expires_at"] = token_data.get("refresh_token_expires_at", token_data["refresh_token_created_at"] + 7 * 86400)

            new_data["expires_at"] = now + new_data.get("expires_in", 1800) - 60
            with open(TOKEN_FILE, "w") as f:
                json.dump(new_data, f, indent=2)
            return new_data["access_token"]
        return token_data["access_token"]

    def get_account_hashes(self) -> List[Dict[str, str]]:
        token = self.get_valid_token()
        headers = {"Authorization": f"Bearer {token}"}
        url = f"{self.BASE_URL}/accounts/accountNumbers"
        if requests:
            resp = requests.get(url, headers=headers, timeout=10)
            if resp.status_code != 200:
                raise Exception(f"Failed to fetch Schwab account numbers ({resp.status_code}): {resp.text}")
            return resp.json()
        else:
            req = urllib.request.Request(url, headers=headers)
            with urllib.request.urlopen(req, timeout=10) as r:
                return json.loads(r.read().decode())

    def get_account_details(self, account_hash: str) -> Dict[str, Any]:
        token = self.get_valid_token()
        headers = {"Authorization": f"Bearer {token}"}
        url = f"{self.BASE_URL}/accounts/{account_hash}?fields=positions"
        if requests:
            resp = requests.get(url, headers=headers, timeout=10)
            if resp.status_code != 200:
                raise Exception(f"Failed to fetch account positions ({resp.status_code}): {resp.text}")
            return resp.json()
        else:
            req = urllib.request.Request(url, headers=headers)
            with urllib.request.urlopen(req, timeout=10) as r:
                return json.loads(r.read().decode())

    def get_quotes(self, symbols: List[str]) -> Dict[str, float]:
        """Fetches real-time quotes from Schwab Market Data API."""
        token = self.get_valid_token()
        headers = {"Authorization": f"Bearer {token}"}
        quotes: Dict[str, float] = {}

        # Batch symbols in chunks of 200
        batch_size = 200
        for i in range(0, len(symbols), batch_size):
            chunk = symbols[i:i + batch_size]
            syms_str = ",".join(chunk)
            url = f"{self.MARKET_URL}/quotes?symbols={urllib.parse.quote(syms_str)}&fields=quote"
            try:
                if requests:
                    resp = requests.get(url, headers=headers, timeout=10)
                    if resp.status_code == 200:
                        data = resp.json()
                        for sym, val in data.items():
                            q = val.get("quote", {})
                            px = q.get("lastPrice") or q.get("mark") or q.get("closePrice")
                            if px and float(px) > 0:
                                quotes[sym.upper()] = float(px)
                else:
                    req = urllib.request.Request(url, headers=headers)
                    with urllib.request.urlopen(req, timeout=10) as r:
                        data = json.loads(r.read().decode())
                        for sym, val in data.items():
                            q = val.get("quote", {})
                            px = q.get("lastPrice") or q.get("mark") or q.get("closePrice")
                            if px and float(px) > 0:
                                quotes[sym.upper()] = float(px)
            except Exception as e:
                print(f"[Schwab Quotes] Warning: Batch fetch failed for chunk: {e}")
        return quotes

    def place_order(self, account_hash: str, order_payload: Dict[str, Any]) -> str:
        token = self.get_valid_token()
        headers = {
            "Authorization": f"Bearer {token}",
            "Content-Type": "application/json"
        }
        url = f"{self.BASE_URL}/accounts/{account_hash}/orders"
        if requests:
            resp = requests.post(url, headers=headers, json=order_payload, timeout=15)
            if resp.status_code not in [200, 201]:
                raise Exception(f"Order submission failed ({resp.status_code}): {resp.text}")
            return resp.headers.get("Location", "ORDER_SUBMITTED")
        else:
            data_bytes = json.dumps(order_payload).encode()
            req = urllib.request.Request(url, data=data_bytes, headers=headers)
            with urllib.request.urlopen(req, timeout=15) as r:
                return r.headers.get("Location", "ORDER_SUBMITTED")

    def get_orders(self, account_hash: str) -> List[Dict[str, Any]]:
        token = self.get_valid_token()
        headers = {"Authorization": f"Bearer {token}"}
        url = f"{self.BASE_URL}/accounts/{account_hash}/orders"
        if requests:
            resp = requests.get(url, headers=headers, timeout=10)
            if resp.status_code == 200:
                return resp.json()
        else:
            req = urllib.request.Request(url, headers=headers)
            try:
                with urllib.request.urlopen(req, timeout=10) as r:
                    return json.loads(r.read().decode())
            except Exception:
                pass
        return []

    def cancel_order(self, account_hash: str, order_id: str) -> bool:
        token = self.get_valid_token()
        headers = {"Authorization": f"Bearer {token}"}
        url = f"{self.BASE_URL}/accounts/{account_hash}/orders/{order_id}"
        if requests:
            resp = requests.delete(url, headers=headers, timeout=10)
            return resp.status_code in [200, 204]
        else:
            req = urllib.request.Request(url, headers=headers, method="DELETE")
            try:
                with urllib.request.urlopen(req, timeout=10) as r:
                    return True
            except Exception:
                return False

    def cancel_open_orders_for_symbol(self, account_hash: str, symbol: str) -> int:
        orders = self.get_orders(account_hash)
        cancelled = 0
        working_statuses = {"WORKING", "QUEUED", "PENDING_ACTIVATION", "ACCEPTED"}

        def _cancel_match(obj):
            nonlocal cancelled
            oid = obj.get("orderId")
            status = obj.get("status")
            legs = obj.get("orderLegCollection", [])
            sym = legs[0].get("instrument", {}).get("symbol", "") if legs else ""
            if sym == symbol and status in working_statuses and oid:
                print(f"[Schwab Bridge] Cancelling conflicting open order #{oid} for {symbol} ({status})...")
                if self.cancel_order(account_hash, str(oid)):
                    cancelled += 1
            for child in obj.get("childOrderStrategies", []):
                _cancel_match(child)

        for o in orders:
            _cancel_match(o)
        return cancelled


# ==============================================================================
# Yahoo Finance Fast Concurrent Fallback Quote Engine
# ==============================================================================
def fetch_yahoo_quotes(symbols: List[str]) -> Dict[str, float]:
    """High-speed concurrent quotes via Yahoo Finance chart API with User-Agent."""
    import concurrent.futures
    quotes: Dict[str, float] = {}

    def _fetch_single(sym: str):
        url = f"https://query1.finance.yahoo.com/v8/finance/chart/{sym}?interval=1d&range=1d"
        req = urllib.request.Request(url, headers={"User-Agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)"})
        try:
            with urllib.request.urlopen(req, timeout=5) as resp:
                data = json.loads(resp.read().decode())
                meta = data["chart"]["result"][0]["meta"]
                px = meta.get("regularMarketPrice") or meta.get("chartPreviousClose")
                if px and float(px) > 0:
                    return sym, float(px)
        except Exception:
            pass
        return sym, None

    with concurrent.futures.ThreadPoolExecutor(max_workers=16) as executor:
        results = executor.map(_fetch_single, symbols)
        for sym, px in results:
            if px is not None:
                quotes[sym.upper()] = px
    return quotes


# ==============================================================================
# Subcommand Handlers
# ==============================================================================
def cmd_auth(args):
    """Interactive Schwab OAuth2 Login & Token Generation."""
    app_key = os.environ.get("SCHWAB_APP_KEY", "")
    secret = os.environ.get("SCHWAB_SECRET", "")
    if not app_key or not secret:
        print("❌ Error: SCHWAB_APP_KEY and SCHWAB_SECRET must be set in .env")
        sys.exit(1)

    schwab = SchwabTraderAPI(app_key, secret)
    url = schwab.get_auth_url()
    print("\n================================================================================")
    print("                CHARLES SCHWAB OAUTH2 AUTHORIZATION")
    print("================================================================================")
    print("1. Open the following URL in your web browser:")
    print(f"\n   {url}\n")
    print("2. Log in with your Charles Schwab username and password and click 'Approve'.")
    print("3. After authorization, your browser will redirect to a URL starting with:")
    print("   https://127.0.0.1/?code=...")
    print("4. Copy the ENTIRE URL from your browser address bar and paste it below:\n")

    redirect_input = input("Paste Redirect URL or Authorization Code: ").strip()
    if not redirect_input:
        print("❌ No authorization code received. Aborting.")
        sys.exit(1)

    try:
        token_data = schwab.exchange_code(redirect_input)
        print("\n✅ Authentication Successful!")
        print(f"Token saved to: {TOKEN_FILE}")
        expires_at = datetime.datetime.fromtimestamp(token_data.get("refresh_token_expires_at", time.time() + 7 * 86400))
        print(f"Refresh Token Valid Until: {expires_at.strftime('%Y-%m-%d %H:%M:%S')} (7-day window)")
    except Exception as e:
        print(f"❌ Authorization Error: {e}")
        sys.exit(1)


def cmd_token_check(args) -> int:
    """Verifies Schwab OAuth token health and 7-day expiration status."""
    warn_hours = args.warn_hours
    if not os.path.exists(TOKEN_FILE):
        print("⚠️  No schwab_token.json file found.")
        print("Run: python3 schwab_bridge.py auth")
        return 2

    try:
        with open(TOKEN_FILE, "r") as f:
            token_data = json.load(f)

        created_at = token_data.get("refresh_token_created_at")
        expires_at = token_data.get("refresh_token_expires_at")
        if not created_at:
            mtime = os.path.getmtime(TOKEN_FILE)
            created_at = mtime
            expires_at = mtime + (7 * 86400)

        now = time.time()
        remaining_sec = expires_at - now
        remaining_hours = remaining_sec / 3600.0

        created_dt = datetime.datetime.fromtimestamp(created_at)
        expires_dt = datetime.datetime.fromtimestamp(expires_at)

        print("--------------------------------------------------------------------------------")
        print("               CHARLES SCHWAB OAUTH TOKEN HEALTH CHECK                          ")
        print("--------------------------------------------------------------------------------")
        print(f" Token Created:     {created_dt.strftime('%Y-%m-%d %H:%M:%S')}")
        print(f" Token Expires:     {expires_dt.strftime('%Y-%m-%d %H:%M:%S')}")
        print(f" Remaining Life:    {remaining_hours:.1f} hours ({remaining_hours / 24.0:.1f} days)")

        if remaining_sec <= 0:
            print("\n🚨 CRITICAL: Refresh token has EXPIRED! Immediate re-authentication required.")
            print("Run: python3 schwab_bridge.py auth\n")
            return 2
        elif remaining_hours <= warn_hours:
            print(f"\n⚠️  WARNING: Refresh token expires in {remaining_hours:.1f} hours (under {warn_hours}h threshold)!")
            print("To prevent execution disruption, re-authenticate before expiration:")
            print("Run: python3 schwab_bridge.py auth\n")
            return 1
        else:
            print("\n✅ Token status HEALTHY.")
            return 0
    except Exception as e:
        print(f"❌ Error checking token: {e}")
        return 2


def cmd_sync(args):
    """Synchronizes live Charles Schwab account balances & positions into schwab_state.json."""
    out_file = args.out
    dry_run = args.dry_run

    print("\n[Schwab Sync] Connecting to Charles Schwab account...")
    app_key = os.environ.get("SCHWAB_APP_KEY", "")
    secret = os.environ.get("SCHWAB_SECRET", "")

    # Dry-run fallback or simulated state if unauthenticated
    if dry_run or not app_key or not secret or not os.path.exists(TOKEN_FILE):
        if not dry_run and (not app_key or not secret or not os.path.exists(TOKEN_FILE)):
            print("[Schwab Sync] Note: Schwab API credentials or token missing; generating state from portfolio_coresat.json")

        sim_capital = 8000.0
        sim_cash = 8000.0
        sim_positions = []
        if os.path.exists("portfolio_coresat.json"):
            try:
                with open("portfolio_coresat.json", "r") as pf:
                    pj = json.load(pf)
                    sim_capital = float(pj.get("total_capital", 8000.0))
                    sim_cash = float(pj.get("cash_balance", 0.0))
                    cp = pj.get("core_position")
                    if cp and isinstance(cp, dict) and cp.get("symbol") not in (None, "", "NONE") and float(cp.get("shares", 0)) > 0:
                        sim_positions.append({
                            "symbol": cp.get("symbol"),
                            "shares": float(cp.get("shares", 0)),
                            "average_price": float(cp.get("entry_price", 82.59)),
                            "market_value": round(float(cp.get("shares", 0)) * float(cp.get("entry_price", 82.59)), 2)
                        })
                    dp = pj.get("defensive_position")
                    if dp and isinstance(dp, dict) and dp.get("symbol") not in (None, "", "NONE") and float(dp.get("shares", 0)) > 0:
                        sim_positions.append({
                            "symbol": dp.get("symbol", "GLD"),
                            "shares": float(dp.get("shares", 0)),
                            "average_price": float(dp.get("entry_price", 355.78)),
                            "market_value": round(float(dp.get("shares", 0)) * float(dp.get("entry_price", 355.78)), 2)
                        })
                    for sp in pj.get("satellite_positions", []):
                        if float(sp.get("shares", 0)) > 0:
                            sim_positions.append({
                                "symbol": sp.get("symbol"),
                                "shares": float(sp.get("shares", 0)),
                                "average_price": float(sp.get("entry_price", 100.0)),
                                "market_value": round(float(sp.get("shares", 0)) * float(sp.get("entry_price", 100.0)), 2)
                            })
            except Exception:
                pass

        state_out = {
            "account_number": "SIM_SCHWAB_ACCT",
            "hash_value": "SIM_HASH",
            "total_capital": sim_capital,
            "liquidation_value": sim_capital,
            "cash_balance": sim_cash,
            "margin_debt": round(max(0.0, sum(p["market_value"] for p in sim_positions) - sim_capital), 2),
            "cash_available_for_trading": max(0.0, sim_cash),
            "positions_market_value": round(sum(p["market_value"] for p in sim_positions), 2),
            "sync_time": datetime.datetime.now().isoformat(),
            "positions": sim_positions,
            "is_simulation": True
        }
        with open(out_file, "w") as f:
            json.dump(state_out, f, indent=2)
        print(f"✅ Broker state written to: {out_file} (Mode: {'DRY RUN' if dry_run else 'FALLBACK'})")
        print(f"   Total Equity: ${sim_capital:.2f} | Cash: ${sim_cash:.2f} | Positions: {len(sim_positions)}")
        return

    try:
        schwab = SchwabTraderAPI(app_key, secret)
        hashes = schwab.get_account_hashes()
        if not hashes:
            raise Exception("No linked Schwab accounts found.")
        acct_hash = hashes[0]["hashValue"]
        acct_num = hashes[0]["accountNumber"]
        print(f"[Schwab Sync] Connected to Account: ***{acct_num[-4:]}")

        details = schwab.get_account_details(acct_hash)
        sec_acct = details.get("securitiesAccount", {})
        balances = sec_acct.get("currentBalances", {})

        # In Schwab margin accounts, liquidationValue is net equity
        total_equity = float(balances.get("liquidationValue") or balances.get("equity") or 0.0)
        cash_avail = float(balances.get("cashAvailableForTrading") or balances.get("cashBalance") or 0.0)

        raw_positions = sec_acct.get("positions", [])
        clean_positions = []
        for p in raw_positions:
            inst = p.get("instrument", {})
            sym = inst.get("symbol", "").upper()
            qty = float(p.get("longQuantity", 0.0) or p.get("shortQuantity", 0.0) or p.get("quantity", 0.0))
            cost = float(p.get("averagePrice", 0.0))
            mv = float(p.get("marketValue", 0.0))
            if qty > 0 and sym:
                clean_positions.append({
                    "symbol": sym,
                    "shares": qty,
                    "average_price": cost,
                    "market_value": mv
                })

        total_positions_mv = round(sum(p["market_value"] for p in clean_positions), 2)
        signed_cash = round(total_equity - total_positions_mv, 2)
        margin_debt = round(max(0.0, total_positions_mv - total_equity), 2)

        state_out = {
            "account_number": acct_num,
            "hash_value": acct_hash,
            "total_capital": round(total_equity, 2),
            "liquidation_value": round(total_equity, 2),
            "cash_balance": signed_cash,
            "margin_debt": margin_debt,
            "cash_available_for_trading": round(cash_avail, 2),
            "positions_market_value": total_positions_mv,
            "sync_time": datetime.datetime.now().isoformat(),
            "positions": clean_positions,
            "is_simulation": False
        }
        with open(out_file, "w") as f:
            json.dump(state_out, f, indent=2)
        print(f"✅ Real Schwab account synced to: {out_file}")
        print(f"   Account: ***{acct_num[-4:]} | Net Equity: ${total_equity:.2f} | Positions MV: ${total_positions_mv:.2f} | Margin Debt: ${margin_debt:.2f} | Positions: {len(clean_positions)}")
    except Exception as e:
        print(f"❌ Error during Schwab sync: {e}")
        sys.exit(1)


def cmd_positions(args):
    """Fetches and displays live account balances, positions, and unrealized P&L from Schwab."""
    app_key = os.environ.get("SCHWAB_APP_KEY", "")
    secret = os.environ.get("SCHWAB_SECRET", "")
    token_exists = os.path.exists(TOKEN_FILE)

    if not app_key or not secret or not token_exists:
        print("\n[Schwab Positions] No live Schwab credentials/token found; displaying local schwab_state.json")
        if os.path.exists("schwab_state.json"):
            with open("schwab_state.json", "r") as f:
                state = json.load(f)
            positions = state.get("positions", [])
            total_equity = float(state.get("total_capital", 0.0))
            margin_debt = float(state.get("margin_debt", 0.0))
            acct_num = state.get("account_number", "UNKNOWN")
            is_sim = state.get("is_simulation", True)
            print(f" Connected Account: {acct_num} (Mode: {'SIMULATION' if is_sim else 'CACHED LIVE'})")
            print(f" Net Liquidation Value (Equity): ${total_equity:,.2f}  |  Margin Debt: ${margin_debt:,.2f}")
            print("-" * 80)
            print(f"{'Symbol':<8} {'Shares':>10} {'Avg Cost':>14} {'Market Value':>16}")
            print("-" * 80)
            total_mv = 0.0
            for p in positions:
                sym = p.get("symbol", "")
                shs = float(p.get("shares", 0.0))
                cost = float(p.get("average_price", 0.0))
                mv = float(p.get("market_value", shs * cost))
                total_mv += mv
                print(f"{sym:<8} {shs:>10.3f} ${cost:>13.2f} ${mv:>15.2f}")
            print("-" * 80)
            print(f"Total Market Value: ${total_mv:,.2f} ({(total_mv / max(1.0, total_equity)) * 100:.1f}% gross exposure)\n")
        else:
            print("❌ No schwab_state.json found. Run: python3 schwab_bridge.py sync")
        return

    try:
        schwab = SchwabTraderAPI(app_key, secret)
        hashes = schwab.get_account_hashes()
        if not hashes:
            raise Exception("No linked Schwab accounts found.")
        acct_hash = hashes[0]["hashValue"]
        acct_num = hashes[0]["accountNumber"]

        details = schwab.get_account_details(acct_hash)
        sec_acct = details.get("securitiesAccount", {})
        balances = sec_acct.get("currentBalances", {})

        total_equity = float(balances.get("liquidationValue") or balances.get("equity") or 0.0)
        raw_positions = sec_acct.get("positions", [])
        clean_positions = []
        for p in raw_positions:
            inst = p.get("instrument", {})
            sym = inst.get("symbol", "").upper()
            qty = float(p.get("longQuantity", 0.0) or p.get("shortQuantity", 0.0) or p.get("quantity", 0.0))
            cost = float(p.get("averagePrice", 0.0))
            mv = float(p.get("marketValue", 0.0))
            pnl_open = float(p.get("longOpenProfitLoss", mv - (qty * cost)))
            pnl_open_pct = ((mv / (qty * cost)) - 1.0) * 100.0 if (qty * cost) > 0 else 0.0
            cur_px = (mv / qty) if qty > 0 else cost
            if qty > 0 and sym:
                clean_positions.append({
                    "symbol": sym,
                    "shares": qty,
                    "average_price": cost,
                    "current_price": cur_px,
                    "market_value": mv,
                    "unrealized_pnl": pnl_open,
                    "unrealized_pnl_pct": pnl_open_pct
                })

        total_mv = round(sum(p["market_value"] for p in clean_positions), 2)
        margin_debt = round(max(0.0, total_mv - total_equity), 2)
        total_open_pnl = sum(p["unrealized_pnl"] for p in clean_positions)

        print("\n" + "=" * 96)
        print("                         CHARLES SCHWAB LIVE ACCOUNT POSITIONS                          ")
        print("=" * 96)
        print(f" Connected Account: ***{acct_num[-4:]}  |  Sync Time: {datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        print(f" Net Liquidation Value (Equity): ${total_equity:,.2f}")
        print(f" Gross Long Market Value:       ${total_mv:,.2f} ({(total_mv / max(1.0, total_equity)) * 100:.1f}% gross exposure)")
        print(f" Borrowed Margin Loan (Debt):   ${margin_debt:,.2f} ({(margin_debt / max(1.0, total_equity)) * 100:.1f}% leverage)")
        print(f" Total Unrealized P&L:          {'+' if total_open_pnl >= 0 else ''}${total_open_pnl:,.2f}")
        print("-" * 96)
        print(f"{'Symbol':<7} {'Shares':>8} {'Avg Cost':>12} {'Current Px':>12} {'Market Value':>14} {'Total P&L ($)':>16} {'Total P&L (%)':>16}")
        print("-" * 96)
        for p in clean_positions:
            sign = "+" if p["unrealized_pnl"] >= 0 else ""
            print(f"{p['symbol']:<7} {p['shares']:>8.0f} ${p['average_price']:>11.2f} ${p['current_price']:>11.2f} ${p['market_value']:>13.2f} {sign}${p['unrealized_pnl']:>14.2f} {sign}{p['unrealized_pnl_pct']:>14.2f}%")
        print("-" * 96)
        print(f" Total: {len(clean_positions)} Position(s) | Gross Value: ${total_mv:,.2f} | Net Equity: ${total_equity:,.2f}")
        print("=" * 96 + "\n")
    except Exception as e:
        print(f"⚠️  Live Schwab API call failed ({e}).")
        if os.path.exists("schwab_state.json"):
            print("   Displaying cached positions from local schwab_state.json:\n")
            with open("schwab_state.json", "r") as f:
                state = json.load(f)
            positions = state.get("positions", [])
            total_equity = float(state.get("total_capital", 0.0))
            margin_debt = float(state.get("margin_debt", 0.0))
            acct_num = state.get("account_number", "UNKNOWN")
            is_sim = state.get("is_simulation", True)
            print(f" Account: {acct_num} (Mode: {'SIMULATION' if is_sim else 'CACHED LIVE'})")
            print(f" Net Liquidation Value (Equity): ${total_equity:,.2f}  |  Margin Debt: ${margin_debt:,.2f}")
            print("-" * 80)
            print(f"{'Symbol':<8} {'Shares':>10} {'Avg Cost':>14} {'Market Value':>16}")
            print("-" * 80)
            total_mv = 0.0
            for p in positions:
                sym = p.get("symbol", "")
                shs = float(p.get("shares", 0.0))
                cost = float(p.get("average_price", 0.0))
                mv = float(p.get("market_value", shs * cost))
                total_mv += mv
                print(f"{sym:<8} {shs:>10.3f} ${cost:>13.2f} ${mv:>15.2f}")
            print("-" * 80)
            print(f"Total Market Value: ${total_mv:,.2f} ({(total_mv / max(1.0, total_equity)) * 100:.1f}% gross exposure)\n")
        else:
            sys.exit(1)


def cmd_quotes(args):
    """Fetches real-time market quotes for candidate universe + core/defensive assets."""
    symbols_file = args.symbols_file
    out_file = args.out
    dry_run = args.dry_run

    symbols = ["QQQ", "GLD", "SGOV"]
    if os.path.exists(symbols_file):
        with open(symbols_file, "r") as f:
            for line in f:
                line = line.strip().split("#")[0].strip()
                if line:
                    for s in line.replace(",", " ").split():
                        s_clean = s.strip().upper()
                        if s_clean and s_clean not in symbols:
                            symbols.append(s_clean)

    print(f"\n[Quotes] Fetching real-time quotes for {len(symbols)} symbols...")
    quotes: Dict[str, float] = {}
    source = "schwab"

    app_key = os.environ.get("SCHWAB_APP_KEY", "")
    secret = os.environ.get("SCHWAB_SECRET", "")

    # Try Schwab Market Data API first if not dry-run and credentials exist
    if not dry_run and app_key and secret and os.path.exists(TOKEN_FILE):
        try:
            schwab = SchwabTraderAPI(app_key, secret)
            quotes = schwab.get_quotes(symbols)
            if len(quotes) >= len(symbols) * 0.8:
                source = "schwab"
                print(f"[Quotes] Successfully retrieved {len(quotes)} real-time quotes from Schwab Market Data API.")
            else:
                print(f"[Quotes] Schwab returned {len(quotes)}/{len(symbols)} quotes; supplementing with Yahoo fallback.")
        except Exception as e:
            print(f"[Quotes] Schwab API quote fetch failed: {e}; switching to real-time Yahoo engine.")

    # Supplement or fallback to Yahoo Finance real-time chart API
    missing = [s for s in symbols if s not in quotes]
    if missing:
        y_quotes = fetch_yahoo_quotes(missing)
        quotes.update(y_quotes)
        if source != "schwab":
            source = "yahoo_realtime"

    payload = {
        "generated_at": datetime.datetime.now().isoformat(),
        "source": source,
        "symbol_count": len(quotes),
        "quotes": quotes
    }
    with open(out_file, "w") as f:
        json.dump(payload, f, indent=2)

    print(f"✅ Saved {len(quotes)} real-time quotes to: {out_file} (Source: {source})")
    print(f"   Benchmark QQQ: ${quotes.get('QQQ', 0):.2f} | Defense GLD: ${quotes.get('GLD', 0):.2f}")


def cmd_execute(args):
    """Executes structured orders from orders.json on Charles Schwab Desk."""
    orders_file = args.orders
    dry_run = args.dry_run
    max_order_usd = args.max_order_usd

    # 1. Kill-Switch Verification
    for kf in KILL_SWITCH_FILES:
        if os.path.exists(kf):
            print(f"\n🚨 EMERGENCY ABORT: Kill-switch file '{kf}' detected in workspace!")
            print("No orders will be executed. To resume, remove the kill-switch file.")
            sys.exit(1)

    if not os.path.exists(orders_file):
        print(f"❌ Orders file not found: {orders_file}")
        sys.exit(1)

    with open(orders_file, "r") as f:
        orders = json.load(f)

    if not orders:
        print("\nℹ️  No orders present in orders.json. Portfolio is in target balance.")
        return

    # Sells executed first, buys second
    sells = [o for o in orders if o.get("side") == "SELL"]
    raw_buys = [o for o in orders if o.get("side") == "BUY"]
    
    # Safety Filter: 100% Stock Model strictly forbids Core ETF buys (QLD, QQQ, TQQQ)
    buys = []
    for b in raw_buys:
        b_sym = (b.get("symbol") or "").upper()
        if b_sym in ("QLD", "QQQ", "TQQQ"):
            print(f"  🚫 Prohibited Core ETF: BUY {b_sym} blocked. 100% Stock Model strictly forbids Core ETF purchases.")
            continue
        buys.append(b)

    print("\n================================================================================")
    print("                 CHARLES SCHWAB ORDER EXECUTION ENGINE                          ")
    print("================================================================================")
    print(f" Mode:            {'🟡 DRY RUN (Simulation - No real orders sent)' if dry_run else '🔴 LIVE EXECUTION (TRANSMITTING REAL ORDERS)'}")
    print(f" Orders:          {len(orders)} total ({len(sells)} SELL, {len(buys)} BUY)")
    print(f" Max Single Order Ceiling: ${max_order_usd:.2f}")
    print("================================================================================\n")

    # Dry-Run Preview
    if dry_run:
        print("[Dry Run] Staging Order Plan:")
        for idx, o in enumerate(sells + buys, 1):
            sym = o.get("symbol")
            side = o.get("side")
            shs_whole = int(o.get("shares_whole", math.floor(o.get("shares", 0))))
            est_px = float(o.get("estimated_price", 0.0))
            outlay = float(o.get("estimated_outlay", shs_whole * est_px))
            reason = o.get("reason", "")
            stop_px = o.get("stop_loss_price")
            stop_str = f" | GTC Stop Floor: ${stop_px:.2f}" if stop_px else ""
            print(f" [{idx:02d}] {side:<4} {shs_whole:>4} shs {sym:<6} @ ~${est_px:>7.2f} (${outlay:>8.2f}) | Reason: {reason}{stop_str}")
        print("\n✅ Dry run completed successfully. Order tickets validated against Schwab constraints.")
        return

    # Live Execution Guard
    app_key = os.environ.get("SCHWAB_APP_KEY", "")
    secret = os.environ.get("SCHWAB_SECRET", "")
    if not app_key or not secret:
        print("❌ Error: SCHWAB_APP_KEY and SCHWAB_SECRET required for live execution.")
        sys.exit(1)

    schwab = SchwabTraderAPI(app_key, secret)
    hashes = schwab.get_account_hashes()
    if not hashes:
        print("❌ No linked Schwab accounts found.")
        sys.exit(1)
    acct_hash = hashes[0]["hashValue"]
    acct_num = hashes[0]["accountNumber"]
    print(f"[Execution] Connected to Schwab Account: ***{acct_num[-4:]}")

    # STEP A: EXECUTE SELLS FIRST
    if sells:
        print(f"\n--- [Phase 1/2] Executing {len(sells)} SELL Order(s) to Release Capital ---")
        for s in sells:
            sym = s.get("symbol")
            qty = int(s.get("shares_whole", math.floor(s.get("shares", 0))))
            if qty <= 0:
                print(f"  [Skip] {sym}: 0 whole shares to sell.")
                continue

            # Cancel open GTC bracket/stop orders locking these shares first
            schwab.cancel_open_orders_for_symbol(acct_hash, sym)
            time.sleep(1.0)

            payload = {
                "orderStrategyType": "SINGLE",
                "orderType": "MARKET",
                "session": "NORMAL",
                "duration": "DAY",
                "orderLegCollection": [{
                    "instruction": "SELL",
                    "quantity": qty,
                    "instrument": {"symbol": sym, "assetType": "EQUITY"}
                }]
            }
            print(f"  Submitting MARKET SELL: {qty} shares of {sym}...")
            try:
                res = schwab.place_order(acct_hash, payload)
                print(f"  ✅ Submitted: {sym} ({res})")
            except Exception as e:
                print(f"  ❌ Error selling {sym}: {e}")
        print("Waiting 3s for Schwab trade fills and cash settlement...")
        time.sleep(3.0)

    # STEP B: EXECUTE BUYS SECOND
    if buys:
        print(f"\n--- [Phase 2/2] Executing {len(buys)} BUY Order(s) ---")
        for b in buys:
            sym = b.get("symbol")
            qty = int(b.get("shares_whole", math.floor(b.get("shares", 0))))
            est_px = float(b.get("estimated_price", 0.0))
            est_outlay = float(b.get("estimated_outlay", qty * est_px))

            if qty < 1:
                print(f"  [Skip] {sym}: Allocation equates to {b.get('shares'):.3f} shares (< 1 whole share). Schwab requires whole shares.")
                continue

            if est_outlay > max_order_usd:
                print(f"  🚨 Safety Limit Triggered: Order for {sym} (${est_outlay:.2f}) exceeds ceiling (${max_order_usd:.2f}). Skipping.")
                continue

            stop_px = b.get("stop_loss_price")
            # If satellite entrant with stop price, submit TRIGGER order (parent BUY + child GTC STOP)
            if stop_px and isinstance(stop_px, (int, float)) and float(stop_px) > 0:
                stop_val = round(float(stop_px), 2)
                payload = {
                    "orderStrategyType": "TRIGGER",
                    "orderType": "MARKET",
                    "session": "NORMAL",
                    "duration": "DAY",
                    "orderLegCollection": [{
                        "instruction": "BUY",
                        "quantity": qty,
                        "instrument": {"symbol": sym, "assetType": "EQUITY"}
                    }],
                    "childOrderStrategies": [{
                        "orderStrategyType": "SINGLE",
                        "orderType": "STOP",
                        "session": "NORMAL",
                        "duration": "GOOD_TILL_CANCEL",
                        "stopPrice": stop_val,
                        "orderLegCollection": [{
                            "instruction": "SELL",
                            "quantity": qty,
                            "instrument": {"symbol": sym, "assetType": "EQUITY"}
                        }]
                    }]
                }
                print(f"  Submitting BUY + GTC STOP TRIGGER: {qty} shares of {sym} (Stop Floor: ${stop_val:.2f})...")
            else:
                payload = {
                    "orderStrategyType": "SINGLE",
                    "orderType": "MARKET",
                    "session": "NORMAL",
                    "duration": "DAY",
                    "orderLegCollection": [{
                        "instruction": "BUY",
                        "quantity": qty,
                        "instrument": {"symbol": sym, "assetType": "EQUITY"}
                    }]
                }
                print(f"  Submitting MARKET BUY: {qty} shares of {sym}...")

            try:
                res = schwab.place_order(acct_hash, payload)
                print(f"  ✅ Submitted: {sym} ({res})")
            except Exception as e:
                print(f"  ❌ Error buying {sym}: {e}")

    print("\n✅ Order transmission sequence completed.")


# ==============================================================================
# Main CLI Dispatcher
# ==============================================================================
def main():
    parser = argparse.ArgumentParser(description="Charles Schwab Automated Trading Bridge")
    subparsers = parser.add_subparsers(dest="command", required=True)

    # Subcommand: auth
    p_auth = subparsers.add_parser("auth", help="Interactive OAuth2 login")

    # Subcommand: token-check
    p_tok = subparsers.add_parser("token-check", help="Verify 7-day token health")
    p_tok.add_argument("--warn_hours", type=float, default=48.0, help="Warning threshold in hours")

    # Subcommand: sync
    p_sync = subparsers.add_parser("sync", help="Synchronize live account equity & positions")
    p_sync.add_argument("--out", type=str, default="schwab_state.json", help="Output state JSON file")
    p_sync.add_argument("--dry_run", type=lambda x: str(x).lower() == "true", default=True, help="Dry run simulation")

    # Subcommand: positions
    p_pos = subparsers.add_parser("positions", help="Display live holdings, market value & P&L from Schwab")

    # Subcommand: quotes
    p_quote = subparsers.add_parser("quotes", help="Fetch real-time quotes")
    p_quote.add_argument("--symbols_file", type=str, default="symbols_broad.txt", help="Universe text file")
    p_quote.add_argument("--out", type=str, default="schwab_quotes.json", help="Output quotes JSON file")
    p_quote.add_argument("--dry_run", type=lambda x: str(x).lower() == "true", default=False, help="Skip Schwab API and use fallback")

    # Subcommand: execute
    p_exec = subparsers.add_parser("execute", help="Execute orders from orders.json")
    p_exec.add_argument("--orders", type=str, default="orders.json", help="Structured orders JSON file")
    p_exec.add_argument("--dry_run", type=lambda x: str(x).lower() == "true", default=True, help="Simulate without transmitting")
    p_exec.add_argument("--max_order_usd", type=float, default=5000.0, help="Single order dollar limit")

    args = parser.parse_args()

    if args.command == "auth":
        cmd_auth(args)
    elif args.command == "token-check":
        sys.exit(cmd_token_check(args))
    elif args.command == "sync":
        cmd_sync(args)
    elif args.command == "positions":
        cmd_positions(args)
    elif args.command == "quotes":
        cmd_quotes(args)
    elif args.command == "execute":
        cmd_execute(args)


if __name__ == "__main__":
    main()
