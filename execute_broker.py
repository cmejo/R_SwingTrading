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
        r"--- ORDER TICKET #(\d+): ([A-Za-z0-9\.\-]+) \(P\(Up\): ([\d\.]+)%.*?Sector: ([^\)]+)\) ---\n(.*?)(?=(?:--- ORDER TICKET|========================================================================================|\Z))",
        content,
        re.DOTALL
    )

    for rank, sym, prob, sector, body in ticket_blocks:
        # Extract Action & Shares
        act_match = re.search(r"Action:\s+BUY\s+([\d\.]+)\s+SHARES", body)
        shares = float(act_match.group(1)) if act_match else 0.0

        if shares <= 0:
            continue

        # Extract Stop Loss & Risk Percentage
        stop_match = re.search(r"GTC Stop-Loss:\s+\$([\d\.]+)(?:\s+\(-?([\d\.]+)%\))?", body)
        stop_px = float(stop_match.group(1)) if stop_match else 0.0
        stop_pct = float(stop_match.group(2)) / 100.0 if (stop_match and stop_match.group(2)) else 0.0

        # Extract Signal / Reference Price
        sig_match = re.search(r"(?:Signal Price:\s+\$|Breakeven\s+\$|EXECUTE AT MARKET \(\$|@ ~\$)([\d\.]+)", body)
        sig_px = float(sig_match.group(1)) if sig_match else 0.0

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

        if sig_px == 0.0 and outlay > 0 and shares > 0:
            sig_px = round(outlay / shares, 2)
        if stop_pct == 0.0 and sig_px > 0 and stop_px > 0 and sig_px > stop_px:
            stop_pct = round((sig_px - stop_px) / sig_px, 4)

        tickets.append({
            "rank": int(rank),
            "symbol": sym,
            "shares": shares,
            "sector": sector.strip(),
            "p_up": float(prob),
            "signal_price": sig_px,
            "stop_pct": stop_pct,
            "stop_loss": stop_px,
            "tier1_shares": t1_shares,
            "tier1_target": t1_price,
            "tier2_shares": t2_shares,
            "tier2_target": t2_price,
            "estimated_outlay": outlay
        })

    # Match Core-Satellite QQQ Cash Yield ticket
    qqq_match = re.search(
        r"CORE-SATELLITE CASH YIELD DEPLOYMENT.*?Actionable Ticket:\s+BUY\s+([\d\.]+)\s+shares of QQQ @ ~\$([\d\.]+)\s+\(\$([\d\.]+)\s+outlay\)",
        content,
        re.DOTALL
    )
    if qqq_match:
        qqq_shs = float(qqq_match.group(1))
        qqq_px = float(qqq_match.group(2))
        qqq_outlay = float(qqq_match.group(3))
        if qqq_shs >= 1.0:
            tickets.append({
                "rank": 99,
                "action": "BUY",
                "symbol": "QQQ",
                "shares": qqq_shs,
                "sector": "Index_ETF",
                "p_up": 0.99,
                "is_cash_park": True,
                "stop_loss": 0.0,
                "tier1_shares": 0.0,
                "tier1_target": 0.0,
                "tier2_shares": 0.0,
                "tier2_target": 0.0,
                "estimated_outlay": qqq_outlay
            })

    # =========================================================================
    # Parse Automated SELL Exit Alerts from PORTFOLIO ACTION ALERTS
    # =========================================================================
    port_positions = {}
    if os.path.exists("portfolio.json"):
        try:
            with open("portfolio.json", "r") as pf:
                pj = json.load(pf)
                port_positions = {p["symbol"]: p for p in pj.get("positions", [])}
        except Exception:
            pass

    # 1. Stop-Loss Hit
    # e.g.: [STOP-LOSS HIT] QQQ breached stop at $754.01 (Current: $742.19). Exit immediately.
    stop_alerts = re.findall(
        r"\[STOP-LOSS HIT\]\s*([A-Za-z0-9\.\-]+)",
        content
    )
    for sym in stop_alerts:
        pos = port_positions.get(sym, {})
        shs = float(pos.get("shares", 0.0))
        if shs > 0:
            tickets.append({
                "rank": 0,
                "action": "SELL",
                "symbol": sym,
                "shares": shs,
                "reason": "STOP_LOSS_HIT",
                "price": float(pos.get("current_price", pos.get("entry_price", 0.0)))
            })

    # 2. Tier 1 Target Hit (+1.5R Scale-Out 50%)
    # e.g.: [TIER 1 HIT] PENG hit +1.5R target ($76.85). Sell 50% (13.500 shares) and raise stop to Breakeven $72.50!
    t1_alerts = re.findall(
        r"\[TIER 1 HIT\]\s*([A-Za-z0-9\.\-]+).*?Sell 50%\s*\(([\d\.]+)\s*shares\)",
        content
    )
    for sym, shs in t1_alerts:
        pos = port_positions.get(sym, {})
        tickets.append({
            "rank": 0,
            "action": "SELL",
            "symbol": sym,
            "shares": float(shs),
            "reason": "SCALE_OUT_TIER1",
            "price": float(pos.get("current_price", pos.get("entry_price", 0.0)))
        })

    # 3. Tier 2 Target Hit (+3.0R Scale-Out 25%)
    # e.g.: [TIER 2 HIT] SYMBOL hit +3.0R target ($XX). Sell YY shares.
    t2_alerts = re.findall(
        r"\[TIER 2 HIT\]\s*([A-Za-z0-9\.\-]+).*?Sell\s+([\d\.]+)\s*shares",
        content
    )
    for sym, shs in t2_alerts:
        pos = port_positions.get(sym, {})
        tickets.append({
            "rank": 0,
            "action": "SELL",
            "symbol": sym,
            "shares": float(shs),
            "reason": "SCALE_OUT_TIER2",
            "price": float(pos.get("current_price", pos.get("entry_price", 0.0)))
        })

    # 4. 7-Day Time Horizon Expiration Exit
    # e.g.: [TIME EXIT] SYMBOL held for 7 trading days without reaching Tier 2 runner.
    time_alerts = re.findall(
        r"\[TIME EXIT\]\s*([A-Za-z0-9\.\-]+)",
        content
    )
    for sym in time_alerts:
        pos = port_positions.get(sym, {})
        shs = float(pos.get("shares", 0.0))
        if shs > 0:
            tickets.append({
                "rank": 0,
                "action": "SELL",
                "symbol": sym,
                "shares": shs,
                "reason": "MAX_7DAY_TIME_EXIT",
                "price": float(pos.get("current_price", pos.get("entry_price", 0.0)))
            })

    # 5. Defensive Friday Weekend Exit
    # e.g.: [DEFENSIVE WEEKEND EXIT] SYMBOL must be closed before 16:00 Friday
    wknd_alerts = re.findall(
        r"\[DEFENSIVE WEEKEND EXIT\]\s*([A-Za-z0-9\.\-]+)",
        content
    )
    for sym in wknd_alerts:
        pos = port_positions.get(sym, {})
        shs = float(pos.get("shares", 0.0))
        if shs > 0:
            tickets.append({
                "rank": 0,
                "action": "SELL",
                "symbol": sym,
                "shares": shs,
                "reason": "DEFENSIVE_WEEKEND_EXIT",
                "price": float(pos.get("current_price", pos.get("entry_price", 0.0)))
            })

    return tickets

# Alias for backwards compatibility across bridges
read_latest_tickets = parse_latest_tickets

def build_ibkr_payload(ticket: Dict[str, Any]) -> Dict[str, Any]:
    """Generates TWS / IB Gateway Bracket Order Specification.
    Delegates to execute_ibkr.py so both entry points share one OCA-linked bracket design."""
    from execute_ibkr import build_ibkr_bracket_spec
    return build_ibkr_bracket_spec(ticket)


def _schwab_leg_order(symbol: str, qty: int, stop_px: float, target_px: float) -> Dict[str, Any]:
    """One Schwab FIRST_TRIGGERS_OCO order: BUY qty at market, then an OCO of
    STOP (qty) and LIMIT target (qty). Stop and target are the same size, so a
    filled target cancels the stop and nothing is left unprotected or oversized."""
    instrument = {"symbol": symbol, "assetType": "EQUITY"}
    return {
        "orderStrategyType": "TRIGGER",
        "orderType": "MARKET",
        "session": "NORMAL",
        "duration": "DAY",
        "orderLegCollection": [{"instruction": "BUY", "quantity": qty, "instrument": instrument}],
        "childOrderStrategies": [{
            "orderStrategyType": "OCO",
            "childOrderStrategies": [
                {
                    "orderStrategyType": "SINGLE",
                    "orderType": "STOP",
                    "session": "NORMAL",
                    "duration": "GOOD_TILL_CANCEL",
                    "stopPrice": round(stop_px, 2),
                    "orderLegCollection": [{"instruction": "SELL", "quantity": qty, "instrument": instrument}]
                },
                {
                    "orderStrategyType": "SINGLE",
                    "orderType": "LIMIT",
                    "session": "NORMAL",
                    "duration": "GOOD_TILL_CANCEL",
                    "price": round(target_px, 2),
                    "orderLegCollection": [{"instruction": "SELL", "quantity": qty, "instrument": instrument}]
                }
            ]
        }]
    }


def build_schwab_payloads(ticket: Dict[str, Any]) -> List[Dict[str, Any]]:
    """Generates Charles Schwab Trader API orders for one ticket.
    For exit tickets (action == 'SELL'), generates a single day MARKET sell order.
    For standard swing stocks, splits into two FIRST_TRIGGERS_OCO brackets (Tier 1 & Tier 2).
    For Core-Satellite QQQ cash park, generates a single day MARKET buy order."""
    if ticket.get("action") == "SELL":
        qty = int(ticket["shares"])
        if qty > 0:
            return [{
                "orderStrategyType": "SINGLE",
                "orderType": "MARKET",
                "session": "NORMAL",
                "duration": "DAY",
                "orderLegCollection": [{
                    "instruction": "SELL",
                    "quantity": qty,
                    "instrument": {"symbol": ticket["symbol"], "assetType": "EQUITY"}
                }]
            }]
        return []

    if ticket.get("is_cash_park"):
        total = int(ticket["shares"])
        if total > 0:
            return [{
                "orderStrategyType": "SINGLE",
                "orderType": "MARKET",
                "session": "NORMAL",
                "duration": "DAY",
                "orderLegCollection": [{
                    "instruction": "BUY",
                    "quantity": total,
                    "instrument": {"symbol": ticket["symbol"], "assetType": "EQUITY"}
                }]
            }]
        return []

    total = int(ticket["shares"])
    tier1 = total // 2
    tier2 = total - tier1
    orders = []
    if tier1 > 0:
        orders.append(_schwab_leg_order(ticket["symbol"], tier1, ticket["stop_loss"], ticket["tier1_target"]))
    if tier2 > 0:
        orders.append(_schwab_leg_order(ticket["symbol"], tier2, ticket["stop_loss"], ticket["tier2_target"]))
    return orders


def build_schwab_payload(ticket: Dict[str, Any]) -> Dict[str, Any]:
    """Backward-compatible wrapper: returns a preview dict containing every leg order."""
    return {"broker": "Charles Schwab Trader API", "orders": build_schwab_payloads(ticket)}


def execute_ibkr(tickets: List[Dict[str, Any]], host: str, port: int, client_id: int, dry_run: bool):
    """Delegates to execute_ibkr.py (OCA-linked two-leg brackets, whole shares,
    contract qualification checks)."""
    from execute_ibkr import execute_ibkr as _execute_ibkr
    _execute_ibkr(tickets, host=host, port=port, client_id=client_id, dry_run=dry_run)

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

class SchwabTraderAPI:
    """Official Charles Schwab Trader REST & OAuth2 Client."""
    TOKEN_FILE = "schwab_token.json"
    BASE_URL = "https://api.schwabapi.com/trader/v1"
    AUTH_URL = "https://api.schwabapi.com/v1/oauth/authorize"
    TOKEN_URL = "https://api.schwabapi.com/v1/oauth/token"

    def __init__(self, app_key: str = None, secret: str = None):
        self.app_key = app_key or os.environ.get("SCHWAB_APP_KEY", "")
        self.secret = secret or os.environ.get("SCHWAB_SECRET", "")
        self.redirect_uri = os.environ.get("SCHWAB_REDIRECT_URI", "https://127.0.0.1")

    def get_auth_url(self) -> str:
        return f"{self.AUTH_URL}?client_id={self.app_key}&redirect_uri={self.redirect_uri}"

    def exchange_code(self, code_or_url: str) -> Dict[str, Any]:
        import base64
        import time
        import requests
        code = code_or_url.strip()
        if "code=" in code:
            code = code.split("code=")[1].split("&")[0]
        code = code.replace("%40", "@")

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
        resp = requests.post(self.TOKEN_URL, headers=headers, data=data)
        if resp.status_code != 200:
            raise Exception(f"Token exchange failed ({resp.status_code}): {resp.text}")
        token_data = resp.json()
        token_data["expires_at"] = time.time() + token_data.get("expires_in", 1800) - 60
        with open(self.TOKEN_FILE, "w") as f:
            json.dump(token_data, f, indent=2)
        return token_data

    def get_valid_token(self) -> str:
        import base64
        import time
        import requests
        if not os.path.exists(self.TOKEN_FILE):
            raise Exception(f"No token file found at {self.TOKEN_FILE}. Run: python3 execute_broker.py --broker=schwab --auth")
        with open(self.TOKEN_FILE, "r") as f:
            token_data = json.load(f)

        if time.time() >= token_data.get("expires_at", 0):
            refresh_token = token_data.get("refresh_token")
            if not refresh_token:
                raise Exception("Refresh token missing. Re-authenticate via --auth.")
            auth_header = base64.b64encode(f"{self.app_key}:{self.secret}".encode()).decode()
            headers = {
                "Authorization": f"Basic {auth_header}",
                "Content-Type": "application/x-www-form-urlencoded"
            }
            data = {
                "grant_type": "refresh_token",
                "refresh_token": refresh_token
            }
            resp = requests.post(self.TOKEN_URL, headers=headers, data=data)
            if resp.status_code != 200:
                raise Exception(f"Token refresh failed ({resp.status_code}): {resp.text}")
            new_data = resp.json()
            # Preserve existing refresh_token if not returned in response
            if "refresh_token" not in new_data:
                new_data["refresh_token"] = refresh_token
            new_data["expires_at"] = time.time() + new_data.get("expires_in", 1800) - 60
            with open(self.TOKEN_FILE, "w") as f:
                json.dump(new_data, f, indent=2)
            return new_data["access_token"]
        return token_data["access_token"]

    def get_account_hashes(self) -> List[Dict[str, str]]:
        import requests
        token = self.get_valid_token()
        headers = {"Authorization": f"Bearer {token}"}
        resp = requests.get(f"{self.BASE_URL}/accounts/accountNumbers", headers=headers)
        if resp.status_code != 200:
            raise Exception(f"Failed to fetch Schwab account numbers ({resp.status_code}): {resp.text}")
        return resp.json()

    def place_order(self, account_hash: str, order_payload: Dict[str, Any]) -> Any:
        import requests
        token = self.get_valid_token()
        headers = {
            "Authorization": f"Bearer {token}",
            "Content-Type": "application/json"
        }
        api_payload = {k: v for k, v in order_payload.items() if k != "broker"}
        resp = requests.post(f"{self.BASE_URL}/accounts/{account_hash}/orders", headers=headers, json=api_payload)
        if resp.status_code not in [200, 201]:
            raise Exception(f"Order submission failed ({resp.status_code}): {resp.text}")
        return resp.headers.get("Location", "ORDER_SUBMITTED")

    def get_account_details(self, account_hash: str) -> Dict[str, Any]:
        import requests
        token = self.get_valid_token()
        headers = {"Authorization": f"Bearer {token}"}
        resp = requests.get(f"{self.BASE_URL}/accounts/{account_hash}?fields=positions", headers=headers)
        if resp.status_code != 200:
            raise Exception(f"Failed to fetch account positions ({resp.status_code}): {resp.text}")
        return resp.json()

    def get_orders(self, account_hash: str, from_date: str = None, to_date: str = None) -> List[Dict[str, Any]]:
        import datetime
        import requests
        token = self.get_valid_token()
        headers = {"Authorization": f"Bearer {token}"}
        url = f"{self.BASE_URL}/accounts/{account_hash}/orders"
        
        now_utc = datetime.datetime.now(datetime.timezone.utc)
        if not from_date:
            from_iso = (now_utc - datetime.timedelta(days=7)).strftime("%Y-%m-%dT00:00:00.000Z")
        else:
            from_iso = f"{from_date}T00:00:00.000Z" if "T" not in from_date else from_date
            
        if not to_date:
            to_iso = (now_utc + datetime.timedelta(days=1)).strftime("%Y-%m-%dT23:59:59.000Z")
        else:
            to_iso = f"{to_date}T23:59:59.000Z" if "T" not in to_date else to_date

        params = {
            "fromEnteredTime": from_iso,
            "toEnteredTime": to_iso
        }
        resp = requests.get(url, headers=headers, params=params)
        if resp.status_code != 200:
            raise Exception(f"Failed to fetch Schwab orders ({resp.status_code}): {resp.text}")
        return resp.json()

    def cancel_order(self, account_hash: str, order_id: str) -> bool:
        import requests
        token = self.get_valid_token()
        headers = {"Authorization": f"Bearer {token}"}
        resp = requests.delete(f"{self.BASE_URL}/accounts/{account_hash}/orders/{order_id}", headers=headers)
        if resp.status_code not in [200, 204]:
            raise Exception(f"Failed to cancel order {order_id} ({resp.status_code}): {resp.text}")
        return True

    def cancel_open_orders_for_symbol(self, account_hash: str, symbol: str) -> int:
        import time
        orders = self.get_orders(account_hash)
        cancelled = 0
        working_statuses = {"WORKING", "QUEUED", "PENDING_ACTIVATION", "ACCEPTED"}

        def _cancel_if_match(obj):
            nonlocal cancelled
            oid = obj.get("orderId")
            status = obj.get("status")
            legs = obj.get("orderLegCollection", [])
            sym = legs[0].get("instrument", {}).get("symbol", "") if legs else ""
            if sym == symbol and status in working_statuses and oid:
                print(f"[Schwab Bridge] Cancelling conflicting open order #{oid} for {symbol} ({status})...")
                try:
                    self.cancel_order(account_hash, str(oid))
                    cancelled += 1
                except Exception as err:
                    print(f"[Schwab Bridge] Warning: Could not cancel order #{oid}: {err}")
            
            for child in obj.get("childOrderStrategies", []):
                _cancel_if_match(child)

        for o in orders:
            _cancel_if_match(o)

        if cancelled > 0:
            print(f"[Schwab Bridge] Cancelled {cancelled} open bracket order(s) for {symbol}. Waiting 2s for Schwab book refresh...")
            time.sleep(2.0)
        return cancelled

def execute_schwab(tickets: List[Dict[str, Any]], dry_run: bool):
    print("\n[Schwab Bridge] Initializing Charles Schwab Trader API Bridge...")
    
    app_key = os.environ.get("SCHWAB_APP_KEY", "")
    secret = os.environ.get("SCHWAB_SECRET", "")

    # Sort tickets: SELL orders first (to unlock cash and eliminate risk), then BUY orders
    tickets.sort(key=lambda t: 0 if t.get("action") == "SELL" else 1)

    if dry_run:
        print("[Schwab Bridge] MODE: DRY RUN (Simulation only. No orders transmitted).")
        for t in tickets:
            legs = build_schwab_payloads(t)
            is_sell = (t.get("action") == "SELL")
            if is_sell:
                print(f"\n--- Staging Schwab MARKET SELL: {t['symbol']} ({int(t['shares'])} whole shares | Reason: {t.get('reason', 'EXIT')}) ---")
            else:
                print(f"\n--- Staging Schwab REST Bracket BUY: {t['symbol']} ({int(t['shares'])} whole shares, {len(legs)} leg orders) ---")
            print(json.dumps({"broker": "Charles Schwab Trader API", "orders": legs}, indent=2))
        print("\n[Schwab Bridge] Dry-run validation SUCCESS: All Schwab order payloads valid.")
        return

    if not app_key or not secret:
        print("[Schwab Bridge] Error: SCHWAB_APP_KEY and SCHWAB_SECRET required in .env or environment.")
        print("  Configure .env with:")
        print("    SCHWAB_APP_KEY=\"your_schwab_app_key\"")
        print("    SCHWAB_SECRET=\"your_schwab_secret\"")
        sys.exit(1)

    try:
        schwab = SchwabTraderAPI(app_key, secret)
        hashes = schwab.get_account_hashes()
        if not hashes:
            print("[Schwab Bridge] No linked Schwab trading accounts found.")
            sys.exit(1)
        acct_hash = hashes[0]["hashValue"]
        acct_num = hashes[0]["accountNumber"]
        print(f"[Schwab Bridge] Connected to Schwab Account: ***{acct_num[-4:]} (Hash: {acct_hash[:8]}...)")

        for t in tickets:
            legs = build_schwab_payloads(t)
            if not legs:
                print(f"[Schwab Bridge] Skipping {t['symbol']}: {t['shares']} shares rounds to 0 whole shares.")
                continue
            is_sell = (t.get("action") == "SELL")
            if is_sell:
                # Cancel conflicting open GTC bracket orders holding these shares first
                schwab.cancel_open_orders_for_symbol(acct_hash, t['symbol'])

            for leg_i, leg in enumerate(legs, 1):
                qty = leg["orderLegCollection"][0]["quantity"]
                if is_sell:
                    print(f"[Schwab Bridge] Submitting MARKET SELL for {t['symbol']} ({qty} shares | Reason: {t.get('reason', 'EXIT')})...")
                else:
                    print(f"[Schwab Bridge] Submitting bracket leg {leg_i}/{len(legs)} for {t['symbol']} ({qty} shares)...")
                try:
                    res = schwab.place_order(acct_hash, leg)
                    print(f"[Schwab Bridge] Order Submitted Successfully: {res}")
                except Exception as leg_err:
                    print(f"[Schwab Bridge] Order for {t['symbol']} failed: {leg_err}")
    except Exception as e:
        print(f"[Schwab Bridge] Execution error: {e}")
        sys.exit(1)

def sync_schwab(portfolio_file: str, dry_run: bool):
    import datetime
    print(f"\n[Schwab Sync] Reconciling Schwab account with {portfolio_file}...")
    if not os.path.exists(portfolio_file):
        port_data = {
            "total_capital": 10000.0,
            "cash_balance": 10000.0,
            "max_positions": 5,
            "peak_equity": 10000.0,
            "last_updated": str(datetime.datetime.now()),
            "positions": [],
            "closed_trades": []
        }
    else:
        with open(portfolio_file, "r") as f:
            port_data = json.load(f)

    if dry_run:
        print("[Schwab Sync] MODE: DRY RUN (Simulating portfolio sync with Charles Schwab)")
        print(f"[Schwab Sync] Recorded Cash Balance: ${port_data.get('cash_balance', 0):.2f}")
        print(f"[Schwab Sync] Recorded Total Equity: ${port_data.get('total_capital', 0):.2f}")
        print(f"[Schwab Sync] Open Positions in Local Portfolio: {len(port_data.get('positions', []))}")
        print("\n[Schwab Sync] Dry-run reconciliation check complete.")
        return

    app_key = os.environ.get("SCHWAB_APP_KEY", "")
    secret = os.environ.get("SCHWAB_SECRET", "")
    if not app_key or not secret:
        print("[Schwab Sync] Error: SCHWAB_APP_KEY and SCHWAB_SECRET required.")
        sys.exit(1)

    try:
        schwab = SchwabTraderAPI(app_key, secret)
        hashes = schwab.get_account_hashes()
        acct_hash = hashes[0]["hashValue"]
        details = schwab.get_account_details(acct_hash)
        
        sec_acct = details.get("securitiesAccount", {})
        balances = sec_acct.get("currentBalances", {})
        cash = balances.get("cashAvailableForTrading", balances.get("cashBalance", 10000.0))
        liquidation_val = balances.get("liquidationValue", 10000.0)

        port_data["cash_balance"] = round(float(cash), 2)
        port_data["total_capital"] = round(float(liquidation_val), 2)
        port_data["peak_equity"] = max(port_data.get("peak_equity", float(liquidation_val)), float(liquidation_val))

        ticket_lookup = {}
        if os.path.exists("LATEST_TICKET.txt"):
            try:
                ticket_lookup = {t["symbol"]: t for t in read_latest_tickets("LATEST_TICKET.txt")}
            except Exception:
                pass

        positions = sec_acct.get("positions", [])
        active_symbols = set()
        for p in positions:
            sym = p.get("instrument", {}).get("symbol", "")
            long_qty = float(p.get("longQuantity", 0))
            if sym and long_qty > 0:
                active_symbols.add(sym)
                avg_px = float(p.get("averagePrice", 0))
                existing = [pos for pos in port_data["positions"] if pos["symbol"] == sym]
                if existing:
                    existing[0]["shares"] = long_qty
                    existing[0]["cost_basis"] = round(long_qty * avg_px, 2)
                else:
                    t_match = ticket_lookup.get(sym)
                    # Dynamically anchor stop-loss and targets to actual Schwab fill price (avg_px)
                    risk_pct = 0.04
                    if t_match:
                        if t_match.get("stop_pct", 0) > 0:
                            risk_pct = float(t_match["stop_pct"])
                        elif t_match.get("signal_price", 0) > 0 and t_match.get("stop_loss", 0) > 0:
                            sig_p = float(t_match["signal_price"])
                            risk_pct = max(0.015, min(0.12, (sig_p - float(t_match["stop_loss"])) / sig_p))
                        elif t_match.get("stop_loss", 0) > 0 and float(t_match["stop_loss"]) < avg_px:
                            risk_pct = max(0.015, min(0.12, (avg_px - float(t_match["stop_loss"])) / avg_px))

                    # Calculate stop anchored to actual fill price
                    stop_px = round(avg_px * (1.0 - risk_pct), 2)

                    # INVERTED STOP SAFETY GUARD: Stop must be strictly lower than fill price!
                    if stop_px >= avg_px or stop_px <= 0:
                        stop_px = round(avg_px * 0.96, 2)

                    # Dynamically anchor Tier 1 (+1.5R) and Tier 2 (+3.0R) to actual fill price
                    risk_1r = max(0.01, avg_px - stop_px)
                    t1_px = round(avg_px + 1.5 * risk_1r, 2)
                    tgt_px = round(avg_px + 3.0 * risk_1r, 2)

                    port_data["positions"].append({
                        "symbol": sym,
                        "shares": long_qty,
                        "initial_shares": long_qty,
                        "entry_price": avg_px,
                        "entry_date": str(datetime.date.today()),
                        "stop_loss": stop_px,
                        "take_profit": tgt_px,
                        "tier1_target": t1_px,
                        "tier2_target": tgt_px,
                        "tier1_hit": False,
                        "tier2_hit": False,
                        "is_runner": False,
                        "highest_price": avg_px,
                        "atr": round(avg_px * 0.025, 2),
                        "cost_basis": round(long_qty * avg_px, 2),
                        "status": "OPEN"
                    })

        remaining = [pos for pos in port_data["positions"] if pos["symbol"] in active_symbols]
        closed = [pos for pos in port_data["positions"] if pos["symbol"] not in active_symbols]

        recent_orders = []
        try:
            recent_orders = schwab.get_orders(acct_hash)
        except Exception:
            pass

        for c in closed:
            sym_c = c["symbol"]
            real_fill_px = None
            for o in recent_orders:
                if o.get("status") == "FILLED":
                    legs = o.get("orderLegCollection", [])
                    o_sym = legs[0].get("instrument", {}).get("symbol", "") if legs else ""
                    o_inst = legs[0].get("instruction", "") if legs else ""
                    if o_sym == sym_c and o_inst == "SELL":
                        acts = o.get("orderActivityCollection", [])
                        for act in acts:
                            for elog in act.get("executionLegs", []):
                                if elog.get("price"):
                                    real_fill_px = float(elog.get("price"))
                                    break
                        if real_fill_px is not None:
                            break

            if real_fill_px is not None and real_fill_px > 0:
                exit_px = real_fill_px
            else:
                exit_px = _get_quote_or_fallback(sym_c)
                if exit_px is None or exit_px <= 0:
                    exit_px = float(c.get("current_price") or c.get("entry_price") or 0.0)
            c["exit_price"] = round(exit_px, 2)
            ent_px = float(c.get("entry_price", 0.0))
            shs = float(c.get("shares", 0.0))
            c["pnl_dollar"] = round((exit_px - ent_px) * shs, 2)
            c["pnl_pct"] = round(((exit_px / ent_px) - 1.0) * 100.0, 2) if ent_px > 0 else 0.0
            c["exit_date"] = str(datetime.date.today())
            c["reason"] = "SCHWAB_SYNC_CLOSED"
            port_data["closed_trades"].append(c)

        port_data["positions"] = remaining
        port_data["last_updated"] = str(datetime.datetime.now())

        with open(portfolio_file, "w") as f:
            json.dump(port_data, f, indent=2)

        print(f"[Schwab Sync] SUCCESS: Synchronized {len(port_data['positions'])} positions, ${port_data['cash_balance']:.2f} cash.")
    except Exception as e:
        print(f"[Schwab Sync] Error: {e}")
        sys.exit(1)

def check_schwab_orders():
    import datetime
    app_key = os.environ.get("SCHWAB_APP_KEY", "")
    secret = os.environ.get("SCHWAB_SECRET", "")
    if not app_key or not secret:
        print("[Schwab Orders] Error: Set SCHWAB_APP_KEY and SCHWAB_SECRET in environment or .env.")
        sys.exit(1)
    schwab = SchwabTraderAPI(app_key, secret)
    hashes = schwab.get_account_hashes()
    if not hashes:
        print("[Schwab Orders] No linked Schwab trading accounts found.")
        sys.exit(1)
    acct_hash = hashes[0]["hashValue"]
    acct_num = hashes[0]["accountNumber"]
    print(f"\n[Schwab Orders] Fetching recent orders for Account ***{acct_num[-4:]}...")
    try:
        orders = schwab.get_orders(acct_hash)
    except Exception as e:
        print(f"[Schwab Orders] Query error: {e}")
        return

    if not orders:
        print("  No recent orders found.")
        return

    print(f" Found {len(orders)} order(s):\n")
    for o in orders:
        oid = o.get("orderId", "N/A")
        otype = o.get("orderType", "MARKET")
        status = o.get("status", "UNKNOWN")
        legs = o.get("orderLegCollection", [])
        sym = legs[0].get("instrument", {}).get("symbol", "N/A") if legs else "N/A"
        inst = legs[0].get("instruction", "ORDER") if legs else "N/A"
        qty = o.get("quantity", legs[0].get("quantity", 0) if legs else 0)
        filled_qty = o.get("filledQuantity", 0)
        entered_time = o.get("enteredTime", "N/A")
        acts = o.get("orderActivityCollection", [])
        exec_px = None
        exec_time = None
        for act in acts:
            for elog in act.get("executionLegs", []):
                exec_px = elog.get("price")
                exec_time = elog.get("time")
        
        status_desc = o.get("statusDescription") or o.get("cancelDescription") or o.get("rejectReason") or ""
        desc_info = f" -> {status_desc}" if status_desc else ""
        status_icon = "✓ [FILLED]" if status == "FILLED" else (f"❌ [{status}]" if status in ["CANCELED", "REJECTED", "EXPIRED"] else f"⏳ [{status}]")
        px_info = f" @ ${exec_px:.2f}" if exec_px is not None else ""
        time_info = f" (Filled: {exec_time})" if exec_time else f" (Entered: {entered_time})"
        print(f"  * {status_icon} Order #{oid}: {inst} {filled_qty}/{qty} {sym} ({otype}){px_info}{time_info}{desc_info}")
        
        # Display any linked child bracket orders (e.g. GTC stop-loss and profit target)
        children = o.get("childOrderStrategies", [])
        for ch in children:
            sub_orders = ch.get("childOrderStrategies", []) if ch.get("orderStrategyType") == "OCO" else [ch]
            for sub in sub_orders:
                sub_id = sub.get("orderId", "")
                sub_status = sub.get("status", "UNKNOWN")
                sub_type = sub.get("orderType", "LIMIT")
                sub_px = sub.get("price") or sub.get("stopPrice") or 0.0
                sub_qty = sub.get("quantity", 0)
                sub_filled = sub.get("filledQuantity", 0)
                sub_legs = sub.get("orderLegCollection", [])
                sub_sym = sub_legs[0].get("instrument", {}).get("symbol", sym) if sub_legs else sym
                sub_inst = sub_legs[0].get("instruction", "SELL") if sub_legs else "SELL"
                sub_icon = "✓ [FILLED]" if sub_status == "FILLED" else (f"⏳ [{sub_status}]" if sub_status in ["WORKING", "QUEUED", "PENDING_ACTIVATION"] else f"[{sub_status}]")
                px_label = f"${sub_px:.2f}" if sub_px else ""
                print(f"      ↳ {sub_icon} Child #{sub_id}: {sub_inst} {sub_filled}/{sub_qty} {sub_sym} {sub_type} {px_label}")

def _get_quote_or_fallback(symbol: str):
    """Fetches latest quote for a symbol with fallback to Yahoo finance chart API."""
    try:
        import urllib.request
        url = f"https://query1.finance.yahoo.com/v8/finance/chart/{symbol}?interval=1d&range=1d"
        req = urllib.request.Request(url, headers={"User-Agent": "Mozilla/5.0"})
        with urllib.request.urlopen(req, timeout=5) as resp:
            data = json.loads(resp.read().decode())
            meta = data["chart"]["result"][0]["meta"]
            px = meta.get("regularMarketPrice") or meta.get("chartPreviousClose")
            if px:
                return float(px)
    except Exception:
        pass
    return None

def sync_ibkr(portfolio_file: str, host: str, port: int, client_id: int, dry_run: bool):
    import datetime
    print(f"\n[IBKR Sync] Connecting to TWS/Gateway at {host}:{port} (Client ID: {client_id})...")
    
    if not os.path.exists(portfolio_file):
        port_data = {
            "total_capital": 10000.0,
            "cash_balance": 10000.0,
            "max_positions": 5,
            "peak_equity": 10000.0,
            "last_updated": str(datetime.datetime.now()),
            "positions": [],
            "closed_trades": []
        }
    else:
        with open(portfolio_file, "r") as f:
            port_data = json.load(f)

    if dry_run:
        print("[IBKR Sync] MODE: DRY RUN (Simulating portfolio sync with IBKR)")
        print(f"[IBKR Sync] Recorded Cash Balance: ${port_data.get('cash_balance', 0):.2f}")
        print(f"[IBKR Sync] Recorded Total Equity: ${port_data.get('total_capital', 0):.2f}")
        print(f"[IBKR Sync] Open Positions in Local Portfolio: {len(port_data.get('positions', []))}")
        for pos in port_data.get("positions", []):
            print(f"  * {pos['symbol']}: {pos['shares']} shs @ ${pos['entry_price']:.2f}")
        print("\n[IBKR Sync] Dry-run reconciliation check complete. (Live connection requires --dry_run=false)")
        return

    try:
        try:
            from ib_async import IB
        except ImportError:
            from ib_insync import IB
        ib = IB()
        ib.connect(host, port, clientId=client_id)
        
        ib_positions = ib.positions()
        account_values = ib.accountSummary()
        
        cash_val = None
        equity_val = None
        for item in account_values:
            if item.tag == "TotalCashValue":
                cash_val = float(item.value)
            elif item.tag == "NetLiquidation":
                equity_val = float(item.value)
                
        if cash_val is not None:
            port_data["cash_balance"] = round(cash_val, 2)
        if equity_val is not None:
            port_data["total_capital"] = round(equity_val, 2)
            port_data["peak_equity"] = max(port_data.get("peak_equity", equity_val), equity_val)
            
        ticket_lookup = {}
        if os.path.exists("LATEST_TICKET.txt"):
            try:
                ticket_lookup = {t["symbol"]: t for t in read_latest_tickets("LATEST_TICKET.txt")}
            except Exception:
                pass

        active_symbols = set()
        for p in ib_positions:
            if p.position > 0:
                sym = p.contract.symbol
                active_symbols.add(sym)
                existing = [pos for pos in port_data["positions"] if pos["symbol"] == sym]
                if existing:
                    existing[0]["shares"] = float(p.position)
                    existing[0]["cost_basis"] = round(float(p.position) * float(p.avgCost), 2)
                else:
                    t_match = ticket_lookup.get(sym)
                    stop_px = round(float(t_match["stop_loss"]), 2) if t_match else round(float(p.avgCost) * 0.95, 2)
                    tgt_px = round(float(t_match["tier2_target"]), 2) if t_match else round(float(p.avgCost) * 1.10, 2)
                    t1_px = round(float(t_match["tier1_target"]), 2) if t_match else round(float(p.avgCost) * 1.05, 2)
                    port_data["positions"].append({
                        "symbol": sym,
                        "shares": float(p.position),
                        "initial_shares": float(p.position),
                        "entry_price": round(float(p.avgCost), 2),
                        "entry_date": str(datetime.date.today()),
                        "stop_loss": stop_px,
                        "take_profit": tgt_px,
                        "tier1_target": t1_px,
                        "tier2_target": tgt_px,
                        "tier1_hit": False,
                        "tier2_hit": False,
                        "is_runner": False,
                        "highest_price": round(float(p.avgCost), 2),
                        "atr": round(float(p.avgCost) * 0.025, 2),
                        "cost_basis": round(float(p.position) * float(p.avgCost), 2),
                        "status": "OPEN"
                    })
                    
        closed = []
        remaining = []
        for pos in port_data["positions"]:
            if pos["symbol"] not in active_symbols:
                closed.append(pos)
            else:
                remaining.append(pos)
                
        for c in closed:
            exit_px = _get_quote_or_fallback(c["symbol"])
            if exit_px is None or exit_px <= 0:
                exit_px = float(c.get("current_price") or c.get("entry_price") or 0.0)
            c["exit_price"] = round(exit_px, 2)
            ent_px = float(c.get("entry_price", 0.0))
            shs = float(c.get("shares", 0.0))
            c["pnl_dollar"] = round((exit_px - ent_px) * shs, 2)
            c["pnl_pct"] = round(((exit_px / ent_px) - 1.0) * 100.0, 2) if ent_px > 0 else 0.0
            c["exit_date"] = str(datetime.date.today())
            c["reason"] = "IBKR_SYNC_CLOSED"
            port_data["closed_trades"].append(c)
            print(f"[IBKR Sync] Archived closed position: {c['symbol']} @ ${exit_px:.2f} (PnL: ${c['pnl_dollar']:+.2f})")
            
        port_data["positions"] = remaining
        port_data["last_updated"] = str(datetime.datetime.now())
        
        with open(portfolio_file, "w") as f:
            json.dump(port_data, f, indent=2)
            
        print(f"[IBKR Sync] SUCCESS: Synchronized {len(port_data['positions'])} positions, ${port_data['cash_balance']:.2f} cash.")
        ib.disconnect()
    except ImportError:
        print("[IBKR Sync] 'ib_async' package required for live IBKR sync. Install with: pip install ib_async")
    except Exception as e:
        print(f"[IBKR Sync] Error: {e}")
        sys.exit(1)

def interactive_edit_tickets(tickets: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
    """Allows user to interactively inspect and adjust order parameters before transmission."""
    edited_tickets = []
    print("\n[Interactive Order Editor] Review and adjust order parameters below (press Enter to keep default):")
    for idx, t in enumerate(tickets, 1):
        print(f"\n------------------------------------------------------------")
        if t.get("action") == "SELL":
            print(f" Ticket #{idx}: [SELL] {t['symbol']} | Reason: {t.get('reason', 'EXIT')}")
            print(f" Current: SELL {t['shares']} shares")
            print(f"------------------------------------------------------------")
            choice = input(f" Action for {t['symbol']} [1=Keep, 2=Edit Shares, 3=Skip]: ").strip().lower()
            if choice in ["3", "s", "skip"]:
                print(f" -> Skipped {t['symbol']}.")
                continue
            elif choice in ["2", "e", "edit"]:
                new_shares_str = input(f"   Shares [{t['shares']}]: ").strip()
                if new_shares_str:
                    try:
                        t['shares'] = float(new_shares_str)
                    except ValueError:
                        print("   Invalid number, keeping default.")
            edited_tickets.append(t)
            continue

        print(f" Ticket #{idx}: {t['symbol']} ({t.get('sector', 'Swing')}) | P(Up): {t.get('p_up', 0.0)}%")
        print(f" Current: {t['shares']} shares | Stop: ${t.get('stop_loss', 0.0):.2f} | T1: ${t.get('tier1_target', 0.0):.2f} | T2: ${t.get('tier2_target', 0.0):.2f}")
        print(f"------------------------------------------------------------")
        choice = input(f" Action for {t['symbol']} [1=Keep, 2=Edit, 3=Skip]: ").strip().lower()
        if choice in ["3", "s", "skip"]:
            print(f" -> Skipped {t['symbol']}.")
            continue
        elif choice in ["2", "e", "edit"]:
            # Edit shares
            new_shares_str = input(f"   Shares [{t['shares']}]: ").strip()
            if new_shares_str:
                try:
                    t['shares'] = float(new_shares_str)
                    t['tier1_shares'] = round(t['shares'] / 2.0, 4)
                    t['tier2_shares'] = round(t['shares'] - t['tier1_shares'], 4)
                except ValueError:
                    print("   Invalid number, keeping default.")
            
            # Edit stop loss
            new_stop_str = input(f"   Stop-Loss Price [${t['stop_loss']:.2f}]: ").strip().replace("$", "")
            if new_stop_str:
                try:
                    t['stop_loss'] = float(new_stop_str)
                except ValueError:
                    print("   Invalid price, keeping default.")

            # Edit Tier 1 target
            new_t1_str = input(f"   Tier 1 Target [${t['tier1_target']:.2f}]: ").strip().replace("$", "")
            if new_t1_str:
                try:
                    t['tier1_target'] = float(new_t1_str)
                except ValueError:
                    print("   Invalid price, keeping default.")

            # Edit Tier 2 target
            new_t2_str = input(f"   Tier 2 Target [${t['tier2_target']:.2f}]: ").strip().replace("$", "")
            if new_t2_str:
                try:
                    t['tier2_target'] = float(new_t2_str)
                except ValueError:
                    print("   Invalid price, keeping default.")

            print(f" -> Updated {t['symbol']}: {t['shares']} shs | Stop: ${t['stop_loss']:.2f} | T1: ${t['tier1_target']:.2f} | T2: ${t['tier2_target']:.2f}")
        else:
            print(f" -> Kept {t['symbol']} as is.")
        
        edited_tickets.append(t)
    return edited_tickets

def main():
    parser = argparse.ArgumentParser(description="Multi-Broker Execution Bridge for Swing Trading System")
    parser.add_argument("--broker", choices=["ibkr", "schwab"], default="ibkr", help="Target broker (ibkr or schwab)")
    parser.add_argument("--dry_run", type=str, default="true", help="Dry run mode (true/false)")
    parser.add_argument("--ticket_file", default="LATEST_TICKET.txt", help="Path to latest ticket file")
    parser.add_argument("--symbols", type=str, default="", help="Comma-separated symbols to filter (e.g. AMD,SNDK)")
    parser.add_argument("--interactive", "-i", action="store_true", help="Interactively review and edit orders before execution")
    parser.add_argument("--yes", "-y", action="store_true", help="Skip live execution confirmation prompt")
    parser.add_argument("--portfolio_file", default="portfolio.json", help="Path to portfolio state file")
    parser.add_argument("--sync", action="store_true", help="Synchronize local portfolio state with live broker")
    parser.add_argument("--orders", action="store_true", help="Display recent order status, executions, and fills from broker")
    parser.add_argument("--sell", type=str, default="", help="Direct market sell with automatic open-bracket clearance (e.g. --sell=PENG:13)")
    parser.add_argument("--auth", action="store_true", help="Authenticate with Charles Schwab OAuth2")
    parser.add_argument("--port", type=int, default=7497, help="IBKR TWS/Gateway port (7497 Paper, 7496 Live)")
    parser.add_argument("--host", default="127.0.0.1", help="IBKR Host IP")
    parser.add_argument("--client_id", type=int, default=1, help="IBKR Client ID")

    args = parser.parse_args()
    dry_run = args.dry_run.lower() in ["true", "1", "yes"]

    print("================================================================================")
    print("                 MULTI-BROKER ORDER EXECUTION & BRACKET BRIDGE                  ")
    print("================================================================================")
    print(f" Target Broker: {args.broker.upper()} | Mode: {'DRY RUN (Preview)' if dry_run else 'LIVE ACTION'}")

    if args.orders:
        print(f" Operation: ORDER STATUS & FILLS QUERY")
        if args.broker == "schwab":
            check_schwab_orders()
        else:
            print("[Orders] --orders currently implemented for Charles Schwab.")
        print("\n================================================================================")
        return

    if args.sell:
        parts = args.sell.split(":")
        sell_sym = parts[0].strip().upper()
        sell_qty = float(parts[1]) if len(parts) > 1 else 0.0
        if sell_qty <= 0:
            print(f"[Direct Sell] Error: Specify valid positive quantity (e.g. --sell={sell_sym}:13)")
            sys.exit(1)
        print(f" Operation: DIRECT MARKET SELL for {sell_sym} ({sell_qty} shares)")
        direct_ticket = [{
            "rank": 0,
            "action": "SELL",
            "symbol": sell_sym,
            "shares": sell_qty,
            "reason": "DIRECT_MANUAL_SELL"
        }]
        if args.broker == "schwab":
            execute_schwab(direct_ticket, dry_run)
            if not dry_run:
                sync_schwab(args.portfolio_file, False)
        else:
            execute_ibkr(direct_ticket, args.host, args.port, args.client_id, dry_run)
        print("\n================================================================================")
        return
    
    if args.auth:
        if args.broker != "schwab":
            print("[Auth] --auth is used for Charles Schwab OAuth2.")
            sys.exit(0)
        app_key = os.environ.get("SCHWAB_APP_KEY", "")
        secret = os.environ.get("SCHWAB_SECRET", "")
        if not app_key or not secret:
            print("[Schwab Auth] Error: Set SCHWAB_APP_KEY and SCHWAB_SECRET in environment or .env first.")
            sys.exit(1)
        api = SchwabTraderAPI(app_key, secret)
        print("\n================ Charles Schwab OAuth2 Authorization ================")
        print("1. Open this URL in your web browser:")
        print(f"\n   {api.get_auth_url()}\n")
        print("2. Log in with your Charles Schwab username/password and approve the application.")
        print("3. Schwab will redirect your browser to 127.0.0.1 (it may display a connection error or blank page).")
        print("4. Copy the ENTIRE URL from your browser's address bar (containing 'code=...') and paste it below:\n")
        pasted_url = input("Paste redirected URL: ").strip()
        if pasted_url:
            api.exchange_code(pasted_url)
            print(f"\n[Schwab Auth] SUCCESS: OAuth2 tokens saved to {api.TOKEN_FILE}!")
        print("================================================================================")
        sys.exit(0)

    if args.sync:
        print(f" Operation: PORTFOLIO RECONCILIATION (--sync) with {args.portfolio_file}")
        if args.broker == "ibkr":
            sync_ibkr(args.portfolio_file, args.host, args.port, args.client_id, dry_run)
        else:
            sync_schwab(args.portfolio_file, dry_run)
        print("\n================================================================================")
        return

    print(f" Source Ticket: {args.ticket_file}")
    tickets = parse_latest_tickets(args.ticket_file)

    # Filter symbols if requested
    if args.symbols:
        allowed = {s.strip().upper() for s in args.symbols.split(",") if s.strip()}
        tickets = [t for t in tickets if t["symbol"].upper() in allowed]
        print(f" Filtered to {len(tickets)} requested symbol(s): {', '.join(allowed)}")

    print(f" Loaded {len(tickets)} Actionable Order Ticket(s):\n")

    for t in tickets:
        if t.get("action") == "SELL":
            print(f"  * [SELL] {t['symbol']}: SELL {t['shares']} shs | Reason: {t.get('reason', 'EXIT')}")
        else:
            sec = t.get("sector", "Swing")
            stp = t.get("stop_loss", 0.0)
            t1 = t.get("tier1_target", 0.0)
            t2 = t.get("tier2_target", 0.0)
            print(f"  * #{t.get('rank', 1)} {t['symbol']} ({sec}): BUY {t['shares']} shs | Stop: ${stp} | T1: ${t1} | T2: ${t2}")

    # Interactive edit mode
    if args.interactive:
        tickets = interactive_edit_tickets(tickets)
        if not tickets:
            print("\n[Notice] No orders remaining after interactive review. Exiting.")
            sys.exit(0)

    # Confirmation before live execution
    if not dry_run and not args.yes:
        print(f"\n[Live Confirmation Required] You are about to transmit {len(tickets)} order(s) to {args.broker.upper()}:")
        for t in tickets:
            if t.get("action") == "SELL":
                print(f"   -> SELL {t['shares']} {t['symbol']} | Reason: {t.get('reason', 'EXIT')}")
            else:
                stp = t.get("stop_loss", 0.0)
                t1 = t.get("tier1_target", 0.0)
                print(f"   -> BUY {t['shares']} {t['symbol']} | Stop: ${stp:.2f} | T1: ${t1:.2f}")
        confirm = input("\nType 'yes' to transmit orders to broker: ").strip().lower()
        if confirm not in ["yes", "y"]:
            print("[Cancelled] Transmission aborted. No orders were sent.")
            sys.exit(0)

    if args.broker == "ibkr":
        execute_ibkr(tickets, args.host, args.port, args.client_id, dry_run)
    elif args.broker == "schwab":
        execute_schwab(tickets, dry_run)

    print("\n================================================================================")

if __name__ == "__main__":
    main()
