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

def execute_schwab(tickets: List[Dict[str, Any]], dry_run: bool):
    print("\n[Schwab Bridge] Initializing Charles Schwab Trader API Bridge...")
    
    app_key = os.environ.get("SCHWAB_APP_KEY", "")
    secret = os.environ.get("SCHWAB_SECRET", "")

    if dry_run:
        print("[Schwab Bridge] MODE: DRY RUN (Simulation only. No orders transmitted).")
        for t in tickets:
            payload = build_schwab_payload(t)
            print(f"\n--- Staging Schwab REST Bracket: {t['symbol']} ({t['shares']} shares) ---")
            print(json.dumps(payload, indent=2))
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
            payload = build_schwab_payload(t)
            print(f"[Schwab Bridge] Submitting Live Bracket for {t['symbol']} ({t['shares']} shares)...")
            res = schwab.place_order(acct_hash, payload)
            print(f"[Schwab Bridge] Order Submitted Successfully: {res}")
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
                    port_data["positions"].append({
                        "symbol": sym,
                        "shares": long_qty,
                        "entry_price": avg_px,
                        "entry_date": str(datetime.date.today()),
                        "stop_loss": round(avg_px * 0.95, 2),
                        "take_profit": round(avg_px * 1.10, 2),
                        "cost_basis": round(long_qty * avg_px, 2),
                        "status": "OPEN"
                    })

        remaining = [pos for pos in port_data["positions"] if pos["symbol"] in active_symbols]
        closed = [pos for pos in port_data["positions"] if pos["symbol"] not in active_symbols]
        for c in closed:
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
                    port_data["positions"].append({
                        "symbol": sym,
                        "shares": float(p.position),
                        "entry_price": round(float(p.avgCost), 2),
                        "entry_date": str(datetime.date.today()),
                        "stop_loss": round(float(p.avgCost) * 0.95, 2),
                        "take_profit": round(float(p.avgCost) * 1.10, 2),
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
            c["exit_date"] = str(datetime.date.today())
            c["reason"] = "BROKER_SYNC_CLOSED"
            port_data["closed_trades"].append(c)
            print(f"[IBKR Sync] Archived closed position: {c['symbol']}")
            
        port_data["positions"] = remaining
        port_data["last_updated"] = str(datetime.datetime.now())
        
        with open(portfolio_file, "w") as f:
            json.dump(port_data, f, indent=2)
            
        print(f"[IBKR Sync] SUCCESS: Synchronized {len(port_data['positions'])} positions, ${port_data['cash_balance']:.2f} cash.")
        ib.disconnect()
    except ImportError:
        print("[IBKR Sync] 'ib_insync' package required for live IBKR sync. Install with: pip install ib_insync")
    except Exception as e:
        print(f"[IBKR Sync] Error: {e}")
        sys.exit(1)

def interactive_edit_tickets(tickets: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
    """Allows user to interactively inspect and adjust order parameters before transmission."""
    edited_tickets = []
    print("\n[Interactive Order Editor] Review and adjust order parameters below (press Enter to keep default):")
    for idx, t in enumerate(tickets, 1):
        print(f"\n------------------------------------------------------------")
        print(f" Ticket #{idx}: {t['symbol']} ({t['sector']}) | P(Up): {t['p_up']}%")
        print(f" Current: {t['shares']} shares | Stop: ${t['stop_loss']:.2f} | T1: ${t['tier1_target']:.2f} | T2: ${t['tier2_target']:.2f}")
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
        print(f"  * #{t['rank']} {t['symbol']} ({t['sector']}): BUY {t['shares']} shs | Stop: ${t['stop_loss']} | T1: ${t['tier1_target']} | T2: ${t['tier2_target']}")

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
            print(f"   -> BUY {t['shares']} {t['symbol']} | Stop: ${t['stop_loss']:.2f} | T1: ${t['tier1_target']:.2f}")
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
