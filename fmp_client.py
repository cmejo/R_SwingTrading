#!/usr/bin/env python3
"""
fmp_client.py
Financial Modeling Prep (FMP) Stable API Client.
Reads FMP_API_KEY from .env and fetches Wall Street consensus targets,
analyst ratings, and fundamental growth metrics for portfolio monitoring.
"""

import os
import sys
import json
import urllib.request
import urllib.error
from typing import Dict, List, Optional, Any

BASE_URL = "https://financialmodelingprep.com/stable"


def load_fmp_api_key(env_path: str = ".env") -> str:
    """Safely loads FMP_API_KEY from environment or .env file."""
    if "FMP_API_KEY" in os.environ and os.environ["FMP_API_KEY"].strip():
        return os.environ["FMP_API_KEY"].strip()
    if os.path.exists(env_path):
        with open(env_path, "r", encoding="utf-8") as f:
            for line in f:
                line = line.strip()
                if line and not line.startswith("#") and "=" in line:
                    k, v = line.split("=", 1)
                    if k.strip() == "FMP_API_KEY":
                        return v.strip().strip("\"'")
    return ""


def fmp_get(endpoint: str, api_key: str, params: Optional[Dict[str, str]] = None) -> Any:
    """Helper to query FMP Stable API endpoints."""
    if not api_key:
        return None
    url = f"{BASE_URL}/{endpoint}"
    p = params.copy() if params else {}
    p["apikey"] = api_key
    query_str = "&".join(f"{k}={v}" for k, v in p.items())
    full_url = f"{url}?{query_str}"
    req = urllib.request.Request(full_url, headers={"User-Agent": "Antigravity/1.0"})
    try:
        with urllib.request.urlopen(req, timeout=10) as resp:
            return json.loads(resp.read().decode())
    except urllib.error.HTTPError as e:
        return None
    except Exception:
        return None


def get_symbol_fundamentals(symbol: str, api_key: str) -> Dict[str, Any]:
    """Fetches consensus price targets, analyst ratings, and growth metrics."""
    result = {
        "symbol": symbol.upper(),
        "price": None,
        "target_consensus": None,
        "target_upside_pct": None,
        "consensus_grade": "N/A",
        "buy_count": 0,
        "hold_count": 0,
        "sell_count": 0,
        "eps_growth_pct": None,
        "revenue_growth_pct": None
    }
    if not api_key:
        return result

    sym = symbol.upper()

    # 1. Real-time Quote
    quote = fmp_get("quote", api_key, {"symbol": sym})
    if quote and isinstance(quote, list) and len(quote) > 0:
        result["price"] = quote[0].get("price")

    # 2. Price Target Consensus
    targets = fmp_get("price-target-consensus", api_key, {"symbol": sym})
    if targets and isinstance(targets, list) and len(targets) > 0:
        tc = targets[0].get("targetConsensus")
        result["target_consensus"] = tc
        if tc and result["price"] and result["price"] > 0:
            result["target_upside_pct"] = round((tc / result["price"] - 1.0) * 100.0, 1)

    # 3. Analyst Grades Consensus
    grades = fmp_get("grades-consensus", api_key, {"symbol": sym})
    if grades and isinstance(grades, list) and len(grades) > 0:
        result["consensus_grade"] = grades[0].get("consensus", "N/A")
        result["buy_count"] = grades[0].get("buy", 0)
        result["hold_count"] = grades[0].get("hold", 0)
        result["sell_count"] = grades[0].get("sell", 0)

    # 4. Financial Growth (Annual)
    growth = fmp_get("financial-growth", api_key, {"symbol": sym, "period": "annual"})
    if growth and isinstance(growth, list) and len(growth) > 0:
        eg = growth[0].get("epsgrowth")
        rg = growth[0].get("revenueGrowth")
        if eg is not None:
            result["eps_growth_pct"] = round(eg * 100.0, 1)
        if rg is not None:
            result["revenue_growth_pct"] = round(rg * 100.0, 1)

    return result


def print_summary_table(symbols: List[str], api_key: str):
    """Prints a clean tabular summary for a list of tickers."""
    header = "%-7s | %-8s | %-8s | %-9s | %-10s | %-14s | %-10s | %-10s" % (
        "Symbol", "Price", "Target", "Upside", "Consensus", "Buy/Hold/Sell", "EPS Growth", "Rev Growth"
    )
    print("\n" + "=" * len(header))
    print("                     WALL STREET CONSENSUS & FUNDAMENTAL SNAPSHOT (FMP)       ")
    print("=" * len(header))
    print(header)
    print("-" * len(header))
    for s in symbols:
        data = get_symbol_fundamentals(s, api_key)
        px_str = f"${data['price']:.2f}" if data['price'] else "N/A"
        tg_str = f"${data['target_consensus']:.2f}" if data['target_consensus'] else "N/A"
        up_str = f"{data['target_upside_pct']:+.1f}%" if data['target_upside_pct'] is not None else "N/A"
        b_h_s = f"{data['buy_count']}/{data['hold_count']}/{data['sell_count']}"
        eg_str = f"{data['eps_growth_pct']:+.1f}%" if data['eps_growth_pct'] is not None else "N/A"
        rg_str = f"{data['revenue_growth_pct']:+.1f}%" if data['revenue_growth_pct'] is not None else "N/A"
        print("%-7s | %-8s | %-8s | %-9s | %-10s | %-14s | %-10s | %-10s" % (
            data["symbol"], px_str, tg_str, up_str, data["consensus_grade"], b_h_s, eg_str, rg_str
        ))
    print("=" * len(header) + "\n")


if __name__ == "__main__":
    key = load_fmp_api_key()
    if not key:
        print("[fmp_client] ERROR: FMP_API_KEY not found in environment or .env file.", file=sys.stderr)
        sys.exit(1)

    target_symbols = sys.argv[1:]
    if not target_symbols:
        # Default to current portfolio satellites if available
        if os.path.exists("portfolio_coresat.json"):
            try:
                with open("portfolio_coresat.json") as pf:
                    target_symbols = [s["symbol"] for s in json.load(pf).get("satellite_positions", [])]
            except Exception:
                target_symbols = ["AAPL", "NVDA", "MSFT"]
        else:
            target_symbols = ["AAPL", "NVDA", "MSFT"]

    print_summary_table(target_symbols, key)
