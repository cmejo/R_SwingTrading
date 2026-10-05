#!/usr/bin/env python3
"""
Python Broker Execution & Bridge Test Suite (F5)
Tests:
  1. Ticket parsing from LATEST_TICKET.txt format
  2. Schwab FIRST_TRIGGERS_OCO bracket payload structure
  3. IBKR OCA bracket payload structure & whole-share leg splitting
  4. Robinhood exit daemon trading-day counting & portfolio.json sync
"""

import sys
import os
import json
import unittest
import tempfile
from typing import Dict, Any

# Ensure parent directory is on sys.path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))

from execute_broker import read_latest_tickets, _schwab_leg_order, build_schwab_payloads
from execute_ibkr import split_whole_share_legs, build_ibkr_bracket_spec
from poll_robinhood_exits import count_trading_days, sync_exit_to_portfolio_json


class TestBrokerBridges(unittest.TestCase):

    def setUp(self):
        self.sample_ticket_text = (
            "========================================================================================\n"
            "               MULTI-ASSET SWING TRADING OPPORTUNITY LEADERBOARD\n"
            "========================================================================================\n"
            "--- ORDER TICKET #1: AMD (P(Up): 68.5% | Macro: Bullish | Sector: Semiconductors) ---\n"
            "  Action:            BUY 20.000 SHARES\n"
            "  Limit/Market:      EXECUTE AT MARKET ($110.50)\n"
            "  GTC Stop-Loss:     $98.50 (-10.9% / 2.0x Daily Volatility)\n"
            "  Tier 1 (50% = 10.0 shs): Target $120.00 (+8.6% / 1.5R Risk-Reward)\n"
            "  Tier 2 (50% = 10.0 shs): Target $129.50 (+17.2% / 3.0R Risk-Reward)\n"
            "  Actual Outlay:     $2210.00 (22.1% of Capital)\n"
            "========================================================================================\n"
        )

    def test_parse_latest_tickets(self):
        with tempfile.NamedTemporaryFile("w+", delete=False) as f:
            f.write(self.sample_ticket_text)
            temp_path = f.name

        try:
            tickets = read_latest_tickets(temp_path)
            self.assertEqual(len(tickets), 1)
            t = tickets[0]
            self.assertEqual(t["symbol"], "AMD")
            self.assertEqual(t["shares"], 20.0)
            self.assertEqual(t["stop_loss"], 98.50)
            self.assertEqual(t["tier1_target"], 120.00)
            self.assertEqual(t["tier2_target"], 129.50)
            self.assertEqual(t["tier1_shares"], 10.0)
            self.assertEqual(t["tier2_shares"], 10.0)
            self.assertEqual(t["estimated_outlay"], 2210.00)
        finally:
            if os.path.exists(temp_path):
                os.remove(temp_path)

    def test_split_whole_share_legs_ibkr(self):
        # Even number
        split_even = split_whole_share_legs({"shares": 10})
        self.assertEqual(split_even["total"], 10)
        self.assertEqual(split_even["tier1"], 5)
        self.assertEqual(split_even["tier2"], 5)

        # Odd number
        split_odd = split_whole_share_legs({"shares": 9})
        self.assertEqual(split_odd["total"], 9)
        self.assertEqual(split_odd["tier1"], 4)
        self.assertEqual(split_odd["tier2"], 5)

        # Single share
        split_one = split_whole_share_legs({"shares": 1})
        self.assertEqual(split_one["total"], 1)
        self.assertEqual(split_one["tier1"], 0)
        self.assertEqual(split_one["tier2"], 1)

    def test_build_ibkr_bracket_spec(self):
        ticket = {
            "symbol": "AMD",
            "shares": 10,
            "stop_loss": 98.50,
            "tier1_target": 120.00,
            "tier2_target": 129.50
        }
        spec = build_ibkr_bracket_spec(ticket)
        self.assertIsNotNone(spec)
        self.assertEqual(spec["symbol"], "AMD")
        self.assertEqual(spec["parent_order"]["totalQuantity"], 10)
        self.assertEqual(len(spec["exit_legs"]), 2)  # Tier 1 OCA and Tier 2 OCA legs

        t1_leg = spec["exit_legs"][0]
        t2_leg = spec["exit_legs"][1]
        self.assertEqual(t1_leg["ocaGroup"], "OCA_AMD_T1")
        self.assertEqual(t2_leg["ocaGroup"], "OCA_AMD_T2")
        self.assertEqual(t1_leg["target"]["lmtPrice"], 120.00)
        self.assertEqual(t2_leg["target"]["lmtPrice"], 129.50)

    def test_build_schwab_payloads(self):
        ticket = {
            "symbol": "AMD",
            "shares": 10,
            "stop_loss": 98.50,
            "tier1_shares": 5,
            "tier1_target": 120.00,
            "tier2_shares": 5,
            "tier2_target": 129.50
        }
        payloads = build_schwab_payloads(ticket)
        self.assertEqual(len(payloads), 2)  # Two separate FIRST_TRIGGERS_OCO orders
        for p in payloads:
            self.assertEqual(p["orderStrategyType"], "TRIGGER")
            self.assertEqual(p["orderLegCollection"][0]["instruction"], "BUY")
            child_oco = p["childOrderStrategies"][0]
            self.assertEqual(child_oco["orderStrategyType"], "OCO")
            self.assertEqual(len(child_oco["childOrderStrategies"]), 2)

    def test_count_trading_days(self):
        # Empty string should return 0
        self.assertEqual(count_trading_days(""), 0)
        # Invalid format returns 0
        self.assertEqual(count_trading_days("not-a-date"), 0)

    def test_sync_exit_to_portfolio_json(self):
        temp_port = {
            "total_capital": 10000.0,
            "cash_balance": 8000.0,
            "max_positions": 5,
            "positions": [
                {
                    "symbol": "AMD",
                    "shares": 20.0,
                    "entry_price": 100.0,
                    "stop_loss": 90.0,
                    "take_profit": 130.0,
                    "cost_basis": 2000.0
                }
            ],
            "closed_trades": []
        }
        with tempfile.NamedTemporaryFile("w+", delete=False) as f:
            json.dump(temp_port, f)
            temp_path = f.name

        try:
            # Override PORTFOLIO_FILE in module temporarily
            import poll_robinhood_exits
            old_port_file = poll_robinhood_exits.PORTFOLIO_FILE
            poll_robinhood_exits.PORTFOLIO_FILE = temp_path

            # Partial exit of 10 shares @ 120
            poll_robinhood_exits.sync_exit_to_portfolio_json("AMD", 120.0, 10.0, "TIER1_TARGET", is_partial=True)

            with open(temp_path, "r") as f:
                updated = json.load(f)

            self.assertEqual(len(updated["positions"]), 1)
            self.assertEqual(updated["positions"][0]["shares"], 10.0)
            self.assertEqual(updated["cash_balance"], 8000.0 + 1200.0)
            self.assertEqual(len(updated["closed_trades"]), 1)
            self.assertEqual(updated["closed_trades"][0]["pnl_dollar"], 200.0)

            # Full exit of remaining 10 shares @ 130
            poll_robinhood_exits.sync_exit_to_portfolio_json("AMD", 130.0, 10.0, "TIER2_TARGET", is_partial=False)

            with open(temp_path, "r") as f:
                final = json.load(f)

            self.assertEqual(len(final["positions"]), 0)
            self.assertEqual(final["cash_balance"], 9200.0 + 1300.0)
            self.assertEqual(len(final["closed_trades"]), 2)
            self.assertEqual(final["closed_trades"][1]["pnl_dollar"], 300.0)
        finally:
            poll_robinhood_exits.PORTFOLIO_FILE = old_port_file
            if os.path.exists(temp_path):
                os.remove(temp_path)


if __name__ == "__main__":
    unittest.main()
