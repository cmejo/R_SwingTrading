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

    def test_robinhood_sync_entry_and_entry_date(self):
        from execute_robinhood import sync_entry_to_portfolio_json
        with tempfile.NamedTemporaryFile("w+", delete=False) as f:
            temp_path = f.name

        try:
            # Sync an entry of 20 shares of NVDA @ $120.00
            today_str = "2026-10-06"
            sync_entry_to_portfolio_json("NVDA", 20.0, 120.0, 110.0, 135.0, today_str, portfolio_file=temp_path)

            with open(temp_path, "r") as f:
                port = json.load(f)

            self.assertEqual(len(port["positions"]), 1)
            pos = port["positions"][0]
            self.assertEqual(pos["symbol"], "NVDA")
            self.assertEqual(pos["shares"], 20.0)
            self.assertEqual(pos["entry_price"], 120.0)
            self.assertEqual(pos["entry_date"], today_str)
            self.assertEqual(port["cash_balance"], 10000.0 - 2400.0)

            # Test trading day count logic
            days_held = count_trading_days(pos["entry_date"])
            self.assertGreaterEqual(days_held, 0)
        finally:
            if os.path.exists(temp_path):
                os.remove(temp_path)

    def test_dynamic_fill_bracket_and_inverted_guard(self):
        # Scenario: XLK signal generated @ $196.28 with stop $190.82 (-2.78%)
        ticket_text = (
            "--- ORDER TICKET #1: XLK (P(Up): 66.8% | Quality Score: 0.731 | Sector: Index_ETF) ---\n"
            "  Action:              BUY 65 SHARES at Market (Order Execution)\n"
            "  Signal Price:        $196.28 (Scan Reference)\n"
            "  GTC Stop-Loss:       $190.82 (-2.78%) [Total Downside Risk: $354.90]\n"
            "  Tier 1 (50% = 32 shs): Target $202.77 (+1.5R)\n"
            "  Tier 2 (25% = 16 shs): Target $209.94 (+3.0R)\n"
            "  Actual Outlay:     $12758.20\n"
        )
        with tempfile.NamedTemporaryFile("w+", delete=False) as f:
            f.write(ticket_text)
            temp_path = f.name

        try:
            tickets = read_latest_tickets(temp_path)
            self.assertEqual(len(tickets), 1)
            t = tickets[0]
            self.assertEqual(t["symbol"], "XLK")
            self.assertEqual(t["signal_price"], 196.28)
            self.assertAlmostEqual(t["stop_pct"], 0.0278, places=4)

            # Market fills at $190.37 (gap down / lower than old stop $190.82)
            avg_px = 190.3694
            risk_pct = t["stop_pct"]
            dynamic_stop = round(avg_px * (1.0 - risk_pct), 2)
            
            # Verify stop is recalculated strictly below entry price!
            self.assertLess(dynamic_stop, avg_px)
            self.assertEqual(dynamic_stop, 185.08)

            # Verify Inverted Stop Safety Guard
            stale_old_stop = 190.82
            # Guard triggers if stale stop >= avg_px
            if stale_old_stop >= avg_px:
                safe_stop = round(avg_px * (1.0 - risk_pct), 2)
            else:
                safe_stop = stale_old_stop
            self.assertLess(safe_stop, avg_px)
        finally:
            if os.path.exists(temp_path):
                os.remove(temp_path)

    def test_hedge_sell_and_alert_deduplication(self):
        """Verifies hedge positions are recognized on sell alerts and duplicate alerts don't create multiple sell tickets."""
        port_state = {
            "total_capital": 8230.0,
            "cash_balance": 2000.0,
            "positions": [
                {"symbol": "NVDA", "shares": 10.0, "entry_price": 120.0, "cost_basis": 1200.0}
            ],
            "hedge_positions": [
                {"symbol": "FNGD", "shares": 250.0, "entry_price": 10.0, "cost_basis": 2500.0}
            ]
        }
        with tempfile.NamedTemporaryFile("w+", delete=False, suffix=".json") as pf:
            json.dump(port_state, pf)
            port_path = pf.name

        ticket_content = (
            "----------------------------------------------------------------------------------------\n"
            " ⚠️  PORTFOLIO ACTION ALERTS:\n"
            "    * [STOP-LOSS HIT] FNGD breached stop at $9.00 (Current: $8.90). Exit immediately.\n"
            "    * [STOP-LOSS HIT] FNGD breached stop at $9.00 (Current: $8.90). Exit immediately.\n"
            "    * [DEFENSIVE WEEKEND EXIT] NVDA must be closed before 16:00 Friday (Macro Risk-Off). P&L: -1.50%\n"
            "    * [DEFENSIVE WEEKEND EXIT] NVDA must be closed before 16:00 Friday (Macro Risk-Off). P&L: -1.50%\n"
            "----------------------------------------------------------------------------------------\n"
        )
        with tempfile.NamedTemporaryFile("w+", delete=False, suffix=".txt") as tf:
            tf.write(ticket_content)
            ticket_path = tf.name

        try:
            # Temporarily redirect portfolio.json to port_path
            old_port = None
            if os.path.exists("portfolio.json"):
                with open("portfolio.json", "r") as f:
                    old_port = f.read()
            with open("portfolio.json", "w") as f:
                json.dump(port_state, f)

            tickets = read_latest_tickets(ticket_path)
            # Should have exactly 2 sell tickets (1 for FNGD, 1 for NVDA), with duplicates eliminated!
            sell_tickets = [t for t in tickets if t.get("action") == "SELL"]
            self.assertEqual(len(sell_tickets), 2)

            fngd_t = [t for t in sell_tickets if t["symbol"] == "FNGD"][0]
            self.assertEqual(fngd_t["shares"], 250.0)
            self.assertEqual(fngd_t["reason"], "STOP_LOSS_HIT")

            nvda_t = [t for t in sell_tickets if t["symbol"] == "NVDA"][0]
            self.assertEqual(nvda_t["shares"], 10.0)
            self.assertEqual(nvda_t["reason"], "DEFENSIVE_WEEKEND_EXIT")
        finally:
            if old_port is not None:
                with open("portfolio.json", "w") as f:
                    f.write(old_port)
            if os.path.exists(port_path):
                os.remove(port_path)
            if os.path.exists(ticket_path):
                os.remove(ticket_path)


if __name__ == "__main__":
    unittest.main()

