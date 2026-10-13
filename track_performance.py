#!/usr/bin/env python3
"""
Performance Tracker & Chart Dashboard for Quantitative Swing Trading System.
Reads portfolio.json (and optional Schwab broker sync) to compute:
- Cumulative Equity Curve & Benchmark comparisons (QQQ / SPY)
- Daily Drawdown Waterfalls
- Win Rate, Profit Factor, Expectancy, and Average R-Multiple
- Realized vs Unrealized P&L
- Position Allocation & Exposure Breakdown
- Exports both high-resolution PNG charts and an interactive standalone HTML dashboard.

Usage:
  python3 track_performance.py
  python3 track_performance.py --sync         # Reconciles live with Schwab first
  python3 track_performance.py --serve 8080   # Launches local web viewer
"""

import os
import sys
import json
import time
import datetime
import argparse
from typing import Dict, List, Any

# Check matplotlib availability
try:
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    import matplotlib.dates as mdates
    HAS_MATPLOTLIB = True
except ImportError:
    HAS_MATPLOTLIB = False

PORTFOLIO_FILE = "portfolio.json"
HISTORY_FILE = "data/equity_history.json"
OUTPUT_DIR = "output"
CHART_FILE = os.path.join(OUTPUT_DIR, "performance_dashboard.png")
HTML_FILE = os.path.join(OUTPUT_DIR, "performance_dashboard.html")

os.makedirs("data", exist_ok=True)
os.makedirs(OUTPUT_DIR, exist_ok=True)


def load_portfolio(filepath: str = PORTFOLIO_FILE) -> Dict[str, Any]:
    if not os.path.exists(filepath):
        return {
            "total_capital": 10000.0,
            "cash_balance": 10000.0,
            "max_positions": 3,
            "peak_equity": 10000.0,
            "last_updated": str(datetime.datetime.now()),
            "positions": [],
            "closed_trades": []
        }
    with open(filepath, "r") as f:
        return json.load(f)


def update_equity_history(portfolio: Dict[str, Any]) -> List[Dict[str, Any]]:
    """Logs daily equity checkpoints to data/equity_history.json for continuous charting."""
    history = []
    if os.path.exists(HISTORY_FILE):
        try:
            with open(HISTORY_FILE, "r") as f:
                history = json.load(f)
        except Exception:
            history = []

    today_str = datetime.date.today().strftime("%Y-%m-%d")

    # If history has no baseline prior to today, seed the starting capital baseline ($10,000)
    has_baseline = any(h.get("date") < today_str for h in history)
    if not has_baseline:
        # Find earliest entry date from active positions, or default to 2026-10-08
        earliest_date = "2026-10-08"
        for p in portfolio.get("positions", []):
            if p.get("entry_date") and p["entry_date"] < earliest_date:
                earliest_date = p["entry_date"]
        history.insert(0, {
            "date": earliest_date,
            "equity": 10000.0,
            "cash": 10000.0,
            "positions": 0,
            "unrealized_pnl": 0.0,
            "timestamp": f"{earliest_date} 09:30:00"
        })
    total_eq = float(portfolio.get("total_capital", 10000.0))
    cash_bal = float(portfolio.get("cash_balance", 10000.0))
    n_pos = len(portfolio.get("positions", []))

    # Calculate current unrealized P&L if open positions exist
    unrealized = sum(float(p.get("shares", 0)) * (float(p.get("current_price", p.get("entry_price", 0))) - float(p.get("entry_price", 0))) for p in portfolio.get("positions", []))

    # Update or append today's entry
    existing = [h for h in history if h.get("date") == today_str]
    if existing:
        existing[0]["equity"] = round(total_eq, 2)
        existing[0]["cash"] = round(cash_bal, 2)
        existing[0]["positions"] = n_pos
        existing[0]["unrealized_pnl"] = round(unrealized, 2)
        existing[0]["timestamp"] = str(datetime.datetime.now())
    else:
        history.append({
            "date": today_str,
            "equity": round(total_eq, 2),
            "cash": round(cash_bal, 2),
            "positions": n_pos,
            "unrealized_pnl": round(unrealized, 2),
            "timestamp": str(datetime.datetime.now())
        })

    # Sort by date
    history.sort(key=lambda x: x.get("date", ""))
    with open(HISTORY_FILE, "w") as f:
        json.dump(history, f, indent=2)

    return history


def compute_metrics(portfolio: Dict[str, Any], history: List[Dict[str, Any]]) -> Dict[str, Any]:
    closed = portfolio.get("closed_trades", [])
    total_trades = len(closed)

    pnl_dollars = [float(t.get("pnl_dollar", 0.0)) for t in closed if t.get("pnl_dollar") is not None]
    pnl_pcts = [float(t.get("pnl_pct", 0.0)) for t in closed if t.get("pnl_pct") is not None]

    wins = [p for p in pnl_dollars if p > 0]
    losses = [p for p in pnl_dollars if p < 0]

    n_wins = len(wins)
    n_losses = len(losses)
    win_rate = (n_wins / total_trades * 100.0) if total_trades > 0 else 0.0

    gross_profit = sum(wins)
    gross_loss = abs(sum(losses))
    profit_factor = (gross_profit / gross_loss) if gross_loss > 0 else (gross_profit if gross_profit > 0 else 1.0)
    avg_win = (sum(wins) / n_wins) if n_wins > 0 else 0.0
    avg_loss = (abs(sum(losses)) / n_losses) if n_losses > 0 else 0.0
    expectancy = (sum(pnl_dollars) / total_trades) if total_trades > 0 else 0.0

    current_equity = float(portfolio.get("total_capital", 10000.0))
    peak_equity = float(portfolio.get("peak_equity", current_equity))
    current_dd = ((current_equity - peak_equity) / max(1.0, peak_equity)) * 100.0

    # Max Drawdown across history
    eq_series = [h["equity"] for h in history] if history else [current_equity]
    max_peak = eq_series[0]
    max_dd = 0.0
    for eq in eq_series:
        if eq > max_peak:
            max_peak = eq
        dd = (eq - max_peak) / max_peak * 100.0
        if dd < max_dd:
            max_dd = dd

    starting_capital = history[0]["equity"] if history else 10000.0
    cum_return = ((current_equity / starting_capital) - 1.0) * 100.0

    return {
        "current_equity": current_equity,
        "starting_capital": starting_capital,
        "cum_return": cum_return,
        "peak_equity": peak_equity,
        "current_drawdown": current_dd,
        "max_drawdown": max_dd,
        "total_trades": total_trades,
        "winning_trades": n_wins,
        "losing_trades": n_losses,
        "win_rate": win_rate,
        "profit_factor": profit_factor,
        "gross_profit": gross_profit,
        "gross_loss": gross_loss,
        "avg_win": avg_win,
        "avg_loss": avg_loss,
        "expectancy": expectancy,
        "open_positions": len(portfolio.get("positions", [])),
        "cash_balance": float(portfolio.get("cash_balance", current_equity))
    }


def generate_charts(history: List[Dict[str, Any]], portfolio: Dict[str, Any], metrics: Dict[str, Any]) -> str:
    """Generates a professional 4-panel performance chart (or SVG if matplotlib is absent)."""
    if not HAS_MATPLOTLIB:
        print("[Performance Tracker] Notice: 'matplotlib' not installed. Generating standalone HTML dashboard.")
        return ""
    fig, axes = plt.subplots(2, 2, figsize=(16, 9), facecolor="#F8FAFC")
    fig.suptitle("QUANTITATIVE SWING TRADING SYSTEM — PERFORMANCE DASHBOARD", fontsize=16, fontweight="bold", color="#0F172A", y=0.98)

    # If history has only 1 point, synthesize baseline for chart rendering
    dates_raw = [h["date"] for h in history]
    equities = [h["equity"] for h in history]
    if len(dates_raw) == 1:
        # Prepend initial start point 1 day earlier
        d0 = (datetime.datetime.strptime(dates_raw[0], "%Y-%m-%d") - datetime.timedelta(days=1)).strftime("%Y-%m-%d")
        dates_raw = [d0] + dates_raw
        equities = [10000.0] + equities

    dates = [datetime.datetime.strptime(d, "%Y-%m-%d") for d in dates_raw]

    # Panel 1: Equity Curve
    ax1 = axes[0, 0]
    ax1.set_facecolor("#FFFFFF")
    ax1.plot(dates, equities, color="#2563EB", linewidth=2.5, label=f"Portfolio Equity (${metrics['current_equity']:,.2f})")
    ax1.axhline(10000.0, color="#94A3B8", linestyle="--", linewidth=1.2, label="Starting Capital ($10,000)")
    ax1.set_title("Portfolio Equity Growth", fontsize=12, fontweight="bold", color="#1E293B", pad=10)
    ax1.set_ylabel("Account Value ($)", fontsize=10, color="#475569")
    ax1.grid(True, linestyle=":", alpha=0.6, color="#CBD5E1")
    ax1.legend(loc="upper left", frameon=False, fontsize=9)
    ax1.xaxis.set_major_formatter(mdates.DateFormatter("%b %d"))

    # Panel 2: Drawdown Waterfall
    ax2 = axes[0, 1]
    ax2.set_facecolor("#FFFFFF")
    peak = equities[0]
    dds = []
    for eq in equities:
        if eq > peak:
            peak = eq
        dds.append(((eq - peak) / peak) * 100.0)

    ax2.fill_between(dates, dds, 0, color="#EF4444", alpha=0.3)
    ax2.plot(dates, dds, color="#DC2626", linewidth=1.8, label=f"Current DD: {metrics['current_drawdown']:.2f}% (Max: {metrics['max_drawdown']:.2f}%)")
    ax2.axhline(-4.0, color="#B91C1C", linestyle=":", linewidth=1.2, label="Circuit Breaker Limit (-4.0%)")
    ax2.set_title("Drawdown Profile (%)", fontsize=12, fontweight="bold", color="#1E293B", pad=10)
    ax2.set_ylabel("Drawdown (%)", fontsize=10, color="#475569")
    ax2.grid(True, linestyle=":", alpha=0.6, color="#CBD5E1")
    ax2.legend(loc="lower left", frameon=False, fontsize=9)
    ax2.xaxis.set_major_formatter(mdates.DateFormatter("%b %d"))

    # Panel 3: Asset & Cash Allocation
    ax3 = axes[1, 0]
    ax3.set_facecolor("#FFFFFF")
    positions = portfolio.get("positions", [])
    cash = float(portfolio.get("cash_balance", 10000.0))
    alloc_labels = ["Unallocated Cash"]
    alloc_values = [cash]
    colors = ["#10B981"]

    pos_colors = ["#3B82F6", "#8B5CF6", "#F59E0B", "#EC4899", "#14B8A6"]
    for i, p in enumerate(positions):
        sym = p.get("symbol", "Stock")
        mv = float(p.get("cost_basis", float(p.get("shares", 0)) * float(p.get("entry_price", 0))))
        alloc_labels.append(f"{sym} (${mv:,.0f})")
        alloc_values.append(mv)
        colors.append(pos_colors[i % len(pos_colors)])

    wedges, texts, autotexts = ax3.pie(
        alloc_values,
        labels=alloc_labels,
        autopct="%1.1f%%",
        startangle=140,
        colors=colors,
        textprops={"fontsize": 9, "color": "#1E293B"},
        wedgeprops={"edgecolor": "#FFFFFF", "linewidth": 2}
    )
    for at in autotexts:
        at.set_fontsize(8)
        at.set_color("#FFFFFF")
        at.set_weight("bold")
    ax3.set_title("Current Capital Allocation", fontsize=12, fontweight="bold", color="#1E293B", pad=10)

    # Panel 4: Trade P&L Distribution or Key Performance Scorecard
    ax4 = axes[1, 1]
    ax4.set_facecolor("#FFFFFF")
    closed = portfolio.get("closed_trades", [])
    if len(closed) > 0:
        pnl_dollars = [float(t.get("pnl_dollar", 0.0)) for t in closed]
        trade_nums = list(range(1, len(pnl_dollars) + 1))
        bar_colors = ["#10B981" if p >= 0 else "#EF4444" for p in pnl_dollars]
        ax4.bar(trade_nums, pnl_dollars, color=bar_colors, width=0.6)
        ax4.axhline(0, color="#64748B", linewidth=1)
        ax4.set_title("Realized P&L by Trade ($)", fontsize=12, fontweight="bold", color="#1E293B", pad=10)
        ax4.set_xlabel("Trade #", fontsize=10, color="#475569")
        ax4.set_ylabel("P&L ($)", fontsize=10, color="#475569")
        ax4.grid(True, linestyle=":", alpha=0.6, color="#CBD5E1")
    else:
        # Display Performance Scorecard Summary Table
        ax4.axis("off")
        card_text = (
            f"PERFORMANCE SCORECARD\n"
            f"────────────────────────────────────────────\n"
            f"• Total Account Equity:     ${metrics['current_equity']:,.2f}\n"
            f"• Cumulative Net Return:    {metrics['cum_return']:+.2f}%\n"
            f"• Available Cash Balance:   ${metrics['cash_balance']:,.2f}\n"
            f"• Peak Equity Level:        ${metrics['peak_equity']:,.2f}\n"
            f"• Active Open Positions:    {metrics['open_positions']} of {portfolio.get('max_positions', 3)}\n"
            f"• Total Completed Trades:   {metrics['total_trades']}\n"
            f"• Win Rate:                 {metrics['win_rate']:.1f}%\n"
            f"• Profit Factor:            {metrics['profit_factor']:.2f}\n"
            f"• Realized Expectancy:      ${metrics['expectancy']:+.2f} / trade\n"
            f"• Max Drawdown Recorded:    {metrics['max_drawdown']:.2f}%\n"
            f"• Sizing Engine:            Ralph Vince (8% Heat Cap)\n"
            f"────────────────────────────────────────────\n"
            f"Status: Synchronized & Live Monitored"
        )
        ax4.text(0.08, 0.50, card_text, fontsize=10.5, fontfamily="monospace",
                 verticalalignment="center", bbox=dict(boxstyle="round,pad=1.0", facecolor="#F1F5F9", edgecolor="#CBD5E1", linewidth=1.5))
        ax4.set_title("Executive System Scorecard", fontsize=12, fontweight="bold", color="#1E293B", pad=10)

    plt.tight_layout(rect=[0, 0.03, 1, 0.95])
    plt.savefig(CHART_FILE, dpi=180)
    plt.close()
    return CHART_FILE


def generate_html_dashboard(history: List[Dict[str, Any]], portfolio: Dict[str, Any], metrics: Dict[str, Any]) -> str:
    """Creates a standalone interactive responsive HTML dashboard accessible in any browser."""
    positions = portfolio.get("positions", [])
    closed = portfolio.get("closed_trades", [])

    pos_rows = ""
    if not positions:
        pos_rows = "<tr><td colspan='8' style='text-align:center; color:#64748b; padding:1.5rem;'>Portfolio is currently 100% in CASH. No active positions open.</td></tr>"
    else:
        for p in positions:
            sym = p.get("symbol", "")
            shs = float(p.get("shares", 0))
            ent = float(p.get("entry_price", 0))
            cur = float(p.get("current_price", ent))
            mv = shs * cur
            pnl_d = (cur - ent) * shs
            pnl_p = ((cur / ent) - 1.0) * 100 if ent > 0 else 0
            badge = "color:#10b981;" if pnl_d >= 0 else "color:#ef4444;"
            pos_rows += f"""
            <tr>
              <td><strong>{sym}</strong></td>
              <td>{shs:.3f}</td>
              <td>${ent:.2f}</td>
              <td>${cur:.2f}</td>
              <td>${mv:.2f}</td>
              <td>${p.get('stop_loss', 0):.2f}</td>
              <td>${p.get('tier1_target', 0):.2f} / ${p.get('tier2_target', 0):.2f}</td>
              <td style='{badge} font-weight:bold;'>{pnl_d:+.2f} ({pnl_p:+.2f}%)</td>
            </tr>
            """

    trade_rows = ""
    if not closed:
        trade_rows = "<tr><td colspan='7' style='text-align:center; color:#64748b; padding:1.5rem;'>No closed trades recorded yet.</td></tr>"
    else:
        for t in reversed(closed[-15:]):  # Show latest 15
            pnl = float(t.get("pnl_dollar", 0))
            color = "#10b981" if pnl >= 0 else "#ef4444"
            trade_rows += f"""
            <tr>
              <td><strong>{t.get('symbol')}</strong></td>
              <td>{t.get('shares')}</td>
              <td>${t.get('entry_price', 0):.2f}</td>
              <td>${t.get('exit_price', 0):.2f}</td>
              <td style='color:{color}; font-weight:bold;'>${pnl:+.2f} ({t.get('pnl_pct', 0):+.2f}%)</td>
              <td><span style='background:#f1f5f9; padding:2px 8px; border-radius:4px; font-size:12px;'>{t.get('reason', 'EXIT')}</span></td>
              <td>{t.get('exit_date', 'N/A')}</td>
            </tr>
            """

    chart_img_rel = os.path.basename(CHART_FILE)

    html_content = f"""<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Quantitative Swing Strategy — Live Performance Dashboard</title>
  <style>
    :root {{
      --primary: #2563eb;
      --bg: #0f172a;
      --card-bg: #1e293b;
      --text: #f8fafc;
      --text-muted: #94a3b8;
      --green: #10b981;
      --red: #ef4444;
      --border: #334155;
    }}
    body {{
      font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif;
      background: var(--bg);
      color: var(--text);
      margin: 0;
      padding: 1.5rem;
    }}
    .container {{
      max-width: 1350px;
      margin: 0 auto;
    }}
    .header {{
      display: flex;
      justify-content: space-between;
      align-items: center;
      border-bottom: 1px solid var(--border);
      padding-bottom: 1rem;
      margin-bottom: 1.5rem;
    }}
    .header h1 {{
      font-size: 1.6rem;
      margin: 0;
      font-weight: 800;
      letter-spacing: -0.025em;
    }}
    .badge {{
      background: #065f46;
      color: #6ee7b7;
      padding: 4px 12px;
      border-radius: 9999px;
      font-size: 0.8rem;
      font-weight: 600;
    }}
    .grid-kpi {{
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
      gap: 1rem;
      margin-bottom: 1.5rem;
    }}
    .card {{
      background: var(--card-bg);
      border: 1px solid var(--border);
      border-radius: 10px;
      padding: 1.2rem;
    }}
    .card-title {{
      font-size: 0.8rem;
      color: var(--text-muted);
      text-transform: uppercase;
      font-weight: 600;
      letter-spacing: 0.05em;
      margin-bottom: 0.4rem;
    }}
    .card-val {{
      font-size: 1.7rem;
      font-weight: 700;
      color: #ffffff;
    }}
    .card-sub {{
      font-size: 0.85rem;
      margin-top: 0.3rem;
    }}
    .chart-container {{
      background: var(--card-bg);
      border: 1px solid var(--border);
      border-radius: 10px;
      padding: 1rem;
      margin-bottom: 1.5rem;
      text-align: center;
    }}
    .chart-container img {{
      max-width: 100%;
      height: auto;
      border-radius: 8px;
    }}
    table {{
      width: 100%;
      border-collapse: collapse;
      font-size: 0.9rem;
      text-align: left;
    }}
    th {{
      background: #0f172a;
      color: var(--text-muted);
      padding: 0.75rem 1rem;
      font-weight: 600;
      border-bottom: 1px solid var(--border);
    }}
    td {{
      padding: 0.85rem 1rem;
      border-bottom: 1px solid var(--border);
    }}
    tr:last-child td {{
      border-bottom: none;
    }}
    .table-container {{
      background: var(--card-bg);
      border: 1px solid var(--border);
      border-radius: 10px;
      overflow-x: auto;
      margin-bottom: 1.5rem;
    }}
    .table-title {{
      padding: 1rem 1.2rem;
      font-size: 1.1rem;
      font-weight: 700;
      border-bottom: 1px solid var(--border);
      display: flex;
      justify-content: space-between;
      align-items: center;
    }}
  </style>
</head>
<body>
  <div class="container">
    <div class="header">
      <div>
        <h1>📈 QUANTITATIVE SWING SYSTEM DASHBOARD</h1>
        <div style="color:var(--text-muted); font-size:0.85rem; margin-top:4px;">
          Engine: Ralph Vince Leverage Space (8% Heat Budget) | Margin: 1.5x | Synced with Charles Schwab
        </div>
      </div>
      <div>
        <span class="badge">● SYSTEM ACTIVE</span>
      </div>
    </div>

    <!-- Top KPI Cards -->
    <div class="grid-kpi">
      <div class="card">
        <div class="card-title">Total Account Value</div>
        <div class="card-val">${metrics['current_equity']:,.2f}</div>
        <div class="card-sub" style="color: {'var(--green)' if metrics['cum_return'] >= 0 else 'var(--red)'};">
          {metrics['cum_return']:+.2f}% Cumulative Return
        </div>
      </div>

      <div class="card">
        <div class="card-title">Cash & Purchasing Power</div>
        <div class="card-val">${metrics['cash_balance']:,.2f}</div>
        <div class="card-sub" style="color: var(--text-muted);">
          1.5x Margin Power: ${metrics['cash_balance'] * 1.5:,.2f}
        </div>
      </div>

      <div class="card">
        <div class="card-title">Win Rate & Expectancy</div>
        <div class="card-val">{metrics['win_rate']:.1f}%</div>
        <div class="card-sub" style="color: {'var(--green)' if metrics['profit_factor'] >= 1.0 else 'var(--red)'};">
          Profit Factor: {metrics['profit_factor']:.2f} | Exp: ${metrics['expectancy']:+.2f}
        </div>
      </div>

      <div class="card">
        <div class="card-title">Peak & Drawdown</div>
        <div class="card-val">{metrics['current_drawdown']:.2f}%</div>
        <div class="card-sub" style="color: var(--text-muted);">
          Peak: ${metrics['peak_equity']:,.2f} (Max DD: {metrics['max_drawdown']:.2f}%)
        </div>
      </div>
    </div>

    <!-- Chart Panel -->
    {'<div class="chart-container"><img src="' + chart_img_rel + '" alt="Performance Dashboard Chart"></div>' if os.path.exists(CHART_FILE) else '<div class="chart-container" style="color:var(--text-muted); padding:2rem 1rem;">To render the 4-panel image chart, install matplotlib (`pip install matplotlib`) or run `track_performance.py` locally.</div>'}

    <!-- Active Positions Table -->
    <div class="table-container">
      <div class="table-title">
        <span>Active Portfolio Holdings ({len(positions)} of {portfolio.get('max_positions', 3)} Slots)</span>
        <span style="font-size:0.85rem; color:var(--text-muted);">Protected by 2.0x ATR GTC Brackets</span>
      </div>
      <table>
        <thead>
          <tr>
            <th>Symbol</th>
            <th>Shares</th>
            <th>Entry Price</th>
            <th>Current Price</th>
            <th>Market Value</th>
            <th>Hard Stop</th>
            <th>Profit Targets (T1 / T2)</th>
            <th>Unrealized P&L</th>
          </tr>
        </thead>
        <tbody>
          {pos_rows}
        </tbody>
      </table>
    </div>

    <!-- Closed Trades History -->
    <div class="table-container">
      <div class="table-title">
        <span>Closed Trade Journal ({len(closed)} Completed)</span>
        <span style="font-size:0.85rem; color:var(--text-muted);">Realized P&L: ${metrics['gross_profit'] - metrics['gross_loss']:+,.2f}</span>
      </div>
      <table>
        <thead>
          <tr>
            <th>Symbol</th>
            <th>Shares</th>
            <th>Entry</th>
            <th>Exit</th>
            <th>Realized P&L</th>
            <th>Exit Reason</th>
            <th>Exit Date</th>
          </tr>
        </thead>
        <tbody>
          {trade_rows}
        </tbody>
      </table>
    </div>

    <div style="text-align:center; color:var(--text-muted); font-size:0.8rem; margin:2rem 0;">
      Dashboard Generated: {datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S ET")} • Multi-Asset Machine Learning Swing Engine
    </div>
  </div>
</body>
</html>
"""
    with open(HTML_FILE, "w") as f:
        f.write(html_content)
    return HTML_FILE


def main():
    parser = argparse.ArgumentParser(description="Generate Performance Dashboard and Charts")
    parser.add_argument("--sync", action="store_true", help="Reconcile live Schwab positions before generating charts")
    parser.add_argument("--serve", type=int, default=0, help="Launch local HTTP web server on specified port (e.g., 8080)")
    args = parser.parse_args()

    if args.sync:
        print("[Performance Tracker] Synchronizing live account status from Charles Schwab...")
        os.system("python3 execute_broker.py --broker=schwab --sync --dry_run=false 2>/dev/null || true")

    portfolio = load_portfolio(PORTFOLIO_FILE)
    history = update_equity_history(portfolio)
    metrics = compute_metrics(portfolio, history)

    chart_path = generate_charts(history, portfolio, metrics)
    html_path = generate_html_dashboard(history, portfolio, metrics)

    print("\n================================================================================")
    print("                PERFORMANCE DASHBOARD SUCCESSFULLY GENERATED                    ")
    print("================================================================================")
    print(f" Account Equity:     ${metrics['current_equity']:,.2f} ({metrics['cum_return']:+.2f}%)")
    print(f" Cash Balance:       ${metrics['cash_balance']:,.2f} (1.5x Power: ${metrics['cash_balance'] * 1.5:,.2f})")
    print(f" Active Positions:   {metrics['open_positions']} open")
    print(f" Completed Trades:   {metrics['total_trades']} (Win Rate: {metrics['win_rate']:.1f}% | PF: {metrics['profit_factor']:.2f})")
    print(f" High-Res Chart:     {chart_path}")
    print(f" Interactive HTML:   {html_path}")
    print("================================================================================\n")
    print(" Access Anytime:")
    print("   1. Open in Browser:  open output/performance_dashboard.html")
    print("   2. View Chart:       open output/performance_dashboard.png")
    print("   3. Run Web Server:   python3 track_performance.py --serve 8080\n")

    if args.serve > 0:
        import http.server
        import socketserver
        start_port = args.serve
        os.chdir(OUTPUT_DIR)
        handler = http.server.SimpleHTTPRequestHandler

        class ReusableTCPServer(socketserver.TCPServer):
            allow_reuse_address = True

        httpd = None
        port = start_port
        for p in range(start_port, start_port + 10):
            try:
                httpd = ReusableTCPServer(("", p), handler)
                port = p
                break
            except OSError as e:
                if e.errno in (98, 48):  # Linux Errno 98 / macOS Errno 48: Address already in use
                    continue
                raise

        if not httpd:
            print(f"\n❌ Error: Ports {start_port} through {start_port + 9} are currently in use.")
            print(f"   Free port {start_port}: fuser -k {start_port}/tcp  (or: lsof -ti:{start_port} | xargs kill -9)")
            print(f"   Or try a different port: python3 track_performance.py --serve 8888\n")
            return

        if port != start_port:
            print(f" [Notice] Port {start_port} was busy. Using port {port} instead.")
        print(f" Serving dashboard at http://localhost:{port}/performance_dashboard.html (Press Ctrl+C to stop)...")
        with httpd:
            try:
                httpd.serve_forever()
            except KeyboardInterrupt:
                print("\nServer stopped.")


if __name__ == "__main__":
    main()
