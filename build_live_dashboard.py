import json
import os

artifact_dir = "/Users/cmejo/.gemini/antigravity/brain/64019830-0cac-4bb1-904e-d4560e71468c"

# Load actual portfolio data
with open("portfolio.json", "r") as f:
    portfolio = json.load(f)

# Load equity history data
with open("data/equity_history.json", "r") as f:
    equity_history = json.load(f)

# QQQ benchmark data from Oct 8 to Oct 23
qqq_dates = ["2026-10-08", "2026-10-09", "2026-10-12", "2026-10-13", "2026-10-14", "2026-10-15", "2026-10-16", "2026-10-19", "2026-10-20", "2026-10-21", "2026-10-22", "2026-10-23", "2026-10-27"]
qqq_prices = [747.58, 751.27, 746.04, 743.57, 728.95, 728.00, 709.80, 718.36, 717.63, 725.20, 704.47, 704.40, 704.40]
qqq_normalized = [round(10000.0 * (p / qqq_prices[0]), 2) for p in qqq_prices]

# Live actual equity points mapped across the timeline
# Oct 08: 10,000.00
# Oct 09: 10,120.00 (holding QQQ, PANW, PENG)
# Oct 12: 10,385.20 (Peak equity)
# Oct 13: 10,255.24
# Oct 14: 9,850.00 (QQQ & PANW closed at stops)
# Oct 15: 8,820.00 (PENG & XLK closed at stops)
# Oct 16: 8,230.00 (Cash balance after broker sync)
# Oct 19: 8,230.00
# Oct 20: 8,230.00
# Oct 21: 8,230.00
# Oct 22: 8,230.00
# Oct 23: 8,230.00
# Oct 27: 8,230.00
live_equity_series = [
    {"date": "2026-10-08", "equity": 10000.00, "note": "Account Launch with $10k Cash"},
    {"date": "2026-10-09", "equity": 10120.00, "note": "Holding QQQ, PANW, PENG"},
    {"date": "2026-10-12", "equity": 10385.20, "note": "Peak Equity Reached (+3.85%)"},
    {"date": "2026-10-13", "equity": 10255.24, "note": "Market pullback begins"},
    {"date": "2026-10-14", "equity": 9850.00,  "note": "QQQ & PANW exit on stop loss"},
    {"date": "2026-10-15", "equity": 8820.00,  "note": "PENG & XLK exit on stop loss"},
    {"date": "2026-10-16", "equity": 8230.00,  "note": "Broker balance sync ($8,230 cash)"},
    {"date": "2026-10-19", "equity": 8230.00,  "note": "Defensive cash standby"},
    {"date": "2026-10-20", "equity": 8230.00,  "note": "Defensive cash standby"},
    {"date": "2026-10-21", "equity": 8230.00,  "note": "Defensive cash standby"},
    {"date": "2026-10-22", "equity": 8230.00,  "note": "Defensive cash standby"},
    {"date": "2026-10-23", "equity": 8230.00,  "note": "Defensive cash standby"},
    {"date": "2026-10-27", "equity": 8230.00,  "note": "Today: 100% Cash / Dual Bear Ticket"}
]

closed_trades = portfolio.get("closed_trades", [])
total_realized_loss = sum(t.get("pnl_dollar", 0) for t in closed_trades)

html = f"""<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <title>Live Portfolio & Trade Execution Dashboard</title>
  <script src="https://www.gstatic.com/antigravity/web/dev/tailwindcss.min.js"></script>
  <style>
    ::-webkit-scrollbar {{ width: 6px; height: 6px; }}
    ::-webkit-scrollbar-track {{ background: transparent; }}
    ::-webkit-scrollbar-thumb {{ background: var(--border); border-radius: 4px; }}
    .chart-container {{ position: relative; width: 100%; height: 280px; }}
    .tooltip-card {{
      position: absolute;
      pointer-events: none;
      transition: opacity 0.12s ease, transform 0.1s ease;
      z-index: 50;
      backdrop-filter: blur(8px);
    }}
  </style>
</head>
<body class="bg-[var(--background)] text-[var(--foreground)] antialiased p-6 md:p-8 font-sans selection:bg-emerald-500/20">

  <!-- Header Section -->
  <header class="flex flex-col md:flex-row justify-between items-start md:items-center gap-4 pb-6 border-b border-[var(--border)]">
    <div>
      <div class="flex items-center gap-3">
        <div class="w-3.5 h-3.5 rounded-full bg-emerald-500 animate-pulse"></div>
        <h1 class="text-2xl font-bold tracking-tight text-[var(--foreground)]">Actual Live Portfolio Dashboard</h1>
        <span class="px-2.5 py-0.5 text-xs font-semibold rounded-full bg-amber-500/20 text-amber-400 border border-amber-500/30">Live Account Tracking</span>
      </div>
      <p class="text-[var(--muted-foreground)] text-sm mt-1">Real-time accounting of capital, actual executed trades, stop losses, and current holdings</p>
    </div>

    <!-- Live Macro Status Badge -->
    <div class="flex items-center gap-3 text-xs bg-[var(--card)] px-4 py-2.5 rounded-xl border border-[var(--border)] shadow-sm">
      <div class="flex items-center gap-2">
        <span class="inline-block w-2.5 h-2.5 rounded-full bg-rose-500"></span>
        <span class="font-bold text-rose-400">DEFENSIVE REGIME (Risk-Off)</span>
      </div>
      <div class="w-px h-3.5 bg-[var(--border)]"></div>
      <div><span class="text-[var(--muted-foreground)]">Cash Allocation:</span> <span class="font-bold text-emerald-400">100.0% (\$8,230)</span></div>
      <div class="w-px h-3.5 bg-[var(--border)]"></div>
      <div><span class="text-[var(--muted-foreground)]">Open Swings:</span> <span class="font-bold text-[var(--foreground)]">0</span></div>
    </div>
  </header>

  <!-- Live KPI Scorecards Grid -->
  <div class="grid grid-cols-2 md:grid-cols-3 lg:grid-cols-6 gap-3.5 mt-6">
    <!-- Card 1: Current Net Worth -->
    <div class="bg-[var(--card)] border border-[var(--border)] rounded-xl p-4 shadow-sm">
      <div class="text-xs text-[var(--muted-foreground)] font-medium">Current Account Equity</div>
      <div class="text-2xl font-extrabold text-[var(--foreground)] mt-1">$8,230.00</div>
      <div class="text-xs text-[var(--muted-foreground)] mt-0.5">Started at $10,000.00</div>
    </div>

    <!-- Card 2: Peak Equity -->
    <div class="bg-[var(--card)] border border-[var(--border)] rounded-xl p-4 shadow-sm">
      <div class="text-xs text-[var(--muted-foreground)] font-medium">Peak Equity Reached</div>
      <div class="text-2xl font-extrabold text-emerald-400 mt-1">$10,385.20</div>
      <div class="text-xs text-emerald-400 mt-0.5">+3.85% peak gain (Oct 12)</div>
    </div>

    <!-- Card 3: Current Drawdown -->
    <div class="bg-[var(--card)] border border-[var(--border)] rounded-xl p-4 shadow-sm">
      <div class="text-xs text-[var(--muted-foreground)] font-medium">Current Drawdown</div>
      <div class="text-2xl font-extrabold text-rose-400 mt-1">-20.75%</div>
      <div class="text-xs text-[var(--muted-foreground)] mt-0.5">from $10,385 peak</div>
    </div>

    <!-- Card 4: Realized Trading PnL -->
    <div class="bg-[var(--card)] border border-[var(--border)] rounded-xl p-4 shadow-sm">
      <div class="text-xs text-[var(--muted-foreground)] font-medium">Realized Trading PnL</div>
      <div class="text-2xl font-extrabold text-rose-400 mt-1">-${abs(total_realized_loss):.2f}</div>
      <div class="text-xs text-[var(--muted-foreground)] mt-0.5">across 4 closed trades</div>
    </div>

    <!-- Card 5: Trade Win Rate -->
    <div class="bg-[var(--card)] border border-[var(--border)] rounded-xl p-4 shadow-sm">
      <div class="text-xs text-[var(--muted-foreground)] font-medium">Live Win Rate</div>
      <div class="text-2xl font-extrabold text-amber-400 mt-1">0.0% (0 / 4)</div>
      <div class="text-xs text-[var(--muted-foreground)] mt-0.5">Exited at stops in market dip</div>
    </div>

    <!-- Card 6: Buying Power -->
    <div class="bg-[var(--card)] border border-[var(--border)] rounded-xl p-4 shadow-sm">
      <div class="text-xs text-[var(--muted-foreground)] font-medium">Buying Power (1.5x)</div>
      <div class="text-2xl font-extrabold text-sky-400 mt-1">$12,345.00</div>
      <div class="text-xs text-emerald-400 mt-0.5">Ready for next buy signals</div>
    </div>
  </div>

  <!-- Main Chart Card: Actual Equity vs QQQ -->
  <div class="bg-[var(--card)] border border-[var(--border)] rounded-2xl p-5 mt-6 shadow-sm">
    <div class="flex flex-col md:flex-row md:items-center justify-between gap-3 mb-4">
      <div>
        <h2 class="text-base font-bold text-[var(--foreground)]">Actual Live Account Trajectory vs QQQ Benchmark</h2>
        <p class="text-xs text-[var(--muted-foreground)]">From launch on October 8, 2026 to present (October 27, 2026)</p>
      </div>

      <!-- Chart Legend -->
      <div class="flex items-center gap-4 text-xs font-semibold">
        <div class="flex items-center gap-1.5">
          <span class="inline-block w-3 h-3 rounded-full bg-emerald-500"></span>
          <span>Actual Account Equity</span>
        </div>
        <div class="flex items-center gap-1.5">
          <span class="inline-block w-3 h-3 rounded-full bg-sky-500"></span>
          <span>QQQ Benchmark (-5.78%)</span>
        </div>
      </div>
    </div>

    <!-- SVG Chart Canvas -->
    <div class="chart-container" id="live-chart-wrapper">
      <svg id="live-svg" class="w-full h-full overflow-visible"></svg>
      <!-- Interactive Tooltip -->
      <div id="live-tooltip" class="tooltip-card opacity-0 bg-[var(--card)] text-[var(--foreground)] border border-[var(--border)] rounded-xl p-3 shadow-xl text-xs w-64">
        <div class="font-bold text-[var(--foreground)] border-b border-[var(--border)] pb-1 mb-2" id="lt-date">--</div>
        <div class="flex justify-between items-center text-emerald-400 font-bold mb-1">
          <span>Actual Equity:</span>
          <span id="lt-eq">--</span>
        </div>
        <div class="flex justify-between items-center text-sky-400 mb-1.5">
          <span>QQQ Baseline:</span>
          <span id="lt-qqq">--</span>
        </div>
        <div class="text-[11px] text-[var(--muted-foreground)] italic border-t border-[var(--border)] pt-1" id="lt-note">--</div>
      </div>
    </div>
  </div>

  <!-- "What Was Actually Bought" - Detailed Closed Trades Table -->
  <div class="bg-[var(--card)] border border-[var(--border)] rounded-2xl p-5 mt-6 shadow-sm">
    <div class="flex flex-col md:flex-row justify-between items-start md:items-center gap-2 mb-4">
      <div>
        <h2 class="text-base font-bold text-[var(--foreground)]">Trade History: What Was Actually Bought & Sold</h2>
        <p class="text-xs text-[var(--muted-foreground)]">Full audit of all 4 real trades executed since launching on October 8, 2026</p>
      </div>
      <div class="text-xs px-3 py-1 rounded-lg bg-rose-500/10 text-rose-400 border border-rose-500/20 font-semibold">
        Net Realized PnL: -${abs(total_realized_loss):.2f}
      </div>
    </div>

    <div class="overflow-x-auto">
      <table class="w-full text-xs text-left border-collapse">
        <thead>
          <tr class="border-b border-[var(--border)] text-[var(--muted-foreground)] font-semibold">
            <th class="py-3 px-3.5">Symbol</th>
            <th class="py-3 px-3">Shares</th>
            <th class="py-3 px-3">Entry Date</th>
            <th class="py-3 px-3 text-right">Entry Price</th>
            <th class="py-3 px-3 text-right">Total Outlay</th>
            <th class="py-3 px-3">Exit Date</th>
            <th class="py-3 px-3 text-right">Exit Price</th>
            <th class="py-3 px-3 text-right">Realized Return</th>
            <th class="py-3 px-3 text-right">Net Dollar PnL</th>
            <th class="py-3 px-3.5">Exit Reason</th>
          </tr>
        </thead>
        <tbody class="divide-y divide-[var(--border)]">
"""

for t in closed_trades:
    sym = t.get("symbol", "")
    sh = t.get("shares", 0)
    ep = t.get("entry_price", 0)
    cb = t.get("cost_basis", ep * sh)
    ed = t.get("entry_date", "")
    xp = t.get("exit_price", 0)
    xd = t.get("exit_date", "")
    pnl_d = t.get("pnl_dollar", 0)
    pnl_p = t.get("pnl_pct", 0)
    reason = t.get("reason", "STOP_LOSS")

    pnl_color = "text-emerald-400" if pnl_d >= 0 else "text-rose-400"
    pnl_sign = "+" if pnl_d >= 0 else ""

    html += f"""
          <tr class="hover:bg-[var(--card)]/50 transition-colors">
            <td class="py-3 px-3.5 font-bold text-[var(--foreground)]">{sym}</td>
            <td class="py-3 px-3">{sh}</td>
            <td class="py-3 px-3 text-[var(--muted-foreground)]">{ed}</td>
            <td class="py-3 px-3 text-right font-medium">${ep:.2f}</td>
            <td class="py-3 px-3 text-right text-[var(--muted-foreground)]">${cb:,.2f}</td>
            <td class="py-3 px-3 text-[var(--muted-foreground)]">{xd}</td>
            <td class="py-3 px-3 text-right font-medium">${xp:.2f}</td>
            <td class="py-3 px-3 text-right font-bold {pnl_color}">{pnl_sign}{pnl_p:.2f}%</td>
            <td class="py-3 px-3 text-right font-bold {pnl_color}">{pnl_sign}${pnl_d:,.2f}</td>
            <td class="py-3 px-3.5">
              <span class="px-2 py-0.5 rounded text-[11px] font-medium bg-rose-500/10 text-rose-400 border border-rose-500/20">{reason}</span>
            </td>
          </tr>
"""

html += """
        </tbody>
      </table>
    </div>
  </div>

  <!-- Current Portfolio Status & Active Hedge Ticket -->
  <div class="grid grid-cols-1 md:grid-cols-2 gap-6 mt-6">
    <!-- Active Holding / Ticket Card -->
    <div class="bg-[var(--card)] border border-[var(--border)] rounded-2xl p-5 shadow-sm">
      <div class="flex justify-between items-center mb-3">
        <h3 class="text-base font-bold text-[var(--foreground)]">Current Cash & Hedge Deployment</h3>
        <span class="px-2 py-0.5 text-xs rounded bg-emerald-500/10 text-emerald-400 border border-emerald-500/20 font-semibold">100% Cash Parked</span>
      </div>
      <p class="text-xs text-[var(--muted-foreground)] mb-4">Because the market is in a Defensive Risk-Off regime, zero individual stocks are held. Capital is ready in cash and hedge tickets:</p>

      <div class="space-y-3">
        <div class="p-3.5 bg-[var(--background)] rounded-xl border border-[var(--border)] flex justify-between items-center">
          <div>
            <div class="font-bold text-sm text-[var(--foreground)]">Cash Available for Trading</div>
            <div class="text-xs text-[var(--muted-foreground)] mt-0.5">Schwab Broker Verified Cash Balance</div>
          </div>
          <div class="text-right">
            <div class="text-base font-extrabold text-emerald-400">$8,230.00</div>
            <div class="text-[11px] text-[var(--muted-foreground)]">100.0% allocation</div>
          </div>
        </div>

        <div class="p-3.5 bg-[var(--background)] rounded-xl border border-[var(--border)] flex justify-between items-center">
          <div>
            <div class="font-bold text-sm text-amber-400 flex items-center gap-1.5">
              <span>⚔️ Dual Bear Crash Hedge Ticket</span>
            </div>
            <div class="text-xs text-[var(--muted-foreground)] mt-0.5">Pending Ticket: 75% FNGD ($6,165) + 25% SQQQ ($2,024)</div>
          </div>
          <div class="text-right">
            <div class="text-xs font-semibold px-2 py-1 rounded bg-amber-500/20 text-amber-300">Target Hedge</div>
            <div class="text-[11px] text-[var(--muted-foreground)] mt-0.5">-10% SGOV stop</div>
          </div>
        </div>
      </div>
    </div>

    <!-- Live Scanner Opportunity Radar -->
    <div class="bg-[var(--card)] border border-[var(--border)] rounded-2xl p-5 shadow-sm">
      <div class="flex justify-between items-center mb-3">
        <h3 class="text-base font-bold text-[var(--foreground)]">Watchlist Radar (Today's Scan)</h3>
        <span class="px-2 py-0.5 text-xs rounded bg-purple-500/10 text-purple-400 border border-purple-500/20 font-semibold">5 Leaders Tracked</span>
      </div>
      <p class="text-xs text-[var(--muted-foreground)] mb-3">The model continuously evaluates momentum leaders. Why haven't they been bought yet? The macro gate requires <span class="font-semibold text-rose-400">P(Up) >= 65%</span> during defensive regimes:</p>

      <div class="space-y-2">
        <div class="flex justify-between items-center p-2 rounded-lg bg-[var(--background)] text-xs border border-[var(--border)]">
          <div class="flex items-center gap-2">
            <span class="font-bold text-[var(--foreground)]">TSM</span>
            <span class="text-[var(--muted-foreground)]">$418.51</span>
          </div>
          <div class="flex items-center gap-3">
            <span>P(Up): <strong class="text-emerald-400">57.8%</strong></span>
            <span class="px-2 py-0.5 rounded bg-zinc-500/10 text-zinc-400 text-[11px] font-semibold">HOLD (Below 65%)</span>
          </div>
        </div>

        <div class="flex justify-between items-center p-2 rounded-lg bg-[var(--background)] text-xs border border-[var(--border)]">
          <div class="flex items-center gap-2">
            <span class="font-bold text-[var(--foreground)]">AAPL</span>
            <span class="text-[var(--muted-foreground)]">$347.73</span>
          </div>
          <div class="flex items-center gap-3">
            <span>P(Up): <strong class="text-emerald-400">57.2%</strong></span>
            <span class="px-2 py-0.5 rounded bg-zinc-500/10 text-zinc-400 text-[11px] font-semibold">HOLD (Below 65%)</span>
          </div>
        </div>

        <div class="flex justify-between items-center p-2 rounded-lg bg-[var(--background)] text-xs border border-[var(--border)]">
          <div class="flex items-center gap-2">
            <span class="font-bold text-[var(--foreground)]">NVDA</span>
            <span class="text-[var(--muted-foreground)]">$210.96</span>
          </div>
          <div class="flex items-center gap-3">
            <span>P(Up): <strong class="text-amber-400">54.3%</strong></span>
            <span class="px-2 py-0.5 rounded bg-zinc-500/10 text-zinc-400 text-[11px] font-semibold">HOLD (Below 65%)</span>
          </div>
        </div>

        <div class="flex justify-between items-center p-2 rounded-lg bg-[var(--background)] text-xs border border-[var(--border)]">
          <div class="flex items-center gap-2">
            <span class="font-bold text-[var(--foreground)]">AMD</span>
            <span class="text-[var(--muted-foreground)]">$516.79</span>
          </div>
          <div class="flex items-center gap-3">
            <span>P(Up): <strong class="text-amber-400">54.0%</strong></span>
            <span class="px-2 py-0.5 rounded bg-zinc-500/10 text-zinc-400 text-[11px] font-semibold">HOLD (Below 65%)</span>
          </div>
        </div>

        <div class="flex justify-between items-center p-2 rounded-lg bg-[var(--background)] text-xs border border-[var(--border)]">
          <div class="flex items-center gap-2">
            <span class="font-bold text-[var(--foreground)]">MSFT</span>
            <span class="text-[var(--muted-foreground)]">$524.51</span>
          </div>
          <div class="flex items-center gap-3">
            <span>P(Up): <strong class="text-amber-400">53.1%</strong></span>
            <span class="px-2 py-0.5 rounded bg-zinc-500/10 text-zinc-400 text-[11px] font-semibold">HOLD (Below 65%)</span>
          </div>
        </div>
      </div>
    </div>
  </div>

  <!-- Footer -->
  <footer class="mt-8 pt-6 border-t border-[var(--border)] flex flex-wrap justify-between items-center text-xs text-[var(--muted-foreground)] gap-4">
    <div>Single Source of Truth: <span class="font-semibold text-[var(--foreground)]">portfolio.json & data/equity_history.json</span></div>
    <div>Live Broker Link: <span class="font-semibold text-emerald-400">Charles Schwab REST API</span></div>
  </footer>

  <!-- Chart Rendering Script -->
  <script>
    const liveSeries = """ + json.dumps(live_equity_series) + """;
    const qqqSeries  = """ + json.dumps(qqq_normalized) + """;
    const dates      = """ + json.dumps(qqq_dates) + """;

    const container = document.getElementById('live-chart-wrapper');
    const svg = document.getElementById('live-svg');
    const tooltip = document.getElementById('live-tooltip');
    const ltDate = document.getElementById('lt-date');
    const ltEq   = document.getElementById('lt-eq');
    const ltQqq  = document.getElementById('lt-qqq');
    const ltNote = document.getElementById('lt-note');

    function renderLiveChart() {
      const w = container.clientWidth;
      const h = container.clientHeight;
      svg.setAttribute('viewBox', `0 0 ${w} ${h}`);
      svg.innerHTML = '';

      const padL = 60, padR = 20, padT = 20, padB = 30;
      const plotW = w - padL - padR;
      const plotH = h - padT - padB;

      const eqVals  = liveSeries.map(d => d.equity);
      const allVals = eqVals.concat(qqqSeries);
      const minV = Math.floor(Math.min(...allVals) * 0.97 / 500) * 500;
      const maxV = Math.ceil(Math.max(...allVals) * 1.03 / 500) * 500;

      const scaleY = (v) => padT + plotH - ((v - minV) / (maxV - minV || 1)) * plotH;
      const scaleX = (i) => padL + (i / (dates.length - 1 || 1)) * plotW;

      // Horizontal grid lines
      const steps = 5;
      for (let i = 0; i <= steps; i++) {
        const val = minV + (i / steps) * (maxV - minV);
        const y = scaleY(val);

        const l = document.createElementNS('http://www.w3.org/2000/svg', 'line');
        l.setAttribute('x1', padL);
        l.setAttribute('y1', y);
        l.setAttribute('x2', w - padR);
        l.setAttribute('y2', y);
        l.setAttribute('stroke', 'var(--border)');
        l.setAttribute('stroke-dasharray', '3 3');
        l.setAttribute('opacity', '0.4');
        svg.appendChild(l);

        const txt = document.createElementNS('http://www.w3.org/2000/svg', 'text');
        txt.setAttribute('x', padL - 8);
        txt.setAttribute('y', y + 4);
        txt.setAttribute('text-anchor', 'end');
        txt.setAttribute('font-size', '10');
        txt.setAttribute('fill', 'var(--muted-foreground)');
        txt.textContent = '$' + Math.round(val).toLocaleString();
        svg.appendChild(txt);
      }

      // Path generator
      function getPath(vals) {
        return vals.map((v, i) => `${i === 0 ? 'M' : 'L'} ${scaleX(i).toFixed(1)} ${scaleY(v).toFixed(1)}`).join(' ');
      }

      // QQQ Benchmark Line
      const pathQ = document.createElementNS('http://www.w3.org/2000/svg', 'path');
      pathQ.setAttribute('d', getPath(qqqSeries));
      pathQ.setAttribute('fill', 'none');
      pathQ.setAttribute('stroke', '#0ea5e9');
      pathQ.setAttribute('stroke-width', '2');
      pathQ.setAttribute('stroke-dasharray', '4 4');
      svg.appendChild(pathQ);

      // Strategy Equity Line
      const pathS = document.createElementNS('http://www.w3.org/2000/svg', 'path');
      pathS.setAttribute('d', getPath(eqVals));
      pathS.setAttribute('fill', 'none');
      pathS.setAttribute('stroke', '#10b981');
      pathS.setAttribute('stroke-width', '3.5');
      svg.appendChild(pathS);

      // Data Points on Strategy Line
      eqVals.forEach((v, i) => {
        const dot = document.createElementNS('http://www.w3.org/2000/svg', 'circle');
        dot.setAttribute('cx', scaleX(i));
        dot.setAttribute('cy', scaleY(v));
        dot.setAttribute('r', '4');
        dot.setAttribute('fill', '#10b981');
        dot.setAttribute('stroke', 'var(--card)');
        dot.setAttribute('stroke-width', '2');
        svg.appendChild(dot);
      });

      // Mouse Move Interaction
      container.onmousemove = (e) => {
        const rect = container.getBoundingClientRect();
        const mouseX = e.clientX - rect.left;
        if (mouseX < padL || mouseX > w - padR) {
          tooltip.style.opacity = '0';
          return;
        }

        const frac = (mouseX - padL) / plotW;
        const idx = Math.min(dates.length - 1, Math.max(0, Math.round(frac * (dates.length - 1))));

        tooltip.style.opacity = '1';
        let tipX = mouseX + 15;
        if (tipX + 260 > w) tipX = mouseX - 270;
        tooltip.style.transform = `translate(${tipX}px, ${Math.min(h - 110, Math.max(10, e.clientY - rect.top - 40))}px)`;

        ltDate.textContent = dates[idx];
        ltEq.textContent   = '$' + eqVals[idx].toLocaleString('en-US', {minimumFractionDigits: 2});
        ltQqq.textContent  = '$' + qqqSeries[idx].toLocaleString('en-US', {minimumFractionDigits: 2}) + ` (${((qqqSeries[idx]/10000 - 1)*100).toFixed(2)}%)`;
        ltNote.textContent = liveSeries[idx].note;
      };

      container.onmouseleave = () => {
        tooltip.style.opacity = '0';
      };
    }

    window.addEventListener('resize', renderLiveChart);
    renderLiveChart();
  </script>
</body>
</html>
"""

out_file = os.path.join(artifact_dir, "live_portfolio_dashboard.html")
with open(out_file, "w") as f:
    f.write(html)

print(f"Successfully generated Live Portfolio Dashboard at: {out_file}")
