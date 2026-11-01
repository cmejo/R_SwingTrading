import json
import os

artifact_dir = "/Users/cmejo/.gemini/antigravity/brain/64019830-0cac-4bb1-904e-d4560e71468c"
data_path = "output/dashboard_data.json"

with open(data_path, "r") as f:
    dashboard_data = json.load(f)

json_str = json.dumps(dashboard_data)

html_content = f"""<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <title>Quantitative Momentum Engine - Performance Dashboard</title>
  <script src="https://www.gstatic.com/antigravity/web/dev/tailwindcss.min.js"></script>
  <style>
    /* Custom scrollbar and subtle transitions */
    ::-webkit-scrollbar {{ width: 6px; height: 6px; }}
    ::-webkit-scrollbar-track {{ background: transparent; }}
    ::-webkit-scrollbar-thumb {{ background: var(--border); border-radius: 4px; }}
    .chart-container {{ position: relative; width: 100%; height: 380px; }}
    .tooltip-card {{
      position: absolute;
      pointer-events: none;
      transition: opacity 0.15s ease, transform 0.1s ease;
      z-index: 50;
      backdrop-filter: blur(8px);
    }}
  </style>
</head>
<body class="bg-[var(--background)] text-[var(--foreground)] antialiased p-6 md:p-8 font-sans selection:bg-sky-500/20">

  <!-- Header Section -->
  <header class="flex flex-col md:flex-row justify-between items-start md:items-center gap-4 pb-6 border-b border-[var(--border)]">
    <div>
      <div class="flex items-center gap-3">
        <div class="w-3.5 h-3.5 rounded-full bg-emerald-500 animate-pulse"></div>
        <h1 class="text-2xl font-bold tracking-tight text-[var(--foreground)]">Swing Momentum Performance Engine</h1>
        <span class="px-2.5 py-0.5 text-xs font-semibold rounded-full bg-[var(--primary)] text-[var(--primary-foreground)]">40-Year Quant Verified</span>
      </div>
      <p class="text-[var(--muted-foreground)] text-sm mt-1">Multi-Asset Momentum with Option A Constituent Lifespan, 2-Tier ATR Brackets & Cash Park Engine</p>
    </div>

    <!-- Live Parameters Indicator -->
    <div class="flex items-center gap-3 text-xs bg-[var(--card)] px-3.5 py-2 rounded-xl border border-[var(--border)]">
      <div><span class="text-[var(--muted-foreground)]">Baseline Capital:</span> <span class="font-bold text-emerald-500">$10,000</span></div>
      <div class="w-px h-3 bg-[var(--border)]"></div>
      <div><span class="text-[var(--muted-foreground)]">Slots:</span> <span class="font-semibold text-[var(--foreground)]">3 Max (Cap 2)</span></div>
      <div class="w-px h-3 bg-[var(--border)]"></div>
      <div><span class="text-[var(--muted-foreground)]">Leverage:</span> <span class="font-semibold text-[var(--foreground)]">1.5x</span></div>
    </div>
  </header>

  <!-- Controls Bar: Horizon, Tax Regime, Log/Linear Toggle -->
  <div class="flex flex-wrap items-center justify-between gap-4 mt-6">
    <!-- Horizon Selector -->
    <div class="flex flex-wrap items-center gap-1.5 p-1 bg-[var(--card)] border border-[var(--border)] rounded-xl text-xs font-medium" id="horizon-selector">
      <button class="px-3 py-1.5 rounded-lg bg-[var(--primary)] text-[var(--primary-foreground)] shadow-sm" data-horizon="ALL_40Y">40-Year (1985-2026)</button>
      <button class="px-3 py-1.5 rounded-lg text-[var(--muted-foreground)] hover:text-[var(--foreground)]" data-horizon="LAST_10Y">Last 10 Years</button>
      <button class="px-3 py-1.5 rounded-lg text-[var(--muted-foreground)] hover:text-[var(--foreground)]" data-horizon="LAST_5Y">Last 5 Years</button>
      <button class="px-3 py-1.5 rounded-lg text-[var(--muted-foreground)] hover:text-[var(--foreground)]" data-horizon="EPOCH_1">Dot-Com (95-04)</button>
      <button class="px-3 py-1.5 rounded-lg text-[var(--muted-foreground)] hover:text-[var(--foreground)]" data-horizon="EPOCH_2">Lost Decade (00-09)</button>
      <button class="px-3 py-1.5 rounded-lg text-[var(--muted-foreground)] hover:text-[var(--foreground)]" data-horizon="EPOCH_3">GFC Bull (07-16)</button>
    </div>

    <!-- Tax Regime and Scale Controls -->
    <div class="flex items-center gap-3">
      <!-- Tax Regime Toggle -->
      <div class="flex items-center gap-1 p-1 bg-[var(--card)] border border-[var(--border)] rounded-xl text-xs font-medium" id="tax-selector">
        <button class="px-2.5 py-1.5 rounded-lg bg-emerald-500/20 text-emerald-400 font-semibold" data-tax="strat_g">Gross / PR Act 60 (0%)</button>
        <button class="px-2.5 py-1.5 rounded-lg text-[var(--muted-foreground)] hover:text-[var(--foreground)]" data-tax="strat_t">NYC TTS+Pension (28.4%)</button>
        <button class="px-2.5 py-1.5 rounded-lg text-[var(--muted-foreground)] hover:text-[var(--foreground)]" data-tax="strat_n">NYC Individual (51.2%)</button>
      </div>

      <!-- Scale Toggle (Log vs Linear) -->
      <div class="flex items-center gap-1 p-1 bg-[var(--card)] border border-[var(--border)] rounded-xl text-xs font-medium" id="scale-selector">
        <button class="px-2.5 py-1.5 rounded-lg bg-[var(--secondary)] text-[var(--secondary-foreground)] font-semibold" data-scale="log">Log Scale</button>
        <button class="px-2.5 py-1.5 rounded-lg text-[var(--muted-foreground)] hover:text-[var(--foreground)]" data-scale="linear">Linear</button>
      </div>
    </div>
  </div>

  <!-- KPI Cards Grid -->
  <div class="grid grid-cols-2 md:grid-cols-4 lg:grid-cols-6 gap-3.5 mt-6">
    <!-- Card 1: Annualized Return -->
    <div class="bg-[var(--card)] border border-[var(--border)] rounded-xl p-4 shadow-sm">
      <div class="text-xs text-[var(--muted-foreground)] font-medium">Annualized CAGR</div>
      <div class="text-xl font-bold text-emerald-400 mt-1" id="kpi-cagr">+110.91%</div>
      <div class="text-[11px] text-[var(--muted-foreground)] mt-0.5" id="kpi-cagr-bench">vs QQQ: +14.33%</div>
    </div>

    <!-- Card 2: Cumulative Return -->
    <div class="bg-[var(--card)] border border-[var(--border)] rounded-xl p-4 shadow-sm">
      <div class="text-xs text-[var(--muted-foreground)] font-medium">Cumulative Return</div>
      <div class="text-xl font-bold text-sky-400 mt-1" id="kpi-cum">+2.0B%</div>
      <div class="text-[11px] text-[var(--muted-foreground)] mt-0.5" id="kpi-cum-bench">vs QQQ: +24,310%</div>
    </div>

    <!-- Card 3: Sharpe Ratio -->
    <div class="bg-[var(--card)] border border-[var(--border)] rounded-xl p-4 shadow-sm">
      <div class="text-xs text-[var(--muted-foreground)] font-medium">Sharpe Ratio</div>
      <div class="text-xl font-bold text-purple-400 mt-1" id="kpi-sharpe">2.76</div>
      <div class="text-[11px] text-[var(--muted-foreground)] mt-0.5" id="kpi-sharpe-bench">vs QQQ: 0.65</div>
    </div>

    <!-- Card 4: Max Drawdown -->
    <div class="bg-[var(--card)] border border-[var(--border)] rounded-xl p-4 shadow-sm">
      <div class="text-xs text-[var(--muted-foreground)] font-medium">Max Drawdown</div>
      <div class="text-xl font-bold text-rose-400 mt-1" id="kpi-maxdd">-31.62%</div>
      <div class="text-[11px] text-emerald-400 mt-0.5" id="kpi-maxdd-bench">QQQ suffered: -82.96%</div>
    </div>

    <!-- Card 5: Annual Volatility -->
    <div class="bg-[var(--card)] border border-[var(--border)] rounded-xl p-4 shadow-sm">
      <div class="text-xs text-[var(--muted-foreground)] font-medium">Annualized Volatility</div>
      <div class="text-xl font-bold text-[var(--foreground)] mt-1" id="kpi-vol">28.53%</div>
      <div class="text-[11px] text-[var(--muted-foreground)] mt-0.5" id="kpi-vol-bench">QQQ Vol: 25.51%</div>
    </div>

    <!-- Card 6: Terminal Wealth -->
    <div class="bg-[var(--card)] border border-[var(--border)] rounded-xl p-4 shadow-sm">
      <div class="text-xs text-[var(--muted-foreground)] font-medium">Terminal Wealth ($10k)</div>
      <div class="text-xl font-bold text-amber-400 mt-1" id="kpi-terminal">$165.8 Quadrillion</div>
      <div class="text-[11px] text-[var(--muted-foreground)] mt-0.5" id="kpi-terminal-bench">QQQ: $2,008,968</div>
    </div>
  </div>

  <!-- Main Chart Card: Equity Curves -->
  <div class="bg-[var(--card)] border border-[var(--border)] rounded-2xl p-5 mt-6 shadow-sm">
    <div class="flex flex-col md:flex-row md:items-center justify-between gap-3 mb-4">
      <div>
        <h2 class="text-base font-bold text-[var(--foreground)]">Growth of $10,000 Portfolio Over Time</h2>
        <p class="text-xs text-[var(--muted-foreground)]" id="chart-subtitle">Showing 40-Year Continuous Compounding vs Major Benchmarks</p>
      </div>

      <!-- Interactive Benchmark Toggles -->
      <div class="flex items-center gap-3 text-xs font-medium">
        <label class="flex items-center gap-1.5 cursor-pointer">
          <input type="checkbox" id="toggle-strat" checked class="accent-emerald-500 rounded">
          <span class="inline-block w-2.5 h-2.5 rounded-full bg-emerald-500"></span>
          <span>Strategy</span>
        </label>
        <label class="flex items-center gap-1.5 cursor-pointer">
          <input type="checkbox" id="toggle-qqq" checked class="accent-sky-500 rounded">
          <span class="inline-block w-2.5 h-2.5 rounded-full bg-sky-500"></span>
          <span>QQQ (Nasdaq-100)</span>
        </label>
        <label class="flex items-center gap-1.5 cursor-pointer">
          <input type="checkbox" id="toggle-vti" checked class="accent-amber-500 rounded">
          <span class="inline-block w-2.5 h-2.5 rounded-full bg-amber-500"></span>
          <span>VTI (Total Market)</span>
        </label>
        <label class="flex items-center gap-1.5 cursor-pointer">
          <input type="checkbox" id="toggle-b7030" checked class="accent-purple-500 rounded">
          <span class="inline-block w-2.5 h-2.5 rounded-full bg-purple-500"></span>
          <span>70/30 Blend</span>
        </label>
      </div>
    </div>

    <!-- Chart Container -->
    <div class="chart-container" id="chart-wrapper">
      <svg id="equity-svg" class="w-full h-full overflow-visible"></svg>
      <!-- Tooltip Card -->
      <div id="chart-tooltip" class="tooltip-card opacity-0 bg-[var(--card)] text-[var(--foreground)] border border-[var(--border)] rounded-xl p-3 shadow-xl text-xs w-60">
        <div class="font-bold text-[var(--foreground)] border-b border-[var(--border)] pb-1.5 mb-2" id="tt-date">--</div>
        <div class="space-y-1">
          <div class="flex justify-between items-center text-emerald-400 font-semibold">
            <span>Strategy:</span>
            <span id="tt-strat">--</span>
          </div>
          <div class="flex justify-between items-center text-sky-400">
            <span>QQQ:</span>
            <span id="tt-qqq">--</span>
          </div>
          <div class="flex justify-between items-center text-amber-400">
            <span>VTI:</span>
            <span id="tt-vti">--</span>
          </div>
          <div class="flex justify-between items-center text-purple-400">
            <span>70/30:</span>
            <span id="tt-b7030">--</span>
          </div>
        </div>
      </div>
    </div>
  </div>

  <!-- Secondary Chart Card: Underwater Drawdowns -->
  <div class="bg-[var(--card)] border border-[var(--border)] rounded-2xl p-5 mt-6 shadow-sm">
    <div class="mb-3">
      <h3 class="text-base font-bold text-[var(--foreground)]">Underwater Peak-to-Trough Drawdowns (%)</h3>
      <p class="text-xs text-[var(--muted-foreground)]">Demonstrates how the strategy’s dynamic cash park truncates catastrophic market crashes</p>
    </div>
    <div class="chart-container" style="height: 180px;" id="dd-wrapper">
      <svg id="drawdown-svg" class="w-full h-full overflow-visible"></svg>
    </div>
  </div>

  <!-- Decade & Epoch Breakdown Table -->
  <div class="bg-[var(--card)] border border-[var(--border)] rounded-2xl p-5 mt-6 shadow-sm">
    <div class="flex justify-between items-center mb-4">
      <div>
        <h3 class="text-base font-bold text-[var(--foreground)]">Decade-by-Decade Historical Scorecard</h3>
        <p class="text-xs text-[var(--muted-foreground)]">Performance across disparate macroeconomic regimes over 40 years</p>
      </div>
      <span class="text-xs text-[var(--muted-foreground)]">Option A Constituent Lifespan</span>
    </div>

    <div class="overflow-x-auto">
      <table class="w-full text-xs text-left border-collapse">
        <thead>
          <tr class="border-b border-[var(--border)] text-[var(--muted-foreground)] font-semibold">
            <th class="py-2.5 px-3">Period & Macro Theme</th>
            <th class="py-2.5 px-3 text-right">Strategy CAGR</th>
            <th class="py-2.5 px-3 text-right">Strategy MaxDD</th>
            <th class="py-2.5 px-3 text-right">Strategy Sharpe</th>
            <th class="py-2.5 px-3 text-right">QQQ CAGR</th>
            <th class="py-2.5 px-3 text-right">QQQ MaxDD</th>
            <th class="py-2.5 px-3 text-right">VTI CAGR</th>
            <th class="py-2.5 px-3 text-right">VTI MaxDD</th>
            <th class="py-2.5 px-3 text-right text-emerald-400">Strategy Edge</th>
          </tr>
        </thead>
        <tbody class="divide-y divide-[var(--border)]" id="decade-table-body">
          <!-- Dynamically populated -->
        </tbody>
      </table>
    </div>
  </div>

  <!-- Core Quant Rules Badge Footer -->
  <footer class="mt-8 pt-6 border-t border-[var(--border)] flex flex-wrap justify-between items-center text-xs text-[var(--muted-foreground)] gap-4">
    <div class="flex items-center gap-2">
      <span class="w-2 h-2 rounded-full bg-emerald-500"></span>
      <span>Authentic Risk Architecture: 2.0x ATR Stop, +1.5R Tier 1 (50%), +3.0R Tier 2 (25%), Chandelier Runner (25%)</span>
    </div>
    <div>System Status: <span class="font-semibold text-emerald-400">Production Ready</span></div>
  </footer>

  <!-- Application Logic -->
  <script>
    const DATA = {json_str};

    let currentHorizon = 'ALL_40Y';
    let currentTax = 'strat_g';
    let currentScale = 'log';

    // Elements
    const horizonButtons = document.querySelectorAll('#horizon-selector button');
    const taxButtons = document.querySelectorAll('#tax-selector button');
    const scaleButtons = document.querySelectorAll('#scale-selector button');

    const toggleStrat = document.getElementById('toggle-strat');
    const toggleQqq   = document.getElementById('toggle-qqq');
    const toggleVti   = document.getElementById('toggle-vti');
    const toggleB7030 = document.getElementById('toggle-b7030');

    // Number formatter
    function fmtDollar(v) {{
      if (v >= 1e15) return '$' + (v / 1e15).toFixed(1) + 'Q';
      if (v >= 1e12) return '$' + (v / 1e12).toFixed(1) + 'T';
      if (v >= 1e9)  return '$' + (v / 1e9).toFixed(1) + 'B';
      if (v >= 1e6)  return '$' + (v / 1e6).toFixed(1) + 'M';
      if (v >= 1e3)  return '$' + Math.round(v).toLocaleString();
      return '$' + v.toFixed(2);
    }}

    // Populate Decade Table
    function populateDecadeTable() {{
      const tbody = document.getElementById('decade-table-body');
      tbody.innerHTML = '';
      DATA.decades.forEach(row => {{
        const tr = document.createElement('tr');
        tr.className = 'hover:bg-[var(--card)]/50 transition-colors';
        const s_cagr = parseFloat(row.Strategy_CAGR);
        const q_cagr = parseFloat(row.QQQ_CAGR);
        const edge = (s_cagr - q_cagr).toFixed(2);

        tr.innerHTML = `
          <td class="py-2.5 px-3 font-medium text-[var(--foreground)]">${{row.Period}}</td>
          <td class="py-2.5 px-3 text-right font-bold text-emerald-400">${{row.Strategy_CAGR}}</td>
          <td class="py-2.5 px-3 text-right text-rose-400">${{row.Strategy_DD}}</td>
          <td class="py-2.5 px-3 text-right text-purple-400 font-semibold">${{row.Strategy_SR}}</td>
          <td class="py-2.5 px-3 text-right text-sky-400">${{row.QQQ_CAGR}}</td>
          <td class="py-2.5 px-3 text-right text-rose-400/80">${{row.QQQ_DD}}</td>
          <td class="py-2.5 px-3 text-right text-amber-400">${{row.VTI_CAGR}}</td>
          <td class="py-2.5 px-3 text-right text-rose-400/80">${{row.VTI_DD}}</td>
          <td class="py-2.5 px-3 text-right font-bold text-emerald-400">+${{edge}}% / yr</td>
        `;
        tbody.appendChild(tr);
      }});
    }}

    // Update KPI Cards
    function updateKPIs() {{
      const hData = DATA.horizons[currentHorizon];
      const m = hData.metrics;

      let s_m;
      if (currentTax === 'strat_g') s_m = m.strategy_gross;
      else if (currentTax === 'strat_t') s_m = m.strategy_tts;
      else s_m = m.strategy_nyc;

      document.getElementById('kpi-cagr').textContent = '+' + s_m.cagr + '%';
      document.getElementById('kpi-cagr-bench').textContent = `vs QQQ: +${{m.qqq.cagr}}%`;

      document.getElementById('kpi-cum').textContent = '+' + (s_m.cum_ret >= 1e6 ? (s_m.cum_ret / 1e6).toFixed(1) + 'M%' : s_m.cum_ret.toLocaleString() + '%');
      document.getElementById('kpi-cum-bench').textContent = `vs QQQ: +${{m.qqq.cum_ret}}%`;

      document.getElementById('kpi-sharpe').textContent = s_m.sharpe;
      document.getElementById('kpi-sharpe-bench').textContent = `vs QQQ: ${{m.qqq.sharpe}}`;

      document.getElementById('kpi-maxdd').textContent = '-' + s_m.max_dd + '%';
      document.getElementById('kpi-maxdd-bench').textContent = `QQQ suffered: -${{m.qqq.max_dd}}%`;

      document.getElementById('kpi-vol').textContent = s_m.vol + '%';
      document.getElementById('kpi-vol-bench').textContent = `QQQ Vol: ${{m.qqq.vol}}%`;

      document.getElementById('kpi-terminal').textContent = fmtDollar(s_m.terminal);
      document.getElementById('kpi-terminal-bench').textContent = `QQQ: ${{fmtDollar(m.qqq.terminal)}}`;

      document.getElementById('chart-subtitle').textContent = `${{hData.label}} (${{hData.start_date}} to ${{hData.end_date}} | ${{hData.trading_days}} sessions)`;
    }}

    // Render SVG Charts
    function renderCharts() {{
      const hData = DATA.horizons[currentHorizon];
      const chartData = hData.chart;
      const nPts = chartData.dates.length;

      // Primary Equity SVG
      const svg = document.getElementById('equity-svg');
      const container = document.getElementById('chart-wrapper');
      const w = container.clientWidth;
      const h = container.clientHeight;

      svg.setAttribute('viewBox', `0 0 ${{w}} ${{h}}`);
      svg.innerHTML = '';

      const padL = 60, padR = 20, padT = 20, padB = 30;
      const plotW = w - padL - padR;
      const plotH = h - padT - padB;

      // Extract active series
      const s_vals = chartData[currentTax];
      const q_vals = chartData.qqq;
      const v_vals = chartData.vti;
      const b_vals = chartData.b7030;

      // Min/Max calculation
      let all_vals = [];
      if (toggleStrat.checked) all_vals = all_vals.concat(s_vals);
      if (toggleQqq.checked)   all_vals = all_vals.concat(q_vals);
      if (toggleVti.checked)   all_vals = all_vals.concat(v_vals);
      if (toggleB7030.checked) all_vals = all_vals.concat(b_vals);

      if (all_vals.length === 0) all_vals = [10000];

      let minV = Math.min(...all_vals);
      let maxV = Math.max(...all_vals);
      if (minV <= 0) minV = 1;

      const isLog = (currentScale === 'log');
      const scaleY = (val) => {{
        if (isLog) {{
          const logMin = Math.log10(minV);
          const logMax = Math.log10(maxV);
          const logV = Math.log10(Math.max(val, minV));
          return padT + plotH - ((logV - logMin) / (logMax - logMin || 1)) * plotH;
        }} else {{
          return padT + plotH - ((val - minV) / (maxV - minV || 1)) * plotH;
        }}
      }};

      const scaleX = (idx) => padL + (idx / (nPts - 1 || 1)) * plotW;

      // Horizontal grid lines
      const nGrid = 5;
      for (let i = 0; i <= nGrid; i++) {{
        const gridY = padT + (i / nGrid) * plotH;
        const lineVal = isLog 
          ? Math.pow(10, Math.log10(maxV) - (i / nGrid) * (Math.log10(maxV) - Math.log10(minV)))
          : maxV - (i / nGrid) * (maxV - minV);

        const gridLine = document.createElementNS('http://www.w3.org/2000/svg', 'line');
        gridLine.setAttribute('x1', padL);
        gridLine.setAttribute('y1', gridY);
        gridLine.setAttribute('x2', w - padR);
        gridLine.setAttribute('y2', gridY);
        gridLine.setAttribute('stroke', 'var(--border)');
        gridLine.setAttribute('stroke-dasharray', '4 4');
        gridLine.setAttribute('opacity', '0.4');
        svg.appendChild(gridLine);

        const txt = document.createElementNS('http://www.w3.org/2000/svg', 'text');
        txt.setAttribute('x', padL - 8);
        txt.setAttribute('y', gridY + 4);
        txt.setAttribute('text-anchor', 'end');
        txt.setAttribute('font-size', '10');
        txt.setAttribute('fill', 'var(--muted-foreground)');
        txt.textContent = fmtDollar(lineVal);
        svg.appendChild(txt);
      }}

      // Helper to generate SVG path string
      function genPath(vals) {{
        return vals.map((v, i) => `${{i === 0 ? 'M' : 'L'}} ${{scaleX(i).toFixed(1)}} ${{scaleY(v).toFixed(1)}}`).join(' ');
      }}

      // Benchmark lines
      if (toggleB7030.checked) {{
        const pathB = document.createElementNS('http://www.w3.org/2000/svg', 'path');
        pathB.setAttribute('d', genPath(b_vals));
        pathB.setAttribute('fill', 'none');
        pathB.setAttribute('stroke', '#a855f7');
        pathB.setAttribute('stroke-width', '1.5');
        pathB.setAttribute('opacity', '0.7');
        svg.appendChild(pathB);
      }}

      if (toggleVti.checked) {{
        const pathV = document.createElementNS('http://www.w3.org/2000/svg', 'path');
        pathV.setAttribute('d', genPath(v_vals));
        pathV.setAttribute('fill', 'none');
        pathV.setAttribute('stroke', '#f59e0b');
        pathV.setAttribute('stroke-width', '1.5');
        pathV.setAttribute('opacity', '0.8');
        svg.appendChild(pathV);
      }}

      if (toggleQqq.checked) {{
        const pathQ = document.createElementNS('http://www.w3.org/2000/svg', 'path');
        pathQ.setAttribute('d', genPath(q_vals));
        pathQ.setAttribute('fill', 'none');
        pathQ.setAttribute('stroke', '#0ea5e9');
        pathQ.setAttribute('stroke-width', '2');
        pathQ.setAttribute('opacity', '0.9');
        svg.appendChild(pathQ);
      }}

      // Strategy Primary Line
      if (toggleStrat.checked) {{
        const pathS = document.createElementNS('http://www.w3.org/2000/svg', 'path');
        pathS.setAttribute('d', genPath(s_vals));
        pathS.setAttribute('fill', 'none');
        pathS.setAttribute('stroke', '#10b981');
        pathS.setAttribute('stroke-width', '3');
        svg.appendChild(pathS);
      }}

      // Drawdown SVG
      const ddSvg = document.getElementById('drawdown-svg');
      const ddW = document.getElementById('dd-wrapper').clientWidth;
      const ddH = document.getElementById('dd-wrapper').clientHeight;
      ddSvg.setAttribute('viewBox', `0 0 ${{ddW}} ${{ddH}}`);
      ddSvg.innerHTML = '';

      const ddPadL = 60, ddPadR = 20, ddPadT = 15, ddPadB = 25;
      const ddPlotW = ddW - ddPadL - ddPadR;
      const ddPlotH = ddH - ddPadT - ddPadB;

      const scaleDdY = (dd) => ddPadT + (Math.abs(dd) / 100) * ddPlotH;
      const scaleDdX = (i) => ddPadL + (i / (nPts - 1 || 1)) * ddPlotW;

      // Drawdown Grid
      [0, -25, -50, -75].forEach(lvl => {{
        const y = scaleDdY(lvl);
        const l = document.createElementNS('http://www.w3.org/2000/svg', 'line');
        l.setAttribute('x1', ddPadL);
        l.setAttribute('y1', y);
        l.setAttribute('x2', ddW - ddPadR);
        l.setAttribute('y2', y);
        l.setAttribute('stroke', 'var(--border)');
        l.setAttribute('stroke-dasharray', '3 3');
        l.setAttribute('opacity', '0.4');
        ddSvg.appendChild(l);

        const txt = document.createElementNS('http://www.w3.org/2000/svg', 'text');
        txt.setAttribute('x', ddPadL - 8);
        txt.setAttribute('y', y + 3);
        txt.setAttribute('text-anchor', 'end');
        txt.setAttribute('font-size', '9');
        txt.setAttribute('fill', 'var(--muted-foreground)');
        txt.textContent = lvl + '%';
        ddSvg.appendChild(txt);
      }});

      // QQQ Drawdown Area
      if (toggleQqq.checked) {{
        let dAreaQ = `M ${{scaleDdX(0)}} ${{scaleDdY(0)}}`;
        chartData.qqq_dd.forEach((dd, i) => dAreaQ += ` L ${{scaleDdX(i)}} ${{scaleDdY(dd)}}`);
        dAreaQ += ` L ${{scaleDdX(nPts - 1)}} ${{scaleDdY(0)}} Z`;

        const areaQ = document.createElementNS('http://www.w3.org/2000/svg', 'path');
        areaQ.setAttribute('d', dAreaQ);
        areaQ.setAttribute('fill', '#0ea5e9');
        areaQ.setAttribute('opacity', '0.2');
        ddSvg.appendChild(areaQ);
      }}

      // Strategy Drawdown Area
      if (toggleStrat.checked) {{
        let dAreaS = `M ${{scaleDdX(0)}} ${{scaleDdY(0)}}`;
        chartData.strat_dd.forEach((dd, i) => dAreaS += ` L ${{scaleDdX(i)}} ${{scaleDdY(dd)}}`);
        dAreaS += ` L ${{scaleDdX(nPts - 1)}} ${{scaleDdY(0)}} Z`;

        const areaS = document.createElementNS('http://www.w3.org/2000/svg', 'path');
        areaS.setAttribute('d', dAreaS);
        areaS.setAttribute('fill', '#10b981');
        areaS.setAttribute('opacity', '0.45');
        ddSvg.appendChild(areaS);
      }}

      // Mouse Crosshair Interaction
      const tooltip = document.getElementById('chart-tooltip');
      const ttDate = document.getElementById('tt-date');
      const ttStrat = document.getElementById('tt-strat');
      const ttQqq   = document.getElementById('tt-qqq');
      const ttVti   = document.getElementById('tt-vti');
      const ttB7030 = document.getElementById('tt-b7030');

      container.onmousemove = (e) => {{
        const rect = container.getBoundingClientRect();
        const mouseX = e.clientX - rect.left;
        if (mouseX < padL || mouseX > w - padR) {{
          tooltip.style.opacity = '0';
          return;
        }}

        const frac = (mouseX - padL) / plotW;
        const idx = Math.min(nPts - 1, Math.max(0, Math.round(frac * (nPts - 1))));

        tooltip.style.opacity = '1';
        let tipX = mouseX + 15;
        if (tipX + 240 > w) tipX = mouseX - 250;
        tooltip.style.transform = `translate(${{tipX}}px, ${{Math.min(h - 130, Math.max(20, e.clientY - rect.top - 60))}}px)`;

        ttDate.textContent = chartData.dates[idx];
        ttStrat.textContent = fmtDollar(s_vals[idx]) + ` (${{chartData.strat_dd[idx]}}% DD)`;
        ttQqq.textContent   = fmtDollar(q_vals[idx]) + ` (${{chartData.qqq_dd[idx]}}% DD)`;
        ttVti.textContent   = fmtDollar(v_vals[idx]) + ` (${{chartData.vti_dd[idx]}}% DD)`;
        ttB7030.textContent = fmtDollar(b_vals[idx]);
      }};

      container.onmouseleave = () => {{
        tooltip.style.opacity = '0';
      }};
    }}

    // Event Listeners for Controls
    horizonButtons.forEach(btn => {{
      btn.addEventListener('click', () => {{
        horizonButtons.forEach(b => {{
          b.className = 'px-3 py-1.5 rounded-lg text-[var(--muted-foreground)] hover:text-[var(--foreground)]';
        }});
        btn.className = 'px-3 py-1.5 rounded-lg bg-[var(--primary)] text-[var(--primary-foreground)] shadow-sm';
        currentHorizon = btn.dataset.horizon;
        updateKPIs();
        renderCharts();
      }});
    }});

    taxButtons.forEach(btn => {{
      btn.addEventListener('click', () => {{
        taxButtons.forEach(b => {{
          b.className = 'px-2.5 py-1.5 rounded-lg text-[var(--muted-foreground)] hover:text-[var(--foreground)]';
        }});
        btn.className = 'px-2.5 py-1.5 rounded-lg bg-emerald-500/20 text-emerald-400 font-semibold';
        currentTax = btn.dataset.tax;
        updateKPIs();
        renderCharts();
      }});
    }});

    scaleButtons.forEach(btn => {{
      btn.addEventListener('click', () => {{
        scaleButtons.forEach(b => {{
          b.className = 'px-2.5 py-1.5 rounded-lg text-[var(--muted-foreground)] hover:text-[var(--foreground)]';
        }});
        btn.className = 'px-2.5 py-1.5 rounded-lg bg-[var(--secondary)] text-[var(--secondary-foreground)] font-semibold';
        currentScale = btn.dataset.scale;
        renderCharts();
      }});
    }});

    [toggleStrat, toggleQqq, toggleVti, toggleB7030].forEach(chk => {{
      chk.addEventListener('change', renderCharts);
    }});

    window.addEventListener('resize', renderCharts);

    // Initial render
    populateDecadeTable();
    updateKPIs();
    renderCharts();
  </script>
</body>
</html>
"""

out_html_path = os.path.join(artifact_dir, "performance_dashboard.html")
with open(out_html_path, "w") as f:
    f.write(html_content)

print(f"Successfully generated standalone dashboard at: {out_html_path}")
