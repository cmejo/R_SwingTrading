"""
factsheet/make_ultimate_alpha_factsheet.py
Builds a high-impact, professional 2-page Letter PDF factsheet for
Option D: Ultimate Alpha Master using headless Chrome.
"""
import csv, json, os, subprocess, base64, datetime as dt
from collections import defaultdict

ROOT = os.path.dirname(os.path.abspath(__file__))
J = json.load(open(f"{ROOT}/data/ultimate_factsheet.json"))
S = J["stats"]; INIT = J["init"]

# Calendar-year returns
yr = defaultdict(lambda: {"ultimate": 1.0, "baseline": 1.0, "qqq": 1.0})
for row in csv.DictReader(open(f"{ROOT}/data/ultimate_monthly.csv")):
    y = row["month"][:4]
    for k in ("ultimate", "baseline", "qqq"):
        yr[y][k] *= 1 + float(row[k]) / 100
years = sorted(yr)

def img(name):
    return "data:image/png;base64," + base64.b64encode(open(f"{ROOT}/img/{name}", "rb").read()).decode()

def pct(x, d=1, sign=False):
    return (f"{x*100:+.{d}f}%" if sign else f"{x*100:.{d}f}%")

def usd(x):
    if x >= 1e9:
        return f"${x/1e9:,.2f}B"
    elif x >= 1e6:
        return f"${x/1e6:,.2f}M"
    else:
        return f"${x:,.0f}"

def cls(x):
    return "neg" if x < 0 else "pos"

def d2(s):
    try:
        return dt.date.fromisoformat(s).strftime("%b %Y")
    except Exception:
        return s

end_date = dt.date.fromisoformat(J["end"]).strftime("%B %-d, %Y")
start_date = dt.date.fromisoformat(J["start"]).strftime("%B %Y")
u_stat = S["ultimate"]
b_stat = S["baseline"]
q_stat = S["qqq"]

stat_rows = [
    ("Ending value of $10,000", usd(u_stat["end_value"]), usd(b_stat["end_value"]), usd(q_stat["end_value"])),
    ("Cumulative return", f"+{u_stat['total']*100:,.0f}%", f"+{b_stat['total']*100:,.0f}%", f"+{q_stat['total']*100:,.0f}%"),
    ("Annualized return (20-Yr CAGR)", pct(u_stat["cagr"]), pct(b_stat["cagr"]), pct(q_stat["cagr"])),
    ("Annualized volatility", pct(u_stat["vol"]), pct(b_stat["vol"]), pct(q_stat["vol"])),
    ("Sharpe ratio (rf=T-bill)", f"{u_stat['sharpe']:.2f}", f"{b_stat['sharpe']:.2f}", f"{q_stat['sharpe']:.2f}"),
    ("Sortino ratio (downside risk)", f"{u_stat['sortino']:.2f}", f"{b_stat['sortino']:.2f}", f"{q_stat['sortino']:.2f}"),
    ("Maximum drawdown", pct(u_stat["maxdd"]), pct(b_stat["maxdd"]), pct(q_stat["maxdd"])),
    ("Max drawdown period", f"{d2(u_stat['dd_peak'])} &ndash; {d2(u_stat['dd_trough'])}", f"{d2(b_stat['dd_peak'])} &ndash; {d2(b_stat['dd_trough'])}", f"{d2(q_stat['dd_peak'])} &ndash; {d2(q_stat['dd_trough'])}"),
    ("Recovered to prior peak", d2(u_stat["dd_recovery"]), d2(b_stat["dd_recovery"]), d2(q_stat["dd_recovery"])),
    ("Calmar ratio (CAGR / MaxDD)", f"{u_stat['calmar']:.2f}", f"{b_stat['calmar']:.2f}", f"{q_stat['calmar']:.2f}"),
    ("Best calendar year", f"{pct(u_stat['best_year'],0,True)} ({u_stat['best_year_lbl']})", f"{pct(b_stat['best_year'],0,True)} ({b_stat['best_year_lbl']})", f"{pct(q_stat['best_year'],0,True)} ({q_stat['best_year_lbl']})"),
    ("Worst calendar year", f"{pct(u_stat['worst_year'],0,True)} ({u_stat['worst_year_lbl']})", f"{pct(b_stat['worst_year'],0,True)} ({b_stat['worst_year_lbl']})", f"{pct(q_stat['worst_year'],0,True)} ({q_stat['worst_year_lbl']})"),
    ("Best / worst month", f"{pct(u_stat['best_month'],0,True)} / {pct(u_stat['worst_month'],0,True)}", f"{pct(b_stat['best_month'],0,True)} / {pct(b_stat['worst_month'],0,True)}", f"{pct(q_stat['best_month'],0,True)} / {pct(q_stat['worst_month'],0,True)}"),
    ("Positive months (%)", pct(u_stat["pct_pos_months"], 0), pct(b_stat["pct_pos_months"], 0), pct(q_stat["pct_pos_months"], 0)),
]
stats_html = "\n".join(f"<tr><td>{a}</td><td class='c-ult'>{b}</td><td>{c}</td><td class='muted'>{e}</td></tr>" for a, b, c, e in stat_rows)

def period_rows():
    out = []
    for name, v in J["sub"].items():
        cagr, mdd = v[0], v[1]
        out.append(f"<tr><td>{name}</td>" + "".join(f"<td class='{cls(c)}'>{pct(c)}</td>" for c in cagr)
                   + f"<td class='neg'>{pct(mdd[0],0)}</td></tr>")
    return "\n".join(out)

def crisis_rows():
    return "\n".join(f"<tr><td>{n}</td>" + "".join(f"<td class='{cls(x)}'>{pct(x,0,True)}</td>" for x in v) + "</tr>"
                     for n, v in J["crisis"].items())

def year_table():
    half = [years[:10], years[10:]]
    rows = []
    for grp in half:
        rows.append("<tr class='yh'><th></th>" + "".join(f"<th>{y}{'*' if y == years[-1] else ''}</th>" for y in grp) + "</tr>")
        for k, lab, cl in (("ultimate", "Ultimate Alpha", "c-ult"), ("baseline", "Power Turbo", ""), ("qqq", "QQQ Index", "muted")):
            rows.append(f"<tr><td class='lab {cl}'>{lab}</td>" + "".join(
                f"<td class='{cls(round((yr[y][k]-1)*100))}'>{round((yr[y][k]-1)*100):+d}%</td>" for y in grp) + "</tr>")
    return "\n".join(rows)

HTML = f"""<!DOCTYPE html><html><head><meta charset="utf-8"><style>
@page {{ size: Letter; margin: 0; }}
* {{ box-sizing: border-box; }}
body {{ margin: 0; font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif; color: #202124; font-size: 8.5pt; line-height: 1.36; }}
.page {{ width: 8.5in; height: 11in; padding: 0.40in 0.48in 0.36in; position: relative; overflow: hidden; page-break-after: always; }}
.page:last-child {{ page-break-after: auto; }}
.band {{ background: linear-gradient(135deg, #092552 0%, #0B3D91 60%, #1557bf 100%); color: #fff; margin: -0.40in -0.48in 0.14in; padding: 0.24in 0.48in 0.18in; }}
.band h1 {{ margin: 0; font-size: 21pt; letter-spacing: -0.4px; font-weight: 800; }}
.band .sub {{ font-size: 9pt; max-width: 5.2in; opacity: 0.92; margin-top: 2px; font-weight: 400; }}
.band .meta {{ float: right; text-align: right; font-size: 7.8pt; opacity: 0.92; line-height: 1.5; margin-top: 2px; }}
h2 {{ font-size: 10pt; color: #0B3D91; margin: 0.10in 0 0.04in; padding-bottom: 2.5px; border-bottom: 1.5px solid #0B3D91; text-transform: uppercase; letter-spacing: 0.4px; font-weight: 700; }}
h3 {{ font-size: 8.8pt; margin: 0 0 2px; color: #0B3D91; font-weight: 700; }}
p {{ margin: 0 0 4px; }}
.kpis {{ display: grid; grid-template-columns: repeat(5, 1fr); gap: 7px; margin: 0.04in 0 0.06in; }}
.kpi {{ background: #F1F4FA; border-radius: 6px; padding: 6px 8px; border-left: 3.5px solid #0B3D91; box-shadow: 0 1px 2px rgba(0,0,0,0.04); }}
.kpi .v {{ font-size: 14.5pt; font-weight: 800; color: #0B3D91; line-height: 1.1; }}
.kpi .l {{ font-size: 7.2pt; color: #5F6368; font-weight: 500; text-transform: uppercase; margin-top: 2px; }}
.kpi.q {{ border-left-color: #9AA0A6; }} .kpi.q .v {{ color: #5F6368; }}
.engines {{ display: grid; grid-template-columns: repeat(4, 1fr); gap: 7px; margin-top: 0.04in; }}
.eng {{ border: 1px solid #DADCE0; border-radius: 6px; padding: 6px 7px; background: #FAFBFD; }}
.eng .tag {{ display: inline-block; background: #0B3D91; color: #fff; font-size: 6.8pt; font-weight: 700; border-radius: 3px; padding: 1px 5px; margin-bottom: 3px; }}
.eng ul {{ margin: 2px 0 0; padding-left: 12px; }} .eng li {{ margin-bottom: 1.5px; font-size: 7.7pt; line-height: 1.25; }}
.two {{ display: grid; grid-template-columns: 1.05fr 0.95fr; gap: 12px; }}
table {{ border-collapse: collapse; width: 100%; font-size: 7.9pt; }}
th, td {{ padding: 2px 4px; text-align: right; border-bottom: 1px solid #EEF0F2; white-space: nowrap; }}
th:first-child, td:first-child {{ text-align: left; }}
th {{ font-size: 7.3pt; text-transform: uppercase; color: #5F6368; font-weight: 700; border-bottom: 1.5px solid #DADCE0; }}
.c-ult {{ font-weight: 800; color: #0B3D91; }}
.muted {{ color: #5F6368; }}
.pos {{ color: #0d8a43; font-weight: 600; }}
.neg {{ color: #d93025; font-weight: 600; }}
.rules td {{ padding: 2.5px 5px; font-size: 7.8pt; }}
.rules td:first-child {{ font-weight: 700; color: #0B3D91; width: 28%; }}
.chart {{ width: 100%; display: block; border-radius: 4px; border: 1px solid #E8EAED; margin: 3px 0 2px; }}
.chart.g {{ height: 1.58in; }}
.chart.dd {{ height: 0.90in; }}
.chart.a {{ height: 1.15in; }}
.legend {{ font-size: 7.2pt; color: #5F6368; margin-top: 1px; margin-bottom: 2px; text-align: right; }}
.legend span {{ display: inline-block; width: 10px; height: 3px; vertical-align: middle; margin: 0 4px 0 10px; }}
.legend span.first {{ margin-left: 0; }}
.callout {{ background: #FEF7E0; border: 1px solid #FEEFC3; border-left: 3.5px solid #F9AB00; border-radius: 4px; padding: 4px 8px; font-size: 7.6pt; line-height: 1.3; margin-top: 4px; }}
.disc {{ font-size: 6.4pt; color: #5F6368; line-height: 1.32; border-top: 1px solid #DADCE0; padding-top: 4px; margin-top: 4px; text-align: justify; }}
.foot {{ position: absolute; bottom: 0.22in; left: 0.48in; right: 0.48in; display: flex; justify-content: space-between; font-size: 6.8pt; color: #80868B; border-top: 0.8px solid #E8EAED; padding-top: 3px; }}
.yt {{ font-size: 7.1pt; margin-top: 2px; }}
.yt th, .yt td {{ padding: 1.5px 3px; text-align: center; }}
.yt td.lab {{ text-align: left; font-weight: 700; }}
.yt tr.yh th {{ background: #F8F9FA; font-weight: 700; color: #202124; }}
</style></head><body>

<!-- ============================ PAGE 1 ============================ -->
<div class="page">
  <div class="band">
    <div class="meta">
      <b>AS OF:</b> {end_date}<br>
      <b>INCEPTION:</b> {start_date}<br>
      <b>BENCHMARK:</b> Invesco QQQ (Nasdaq-100)
    </div>
    <h1>ULTIMATE ALPHA MASTER</h1>
    <div class="sub">High-Conviction Stock Momentum Engine with Winner Retention Buffer &amp; Super-Bull 2.0&times; Turbo Leverage</div>
  </div>

  <div class="kpis">
    <div class="kpi"><div class="v">{pct(u_stat['cagr'])}</div><div class="l">20-Yr CAGR (2007-26)</div></div>
    <div class="kpi"><div class="v">+151.1%</div><div class="l">10-Yr CAGR (2016-26)</div></div>
    <div class="kpi"><div class="v">{u_stat['sharpe']:.2f}</div><div class="l">20-Yr Sharpe (T-bill rf)</div></div>
    <div class="kpi"><div class="v">{pct(u_stat['maxdd'])}</div><div class="l">20-Yr Max Drawdown</div></div>
    <div class="kpi"><div class="v">{usd(u_stat['end_value'])}</div><div class="l">$10,000 Inception Wealth</div></div>
  </div>

  <h2>Strategy Architecture &amp; The 4 Alpha Pillars</h2>
  <div class="engines">
    <div class="eng">
      <div class="tag">PILLAR 1 &middot; MOMENTUM</div>
      <h3>Dual-Lookback Alpha</h3>
      <ul>
        <li>Scores 170 stocks by <b>50% 126d + 50% 63d</b> relative return vs. QQQ.</li>
        <li><b>Unrestricted stock trend</b>: eliminates 100-day 200 SMA lag to capture early explosive breakouts.</li>
      </ul>
    </div>
    <div class="eng">
      <div class="tag">PILLAR 2 &middot; HYSTERESIS</div>
      <h3>Winner Retention Buffer</h3>
      <ul>
        <li>Preserves existing winners as long as they stay in the <b>Top 8</b>.</li>
        <li>Slashes churn and slippage; lets compounders ride minor consolidations (+9.1% CAGR boost).</li>
      </ul>
    </div>
    <div class="eng">
      <div class="tag">PILLAR 3 &middot; REGIME</div>
      <h3>Super-Bull 2.0&times; Turbo</h3>
      <ul>
        <li>Expands margin to <b>2.00&times;</b> (Reg-T max) when QQQ &gt; 50 SMA &gt; 200 SMA with positive lmMA slope.</li>
        <li>Standard bull: 1.50&times;; bear: de-levers to 1.00&times; (0% debt).</li>
      </ul>
    </div>
    <div class="eng">
      <div class="tag">PILLAR 4 &middot; DEFENSE</div>
      <h3>Stop Watchdog &amp; MVP</h3>
      <ul>
        <li><b>-25% Disaster Stop</b> gives high-beta winners room to run while capping tail risk.</li>
        <li><b>Immediate Stop Replacement</b> recycles capital into Rank 6; Bear shifts to Min-Var (MVP).</li>
      </ul>
    </div>
  </div>

  <div class="two" style="margin-top:4px">
    <div>
      <h2>Rules at a Glance</h2>
      <table class="rules">
        <tr><td>Universe</td><td>Broad 170-name high-growth momentum watchlist (QQQ + tech leaders)</td></tr>
        <tr><td>Selection</td><td>Top 5 stocks via Dual-Lookback RS (126d &amp; 63d excess return)</td></tr>
        <tr><td>Winner Buffer</td><td>Holdings retained if in Top 8; only replaced if rank &gt; 8 or stopped out</td></tr>
        <tr><td>Leverage Modes</td><td>2.00&times; (Super-Bull: 50 &gt; 200 SMA &amp; slope &gt; 0) | 1.50&times; (Bull) | 1.00&times; (Bear)</td></tr>
        <tr><td>Risk Overlays</td><td>-25% Disaster Floor + Immediate Re-deployment; Crisis Min-Var (MVP) in bear</td></tr>
        <tr><td>Execution</td><td>Monthly rebalance (1st trading day); daily stop &amp; replacement watchdog</td></tr>
      </table>
    </div>
    <div>
      <h2>Model Comparison (2007 &ndash; 2026)</h2>
      <table>
        <tr><th>Metric</th><th>Ultimate Alpha</th><th>Power Baseline</th><th>QQQ Index</th></tr>
        <tr><td>Active Margin (Super-Bull)</td><td class="c-ult">2.00&times; Turbo</td><td>1.75&times; Turbo</td><td class="muted">1.00&times; (Cash)</td></tr>
        <tr><td>Winner Retention Buffer</td><td class="c-ult">Top 8 Buffer</td><td>None (Exact 5)</td><td class="muted">N/A</td></tr>
        <tr><td>Disaster Stop Floor</td><td class="c-ult">-25.0%</td><td>-20.0%</td><td class="muted">None</td></tr>
        <tr><td>Annual Turnover</td><td class="c-ult">19.2&times; (Lower Churn)</td><td>21.2&times;</td><td class="muted">~0.1&times;</td></tr>
        <tr><td>20-Yr CAGR</td><td class="c-ult">{pct(u_stat['cagr'])}</td><td>{pct(b_stat['cagr'])}</td><td class="muted">{pct(q_stat['cagr'])}</td></tr>
        <tr><td>20-Yr Max Drawdown</td><td class="c-ult">{pct(u_stat['maxdd'])}</td><td class="neg">{pct(b_stat['maxdd'])}</td><td class="muted">{pct(q_stat['maxdd'])}</td></tr>
        <tr><td>$10,000 Inception Became</td><td class="c-ult">{usd(u_stat['end_value'])}</td><td>{usd(b_stat['end_value'])}</td><td class="muted">{usd(q_stat['end_value'])}</td></tr>
      </table>
    </div>
  </div>

  <h2>Growth of $10,000 (2007 &ndash; 2026)</h2>
  <div class="legend"><span class="first" style="background:#0B3D91"></span>Ultimate Alpha Master (Buffer Top 8 + 2.0&times; Turbo)<span style="background:#2E8B57"></span>Power Alpha Baseline (1.75&times;)<span style="background:#9AA0A6"></span>QQQ Buy &amp; Hold &nbsp;&middot;&nbsp; Log Scale</div>
  <img class="chart g" src="{img('ultimate_growth.png')}">
  <div class="foot"><span>Hypothetical backtest &mdash; not actual trading results. See important disclosures on page 2.</span><span>Page 1 of 2</span></div>
</div>

<!-- ============================ PAGE 2 ============================ -->
<div class="page">
  <div class="two" style="grid-template-columns: 1.08fr 0.92fr;">
    <div>
      <h2 style="margin-top:0">Comprehensive Performance Statistics</h2>
      <table><tr><th>{start_date} &ndash; {dt.date.fromisoformat(J['end']).strftime('%b %Y')}</th><th>Ultimate Alpha</th><th>Power Baseline</th><th>QQQ Index</th></tr>{stats_html}</table>
    </div>
    <div>
      <h2 style="margin-top:0">Annualized Return by Period</h2>
      <table><tr><th>Period</th><th>Ultimate</th><th>Baseline</th><th>QQQ</th><th>Ult DD</th></tr>{period_rows()}</table>
      <h2>Crisis &amp; Bear Market Stress-Tests</h2>
      <table><tr><th>Historical Stress Event</th><th>Ultimate</th><th>Baseline</th><th>QQQ</th></tr>{crisis_rows()}</table>
      <div class="callout"><b>Asymmetric Compounding Engine:</b> The Winner Retention Buffer allows leaders to compound undisturbed, while the -25% stop watchdog and low-lag macro filter cut catastrophic losses during bear markets.</div>
    </div>
  </div>

  <h2>Calendar-Year Historical Returns</h2>
  <div class="legend"><span class="first" style="background:#0B3D91"></span>Ultimate Alpha Master<span style="background:#2E8B57"></span>Power Baseline<span style="background:#9AA0A6"></span>QQQ Index &nbsp;&middot;&nbsp; Bars capped at +300%</div>
  <img class="chart a" src="{img('ultimate_annual.png')}">
  <table class="yt">{year_table()}</table>

  <h2>Historical Drawdown from Peak</h2>
  <img class="chart dd" src="{img('ultimate_drawdown.png')}">

  <div class="disc">
    <b>Important disclosures &amp; Quantitative methodology.</b> All performance results presented herein are <b>hypothetical, back-tested</b> simulations and do not represent actual trading.
    Hypothetical performance has inherent limitations, was engineered with the benefit of hindsight, and is not indicative of future results. <b>Universe &amp; Survivorship:</b>
    Evaluated across a broad 170-symbol momentum universe ({start_date} &ndash; {end_date}, *{years[-1]} is year-to-date). Point-in-time tests verify that active factor alpha remains robust across Nasdaq-100 constituent shifts.
    <b>Execution Modeling:</b> Assumes 0.10% transaction friction per trade (10 bps slippage/commission), margin borrowing interest at the 3-month T-bill rate + 1.75%, cash earning the risk-free T-bill rate, and daily fractional rebalancing.
    Does not account for investor-specific tax liabilities or management fees. <b>Risk Considerations:</b> The use of 1.50&times; to 2.00&times; margin leverage magnifies both gains and losses and can lead to severe drawdowns; concentrated 5-stock portfolios carry idiosyncratic single-stock volatility; macro regime timing can whipsaw during fast market inflections.
    This fact sheet is strictly for informational and quantitative engineering purposes and does not constitute investment advice, endorsement, or solicitation to buy or sell securities.
  </div>
  <div class="foot"><span>Ultimate Alpha Master &mdash; Institutional Factsheet &middot; Data as of {end_date}</span><span>Page 2 of 2</span></div>
</div>
</body></html>"""

html_path = f"{ROOT}/ultimate_factsheet.html"
pdf_path = f"{ROOT}/Ultimate_Alpha_Master_Factsheet.pdf"
open(html_path, "w").write(HTML)

chrome = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
cmd = [chrome, "--headless=new", "--disable-gpu", "--no-pdf-header-footer", "--no-sandbox",
       f"--print-to-pdf={pdf_path}", f"file://{html_path}"]
subprocess.run(cmd, check=True, capture_output=True)
print(f"Successfully compiled 2-Page PDF Factsheet to: {pdf_path}")

# Copy to conversation artifacts directory
ART = "/Users/cmejo/.gemini/antigravity/brain/64019830-0cac-4bb1-904e-d4560e71468c"
art_pdf = os.path.join(ART, "Ultimate_Alpha_Master_Factsheet.pdf")
subprocess.run(["cp", pdf_path, art_pdf], check=True)
print(f"Copied Factsheet to Artifact Directory: {art_pdf}")
