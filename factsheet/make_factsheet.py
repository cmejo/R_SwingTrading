"""
factsheet/make_factsheet.py
Builds a 2-page Letter PDF factsheet from factsheet/data/* and factsheet/img/*
via headless Chrome.  Usage: python3 factsheet/make_factsheet.py
"""
import csv, json, os, subprocess, base64, datetime as dt
from collections import defaultdict

ROOT = os.path.dirname(os.path.abspath(__file__))
J = json.load(open(f"{ROOT}/data/factsheet.json"))
S = J["stats"]; INIT = J["init"]

# Calendar-year returns from monthly file
yr = defaultdict(lambda: {"opt6": 1.0, "opt3": 1.0, "qqq": 1.0})
for row in csv.DictReader(open(f"{ROOT}/data/monthly.csv")):
    y = row["month"][:4]
    for k in ("opt6", "opt3", "qqq"):
        yr[y][k] *= 1 + float(row[k]) / 100
years = sorted(yr)

def img(name):
    return "data:image/png;base64," + base64.b64encode(open(f"{ROOT}/img/{name}", "rb").read()).decode()
def pct(x, d=1, sign=False):
    return (f"{x*100:+.{d}f}%" if sign else f"{x*100:.{d}f}%")
def usd(x):
    return f"${x/1e6:,.2f}M" if x >= 1e6 else f"${x:,.0f}"
def cls(x):
    return "neg" if x < 0 else "pos"
def d2(s):
    try: return dt.date.fromisoformat(s).strftime("%b %Y")
    except Exception: return s

end_date = dt.date.fromisoformat(J["end"]).strftime("%B %-d, %Y")
start_date = dt.date.fromisoformat(J["start"]).strftime("%B %Y")
o6, o3, qq = S["opt6"], S["opt3"], S["qqq"]

stat_rows = [
    ("Ending value of $10,000", usd(o6["end_value"]), usd(o3["end_value"]), usd(qq["end_value"])),
    ("Cumulative return", f"+{o6['total']*100:,.0f}%", f"+{o3['total']*100:,.0f}%", f"+{qq['total']*100:,.0f}%"),
    ("Annualized return (CAGR)", pct(o6["cagr"]), pct(o3["cagr"]), pct(qq["cagr"])),
    ("Annualized volatility", pct(o6["vol"]), pct(o3["vol"]), pct(qq["vol"])),
    ("Sharpe ratio", f"{o6['sharpe']:.2f}", f"{o3['sharpe']:.2f}", f"{qq['sharpe']:.2f}"),
    ("Sortino ratio", f"{o6['sortino']:.2f}", f"{o3['sortino']:.2f}", f"{qq['sortino']:.2f}"),
    ("Maximum drawdown", pct(o6["maxdd"]), pct(o3["maxdd"]), pct(qq["maxdd"])),
    ("Max drawdown period", f"{d2(o6['dd_peak'])} &ndash; {d2(o6['dd_trough'])}", f"{d2(o3['dd_peak'])} &ndash; {d2(o3['dd_trough'])}", f"{d2(qq['dd_peak'])} &ndash; {d2(qq['dd_trough'])}"),
    ("Recovered to prior peak", d2(o6["dd_recovery"]), d2(o3["dd_recovery"]), d2(qq["dd_recovery"])),
    ("Calmar ratio (CAGR / MaxDD)", f"{o6['calmar']:.2f}", f"{o3['calmar']:.2f}", f"{qq['calmar']:.2f}"),
    ("Best calendar year", f"{pct(o6['best_year'],0,True)} ({o6['best_year_lbl']})", f"{pct(o3['best_year'],0,True)} ({o3['best_year_lbl']})", f"{pct(qq['best_year'],0,True)} ({qq['best_year_lbl']})"),
    ("Worst calendar year", f"{pct(o6['worst_year'],0,True)} ({o6['worst_year_lbl']})", f"{pct(o3['worst_year'],0,True)} ({o3['worst_year_lbl']})", f"{pct(qq['worst_year'],0,True)} ({qq['worst_year_lbl']})"),
    ("Best / worst month", f"{pct(o6['best_month'],0,True)} / {pct(o6['worst_month'],0,True)}", f"{pct(o3['best_month'],0,True)} / {pct(o3['worst_month'],0,True)}", f"{pct(qq['best_month'],0,True)} / {pct(qq['worst_month'],0,True)}"),
    ("Positive months", pct(o6["pct_pos_months"], 0), pct(o3["pct_pos_months"], 0), pct(qq["pct_pos_months"], 0)),
]
stats_html = "\n".join(f"<tr><td>{a}</td><td class='c6'>{b}</td><td>{c}</td><td class='muted'>{e}</td></tr>" for a, b, c, e in stat_rows)

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
        for k, lab in (("opt6", "Option 6"), ("opt3", "Option 3"), ("qqq", "QQQ")):
            rows.append(f"<tr><td class='lab'>{lab}</td>" + "".join(
                f"<td class='{cls(round((yr[y][k]-1)*100))}'>{round((yr[y][k]-1)*100):+d}%</td>" for y in grp) + "</tr>")
    return "\n".join(rows)

HTML = f"""<!DOCTYPE html><html><head><meta charset="utf-8"><style>
@page {{ size: Letter; margin: 0; }}
* {{ box-sizing: border-box; }}
body {{ margin: 0; font-family: -apple-system, "Helvetica Neue", Helvetica, Arial, sans-serif; color: #202124; font-size: 8.6pt; line-height: 1.38; }}
.page {{ width: 8.5in; height: 11in; padding: 0.42in 0.5in 0.38in; position: relative; overflow: hidden; page-break-after: always; }}
.page:last-child {{ page-break-after: auto; }}
.band {{ background: #0B3D91; color: #fff; margin: -0.42in -0.5in 0.16in; padding: 0.26in 0.5in 0.2in; }}
.band h1 {{ margin: 0; font-size: 20pt; letter-spacing: -0.3px; font-weight: 700; }}
.band .sub {{ font-size: 9pt; max-width: 4.9in; opacity: 0.88; margin-top: 2px; }}
.band .meta {{ float: right; text-align: right; font-size: 8pt; opacity: 0.9; line-height: 1.5; margin-top: 2px; }}
h2 {{ font-size: 10.5pt; color: #0B3D91; margin: 0.11in 0 0.05in; padding-bottom: 3px; border-bottom: 1.5px solid #0B3D91; text-transform: uppercase; letter-spacing: 0.4px; }}
h3 {{ font-size: 9pt; margin: 0 0 3px; color: #0B3D91; }}
p {{ margin: 0 0 5px; }}
.kpis {{ display: grid; grid-template-columns: repeat(5, 1fr); gap: 7px; margin: 0.04in 0 0.04in; }}
.kpi {{ background: #F1F4FA; border-radius: 6px; padding: 7px 9px; border-left: 3px solid #0B3D91; }}
.kpi .v {{ font-size: 15pt; font-weight: 700; color: #0B3D91; line-height: 1.1; }}
.kpi .l {{ font-size: 7.3pt; color: #5F6368; }}
.kpi.q {{ border-left-color: #9AA0A6; }} .kpi.q .v {{ color: #5F6368; }}
.engines {{ display: grid; grid-template-columns: repeat(3, 1fr); gap: 8px; }}
.eng {{ border: 1px solid #DADCE0; border-radius: 6px; padding: 7px 9px; }}
.eng .tag {{ display: inline-block; background: #0B3D91; color: #fff; font-size: 7pt; font-weight: 700; border-radius: 3px; padding: 1px 6px; margin-bottom: 3px; }}
.eng ul {{ margin: 2px 0 0; padding-left: 13px; }} .eng li {{ margin-bottom: 1.5px; }}
.two {{ display: grid; grid-template-columns: 1fr 1fr; gap: 14px; }}
table {{ border-collapse: collapse; width: 100%; font-size: 8pt; }}
th, td {{ padding: 2.1px 5px; text-align: right; border-bottom: 1px solid #EEF0F2; white-space: nowrap; }}
th {{ color: #5F6368; font-weight: 600; font-size: 7.5pt; border-bottom: 1px solid #BDC1C6; }}
td:first-child, th:first-child {{ text-align: left; }}
.c6 {{ font-weight: 700; color: #0B3D91; }} .muted {{ color: #5F6368; }}
.pos {{ color: #137333; }} .neg {{ color: #B3261E; }}
.rules td {{ white-space: normal; text-align: left; vertical-align: top; }} .rules td:first-child {{ width: 26%; font-weight: 600; color: #3C4043; }}
.chart {{ width: 100%; display: block; }} .g {{ height: 2.38in; }} .a {{ height: 1.62in; }} .dd {{ height: 1.22in; }}
.legend {{ font-size: 7.5pt; color: #5F6368; margin: 1px 0 2px; }}
.legend span {{ display: inline-block; width: 14px; height: 3px; vertical-align: middle; margin: 0 4px 0 10px; }}
.legend span.first {{ margin-left: 0; }}
.yt {{ font-size: 7.3pt; }} .yt th, .yt td {{ padding: 2px 3px; text-align: center; }} .yt td.lab {{ text-align: left; font-weight: 600; color: #3C4043; }}
.yt tr.yh th {{ border-top: 6px solid #fff; }}
.disc {{ font-size: 6.5pt; color: #5F6368; line-height: 1.32; border-top: 1px solid #DADCE0; padding-top: 4px; margin-top: 5px; }}
.disc b {{ color: #3C4043; }}
.foot {{ position: absolute; bottom: 0.14in; left: 0.5in; right: 0.5in; font-size: 6.8pt; color: #9AA0A6; display: flex; justify-content: space-between; }}
.callout {{ background: #FFF8E1; border-left: 3px solid #F9AB00; padding: 5px 8px; font-size: 7.8pt; border-radius: 4px; margin-top: 6px; }}
</style></head><body>

<!-- ============================ PAGE 1 ============================ -->
<div class="page">
  <div class="band">
    <div class="meta">Backtest period: {start_date} &ndash; {end_date}<br>Initial investment: $10,000<br>Rebalance: monthly</div>
    <h1>Triple-Engine Momentum Strategy</h1>
    <div class="sub">Leveraged Nasdaq-100 core &bull; relative-strength stock satellite &bull; volatility-targeted gold overlay</div>
  </div>

  <div class="kpis">
    <div class="kpi"><div class="v">{usd(o6['end_value'])}</div><div class="l">$10,000 grew to (Option 6)</div></div>
    <div class="kpi"><div class="v">{pct(o6['cagr'])}</div><div class="l">Annualized return (CAGR)</div></div>
    <div class="kpi"><div class="v">{pct(o6['maxdd'])}</div><div class="l">Maximum drawdown</div></div>
    <div class="kpi"><div class="v">{o6['sharpe']:.2f}</div><div class="l">Sharpe ratio</div></div>
    <div class="kpi q"><div class="v">{pct(qq['cagr'])}</div><div class="l">QQQ buy &amp; hold CAGR ({usd(qq['end_value'])})</div></div>
  </div>

  <h2>Strategy Overview</h2>
  <p>The Triple-Engine Momentum Strategy is a rules-based, long-only U.S. equity strategy that seeks to compound faster than the Nasdaq-100 by
  combining <b>leveraged index exposure</b> with a <b>concentrated sleeve of the strongest-trending stocks</b>, while using <b>trend and volatility
  rules to step aside in bear markets</b>. Every decision is mechanical and made once a month from price data only; there is no discretionary
  stock picking or forecasting. When conditions deteriorate, capital rotates into gold (GLD) and borrowed margin is fully repaid.</p>

  <h2>How It Works &mdash; Three Engines</h2>
  <div class="engines">
    <div class="eng"><div class="tag">ENGINE 1 &middot; CORE 30%</div><h3>Leveraged index trend</h3>
      <ul><li>Holds <b>QLD</b> (2&times; Nasdaq-100) while QQQ trades above its 200-day moving average.</li>
      <li>Below the 200-day average, the core is sold and moved to <b>gold</b>.</li>
      <li>Captures broad market upside with built-in bear-market exits.</li></ul></div>
    <div class="eng"><div class="tag">ENGINE 2 &middot; SATELLITE 70%</div><h3>Relative-strength leaders</h3>
      <ul><li>Ranks Nasdaq-100 stocks by a 50/50 blend of <b>6-month and 3-month</b> return vs. QQQ.</li>
      <li>Buys the <b>top {J['top_n']}</b> (equal weight) that are above their own 200-day average.</li>
      <li><b>&minus;20% stop</b> per stock; stop proceeds move into QLD until the next rebalance.</li></ul></div>
    <div class="eng"><div class="tag">ENGINE 3 &middot; RISK OVERLAY</div><h3>Volatility &amp; leverage control</h3>
      <ul><li>Targets <b>22% volatility</b>: when QQQ's 20-day volatility is higher, exposure shrinks and the excess goes to <b>GLD</b>.</li>
      <li><b>1.5&times; margin</b> only in confirmed uptrends; <b>0% margin</b> when QQQ &lt; 200-day average.</li></ul></div>
  </div>

  <div class="two" style="margin-top:2px">
    <div>
      <h2>Rules at a Glance</h2>
      <table class="rules">
        <tr><td>Universe</td><td>Nasdaq-100 members at each rebalance date (historical, point-in-time)</td></tr>
        <tr><td>Signal</td><td>Composite relative strength = 0.5 &times; 126-day + 0.5 &times; 63-day excess return vs. QQQ</td></tr>
        <tr><td>Trend filters</td><td>Stock &ge; own 200-day SMA; QQQ &ge; 200-day SMA for core &amp; margin</td></tr>
        <tr><td>Sizing</td><td>Core 30% / satellites 70% &times; min(1, 22% &divide; QQQ 20-day vol) &times; leverage</td></tr>
        <tr><td>Defensive asset</td><td>Gold (GLD) for unused risk budget and bear-market core</td></tr>
        <tr><td>Rebalance</td><td>Monthly (first trading day); stops checked daily</td></tr>
      </table>
    </div>
    <div>
      <h2>Two Versions</h2>
      <table>
        <tr><th></th><th>Option 6</th><th>Option 3</th></tr>
        <tr><td>Account type</td><td>Margin</td><td>Cash / IRA</td></tr>
        <tr><td>Bull-market leverage</td><td>1.5&times;</td><td>1.0&times; (none)</td></tr>
        <tr><td>Peak gross exposure</td><td>~150%</td><td>~100%</td></tr>
        <tr><td>CAGR</td><td class="c6">{pct(o6['cagr'])}</td><td>{pct(o3['cagr'])}</td></tr>
        <tr><td>Maximum drawdown</td><td class="neg">{pct(o6['maxdd'])}</td><td class="neg">{pct(o3['maxdd'])}</td></tr>
        <tr><td>$10,000 became</td><td class="c6">{usd(o6['end_value'])}</td><td>{usd(o3['end_value'])}</td></tr>
      </table>
    </div>
  </div>

  <h2>Growth of $10,000</h2>
  <div class="legend"><span class="first" style="background:#0B3D91"></span>Option 6 (1.5&times; margin)<span style="background:#2E8B57"></span>Option 3 (cash / IRA)<span style="background:#9AA0A6"></span>QQQ buy &amp; hold &nbsp;&middot;&nbsp; log scale</div>
  <img class="chart g" src="{img('growth.png')}">
  <div class="foot"><span>Hypothetical backtest &mdash; not actual trading results. See important disclosures on page 2.</span><span>Page 1 of 2</span></div>
</div>

<!-- ============================ PAGE 2 ============================ -->
<div class="page">
  <div class="two" style="grid-template-columns: 1.08fr 0.92fr;">
    <div>
      <h2 style="margin-top:0">Performance Statistics</h2>
      <table><tr><th>{start_date} &ndash; {dt.date.fromisoformat(J['end']).strftime('%b %Y')}</th><th>Option 6</th><th>Option 3</th><th>QQQ</th></tr>{stats_html}</table>
    </div>
    <div>
      <h2 style="margin-top:0">Annualized Return by Period</h2>
      <table><tr><th>Period</th><th>Opt 6</th><th>Opt 3</th><th>QQQ</th><th>Opt 6 DD</th></tr>{period_rows()}</table>
      <h2>Crisis Periods (total return)</h2>
      <table><tr><th>Event</th><th>Opt 6</th><th>Opt 3</th><th>QQQ</th></tr>{crisis_rows()}</table>
      <div class="callout"><b>Expect deep drawdowns.</b> Option 6 lost about two-thirds of its value in 2008 and took until {d2(o6['dd_recovery'])} to recover. Leverage magnifies losses as well as gains.</div>
    </div>
  </div>

  <h2>Calendar-Year Returns</h2>
  <div class="legend"><span class="first" style="background:#0B3D91"></span>Option 6<span style="background:#2E8B57"></span>Option 3<span style="background:#9AA0A6"></span>QQQ &nbsp;&middot;&nbsp; bars capped at +300%</div>
  <img class="chart a" src="{img('annual.png')}">
  <table class="yt">{year_table()}</table>

  <h2>Drawdown from Prior Peak</h2>
  <img class="chart dd" src="{img('drawdown.png')}">

  <div class="disc">
    <b>Important disclosures.</b> All results are <b>hypothetical, back-tested</b> simulations, not actual trading, and were prepared with the benefit of hindsight;
    no investor achieved these returns. Past or simulated performance does not guarantee future results. <b>Methodology:</b> {start_date} &ndash; {end_date}
    (*{years[-1]} is year-to-date). Satellite stocks are drawn only from <b>actual Nasdaq-100 constituents at each rebalance date</b> (source: jmccarrell/n100tickers) to limit
    survivorship bias; however, ~67 former members that were later delisted (mostly acquired) lack free price history and are excluded, so a small residual bias remains.
    Earlier internal tests using a hand-picked list of today's winners overstated returns by ~20 percentage points per year; those figures are not shown.
    Prices are dividend-adjusted closes. Assumes 0.10% transaction cost per trade, margin interest at the 3-month T-bill rate + 1.75%, cash earning the T-bill rate,
    fractional shares, and no taxes, slippage beyond costs, or management fees. QLD history before its 2006 launch is not used. <b>Risks:</b> leverage and leveraged ETFs
    can produce losses exceeding those of the underlying index, margin calls, and decay in volatile markets; concentrated three-stock positions carry significant
    single-company risk; trend and volatility rules can whipsaw. The live scanner may be configured with a different stock list or position count than shown here.
    This document is for informational purposes only and is not investment advice or an offer to buy or sell any security.
  </div>
  <div class="foot"><span>Triple-Engine Momentum Strategy &mdash; Factsheet &middot; Data as of {end_date}</span><span>Page 2 of 2</span></div>
</div>
</body></html>"""

html_path = f"{ROOT}/factsheet.html"; pdf_path = f"{ROOT}/Triple_Engine_Momentum_Factsheet.pdf"
open(html_path, "w").write(HTML)
chrome = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
subprocess.run([chrome, "--headless=new", "--disable-gpu", "--no-pdf-header-footer", "--no-sandbox",
                f"--print-to-pdf={pdf_path}", f"file://{html_path}"], check=True, capture_output=True)
print("Wrote", pdf_path)
