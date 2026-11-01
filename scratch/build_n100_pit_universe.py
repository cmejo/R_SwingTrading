"""
scratch/build_n100_pit_universe.py
Build point-in-time Nasdaq-100 membership (monthly) from jmccarrell/n100tickers
and download adjusted price history for every ticker that was ever a member.

Outputs:
  data/n100_pit/membership_monthly.csv   (month_start, ticker)
  data/n100_pit/prices_adjclose.csv      (wide: Date x ticker, adjusted close)
  data/n100_pit/download_report.csv      (ticker, yahoo_symbol, status, first, last, n)
"""
import sys, os, datetime as dt
import pandas as pd
import yfinance as yf

sys.path.insert(0, "/tmp/n100tickers/src")
from nasdaq_100_ticker_history import tickers_as_of

OUT = "data/n100_pit"
os.makedirs(OUT, exist_ok=True)

# 1. Monthly point-in-time membership, 2007-01 .. 2026-10
rows = []
for y in range(2007, 2027):
    for m in range(1, 13):
        if (y, m) > (2026, 10):
            break
        for t in tickers_as_of(y, m, 1):
            rows.append((f"{y}-{m:02d}-01", t))
mem = pd.DataFrame(rows, columns=["month_start", "ticker"])
mem.to_csv(f"{OUT}/membership_monthly.csv", index=False)
all_tickers = sorted(mem.ticker.unique())
print(f"Monthly membership rows: {len(mem)} | unique tickers ever in index: {len(all_tickers)}")

# 2. Known renames: index ticker -> current Yahoo symbol (same company, continuous history)
RENAME = {
    "FB": "META", "ERTS": "EA", "GOOG": "GOOG", "BRK.B": "BRK-B",
    "QCOM": "QCOM", "SYMC": "GEN", "NLOK": "GEN", "MYL": "VTRS",
    "CELG": None, "ESRX": None,  # acquired; leave to yahoo attempt below
    "DISCA": "WBD", "DISCK": "WBD", "VIAB": "PARA", "VIAC": "PARA",
    "ATVI": "ATVI", "PCLN": "BKNG", "TFCF": "FOXA", "TFCFA": "FOXA",
    "FOXA": "FOXA", "FOX": "FOX", "LBTYA": "LBTYA", "SIRI": "SIRI",
    "MXIM": "MXIM", "KRFT": "KHC", "BBBY": "BBBYQ", "CTRP": "TCOM",
    "WFM": "WFM", "NTES": "NTES", "SNDK": "SNDK", "JDSU": "VIAV",
    "LLTC": "LLTC", "ALTR": "ALTR", "LINTA": "QRTEA", "QVCA": "QRTEA",
    "MAT": "MAT", "HOLX": "HOLX", "TRIP": "TRIP", "FLEX": "FLEX",
    "FLIR": "FLIR", "GRMN": "GRMN", "XLNX": "XLNX", "BIDU": "BIDU",
    "ADSK": "ADSK", "SPLS": "SPLS", "RYAAY": "RYAAY", "WCRX": "WCRX",
    "JOYG": "JOYG", "HSIC": "HSIC", "MCHP": "MCHP", "NIHD": "NIHD",
    "STX": "STX", "SBAC": "SBAC", "APOL": "APEI", "LBTYK": "LBTYK",
}

def yahoo_symbol(t):
    s = RENAME.get(t, t)
    if s is None:
        s = t
    return s.replace(".", "-")

# 3. Download
START, END = "2005-01-01", "2026-10-31"
series, report = {}, []
for i, t in enumerate(all_tickers):
    ys = yahoo_symbol(t)
    try:
        df = yf.download(ys, start=START, end=END, auto_adjust=True, progress=False, threads=False)
        if df is None or df.empty:
            report.append((t, ys, "MISSING", "", "", 0)); continue
        c = df["Close"]
        if isinstance(c, pd.DataFrame):
            c = c.iloc[:, 0]
        c = c.dropna()
        if len(c) < 60:
            report.append((t, ys, "TOO_SHORT", "", "", len(c))); continue
        series[t] = c
        report.append((t, ys, "OK", c.index[0].date(), c.index[-1].date(), len(c)))
    except Exception as e:
        report.append((t, ys, f"ERROR", "", "", 0))
    if (i + 1) % 50 == 0:
        print(f"  downloaded {i+1}/{len(all_tickers)}")

prices = pd.DataFrame(series).sort_index()
prices.index.name = "Date"
prices.to_csv(f"{OUT}/prices_adjclose.csv")
rep = pd.DataFrame(report, columns=["ticker", "yahoo_symbol", "status", "first", "last", "n"])
rep.to_csv(f"{OUT}/download_report.csv", index=False)
print(rep.status.value_counts().to_string())
print("Missing:", " ".join(rep.loc[rep.status != "OK", "ticker"]))
