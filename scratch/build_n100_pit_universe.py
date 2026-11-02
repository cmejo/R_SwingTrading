"""
scratch/build_n100_pit_universe.py
Build point-in-time Nasdaq-100 membership (monthly) from jmccarrell/n100tickers
and download adjusted price history for every ticker that was ever a member.

Outputs:
  data/n100_pit/membership_monthly.csv   (month_start, ticker)
  data/n100_pit/prices_adjclose.csv      (wide: Date x ticker, adjusted close)
  data/n100_pit/download_report.csv      (ticker, yahoo_symbol, status, first, last, n)
"""
import sys, os, glob, datetime as dt
import pandas as pd
import yfinance as yf

N100_SRC = "/tmp/n100tickers/src"
OUT = "data/n100_pit"
os.makedirs(OUT, exist_ok=True)


def tickers_as_of_from_yaml(src_dir=N100_SRC):
    """Same semantics as n100tickers.tickers_as_of, reading its YAML files as data.

    Works on any Python 3 without strictyaml. BaseLoader keeps every scalar a string,
    so tickers such as ON are not coerced to booleans and change dates stay ISO text
    (lexicographic order == chronological order for YYYY-MM-DD).
    """
    import yaml
    files = glob.glob(os.path.join(src_dir, "nasdaq_100_ticker_history", "n100-ticker-changes-*.yaml"))
    if not files:
        sys.exit(f"cannot find n100tickers YAML files under {src_dir}")
    by_year = {}
    for f in files:
        with open(f, encoding="utf-8") as fh:
            d = yaml.load(fh, Loader=yaml.BaseLoader)
        by_year[int(d["year"])] = d

    def tickers_as_of(year, month=1, day=1):
        d = by_year[year]
        result = set(d["tickers_on_Jan_1"])
        q = dt.date(year, month, day).isoformat()
        for when in sorted((d.get("changes") or {}).keys()):
            if when <= q:
                for op, operands in d["changes"][when].items():
                    if op == "union":
                        result |= set(operands)
                    elif op == "difference":
                        result -= set(operands)
                    else:
                        raise NotImplementedError(f"unknown set operation {op} on {when}")
        return frozenset(result)

    return tickers_as_of


def load_tickers_as_of():
    """Prefer the n100tickers package (needs Python >= 3.11 and strictyaml); otherwise
    fall back to tickers_as_of_from_yaml()."""
    sys.path.insert(0, N100_SRC)
    try:
        from nasdaq_100_ticker_history import tickers_as_of
        return tickers_as_of
    except ImportError as e:
        print(f"  n100tickers package unavailable ({e}); using direct YAML reader")
        return tickers_as_of_from_yaml()


# 1. Monthly point-in-time membership, 2007-01 .. 2026-10. The CSV is committed to the
#    repo, so it is reused when present and only rebuilt (from n100tickers) when missing.
MEM_CSV = f"{OUT}/membership_monthly.csv"
if os.path.exists(MEM_CSV):
    mem = pd.read_csv(MEM_CSV)
    print(f"  reusing committed {MEM_CSV}")
else:
    tickers_as_of = load_tickers_as_of()
    rows = []
    for y in range(2007, 2027):
        for m in range(1, 13):
            if (y, m) > (2026, 10):
                break
            for t in sorted(tickers_as_of(y, m, 1)):
                rows.append((f"{y}-{m:02d}-01", t))
    mem = pd.DataFrame(rows, columns=["month_start", "ticker"])
    mem.to_csv(MEM_CSV, index=False)
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
