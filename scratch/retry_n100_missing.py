"""scratch/retry_n100_missing.py - retry missing tickers with rename map + backoff, merge into prices csv."""
import time, pandas as pd, yfinance as yf
OUT = "data/n100_pit"
rep = pd.read_csv(f"{OUT}/download_report.csv")
prices = pd.read_csv(f"{OUT}/prices_adjclose.csv", index_col=0, parse_dates=True)
RENAME = {"ERTS": "EA", "HANS": "MNST", "RIMM": "BB", "KFT": "MDLZ", "UAUA": "UAL", "IACI": "IAC",
          "DISCA": "WBD", "DISCK": "WBD", "WFMI": "WFM", "SUNW": "JAVA", "QVCA": "QVCGA", "LINTA": "QVCGA",
          "QRTEA": "QVCGA", "LMCA": "FWONA", "LMCK": "FWONK", "STRZA": "STRZ", "BBBY": "BBBYQ", "YHOO": "AABA"}
miss = rep[rep.status != "OK"].ticker.tolist()
fixed = []
for t in miss:
    ys = RENAME.get(t, t).replace(".", "-")
    for attempt in range(3):
        try:
            df = yf.download(ys, start="2005-01-01", end="2026-10-31", auto_adjust=True, progress=False, threads=False)
            if df is not None and not df.empty:
                c = df["Close"]; c = c.iloc[:, 0] if isinstance(c, pd.DataFrame) else c; c = c.dropna()
                if len(c) >= 60:
                    c.index = pd.to_datetime(c.index).tz_localize(None)
                    prices = prices.join(c.rename(t), how="outer")
                    rep.loc[rep.ticker == t, ["yahoo_symbol", "status", "first", "last", "n"]] = [ys, "OK", str(c.index[0].date()), str(c.index[-1].date()), len(c)]
                    fixed.append(f"{t}->{ys}")
            break
        except Exception:
            time.sleep(2)
    time.sleep(0.3)
prices.sort_index().to_csv(f"{OUT}/prices_adjclose.csv")
rep.to_csv(f"{OUT}/download_report.csv", index=False)
print("Recovered:", " ".join(fixed))
print(rep.status.value_counts().to_string())
print("Still missing:", " ".join(rep.loc[rep.status != "OK", "ticker"]))
