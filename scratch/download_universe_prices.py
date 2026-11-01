"""scratch/download_universe_prices.py - adjusted closes for every ticker in the five symbol lists."""
import re, pandas as pd, yfinance as yf
FILES = ["symbols.txt", "symbols_n100.txt", "symbols_broad.txt", "symbols_300.txt", "symbols_all2.txt"]
tick = set()
for fn in FILES:
    for line in open(fn):
        if line.strip().startswith("#"): continue
        tick.update(t for t in re.split(r"[,\s]+", line.strip().upper()) if t)
tick = sorted(tick)
print("Unique tickers:", len(tick))
frames = []
for i in range(0, len(tick), 50):
    chunk = tick[i:i + 50]
    df = yf.download([t.replace(".", "-") for t in chunk], start="2004-01-01", end="2026-10-31",
                     auto_adjust=True, progress=False, threads=True, group_by="column")
    c = df["Close"] if isinstance(df.columns, pd.MultiIndex) else df[["Close"]].rename(columns={"Close": chunk[0]})
    c.columns = [x.replace("-", ".") for x in c.columns]
    frames.append(c)
    print(f"  {min(i + 50, len(tick))}/{len(tick)}")
P = pd.concat(frames, axis=1)
P.index = pd.to_datetime(P.index).tz_localize(None); P.index.name = "Date"
P = P.loc[:, P.notna().sum() >= 60]
P.to_csv("data/universe_prices_adjclose.csv")
print("Saved", P.shape[1], "tickers with data;", "missing:", " ".join(sorted(set(tick) - set(P.columns))))
