"""
scratch/update_symbols_n100.py
Regenerate symbols_n100.txt from the current Nasdaq-100 membership
(github.com/jmccarrell/n100tickers - same source as the point-in-time backtest).
Usage: uv run --with strictyaml python scratch/update_symbols_n100.py
"""
import datetime as dt, os, subprocess, sys

REPO = "/tmp/n100tickers"
if os.path.isdir(REPO):
    subprocess.run(["git", "-C", REPO, "pull", "-q"], check=False)
else:
    subprocess.run(["git", "clone", "-q", "--depth", "1", "https://github.com/jmccarrell/n100tickers.git", REPO], check=True)
sys.path.insert(0, f"{REPO}/src")
from nasdaq_100_ticker_history import tickers_as_of

today = dt.date.today()
tickers = sorted(tickers_as_of(today.year, today.month, today.day))
with open("symbols_n100.txt", "w") as f:
    f.write(f"# Nasdaq-100 constituents as of {today}\n")
    f.write("# Source: github.com/jmccarrell/n100tickers (same point-in-time source as the backtest)\n")
    f.write("# Regenerate: uv run --with strictyaml python scratch/update_symbols_n100.py\n")
    f.write("\n".join(tickers) + "\n")
print(f"Wrote {len(tickers)} tickers to symbols_n100.txt")
