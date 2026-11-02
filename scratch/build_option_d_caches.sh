#!/usr/bin/env bash
# ==============================================================================
# scratch/build_option_d_caches.sh
# One-time build of the gitignored price caches used by
# scratch/backtest_option_d_pit.R (and every pit_feature_search.R-based study):
#   data/n100_pit/{membership_monthly,prices_adjclose,download_report}.csv
#   data/universe_prices_adjclose.csv
#   data/data_historical/data_{qqq,qld,gld_full,^irx}.rds
# Needs network access to GitHub (n100tickers) and Yahoo Finance.
# Usage (repo root):  bash scratch/build_option_d_caches.sh
# ==============================================================================
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/.."

python3 -c "import pandas, yfinance, yaml" 2>/dev/null || python3 -m pip install --quiet --user pandas yfinance pyyaml
Rscript -e 'if (!requireNamespace("quantmod", quietly = TRUE)) install.packages("quantmod", repos = "https://cloud.r-project.org")'

# Membership is committed to the repo; n100tickers is only needed to rebuild it.
# Its package needs Python >= 3.11; build_n100_pit_universe.py falls back to reading
# its YAML directly on older Pythons.
if [ ! -f data/n100_pit/membership_monthly.csv ] && [ ! -d /tmp/n100tickers ]; then
  git clone --depth 1 https://github.com/jmccarrell/n100tickers /tmp/n100tickers
fi

echo "[1/3] Point-in-time Nasdaq-100 membership + prices (every ticker ever in the index)"
[ -f data/n100_pit/prices_adjclose.csv ] && echo "  keep data/n100_pit/prices_adjclose.csv" \
  || python3 scratch/build_n100_pit_universe.py

echo "[2/3] Static symbol-list prices (needed by the loader; also the 'broad' comparison)"
[ -f data/universe_prices_adjclose.csv ] && echo "  keep data/universe_prices_adjclose.csv" \
  || python3 scratch/download_universe_prices.py

echo "[3/3] Benchmark and macro series (QQQ, QLD, GLD_FULL, ^IRX)"
Rscript scratch/fetch_core_series.R

echo "Done. Next:  Rscript scratch/backtest_option_d_pit.R pit_n100 --broker=schwab"
