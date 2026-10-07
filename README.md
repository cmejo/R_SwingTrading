# Quantitative Swing Trading System (R & Python)

A systematic, institutional-grade swing trading system implemented in **pure R** with an automated **Python multi-broker execution bridge**. The engine integrates modernized technical indicators, **Linear Model Moving Averages (`lmMA`)**, **GARCH(1,1) Volatility Modeling**, **ElasticNet Regularized Logistic Regression (`cv.glmnet`)**, and **Ralph Vince Leverage Space Portfolio Models (LSPM)** to forecast directional swing edges and manage asymmetric risk.

---

## System Architecture & Key Enhancements

### 1. Quantitative Core & Machine Learning
- **Linear Model Moving Average (`lmMA`)**: Replaces lagged moving averages with rolling linear regressions (`TTR::rollSFM`), isolating trend slope ($\beta$) and instantaneous trend level ($\alpha + \beta t$) with minimal lag.
- **GARCH(1,1) Volatility Modeling**: Models time-varying conditional variance $\sigma_t$ and standardized return shocks ($\epsilon_t / \sigma_t$) via quasi-maximum likelihood estimation (`tseries::garch`).
- **15-Feature Institutional Pipeline**: Extracts orthogonal technical and statistical predictors including 20-day excess return vs. S&P 500 (`SPY`), RSI(14), normalized MACD Histogram, Bollinger %B, 20-day Volume Ratio, and standardized On-Balance Volume slope.
- **ATR Overextension Filter**: Rejects breakout entries if the current price is extended $> 2.5 \times \text{ATR}_{14}$ above its 20-day moving average, preventing buying at blow-off cycle tops.
- **Asymmetric Downside Volatility**: Features separate upward vs. downward realized return variance to penalize erratic downside chop during consolidation.

### 2. Multi-Tier Asymmetric Risk Management
- **3-Tier Profit Bracket Structure**:
  - **Tier 1 (50% scale-out @ $+1.5R$)**: Secures immediate profit and automatically moves the stop-loss to Breakeven (entry price) for a guaranteed risk-free position.
  - **Tier 2 (25% scale-out @ $+3.0R$)**: Captures the primary swing extension.
  - **Tier 3 (25% runner)**: Open-ended runner trailing on an **ATR Chandelier Stop** ($\text{Highest High} - 2.5 \times \text{ATR}_{14}$) to capture multi-week momentum without prematurely clipping compounding gains.
- **Day 4 Profit Ratchet**: If an open trade reaches $\ge 4$ hold days with $\ge +1.0R$ open profit, the Tier 1 target ratchets down to $+1.1R$ to lock in gains ahead of the holding expiration.
- **Dynamic VIX Volatility Regime Filter**: Dynamically monitors CBOE Volatility Index (`^VIX`):
  - `NORMAL` ($<20$): 5 positions, full Safe $f$.
  - `ELEVATED` ($20-28$): Throttled to 3 positions, Safe $f \times 0.60$.
  - `CRISIS` ($>28$): Throttled to 1 position, Safe $f \times 0.30$, probability threshold tightened.
- **Sector & Cluster Concentration Defense**: Enforces a strict cap of $\le 2$ concurrent positions per sector to eliminate correlated sector drawdowns.
- **Portfolio Heat Cap & Account Drawdown Circuit Breaker**:
  - Hard ceiling of $\le 5.0\%$ total portfolio dollars-at-risk across all open positions.
  - Automated circuit breaker cuts Safe $f$ by 50% if account equity drops $\ge 4.0\%$ below peak equity.

### 3. Automated Broker Bridges & Turnkey Execution
- **Charles Schwab Trader API**: Native REST OAuth2 client supporting live `FIRST_TRIGGERS_OCO` bracket orders, automated OAuth token refreshes, position reconciliation, and dynamic auto-compounding.
- **Interactive Brokers (IB Pro)**: Python `ib_insync` integration supporting OCA bracket orders and continuous stop monitoring (`execute_ibkr.py`, `ibkr_manage_stops.py`).
- **Robinhood Bridge**: Direct bracket simulation, limit execution, and background polling daemon (`execute_robinhood.py`, `poll_robinhood_exits.py`).
- **Dynamic Auto-Compounding**: [`auto_trade_schwab.sh`](file:///Volumes/2TB.ssd/_a%20Development/swingtrading/auto_trade_schwab.sh) automatically queries real-time account liquidation equity on startup to size trades, apply margin leverage, and park excess cash in `QQQ` without requiring manual file edits.
- **Mobile Push Alerts**: Lightweight HTTP dispatchers for Discord and Telegram webhooks (`R/send_alert.R`) alerting entry tickets, stop adjustments, and Friday risk reviews.

---

## Watchlist Universes

The strategy supports multiple curated universe files:
- **`symbols_broad.txt`** (311 symbols): **Top Recommended Universe**. High-liquidity mid/large caps with deep sector diversity across Tech, Healthcare, Industrials, Energy, and Consumer Discretionary.
- **`symbols_all2.txt`** (390 symbols): Unified comprehensive universe preserving categorized section headers (`# Large Cap Leaders`, `# Momentum Growth`, etc.) for sector-based filtering.
- **`symbols_300.txt`** (308 symbols): Broader mid-cap and high-beta growth universe.
- **`symbols.txt`** (20 symbols): Concentrated mega-cap focus watchlist.

---

## Empirical Performance Benchmarks

### 5-Year Terminal Wealth Comparison ($10,000 Starting Capital)
Backtested across 1,260 trading bars (2021 – 2026):

| Strategy / Universe | Hold Days | Terminal Wealth | Total Return | Ann. Return | Sharpe Ratio | Max Drawdown | Win Rate |
| :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: |
| **Broad (`symbols_broad.txt`)** | **7d** | **$450,913.31** | **+4,409.13%** | **+114.33%** | **3.72** | **10.83%** | **58.80%** |
| Broad (`symbols_broad.txt`) | 10d | \$427,000.80 | +4,170.01% | +112.00% | 3.79 | 10.43% | 60.14% |
| Broad (`symbols_broad.txt`) | 5d | \$371,428.50 | +3,614.28% | +106.17% | 3.34 | 14.08% | 58.15% |
| All2 (`symbols_all2.txt`) | 10d | \$343,958.04 | +3,339.58% | +102.91% | 3.86 | 15.19% | 60.76% |
| All2 (`symbols_all2.txt`) | 5d | \$316,207.27 | +3,062.07% | +99.52% | 3.36 | 16.61% | 60.40% |
| 300+ (`symbols_300.txt`) | 5d | \$279,110.58 | +2,691.11% | +94.61% | 3.33 | 16.54% | 59.29% |
| Focus (`symbols.txt`) | 5d | \$58,534.29 | +485.34% | +42.43% | 1.48 | 40.39% | 55.01% |
| **QQQ (Nasdaq 100)** | Buy & Hold | **$19,967.00** | **+99.67%** | **+14.83%** | 0.69 | 35.12% | -- |
| **VTI (Total Market)** | Buy & Hold | **$16,508.00** | **+65.08%** | **+10.55%** | 0.54 | 25.61% | -- |

---

## Directory Structure

```
.
├── R/
│   ├── 01_data_loader.R       # Market data ingestion (Yahoo Finance API with local cache)
│   ├── 02_trend_lmMA.R        # Rolling Linear Model Moving Average (lmMA) trend engine
│   ├── 03_volatility_garch.R  # GARCH(1,1) volatility & standardized residual estimator
│   ├── 04_feature_pipeline.R  # 15-feature engineering, ATR filters, & forward target matrix
│   ├── 05_logistic_model.R    # Regularized ElasticNet logistic model with blocked cross-validation
│   ├── 06_swing_backtest.R    # 3-tier bracket simulator, Chandelier stops, & trade metrics
│   ├── 07_leverage_space.R    # Ralph Vince Leverage Space Model (GHPR / Safe f optimizer)
│   ├── portfolio_manager.R    # Portfolio accounting, dynamic capital sync, & active risk checks
│   └── send_alert.R           # Discord & Telegram mobile webhook notification dispatcher
├── auto_trade_schwab.sh       # Turnkey automated daily pipeline with Charles Schwab API & dynamic capital
├── auto_trade_ibkr.sh         # Turnkey automated daily pipeline for Interactive Brokers
├── auto_trade_robinhood.sh    # Turnkey automated daily pipeline for Robinhood
├── daily_signal.R             # Multi-asset live scanner, ranking engine, & bracket ticket builder
├── execute_broker.py          # Unified Python broker execution bridge (Schwab REST & IBKR)
├── execute_ibkr.py            # Dedicated IBKR Pro OCA bracket order bridge
├── execute_robinhood.py       # Dedicated Robinhood order bridge
├── ibkr_manage_stops.py       # Active position stop management daemon for IBKR
├── poll_robinhood_exits.py    # Background exit poller for Robinhood brackets
├── execute_orders.R           # R CLI wrapper for multi-broker execution
├── trade_manager.R            # CLI portfolio manager (manual fills, exits, reconciliation)
├── monthly_review.R           # Monthly performance scorecard & health evaluation script
├── run_daily.sh               # Local automation wrapper with native macOS desktop notifications
├── install_automation.sh      # macOS launchd background scheduler installer
├── uninstall_automation.sh    # macOS launchd scheduler uninstaller
├── symbols_broad.txt          # Recommended 311-stock diversified universe
├── symbols_all2.txt           # 390-stock master universe with category headers
├── symbols_300.txt            # 308-stock mid/large-cap growth universe
├── symbols.txt                # 20-stock concentrated mega-cap watchlist
├── portfolio.json             # Persistent account equity state and open trade log
├── TRADING_PLAYBOOK.md        # Complete operational trading manual & rules
└── output/                    # Visual analytics, scorecards, and backtest comparison charts
```

---

## Quick Start Guide

### 1. Prerequisites & Environment Setup
Requires **R (>= 4.0)** and **Python (>= 3.9)**:
```bash
# Install required R packages
Rscript -e 'install.packages(c("xts", "zoo", "quantmod", "TTR", "glmnet", "tseries", "jsonlite", "pROC"))'

# Install Python requirements
pip install requests python-dotenv
# Optional: for Interactive Brokers
pip install ib_insync
```

Configure your environment credentials in [`.env`](file:///Volumes/2TB.ssd/_a%20Development/swingtrading/.env):
```bash
# Charles Schwab Trader API (Required for Schwab execution)
SCHWAB_APP_KEY="your_schwab_app_key"
SCHWAB_SECRET="your_schwab_secret"
SCHWAB_REDIRECT_URI="https://127.0.0.1"

# Mobile Push Alerts (Optional)
DISCORD_WEBHOOK_URL="https://discord.com/api/webhooks/..."
TELEGRAM_BOT_TOKEN="123456789:ABCDEF..."
TELEGRAM_CHAT_ID="987654321"
```

---

### 2. Live Scanning & Automated Schwab Trading

#### Authenticate Schwab Developer App (One-Time Setup)
```bash
python3 execute_broker.py --broker=schwab --auth
```
Follow the terminal instructions to authorize via your browser. Tokens will be cached in `schwab_token.json` and refreshed automatically thereafter.

#### Run Daily Scan in Dry Run Mode (Preview Tickets)
```bash
./auto_trade_schwab.sh --dry_run=true
```

#### Run Live Execution (Automated Bracket Orders)
```bash
./auto_trade_schwab.sh --dry_run=false
```
*Note: The script automatically queries your live Schwab account liquidation value, sizes positions dynamically, and routes any unallocated capital into `QQQ`.*

---

### 3. Backtesting & Research Tools

#### Multi-Universe & Holding Period Comparison
```bash
# Test 5, 7, 10, 15, and 20-day holding periods across all 4 universes
Rscript test_holding_periods_backtest.R

# Direct comparison of symbols_broad.txt vs symbols_all2.txt vs QQQ & VTI
Rscript compare_broad_all2_vs_vti_qqq.R

# Full 5-universe comprehensive multi-horizon backtest (2M, 12M, 24M, 60M)
Rscript run_comprehensive_5universe_backtest.R
```

#### Scan Watchlists via CLI
```bash
# Scan Broad Universe with custom capital and leverage
Rscript daily_signal.R --symbols_file=symbols_broad.txt --capital=10000 --leverage=1.5 --max_pos=5
```

---

## Operating Schedule & Trading Protocol

| Day / Window | Action | Description |
| :--- | :--- | :--- |
| **Mon – Thu (14:00 – 15:45 EDT)** | **Scan & Buy Window** | Primary execution window. Scans watchlist, verifies Macro Gate, and submits bracket orders at ~15:30 EDT. |
| **Friday (15:30 EDT)** | **Zero Weekend Risk Review** | No new entries are permitted. Positions lacking a $+1.0R$ cushion are closed flat before the 16:00 close to avoid gap risk. |
| **Market Close Daily** | **Account Reconciliation** | Reconciles open positions and cash balances via `--sync`. |

For the complete operating rules, risk limits, and weekend hold criteria, refer to [`TRADING_PLAYBOOK.md`](file:///Volumes/2TB.ssd/_a%20Development/swingtrading/TRADING_PLAYBOOK.md).

---

## License
MIT License. For educational and quantitative research purposes.
