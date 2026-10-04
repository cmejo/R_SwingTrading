# Quantitative Swing Trading System (R)

A statistical swing trading system implemented in pure **R**, utilizing modernized technical indicator filters, **Linear Model Moving Averages (`lmMA`)**, **GARCH(1,1) Volatility Modeling**, and **ElasticNet Logistic Regression (`cv.glmnet`)** to forecast 5-day forward price direction.

---

## Key Features

1. **Linear Model Moving Average (`lmMA`)**: Replaces simple moving averages with rolling linear regressions (`TTR::rollSFM`) to decouple trend direction (instantaneous slope $\beta$) and trend level ($\alpha + \beta t$) with minimal lag.
2. **GARCH(1,1) Volatility Modeling**: Models time-varying conditional variance $\sigma_t$ and standardized return shocks ($\epsilon_t / \sigma_t$) via quasi-maximum likelihood estimation (`tseries::garch`).
3. **15-Feature Pipeline with Momentum, Relative Strength & Volume**: Expands feature space to 15 orthogonal predictors including 20-day excess return vs. S&P 500 (`SPY`), RSI(14), normalized MACD Histogram, Bollinger %B, 20-day Volume Ratio, and standardized On-Balance Volume slope to isolate high-conviction institutional leaders before breakout confirmation.
4. **Dynamic VIX Volatility Regime Switcher**: Queries CBOE Volatility Index (`^VIX`) to toggle between `NORMAL` (<20), `ELEVATED` (20-28), and `CRISIS` (>28) market states. Automatically throttles maximum position slots (5 $\rightarrow$ 3 $\rightarrow$ 1) and scales the Ralph Vince Safe $f$ multiplier (0.50 $\rightarrow$ 0.30 $\rightarrow$ 0.15) while tightening probability thresholds.
5. **Sector & Cluster Concentration Defense**: Enforces strict portfolio diversification by capping exposure at a maximum of 2 active positions per sector (e.g. Semiconductors, Software, Hardware), preventing catastrophic sector-specific drawdowns.
6. **Multi-Tier Profit Bracket Exits with ATR Chandelier Trailing Stop**:
   - **Tier 1 (50% scale-out @ $+1.5R$)**: Locks in initial gains and automatically ratchets the remaining stop-loss to Breakeven (entry price) for a zero-downside trade.
   - **Tier 2 (50% runner @ $+3.0R$)**: Employs an open-ended **ATR Chandelier Trailing Stop** ($\text{Highest High} - 2.5 \times \text{ATR}_{14}$) to capture multi-week trend momentum runs without prematurely capping compounding upside.
7. **Portfolio Heat Cap & Account Drawdown Circuit Breaker**:
   - Enforces a hard ceiling of **$\le 5.0\%$ total portfolio dollars-at-risk** across all open positions.
   - Triggers an automatic **Drawdown Circuit Breaker** if account equity drops $\ge 4.0\%$ below peak equity, cutting Safe $f$ by 50% to preserve capital.
8. **Automated Multi-Broker Execution & Two-Way Sync Bridge**: Python & R CLI bridges supporting automated bracket staging, live transmission, and two-way portfolio reconciliation (`--sync`) across **Interactive Brokers (IBKR Pro)**, **Charles Schwab Trader API**, and **Robinhood**.
9. **Real-Time Mobile Push Alerts (Discord & Telegram Webhooks)**: Automatically pushes Monday 14:00 entry tickets and Friday 15:30 weekend hold reviews directly to your smartphone via lightweight HTTP webhooks (`R/send_alert.R`).
10. **Ralph Vince Leverage Space Model (LSPM)**: Geometric Holding Period Return ($\text{GHPR}$) optimization across joint scenario returns, finding the optimal leverage vector $\mathbf{f}^*$ scaled by dynamic Safe $f$ to maximize long-term geometric compounding with slot-level diversification caps.
11. **Macro Market Regime Gate (`QQQ`) & Earnings Blackout Filter**: Top-down trend filter requiring QQQ above its 50-day `lmMA` with positive slope, plus an automatic 7-day pre-earnings blackout to eliminate binary gap risk.

---

## Directory Structure

```
.
├── R/
│   ├── 01_data_loader.R       # Data retrieval (Yahoo Finance & cache fallback)
│   ├── 02_trend_lmMA.R        # Rolling Linear Model Moving Average (lmMA)
│   ├── 03_volatility_garch.R  # GARCH(1,1) volatility & standardized residuals
│   ├── 04_feature_pipeline.R  # 15-feature engineering & forward target calculation
│   ├── 05_logistic_model.R    # Regularized logistic regression with blocked folds
│   ├── 06_swing_backtest.R    # Swing backtest simulator, metrics & trade logger
│   ├── 07_leverage_space.R    # Ralph Vince Leverage Space Model (Optimal f / Safe f)
│   ├── portfolio_manager.R    # Active portfolio state tracking, P&L sync & exit checks
│   └── send_alert.R           # Mobile push alerts dispatcher (Discord & Telegram Webhooks)
├── main.R                     # Full backtesting & diagnostic plotting pipeline
├── daily_signal.R             # Multi-asset live scanner & order ticket generator
├── execute_broker.py          # Python execution bridge (IBKR & Charles Schwab API)
├── execute_ibkr.py            # Dedicated IBKR Pro OCA bracket order bridge
├── execute_robinhood.py       # Robinhood automated order execution bridge
├── poll_robinhood_exits.py    # Background exit poller for Robinhood stops/targets
├── execute_orders.R           # R CLI runner for automated broker execution
├── trade_manager.R            # CLI portfolio manager (record fills, exits & P&L)
├── monthly_review.R           # Monthly performance & health evaluation script
├── backtest_extended.R        # 1-Year (252d) and 18-Month (378d) walk-forward backtest
├── backtest_broad_universe.R  # 170-stock broad universe walk-forward portfolio simulation
├── backtest_past_2_months.R   # 2-month out-of-sample backtest vs S&P 500 (SPY)
├── backtest_all_symbols.R     # Watchlist-wide backtest across all symbols in symbols.txt
├── test_leverage_space.R      # Verification test for Leverage Space sizing
├── auto_trade_ibkr.sh         # Turnkey automated daily pipeline for IBKR Pro
├── auto_trade_robinhood.sh    # Turnkey automated daily pipeline for Robinhood
├── auto_trade_schwab.sh       # Turnkey automated daily pipeline for Charles Schwab
├── run_daily.sh               # Automation execution script with desktop alerts
├── install_automation.sh      # macOS launchd background scheduler installer
├── uninstall_automation.sh    # macOS launchd uninstaller
├── portfolio.json             # Persistent account state & active positions
├── TRADING_PLAYBOOK.md        # Comprehensive execution playbook & operating rules
├── symbols.txt                # Active trading watchlist
└── output/                    # Exported diagnostic & performance charts
```

---

## Getting Started

### Prerequisites
Requires R (>= 4.0) with packages:
```R
install.packages(c("xts", "zoo", "quantmod", "TTR", "glmnet", "tseries", "jsonlite"))
```
For Interactive Brokers automation (optional):
```bash
pip install ib_insync
```

### Running the Backtests
To execute the complete strategy backtest and generate diagnostic charts:
```bash
Rscript main.R
```

To run the out-of-sample backtest over the **past 2 months benchmarked against the S&P 500 (`SPY`)**:
```bash
Rscript backtest_past_2_months.R
```

To backtest the strategy against **all stocks in `symbols.txt`**:
```bash
Rscript backtest_all_symbols.R
```

### Running the Live Scanner
To scan your watchlist and generate actionable order tickets for a $10,000 account across 5 positions:
```bash
Rscript daily_signal.R --capital=10000 --max_pos=5 --fractional=TRUE
```

### Automated Broker Execution (IBKR & Schwab)
To preview and validate staged bracket orders generated by `daily_signal.R`:
```bash
# Preview Interactive Brokers (IBKR) bracket payloads
Rscript execute_orders.R --broker=ibkr --dry_run=TRUE

# Preview Charles Schwab Trader API bracket payloads
Rscript execute_orders.R --broker=schwab --dry_run=TRUE
```

To transmit orders live to an active Interactive Brokers TWS or IB Gateway instance (port 7497 for paper, 7496 for live):
```bash
Rscript execute_orders.R --broker=ibkr --dry_run=FALSE --port=7497
```

### Automated Broker Portfolio Reconciliation (`--sync`)
To synchronize your active positions, executed fills, and real-time cash balance directly from Interactive Brokers into `portfolio.json`:
```bash
# Preview portfolio reconciliation diff
Rscript execute_orders.R --broker=ibkr --sync --dry_run=TRUE

# Execute live synchronization with TWS / IB Gateway
Rscript execute_orders.R --broker=ibkr --sync --dry_run=FALSE --port=7497
```

### Mobile Push Alerts (Discord & Telegram Webhooks)
Configure environment variables in `.env` (or shell profile):
```bash
DISCORD_WEBHOOK_URL="https://discord.com/api/webhooks/..."
TELEGRAM_BOT_TOKEN="123456789:ABCDEF..."
TELEGRAM_CHAT_ID="987654321"
```
Test notification dispatch:
```bash
Rscript R/send_alert.R --test
```

### Portfolio Management (CLI)
View active portfolio status, open positions, unrealized P&L, and closed trade logs:
```bash
Rscript trade_manager.R --status
```
Record a trade fill:
```bash
Rscript trade_manager.R --buy=AMD:9.034:629.26:578.52:705.37
```
Record a trade exit:
```bash
Rscript trade_manager.R --sell=AMD:705.37:TAKE_PROFIT
```

### Automated Scheduling (macOS)
To schedule automatic scans at **Monday 2:00 PM (14:00) EDT** (weekly entries) and **Friday 3:30 PM (15:30) EDT** (mandatory weekend risk exit):
```bash
./install_automation.sh
```

---

## License
MIT License. For educational and research purposes.
