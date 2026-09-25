# Quantitative Swing Trading System (R)

A statistical swing trading system implemented in pure **R**, utilizing modernized technical indicator filters, **Linear Model Moving Averages (`lmMA`)**, **GARCH(1,1) Volatility Modeling**, and **ElasticNet Logistic Regression (`cv.glmnet`)** to forecast 5-day forward price direction.

---

## Key Features

1. **Linear Model Moving Average (`lmMA`)**: Replaces simple moving averages with rolling linear regressions (`TTR::rollSFM`) to decouple trend direction (instantaneous slope $\beta$) and trend level ($\alpha + \beta t$) with minimal lag.
2. **GARCH(1,1) Volatility Modeling**: Models time-varying conditional variance $\sigma_t$ and standardized return shocks ($\epsilon_t / \sigma_t$) via quasi-maximum likelihood estimation (`tseries::garch`).
3. **Macro Market Regime Gate (`QQQ`)**: Top-down market trend filter. When `QQQ` is above its 50-day `lmMA` with positive slope, full Risk-On allocation (5 positions) is allowed. In Defensive mode, positions are automatically constrained to 2 and minimum entry probability is raised to $P(\text{Up}) \ge 65\%$.
4. **Earnings Date Blackout Filter**: Queries upcoming earnings dates via calendar events. Stocks reporting within 7 trading days (~10 calendar days) are blocked from new purchases to avoid binary gap risk.
5. **Multi-Timeframe Trend Synergy**: Aggregates daily data to weekly bars to calculate weekly `lmMA` slope ($\beta_{weekly}$). Daily buy signals are disqualified if fighting a secular weekly downtrend.
6. **Automated Rolling Walk-Forward Retraining**: Re-estimates ElasticNet models over a rolling 500-trading-day window (~2 years) to adapt to changing volatility regimes without regime decay.
7. **Dynamic Risk Control**: Computes dynamic $-2\sigma$ Stop-Loss and $+3\sigma$ Take-Profit price levels with a 1.50 : 1 reward-to-risk ratio.
8. **Native macOS & GitHub Automation**: Automated weekday background runner (`launchd`) at 4:30 PM ET pushing desktop notifications and GitHub Actions workflow delivering email alerts to your inbox.

---

## Directory Structure

```
.
├── R/
│   ├── 01_data_loader.R       # Data retrieval (Yahoo Finance & cache fallback)
│   ├── 02_trend_lmMA.R        # Rolling Linear Model Moving Average (lmMA)
│   ├── 03_volatility_garch.R  # GARCH(1,1) volatility & standardized residuals
│   ├── 04_feature_pipeline.R  # Feature engineering & forward target calculation
│   ├── 05_logistic_model.R    # Regularized logistic regression & classification
│   └── 06_swing_backtest.R    # Swing backtest simulator, metrics & trade logger
├── main.R                     # Full backtesting & diagnostic plotting pipeline
├── daily_signal.R             # Multi-asset live scanner & order ticket generator
├── monthly_review.R           # Monthly performance & health evaluation script
├── backtest_past_2_months.R   # 2-month out-of-sample backtest vs S&P 500 (SPY)
├── backtest_all_symbols.R     # Watchlist-wide backtest across all symbols in symbols.txt
├── run_daily.sh               # Automation execution script with desktop alerts
├── install_automation.sh      # macOS launchd background scheduler installer
├── uninstall_automation.sh    # macOS launchd uninstaller
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
Rscript daily_signal.R --capital=10000 --max_pos=5
```

### Automated Daily Scheduling (macOS)
To schedule automatic execution every weekday at 4:30 PM ET:
```bash
./install_automation.sh
```

---

## License
MIT License. For educational and research purposes.
