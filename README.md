# Quantitative Swing Trading System (R)

A statistical swing trading system implemented in pure **R**, utilizing modernized technical indicator filters, **Linear Model Moving Averages (`lmMA`)**, **GARCH(1,1) Volatility Modeling**, and **ElasticNet Logistic Regression (`cv.glmnet`)** to forecast 5-day forward price direction.

---

## Key Features

1. **Linear Model Moving Average (`lmMA`)**: Replaces simple moving averages with rolling linear regressions (`TTR::rollSFM`) to decouple trend direction (instantaneous slope $\beta$) and trend level ($\alpha + \beta t$) with minimal lag.
2. **GARCH(1,1) Volatility Modeling**: Models time-varying conditional variance $\sigma_t$ and standardized return shocks ($\epsilon_t / \sigma_t$) via quasi-maximum likelihood estimation (`tseries::garch`).
3. **Machine Learning Classifier**: Fits an ElasticNet regularized logistic regression ($\alpha = 0.5$) with a noise-filtering "Zone of Indifference" ($42\% < P(\text{Up}) < 58\%$).
4. **Multi-Stock Portfolio Scanner**: Reads [`symbols.txt`](symbols.txt), models each stock independently, ranks opportunities by probability $P(\text{Up})$, and allocates capital across top candidates.
5. **Dynamic Risk Control**: Computes dynamic $-2\sigma$ Stop-Loss and $+3\sigma$ Take-Profit price levels with a 1.50 : 1 reward-to-risk ratio.
6. **Native macOS Automation**: Automated weekday background runner (`launchd`) at 4:30 PM ET pushing desktop notifications and updating order tickets.

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
install.packages(c("xts", "zoo", "quantmod", "TTR", "glmnet", "tseries"))
```

### Running the Backtest
To execute the complete pipeline and generate publication-quality diagnostic charts:
```bash
Rscript main.R
```

### Running the Live Scanner
To scan your watchlist and generate actionable order tickets for a $10,000 account:
```bash
Rscript daily_signal.R --capital=10000 --max_pos=2
```

### Automated Daily Scheduling (macOS)
To schedule automatic execution every weekday at 4:30 PM ET:
```bash
./install_automation.sh
```

---

## License
MIT License. For educational and research purposes.
