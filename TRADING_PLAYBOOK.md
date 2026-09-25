# $10,000 Swing Trading Execution Playbook

This playbook outlines the exact operational procedure for deploying and managing the **$10,000** swing trading portfolio.

---

## 1. Operating Rules & Core Principles

1. **Monday Afternoon Entry (No Monday Open Jumping)**:
   - Rather than jumping into volatile morning spreads or relying on stale Friday data, fresh weekly signals are scanned on **Monday at 14:00 EDT (2:00 PM)**.
   - Buys are executed on **Monday afternoon between 14:00 and 15:00 EDT** once morning price action has settled and the intraday trend is confirmed.
2. **Strict "Zero Weekend Exposure" Rule**:
   - **No positions are ever held over the weekend.**
   - All open trades must be closed on **Friday before 16:00 EDT** (market close).
   - An automated scan fires every **Friday at 15:30 EDT** to alert you to liquidate any remaining open positions.
3. **Fractional Share Precision**:
   - The Ralph Vince Leverage Space Model (`Safe f = 0.50`) sizes trades to 3 decimal places (e.g. `0.517 shares`).
   - 100% of available cash is deployed efficiently without leaving hundreds of dollars idle.
4. **Persistent Portfolio State**:
   - Real positions, entry prices, days held, and unrealized P&L are tracked in `portfolio.json`.
   - The CLI helper `trade_manager.R` makes recording fills and exits effortless.

---

## 2. Weekly Execution Timeline

| Day & Time | Event | Action Required |
| :--- | :--- | :--- |
| **Monday 14:00 EDT** | Automated Weekly Scan | Check email / notification banner / `LATEST_TICKET.txt`. |
| **Monday 14:15–15:00 EDT** | Market Entry | Place Buy orders for recommended shares + attach OCO brackets. |
| **Monday Afternoon** | Record Fills | Run `Rscript trade_manager.R --buy=...` to update `portfolio.json`. |
| **Tue–Thu 14:00 EDT** | Daily Monitoring Scan | Automated scanner checks stop/target hits and calculates live P&L. |
| **Friday 15:30 EDT** | Weekend Exit Alert | Close all active positions at market before 16:00 close. |
| **Friday 16:00 EDT** | Weekend Reset | Run `Rscript trade_manager.R --sell=...` or `--reset`. 100% Cash over weekend. |

---

## 3. Step-by-Step Execution Guide

### Step 1: Monday 14:00 EDT — Run / Inspect Weekly Scan

At 2:00 PM EDT on Monday, the scanner runs automatically (or manually via `./run_daily.sh`):

1. **Verify Macro Regime (QQQ)**:
   - Ensure the Macro Gate is **BULLISH (Risk-On)**.
   - If Bearish: Remain **100% in CASH**.
2. **Review Order Tickets in `LATEST_TICKET.txt`**:
   The Vince model calculates the exact capital allocation for each vacant slot:

| Symbol | Action | Allocation ($ / %) | Exact Shares | GTC Stop-Loss | GTC Take-Profit |
| :--- | :--- | :--- | :--- | :--- | :--- |
| **MSFT** | BUY | $3,363.18 (33.6%) | **6.718 shares** | $499.33 (-0.25%) | $502.48 (+0.38%) |
| **SNDK** | BUY | $939.81 (9.4%) | **0.517 shares** | $1692.98 (-6.80%) | $2001.95 (+10.21%) |
| **AMD** | BUY | $5,697.01 (57.0%) | **9.269 shares** | $555.76 (-9.58%) | $702.88 (+14.36%) |

*(All cash is deployed down to pennies; no stranded dollars).*

---

### Step 2: Monday 14:15–15:00 EDT — Broker Execution & Bracket Setup

In your brokerage account (IBKR, Schwab, Fidelity, etc.):

1. **Submit Buy Orders**:
   - Buy the specified shares (fractional or rounded) for each candidate.
2. **Attach One-Cancels-Other (OCO) Bracket Orders**:
   - Immediately following execution, submit an OCO bracket:
     - **Stop-Loss (GTC)**: Set trigger to the ticket's `GTC Stop-Loss`.
     - **Take-Profit (GTC Limit)**: Set limit to the ticket's `GTC Take-Profit`.
3. **Sync Portfolio State**:
   Record your executions in terminal:
   ```bash
   Rscript trade_manager.R --buy=MSFT:6.718:500.59:499.33:502.48
   Rscript trade_manager.R --buy=SNDK:0.517:1816.57:1692.98:2001.95
   Rscript trade_manager.R --buy=AMD:9.269:614.61:555.76:702.88
   ```

---

### Step 3: Tuesday through Thursday — Hands-Off Risk Management

1. **Intraday**:
   - OCO bracket orders protect downside and lock in upside automatically.
2. **Daily 14:00 EDT Check**:
   - The scanner checks live prices against your active positions:
     ```bash
     Rscript trade_manager.R --status
     ```
   - If a target or stop was filled, record the exit:
     ```bash
     Rscript trade_manager.R --sell=AMD:702.88:TAKE_PROFIT
     ```
   - Capital immediately returns to available cash for new opportunities.

---

### Step 4: Friday 15:30 EDT — Mandatory Weekend Risk Close-Out

> [!IMPORTANT]
> **Zero Weekend Exposure Rule**
> Gaps over the weekend cannot be controlled with stop-loss orders. At **15:30 EDT on Friday**, the system triggers a weekend exit alert.

1. **Check Friday Notification**:
   - If any positions remain open, close them at market price before **16:00 EDT**.
2. **Record Exits in State Tracker**:
   ```bash
   Rscript trade_manager.R --sell=MSFT:503.20:WEEKEND_EXIT
   ```
3. **Enjoy the Weekend**:
   - Your account rests **100% in CASH / Treasury yield** over Saturday and Sunday.
   - On Monday at 14:00 EDT, the system automatically begins the cycle anew.
