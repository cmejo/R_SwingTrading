# $10,000 Swing Trading Execution Playbook

This playbook outlines the exact operational procedure for deploying and managing the **$10,000** swing trading portfolio.

---

## 1. Operating Rules & Core Principles

1. **Monday Afternoon Entry (No Monday Open Jumping)**:
   - Rather than jumping into volatile morning spreads or relying on stale Friday data, fresh weekly signals are scanned on **Monday at 14:00 EDT (2:00 PM)**.
   - Buys are executed on **Monday afternoon between 14:00 and 15:00 EDT** once morning price action has settled and the intraday trend is confirmed.
2. **Conditional Weekend Holding (Pruning Weakness, Letting Winners Run)**:
   - **Why hold winners?** Captures Monday morning gap-ups, avoids cutting mid-week entries short before their 3–5 day horizon matures, and eliminates round-trip friction.
   - **Qualification Test**: Every **Friday at 15:30 EDT**, active holdings are automatically evaluated:
     1. **Macro Regime**: QQQ must be **BULLISH (Risk-On)**.
     2. **Model Probability**: $P(\text{Up}) \ge 55.0\%$.
     3. **Weekly Trend Synergy**: Weekly linear regression slope $\beta_{weekly} > 0$.
     4. **Earnings Safety**: No earnings reports within the next 7 trading days.
   - **Hold Decision**: Positions passing all 4 criteria are approved to hold into next week (**`HOLD OVER WEEKEND`**). If a position is in profit by $\ge 1R$ (+3%), the stop-loss is raised to **Breakeven (Entry Price)** for a zero-downside hold.
   - **Defensive Exit**: Any position failing these criteria is flagged for liquidation (**`SELL BEFORE 16:00 FRIDAY`**).
3. **Fractional Share Precision**:
   - The Ralph Vince Leverage Space Model (`Safe f = 0.50`) sizes trades to 3 decimal places (e.g. `0.518 shares`).
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
| **Friday 15:30 EDT** | Weekend Hold Review | Scanner separates active holdings: **Hold Winners** vs **Sell Weakness**. |
| **Friday 15:45–16:00 EDT**| Selective Close-Out | Liquidate only disqualified positions. Ratchet winners' stops to Breakeven. |

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
| **MSFT** | BUY | $3,363.20 (33.6%) | **6.718 shares** | $499.33 (-0.25%) | $502.48 (+0.38%) |
| **SNDK** | BUY | $940.16 (9.4%) | **0.518 shares** | $1692.98 (-6.80%) | $2001.95 (+10.21%) |
| **AMD** | BUY | $5,696.64 (57.0%) | **9.269 shares** | $555.76 (-9.58%) | $702.88 (+14.36%) |

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
   Rscript trade_manager.R --buy=SNDK:0.518:1816.57:1692.98:2001.95
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

### Step 4: Friday 15:30 EDT — Conditional Weekend Hold Review

At **15:30 EDT on Friday**, inspect the **FRIDAY 15:30 EVALUATION** section in `LATEST_TICKET.txt`:

```
========================================================================================
              FRIDAY 15:30 EVALUATION: CONDITIONAL WEEKEND HOLD REVIEW        
========================================================================================
 [APPROVED TO HOLD OVER WEEKEND] (1 Positions with Strong Momentum):
   ✓ AMD: HOLD OVER WEEKEND (Momentum Intact) | Current: $629.26 | P&L: +4.88%

 [DEFENSIVE WEEKEND EXITS] (1 Positions to Liquidate Today):
   ⚠️ NVDA: SELL BEFORE 16:00 (Weekend Risk: P(Up) 39.8% < 55%) | P&L: -2.36% -> SELL AT MARKET BEFORE 16:00 EDT CLOSE!
```

1. **If Marked `HOLD OVER WEEKEND`**:
   - Do nothing, or raise your stop-loss in your broker to **Breakeven (Entry Price)** if suggested.
   - Let the trade compound into Monday.
2. **If Marked `DEFENSIVE WEEKEND EXIT`**:
   - Sell at market before **16:00 EDT** to eliminate weekend headline risk.
   - Record the sale:
     ```bash
     Rscript trade_manager.R --sell=NVDA:224.58:WEEKEND_DEFENSIVE_EXIT
     ```
