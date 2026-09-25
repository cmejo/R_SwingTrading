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
3. **Benchmark Relative Strength (RS vs. SPY) Filter**:
   - Every candidate is benchmarked against the S&P 500 (`SPY`) over a rolling 20-day horizon ($RS_{20}$).
   - **Leader Rule**: Candidates lagging the benchmark ($RS_{20} < 0$) are disqualified from new entries (`RS_LAGGER`), ensuring the portfolio exclusively rotates into high-momentum market leaders.
4. **Multi-Tier Profit Bracket Exits with ATR Chandelier Trailing Stop**:
   - Rather than an all-or-nothing exit, every position executes in two synchronized tiers:
     - **Tier 1 (50% scale-out @ $+1.5R$)**: Captures the first statistical swing move, locking in guaranteed portfolio profit.
     - **Breakeven Stop Ratchet**: The moment Tier 1 fills, the remaining stop-loss is immediately raised to entry price (Breakeven), rendering the position completely risk-free.
     - **Tier 2 (50% runner)**: Employs a dynamic **ATR Chandelier Trailing Stop** ($\text{Highest High} - 2.5 \times \text{ATR}_{14}$), allowing runners to trail upward and capture open-ended $+5R$ to $+10R$ compounding moves.
5. **Portfolio Heat Cap & Account Drawdown Circuit Breaker**:
   - **Heat Defense**: Total portfolio dollars-at-risk ($\sum (\text{Shares} \times (\text{Entry} - \text{Stop}))$) is strictly capped at **$\le 5.0\%$ of total account equity**. If full position sizing would breach 5%, shares are scaled down proportionally.
   - **Drawdown Circuit Breaker**: If total account equity falls $\ge 4.0\%$ below peak equity, the circuit breaker engages: Safe $f$ is automatically slashed by 50% and aggressive entries are halted until capital recovers.
6. **Sector & Cluster Risk Defense**:
   - Caps exposure at a strict maximum of **2 active positions per industry sector** (e.g. Semiconductors, Software, Hardware), preventing concentrated systemic shocks from impacting the portfolio.
7. **Dynamic VIX Volatility Regime Switcher**:
   - Automatically adapts capital allocation based on the CBOE Volatility Index (`^VIX`):
     - **`NORMAL` (VIX < 20)**: Full risk deployment. Up to 5 concurrent positions, Ralph Vince Safe $f = 0.50$, standard $P(\text{Up}) \ge 50\%$.
     - **`ELEVATED` (20 $\le$ VIX $\le$ 28)**: Controlled exposure. Max 3 concurrent positions, Safe $f = 0.30$, entry hurdle raised to $P(\text{Up}) \ge 58\%$.
     - **`CRISIS` (VIX > 28)**: High-volatility capital preservation. Max 1 position, Safe $f = 0.15$, entry hurdle raised to $P(\text{Up}) \ge 65\%$.
8. **Automated Broker API Execution & Two-Way Sync**:
   - Eliminates manual typing errors by bridging tickets directly to Interactive Brokers (IBKR) or Charles Schwab Trader API via `execute_orders.R`.
   - Built-in `--sync` flag queries live broker account balances and positions to update `portfolio.json` automatically.
9. **Real-Time Mobile Push Alerts (Discord & Telegram)**:
   - Dispatches actionable Monday order tickets and Friday weekend reviews directly to your smartphone via `R/send_alert.R`.

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

| Symbol | Action | Allocation ($ / %) | Exact Shares | GTC Stop-Loss | Tier 1 Target (+1.5R) | Chandelier Stop (Tier 2) | RS vs SPY | Sector |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| **AMD** | BUY | $5,684.73 (56.8%) | **9.034 shares** | $578.52 (-8.06%) | $705.37 (4.517 shs) | $565.32 (Trails ATR-14) | +30.7% [LEADER] | Semiconductors |
| **SNDK** | BUY | $673.39 (6.7%) | **0.384 shares** | $1665.25 (-5.04%) | $1886.18 (0.192 shs) | $1467.06 (Trails ATR-14) | +16.8% [LEADER] | Semiconductors |
| **MSFT** | BUY | $3,068.24 (30.7%) | **6.162 shares** | $496.68 (-0.25%) | $499.81 (3.081 shs) | $470.48 (Trails ATR-14) | +0.2% [LEADER] | Software_MegaCap |

---

### Step 2: Monday 14:15–15:00 EDT — Broker Execution & Bracket Setup

#### Option A: Automated Execution & Sync (Recommended)
You can stage and submit bracket orders directly to your broker using the built-in CLI bridge:

1. **Interactive Brokers (IBKR)**:
   ```bash
   # 1. Preview order staging and verify bracket JSON
   Rscript execute_orders.R --broker=ibkr --dry_run=TRUE

   # 2. Transmit live orders to TWS or IB Gateway (Paper: 7497, Live: 7496)
   Rscript execute_orders.R --broker=ibkr --dry_run=FALSE --port=7497

   # 3. Synchronize live broker fills directly into portfolio.json (Zero manual typing)
   Rscript execute_orders.R --broker=ibkr --sync --dry_run=FALSE --port=7497
   ```
2. **Charles Schwab API**:
   ```bash
   # 1. Preview Schwab FIRST_TRIGGERS_OCO JSON payloads
   Rscript execute_orders.R --broker=schwab --dry_run=TRUE
   ```

#### Option B: Manual Execution via Broker Web / Mobile App
In your brokerage account (IBKR, Schwab, Fidelity, etc.):

1. **Submit Buy Orders**:
   - Buy the specified shares (fractional or rounded) for each candidate.
2. **Attach Multi-Tier Bracket Orders**:
   - Immediately following execution, submit child bracket orders:
     - **Stop-Loss (GTC)**: Set trigger to the ticket's `GTC Stop-Loss`.
     - **Tier 1 Take-Profit (GTC Limit)**: Set limit for 50% of position to `Tier 1 Target`.
     - **Tier 2 Chandelier Trailing Stop**: Set trailing stop to trail the highest high minus $2.5 \times \text{ATR}_{14}$.
     - *Note*: Once Tier 1 fills, adjust the remaining stop-loss to Breakeven (Entry Price).
3. **Sync Portfolio State**:
   Record your executions in terminal:
   ```bash
   Rscript trade_manager.R --buy=AMD:9.034:629.26:578.52:705.37
   Rscript trade_manager.R --buy=SNDK:0.384:1753.62:1665.25:1886.18
   Rscript trade_manager.R --buy=MSFT:6.162:497.93:496.68:499.81
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
     Rscript trade_manager.R --sell=AMD:705.37:TAKE_PROFIT
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
