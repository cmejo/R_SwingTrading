# Automated Charles Schwab Trading Guide: Market Open Execution & Exit Lifecycle

**System Architecture:** Multi-Factor ML Momentum Swing Strategy  
**Account Profile:** \$10,071 Live Capital | 1.5x Margin Leverage (\$15,106 Buying Power)  
**Risk Configuration:** Ralph Vince Leverage Space Sizing | **8.0% Max Portfolio Heat Budget**  
**Position Constraints:** Max 3 Concurrent Positions | Max 3 per Sector  

---

## 1. What You Need to Run Right Now for Market Open Execution

To stage your trades so Charles Schwab executes them at the 9:30 AM ET market open:

### Step A: Preview the Order Tickets (Dry Run)
Verify the ticket generation and allocation with your active 8.0% heat budget:
```bash
./auto_trade_schwab.sh --dry_run=true
```
Inspect the output file:
```bash
cat LATEST_TICKET.txt
```
You will see:
- **Order Ticket #2: AMUU** (~10.8 shares, ~$3,362 allocation) with GTC Stop-Loss at \$266.13, Tier 1 at \$378.77, Tier 2 at \$446.36.
- **Order Ticket #3: PENG** (~46.3 shares, ~$3,362 allocation) with GTC Stop-Loss at \$65.85, Tier 1 at \$82.75, Tier 2 at \$92.89.
- **Cash Park Ticket:** QQQ (~4.3 shares, ~$3,274 allocation) to earn index market yield on unallocated cash.

### Step B: Submit Live Orders to Charles Schwab Desk
Run the live execution pipeline:
```bash
./auto_trade_schwab.sh --dry_run=false
```

### What Happens Behind the Scenes at Schwab Overnight & Tomorrow Morning:
1. `execute_broker.py` connects to the Schwab API using your authenticated OAuth tokens.
2. It transmits **parent market buy orders** with session settings `Duration: DAY` and `Session: NORMAL`.
3. Because market hours are closed overnight, Schwab's order desk **queues the orders in pre-market staging**.
4. At **9:30:00 AM ET tomorrow**, the opening bell rings and Schwab automatically fills the buy orders at the market opening print.
5. Upon fill, Schwab's trading servers arm the **OCO (One-Cancels-Other) child bracket orders**:
   - The **GTC Hard Stop-Loss** is activated immediately on Schwab's servers (protecting downside 24/7).
   - The **Tier 1 Limit Sell (+1.5R)** is queued to take 50% profit automatically.
   - The **Tier 2 Limit Sell (+3.0R)** is queued to take 25% profit automatically.
6. The fills are recorded in `portfolio.json`, locking in your purchase prices, stop levels, and entry timestamps.

---

## 2. If Bought on Thursday, Does the Model Sell Next Thursday? (The 7-Day Rule)

> **Yes, strictly for non-runner lots that have not hit profit targets or stops.**

The strategy uses **trading days (business days)**, not calendar days:
- **Day 0 (Thursday):** Entry fill at market open.
- **Day 1 (Friday):** 1st trading day completed.
- **Day 2 (Monday):** 2nd trading day completed.
- **Day 3 (Tuesday):** 3rd trading day completed.
- **Day 4 (Wednesday):** 4th trading day completed.
- **Day 5 (Thursday):** 5th trading day completed.
- **Day 6 (Friday):** 6th trading day completed.
- **Day 7 (Monday of Week 3):** **7th Trading Day (Time Expiration).**

### How Day 7 Expiration Works:
- If a trade has neither stopped out nor reached its Tier 2 target after 7 full trading sessions, its momentum edge has decayed.
- At **3:30 PM ET on Day 7**, `daily_signal.R` flags the position:
  ```text
  Action: SELL (MAX 7-DAY TIME HORIZON)
  ```
- The position is liquidated at market close, freeing up cash for fresh leaderboard candidates.
- **Crucial Exception (The 25% Runner Lot):** If the trade hit Tier 1 and Tier 2 targets earlier, the remaining **25% runner portion is 100% exempt from the 7-day expiration**. It is held indefinitely as long as it stays above its dynamic 2.5x ATR Chandelier trailing stop.

---

## 3. What Happens Every Day When the Cron Job Runs the Scanner?

The daily cron schedule is set to run Monday through Friday at **3:30 PM ET (15:30)** via:
```cron
30 15 * * 1-5 /home/cmejo/Development/R_SwingTrading/auto_trade_schwab.sh --dry_run=false >> /home/cmejo/Development/R_SwingTrading/logs/cron_daily.log 2>&1
```

When the cron fires at 3:30 PM each day, it performs **Portfolio Synchronization & Capital Deployment**:

```
[Cron 15:30 ET]
      │
      ▼
1. Sync Active Portfolio (`portfolio.json`)
   - Checks Schwab fills and current market prices.
   - Calculates elapsed trading days for each open position.
   - Updates trailing stops: ratchets stop to Breakeven (+1R) or Chandelier trailing stop.
      │
      ▼
2. Check Exit Conditions
   - Did any position hit its Stop-Loss, Tier 1 (+1.5R), Tier 2 (+3.0R), or Day 7 Expiry?
   - If Friday: runs Friday Conditional Weekend Defense.
   - Generates SELL orders for any triggered exits before the 4:00 PM close.
      │
      ▼
3. Check Available Cash Slots
   - Are any of the 3 slots empty?
   - IF ALL 3 SLOTS ARE OCCUPIED: The scanner outputs "0 open slots", leaves positions alone,
     and makes NO new purchases (no churning or overtrading).
   - IF 1 OR MORE SLOTS ARE EMPTY: Scans the 170-stock universe, selects the top candidates,
     calculates Ralph Vince sizing (with 8.0% heat cap), and stages BUY tickets for tomorrow.
```

---

## 4. What Causes Selling on Friday? (The Weekend Risk Engine)

Every Friday at 3:30 PM ET (`is_friday = TRUE`), the model evaluates open positions against the **Conditional Weekend Holding Engine** in `R/portfolio_manager.R`.

Over a 48-hour weekend, the market is closed, leaving positions vulnerable to geopolitical news, macro shocks, or earnings announcements that could cause severe gap-downs on Monday morning.

### The 5 Friday Weekend Tests:
A position is **APPROVED TO HOLD OVER THE WEEKEND** if and only if it passes all 5 criteria:
1. **Macro Gate:** QQQ 50-day moving average trend is BULLISH (`Cl > lmMA50`).
2. **Model Probability:** ML Probability of upward continuation remains strong: $P(\text{Up}) \ge 55\%$.
3. **Weekly Synergy:** Weekly trend slope remains positive (`Weekly_Slope > 0`).
4. **Earnings Blackout:** No corporate earnings announcement scheduled within the next 7 calendar days.
5. **Stop Buffer:** Price is currently trading comfortably above the stop-loss level.

### What Triggers an Immediate Friday Sell?
If an open position fails **even one** of these tests:
```text
Action: SELL BEFORE 16:00 (Weekend Risk: [Failure Reasons])
```
The system triggers a market sell order between 3:30 PM and 4:00 PM on Friday to liquidate the position to cash, completely eliminating weekend gap risk.

---

## 5. If Not Sold Friday, Does the Model Always Hold for 7 Days?

> **No. Positions can sell earlier at any time if target or stop triggers hit.**

Here is the exact hierarchy of how and when momentum trades exit:

| Exit Trigger | When It Happens | Portion Sold | Action Taken |
| :--- | :--- | :---: | :--- |
| **1. Hard Stop-Loss** | Any time intraday (armed on Schwab servers) | **100%** | Full exit immediately to protect capital. |
| **2. Tier 1 Target (+1.5R)** | Day 1 to Day 6 intraday | **50%** | Sells 50% lot; moves stop on remainder to Breakeven. |
| **3. Tier 2 Target (+3.0R)** | Day 1 to Day 6 intraday | **25%** | Sells 25% lot; converts remaining 25% to Runner Lot. |
| **4. Friday Defensive Exit** | Friday 3:30 PM ET | **100%** | Liquidates positions that lost momentum or face earnings. |
| **5. 7-Day Time Expiration** | Day 7 at 3:30 PM ET | **Remainder** | Closes non-runner shares whose momentum expired. |
| **6. Chandelier Runner Stop** | Days 7 to 60+ (multi-week trends) | **25%** | Trails highest price by $2.5 \times \text{ATR}$; exits only on trend break. |

---

## 6. Will the Model Run Completely on Its Own Without You?

> **Yes, the pipeline is fully autonomous end-to-end once your cron and Schwab tokens are active.**

### The Autonomous Daily Cycle:
1. **Automated Account Sync:** Charles Schwab cash and portfolio equity are synced automatically at the start of each run.
2. **Automated Order Staging:** Buy tickets are calculated and sent directly to Schwab's API.
3. **Automated Bracket Placement:** Schwab's server hosts the bracket orders, so stops and profit targets fill automatically even if your computer is asleep.
4. **Automated Position Tracking:** `portfolio.json` records every fill, counts holding days, and ratchets trailing stops without manual input.
5. **Automated Alerts:** Mobile webhook notifications (Email, Discord, or Telegram) notify you whenever orders execute or stops adjust.

### Your Only Maintenance Tasks:
- **Weekly Token Refresh:** Charles Schwab OAuth refresh tokens expire every 7 days. Ensure your token refresh script runs weekly or re-authenticate via `./execute_broker.py --broker=schwab` if credentials expire.
- **System Monitoring:** Periodically check `cat LATEST_TICKET.txt` or review logs in `logs/` to confirm your cron job executed on schedule.
