# Charles Schwab Live Trading Setup & Execution Guide

This guide walks you through setting up your approved Charles Schwab Developer Account, authenticating the trading bot tomorrow morning, and executing live orders tomorrow afternoon.

---

## 1. Quick Summary of Strategy Rules

### Can We Buy Tomorrow Afternoon?
**YES!** Tomorrow is **Thursday, October 8, 2026**. Thursday is an active BUY day. You can scan and submit orders tomorrow afternoon between **2:00 PM and 3:45 PM EDT** (or at 3:30 PM EDT, 30 minutes before market close).

### What Days Does the Script Buy On?
- **BUY Days**: **Monday, Tuesday, Wednesday, and Thursday** (2:00 PM – 3:45 PM EDT).
- **FRIDAY Policy (Zero Weekend Risk)**: The script **does NOT buy new stocks on Friday**. Instead, at **15:30 EDT on Friday**, it evaluates existing open positions:
  - Positions without an established profit cushion are liquidated defensively before 16:00 EDT to eliminate weekend gap risk.
  - Only positions with strong momentum (`WEEKEND_HOLD_APPROVED`) are held over Saturday/Sunday.
  - Fresh new buys resume on **Monday afternoon**.

### What is the Max Number of Hold Days?
**5 Trading Days** (calendar weekends are automatically excluded).
- If neither target nor stop is hit by **Day 5**, the position is closed at market to prevent capital stagnation.
- **Tier 1 (+1.5R target)**: Takes **50% profit** off the table and ratchets stop-loss to Breakeven (entry price) or ATR Chandelier trailing stop.
- **Time-Decay Ratchet (Day 4)**: If held for 4 days and position has reached $\ge +1.0R$ profit, the Tier 1 target ratchets down to $+1.1R$ to lock in gains before the 5-day expiration!
- **Tier 2 (+3.0R target)**: Takes an additional **25% profit**.
- **25% Runner Lot**: The remaining 25% runner is governed purely by the ATR Chandelier trailing stop and **can run past 5 days** as long as the uptrend persists.

---

## 2. Morning Setup Checklist (Tomorrow AM)

Complete these 3 steps in the morning before market open:

### Step 1: Configure Your `.env` File
In your project directory (`/Volumes/2TB.ssd/_a Development/swingtrading`), create or edit the `.env` file:

```bash
# Charles Schwab Trader API Credentials
SCHWAB_APP_KEY="your_approved_app_key_here"
SCHWAB_SECRET="your_approved_app_secret_here"
SCHWAB_REDIRECT_URI="https://127.0.0.1"

# Trading Capital & Universe Default
DEFAULT_SYMBOLS_FILE="symbols_broad.txt"
DEFAULT_CAPITAL=10000
DEFAULT_MAX_POS=5
```

> **Note:** The `SCHWAB_REDIRECT_URI` must match the Callback URL configured in your Schwab Developer Portal app settings (standard default is `https://127.0.0.1`).

---

### Step 2: One-Time OAuth2 Authentication
Run the interactive authentication bridge in your terminal:

```bash
cd "/Volumes/2TB.ssd/_a Development/swingtrading"
python3 execute_broker.py --broker=schwab --auth
```

**What will happen:**
1. The script will display an authorization URL:
   ```
   https://api.schwabapi.com/v1/oauth/authorize?client_id=...&redirect_uri=https://127.0.0.1
   ```
2. Open this URL in your web browser.
3. Log in with your Charles Schwab username/password and click **Approve / Consent**.
4. Your browser will redirect you to `https://127.0.0.1/?code=...`.  
   *(Your browser may say "Unable to connect" or display a blank page—this is completely normal!)*
5. **Copy the ENTIRE URL from your browser's address bar** (everything including `https://127.0.0.1/?code=...`).
6. Paste the URL back into your terminal prompt and press **Enter**.
7. The script will exchange the authorization code and save your credentials to `schwab_token.json`.

> **Automatic Refresh:** Once `schwab_token.json` is created, the system **automatically refreshes the token** in the background whenever it expires. You will not need to log in again.

---

### Step 3: Verify Account Connection & Run a Dry Run
Verify that Schwab recognizes your account and permissions:

```bash
# 1. Test account position synchronization (Dry Run preview)
python3 execute_broker.py --broker=schwab --sync --dry_run=true

# 2. Run the full turnkey pipeline in Dry Run mode
./auto_trade_schwab.sh --dry_run=true
```

**Expected output:**
- Successfully connects to your Schwab account (`Connected to Schwab Account: ***1234`).
- Scans `symbols_broad.txt`.
- Ranks top candidates, calculates volatility and position sizes, and displays generated Schwab bracket payloads without transmitting orders.

---

## 3. Tomorrow Afternoon Live Execution (Thursday PM)

Between **2:00 PM and 3:45 PM EDT** (recommended: **3:30 PM EDT**):

### Execute Live Orders
Run the automated Schwab trading script with live execution enabled:

```bash
cd "/Volumes/2TB.ssd/_a Development/swingtrading"
./auto_trade_schwab.sh --dry_run=false
```

### What the Script Does Automatically:
1. **Macro Market Filter**: Inspects QQQ 50-day trend and CBOE VIX volatility regime.
2. **Feature & ML Pipeline**: Generates features across `symbols_broad.txt` and calculates $P(\text{Up})$.
3. **Cross-Sectional Ranking**: Selects top qualifying momentum leaders (up to 5 positions, max 2 per sector).
4. **Ralph Vince Leverage Sizing**: Allocates capital with 5% portfolio heat protection.
5. **Core-Satellite QQQ Cash Yield**: Parks unallocated cash into QQQ if fewer than 5 stocks qualify.
6. **Transmits Live Brackets to Schwab**:
   - Leg 1: Parent Market Buy order.
   - Leg 2 (Tier 1): 50% shares Limit Sell at $+1.5R$, linked with Stop-Loss (OCO).
   - Leg 3 (Tier 2): 25% shares Limit Sell at $+3.0R$, linked with Stop-Loss (OCO).
   - Leg 4 (Runner): 25% shares governed by trailing Chandelier stop.
7. **Reconciliation**: Syncs filled positions directly into `portfolio.json` and sends a native macOS notification banner.

---

## 4. Useful Management Commands

```bash
# View active portfolio positions and PnL:
Rscript daily_signal.R --portfolio_file=portfolio.json

# Preview generated tickets without placing orders:
./auto_trade_schwab.sh --dry_run=true

# Force synchronize Schwab account positions with local portfolio:
python3 execute_broker.py --broker=schwab --sync --dry_run=false

# Inspect daily execution log:
tail -f logs/auto_trade_*.log
```
