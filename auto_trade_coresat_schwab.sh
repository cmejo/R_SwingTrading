#!/usr/bin/env bash
# ==============================================================================
# Turnkey Charles Schwab Automated Trading Engine for Triple-Engine Model
# ==============================================================================
# Usage:
#   # 1. Preview / Dry Run (Simulate execution with fresh quotes & state):
#   ./auto_trade_coresat_schwab.sh --dry_run=true
#
#   # 2. Live Execution at 3:45 PM ET (Submits live orders to Charles Schwab):
#   ./auto_trade_coresat_schwab.sh --dry_run=false
#
#   # 3. Force Immediate Rebalance (e.g. Month Start or Portfolio Reset):
#   ./auto_trade_coresat_schwab.sh --force_rebalance --dry_run=true
# ==============================================================================

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Ensure PATH has Python and R binaries across macOS and Linux (trantor)
export PATH="$HOME/.pyenv/shims:/opt/homebrew/Caskroom/miniforge/base/bin:$HOME/miniforge3/bin:$PATH:/usr/local/bin:/usr/bin:/opt/homebrew/bin"

# Default CLI parameters
DRY_RUN="true"
FORCE_REBALANCE="false"
RESET="false"
CAPITAL=""
SYMBOLS_FILE="symbols_broad.txt"
ORDERS_FILE="orders.json"
STATE_FILE="schwab_state.json"
QUOTES_FILE="schwab_quotes.json"
MAX_ORDER_USD="5000"

# Parse CLI overrides
for arg in "$@"; do
  case $arg in
    --dry_run=*)
      DRY_RUN="${arg#*=}"
      ;;
    --force_rebalance)
      FORCE_REBALANCE="true"
      ;;
    --reset)
      RESET="true"
      FORCE_REBALANCE="true"
      ;;
    --capital=*)
      CAPITAL="${arg#*=}"
      ;;
    --symbols_file=*)
      SYMBOLS_FILE="${arg#*=}"
      ;;
    --max_order_usd=*)
      MAX_ORDER_USD="${arg#*=}"
      ;;
  esac
done

mkdir -p "$SCRIPT_DIR/logs"
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
LOG_FILE="$SCRIPT_DIR/logs/schwab_coresat_${TIMESTAMP}.log"

echo "================================================================================"
echo "      CHARLES SCHWAB AUTOMATED TRADING PIPELINE (100% STOCK: 1.5x MARGIN)       "
echo "================================================================================"
echo " Mode:          $([ "$DRY_RUN" = "false" ] && echo '🔴 LIVE EXECUTION' || echo '🟡 DRY RUN (Preview)')"
echo " Watchlist:     $SYMBOLS_FILE"
echo " Force Reb:     $FORCE_REBALANCE"
echo " Timestamp:     $(date)"
echo " Log Destination: $LOG_FILE"
echo "================================================================================"

# Step 0: Emergency Kill-Switch Check
if [ -f "STOP" ] || [ -f "HALT" ] || [ -f "EMERGENCY_STOP" ]; then
  echo "🚨 EMERGENCY ABORT: Kill-switch detected ('STOP' file present). Aborting pipeline."
  exit 1
fi

# Step 1: Schwab OAuth Token Watchdog
echo -e "\n[Step 1/5] Checking Charles Schwab OAuth token health..."
if [ -f "schwab_bridge.py" ]; then
  python3 schwab_bridge.py token-check --warn_hours=48 || {
    TOKEN_STATUS=$?
    if [ "$DRY_RUN" = "false" ] && [ "$TOKEN_STATUS" -eq 2 ]; then
      echo "❌ Live execution blocked: Schwab refresh token is expired or revoked."
      echo "Run: python3 schwab_bridge.py auth"
      exit 1
    fi
  }
fi

# Step 2: Account Balances & Position Synchronization
echo -e "\n[Step 2/5] Synchronizing broker balances and positions into $STATE_FILE..."
python3 schwab_bridge.py sync --out="$STATE_FILE" --dry_run="$DRY_RUN"

# Step 3: Fetch Real-Time Market Quotes
echo -e "\n[Step 3/5] Fetching fresh real-time quotes for universe into $QUOTES_FILE..."
python3 schwab_bridge.py quotes --symbols_file="$SYMBOLS_FILE" --out="$QUOTES_FILE" --dry_run="$DRY_RUN"

# Step 4: Run R Quantitative Signal Engine
echo -e "\n[Step 4/5] Executing 100% Stock Quantitative Momentum Model in R..."
rm -f "$ORDERS_FILE"
R_ARGS=(
  "--symbols_file=$SYMBOLS_FILE"
  "--broker_state=$STATE_FILE"
  "--quotes_file=$QUOTES_FILE"
  "--orders_out=$ORDERS_FILE"
  "--core_weight=0.00"
  "--core_asset=NONE"
)

if [ "$FORCE_REBALANCE" = "true" ]; then
  R_ARGS+=("--force_rebalance")
fi
if [ "$RESET" = "true" ]; then
  R_ARGS+=("--reset")
fi
if [ -n "$CAPITAL" ]; then
  R_ARGS+=("--capital=$CAPITAL")
fi

Rscript core_satellite_signal.R "${R_ARGS[@]}"

# Step 4b: Optional FMP Institutional Consensus Enrichment
if [ -n "$FMP_API_KEY" ] || grep -q "FMP_API_KEY" .env 2>/dev/null; then
  echo -e "\n[Step 4b] Fetching Wall Street consensus targets and fundamental metrics (FMP)..."
  python3 fmp_client.py || true
fi

# Step 5: Execute Machine-Readable Orders via Schwab Desk
echo -e "\n[Step 5/5] Processing order queue via Charles Schwab Trader API..."
python3 schwab_bridge.py execute \
  --orders="$ORDERS_FILE" \
  --dry_run="$DRY_RUN" \
  --max_order_usd="$MAX_ORDER_USD"

echo "================================================================================"
echo " Pipeline run completed successfully at $(date)."
echo "================================================================================"
