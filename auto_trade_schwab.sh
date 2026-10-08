#!/usr/bin/env bash
# ==============================================================================
# Turnkey End-to-End Automated Scanner & Charles Schwab Order Executor
# ==============================================================================
# Usage:
#   # 1. Preview / Dry Run (Simulate scan and bracket payload without sending):
#   ./auto_trade_schwab.sh --dry_run=true
#
#   # 2. Live Execution (Transmits live FIRST_TRIGGERS_OCO brackets to Schwab):
#   ./auto_trade_schwab.sh --dry_run=false
#
#   # 3. Custom Watchlist & Leverage:
#   ./auto_trade_schwab.sh --symbols_file=symbols_broad.txt --leverage=1.5 --dry_run=false
# ==============================================================================

set -e
export PATH="/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin:$PATH"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="${PROJECT_DIR:-$SCRIPT_DIR}"
cd "$PROJECT_DIR" || exit 1

# Default Parameters
DRY_RUN="true"
SYMBOLS_FILE="$PROJECT_DIR/symbols_broad.txt"
LEVERAGE="1.5"
MAX_POS="3"
MAX_PER_SECTOR="3"
MAX_HEAT="0.08"

CAPITAL=""
AUTO_CAPITAL="true"

# Parse CLI arguments
for arg in "$@"; do
  case $arg in
    --dry_run=*)
      DRY_RUN="${arg#*=}"
      ;;
    --symbols_file=*)
      SYMBOLS_FILE="${arg#*=}"
      ;;
    --leverage=*)
      LEVERAGE="${arg#*=}"
      ;;
    --max_pos=*)
      MAX_POS="${arg#*=}"
      ;;
    --max_per_sector=*)
      MAX_PER_SECTOR="${arg#*=}"
      ;;
    --max_heat=*|--heat_budget=*)
      MAX_HEAT="${arg#*=}"
      ;;
    --capital=*)
      CAPITAL="${arg#*=}"
      AUTO_CAPITAL="false"
      ;;
  esac
done

# If no explicit capital passed, dynamically sync/extract full account equity
if [ "$AUTO_CAPITAL" = "true" ] || [ -z "$CAPITAL" ]; then
  # If Schwab credentials exist and live mode or token present, sync live account balance first
  if [ -f "$PROJECT_DIR/schwab_token.json" ]; then
    printf "[Auto-Capital] Syncing live account equity from Charles Schwab...\n"
    python3 execute_broker.py --broker=schwab --sync --dry_run=false >/dev/null 2>&1 || true
  fi

  # Extract total liquidation value from portfolio.json if available
  if [ -f "$PROJECT_DIR/portfolio.json" ]; then
    EXTRACTED_CAP=$(python3 -c "import json; d=json.load(open('$PROJECT_DIR/portfolio.json')); print(int(round(float(d.get('total_capital', 10000)))))" 2>/dev/null || echo "10000")
    if [ -n "$EXTRACTED_CAP" ] && [ "$EXTRACTED_CAP" -gt 0 ] 2>/dev/null; then
      CAPITAL="$EXTRACTED_CAP"
      echo "[Auto-Capital] Dynamically scaled trading capital to full account balance: \$$CAPITAL"
    else
      CAPITAL="10000"
    fi
  else
    CAPITAL="10000"
  fi
fi

TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
LOG_FILE="$PROJECT_DIR/logs/auto_trade_${TIMESTAMP}.log"
LATEST_TICKET="$PROJECT_DIR/LATEST_TICKET.txt"

mkdir -p "$PROJECT_DIR/logs"

echo "================================================================================"
echo "          AUTOMATED SCANNER & CHARLES SCHWAB EXECUTION PIPELINE                 "
echo "================================================================================"
echo " Mode:         $([ "$DRY_RUN" = "false" ] && echo '🔴 LIVE EXECUTION' || echo '🟡 DRY RUN (Preview)')"
echo " Watchlist:    $SYMBOLS_FILE"
echo " Capital:      \$$CAPITAL"
echo " Leverage:     ${LEVERAGE}x"
echo " Max Holdings: $MAX_POS (Max $MAX_PER_SECTOR per sector | Heat Cap: $(awk "BEGIN {print $MAX_HEAT * 100}")%)"
echo " Timestamp:    $(date)"
echo "================================================================================"

# Step 0: Check Schwab OAuth Token Health (Warns 24-48h before 7-day expiration)
if [ -f "$PROJECT_DIR/check_schwab_token.py" ]; then
  python3 "$PROJECT_DIR/check_schwab_token.py" --warn_hours=48 || true
fi

# Step 1: Run Multi-Asset Scanner & Risk Management Engine
printf "\n[Step 1/3] Running Quantitative Scanner & Ralph Vince Leverage Sizing...\n"
Rscript daily_signal.R \
  --capital="$CAPITAL" \
  --symbols_file="$SYMBOLS_FILE" \
  --leverage="$LEVERAGE" \
  --max_pos="$MAX_POS" \
  --max_per_sector="$MAX_PER_SECTOR" \
  --max_heat="$MAX_HEAT" \
  --sizing_mode="vince" \
  --portfolio_file="$PROJECT_DIR/portfolio.json" 2>&1 | tee "$LOG_FILE" "$LATEST_TICKET"

# Step 2: Check for Actionable Buy Tickets (Swing Stocks + QQQ Cash Park)
N_ACTIONABLE=$(grep -c -E "(Action: *BUY [0-9]|Actionable Ticket: *BUY [0-9].*QQQ)" "$LATEST_TICKET" || true)
MACRO_GATE=$(grep -m 1 "Macro Gate:" "$LATEST_TICKET" | sed 's/.*Macro Gate: \([^ |]*\).*/\1/' || echo "Active")

printf "\n[Step 2/3] Scanner Finished. Macro Gate: [%s] | Qualifying Orders: %s\n" "$MACRO_GATE" "$N_ACTIONABLE"

if [ "$N_ACTIONABLE" -eq 0 ]; then
  echo " [Notice] No actionable buy orders generated today (Macro Gate risk-off, or no symbols met 58% threshold)."
  echo " Maintaining 100% Cash / Existing Portfolio buffer."
  if command -v osascript &>/dev/null; then
    osascript -e "display notification \"No BUY criteria met today. Cash buffer preserved.\" with title \"Schwab Bot: 100% Cash Buffer\"" || true
  fi
  exit 0
fi

# Step 3: Charles Schwab Execution Bridge
printf "\n[Step 3/3] Initiating Charles Schwab Trader API Bridge...\n"
if [ "$DRY_RUN" = "false" ]; then
  echo " Transmitting $N_ACTIONABLE LIVE bracket orders to Charles Schwab..."
  python3 execute_broker.py --broker=schwab --dry_run=false --yes --ticket_file="$LATEST_TICKET"
  
  printf "\n[Reconciliation] Synchronizing active Schwab account positions into portfolio.json...\n"
  python3 execute_broker.py --broker=schwab --sync --dry_run=false || true
  
  NOTIF_MSG="Successfully submitted $N_ACTIONABLE live bracket order(s) to Charles Schwab!"
  if command -v osascript &>/dev/null; then
    osascript -e "display notification \"${NOTIF_MSG}\" with title \"Schwab Bot: LIVE ORDERS SENT\" sound name \"Glass\"" || true
  fi
else
  echo " Staging and validating Schwab REST bracket payloads (Dry Run Simulation)..."
  python3 execute_broker.py --broker=schwab --dry_run=true --ticket_file="$LATEST_TICKET"
  
  NOTIF_MSG="Validated $N_ACTIONABLE Schwab bracket payload(s) in simulation."
  if command -v osascript &>/dev/null; then
    osascript -e "display notification \"${NOTIF_MSG}\" with title \"Schwab Bot: Dry Run Validated\"" || true
  fi
fi

printf "\n================================================================================\n"
echo " Pipeline execution completed successfully. Log: $LOG_FILE"
echo "================================================================================"
