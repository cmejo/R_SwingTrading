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

PROJECT_DIR="/Volumes/2TB.ssd/_a Development/swingtrading"
cd "$PROJECT_DIR" || exit 1

# Default Parameters
DRY_RUN="true"
SYMBOLS_FILE="$PROJECT_DIR/symbols.txt"
LEVERAGE="1.0"
MAX_POS="5"
MAX_PER_SECTOR="2"

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
  esac
done

TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
LOG_FILE="$PROJECT_DIR/logs/auto_trade_${TIMESTAMP}.log"
LATEST_TICKET="$PROJECT_DIR/LATEST_TICKET.txt"

mkdir -p "$PROJECT_DIR/logs"

echo "================================================================================"
echo "          AUTOMATED SCANNER & CHARLES SCHWAB EXECUTION PIPELINE                 "
echo "================================================================================"
echo " Mode:         $([ "$DRY_RUN" = "false" ] && echo '🔴 LIVE EXECUTION' || echo '🟡 DRY RUN (Preview)')"
echo " Watchlist:    $SYMBOLS_FILE"
echo " Leverage:     ${LEVERAGE}x"
echo " Max Holdings: $MAX_POS (Max $MAX_PER_SECTOR per sector)"
echo " Timestamp:    $(date)"
echo "================================================================================"

# Step 1: Run Multi-Asset Scanner & Risk Management Engine
echo "\n[Step 1/3] Running Quantitative Scanner & Ralph Vince Leverage Sizing..."
Rscript daily_signal.R \
  --symbols_file="$SYMBOLS_FILE" \
  --leverage="$LEVERAGE" \
  --max_pos="$MAX_POS" \
  --max_per_sector="$MAX_PER_SECTOR" \
  --portfolio_file="$PROJECT_DIR/portfolio.json" 2>&1 | tee "$LOG_FILE" > "$LATEST_TICKET"

# Step 2: Check for Actionable Buy Tickets
N_ACTIONABLE=$(grep -c "Action: *BUY [1-9]" "$LATEST_TICKET" || true)
MACRO_GATE=$(grep -m 1 "Macro Gate:" "$LATEST_TICKET" | sed 's/.*Macro Gate: \([^ |]*\).*/\1/' || echo "Active")

echo "\n[Step 2/3] Scanner Finished. Macro Gate: [$MACRO_GATE] | Qualifying Orders: $N_ACTIONABLE"

if [ "$N_ACTIONABLE" -eq 0 ]; then
  echo " [Notice] No actionable buy orders generated today (Macro Gate risk-off, or no symbols met 58% threshold)."
  echo " Maintaining 100% Cash / Existing Portfolio buffer."
  osascript -e "display notification \"No BUY criteria met today. Cash buffer preserved.\" with title \"Schwab Bot: 100% Cash Buffer\"" || true
  exit 0
fi

# Step 3: Charles Schwab Execution Bridge
echo "\n[Step 3/3] Initiating Charles Schwab Trader API Bridge..."
if [ "$DRY_RUN" = "false" ]; then
  echo " Transmitting $N_ACTIONABLE LIVE bracket orders to Charles Schwab..."
  python3 execute_broker.py --broker=schwab --dry_run=false --yes --ticket_file="$LATEST_TICKET"
  
  echo "\n[Reconciliation] Synchronizing active Schwab account positions into portfolio.json..."
  python3 execute_broker.py --broker=schwab --sync --dry_run=false || true
  
  NOTIF_MSG="Successfully submitted $N_ACTIONABLE live bracket order(s) to Charles Schwab!"
  osascript -e "display notification \"${NOTIF_MSG}\" with title \"Schwab Bot: LIVE ORDERS SENT\" sound name \"Glass\"" || true
else
  echo " Staging and validating Schwab REST bracket payloads (Dry Run Simulation)..."
  python3 execute_broker.py --broker=schwab --dry_run=true --ticket_file="$LATEST_TICKET"
  
  NOTIF_MSG="Validated $N_ACTIONABLE Schwab bracket payload(s) in simulation."
  osascript -e "display notification \"${NOTIF_MSG}\" with title \"Schwab Bot: Dry Run Validated\"" || true
fi

echo "\n================================================================================"
echo " Pipeline execution completed successfully. Log: $LOG_FILE"
echo "================================================================================"
