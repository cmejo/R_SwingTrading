#!/usr/bin/env bash
# ==============================================================================
# Turnkey Automated Scanner & Interactive Brokers (IBKR Pro) Execution Pipeline
# ==============================================================================
# Usage:
#   # 1. Preview / Dry Run (Simulate scan and bracket payload without sending):
#   ./auto_trade_ibkr.sh --dry_run=true
#
#   # 2. Live Execution to Paper Account (TWS default port 7497):
#   ./auto_trade_ibkr.sh --dry_run=false --port=7497
#
#   # 3. Live Execution to Real IBKR Pro Account (TWS default port 7496 or IB Gateway 4001):
#   ./auto_trade_ibkr.sh --dry_run=false --port=7496
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
MAX_POS="5"
MAX_PER_SECTOR="2"
CAPITAL="10000"
IB_PORT="7497" # 7497 = TWS Paper, 7496 = TWS Live, 4001 = IB Gateway Live, 4002 = IB Gateway Paper

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
    --capital=*)
      CAPITAL="${arg#*=}"
      ;;
    --port=*)
      IB_PORT="${arg#*=}"
      ;;
  esac
done

TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
LOG_FILE="$PROJECT_DIR/logs/auto_trade_ibkr_${TIMESTAMP}.log"
LATEST_TICKET="$PROJECT_DIR/LATEST_TICKET.txt"

mkdir -p "$PROJECT_DIR/logs"

echo "================================================================================"
echo "        AUTOMATED SCANNER & INTERACTIVE BROKERS (IBKR) PIPELINE                 "
echo "================================================================================"
echo " Mode:         $([ "$DRY_RUN" = "false" ] && echo '🔴 LIVE EXECUTION' || echo '🟡 DRY RUN (Preview)')"
echo " Watchlist:    $SYMBOLS_FILE"
echo " Capital:      \$$CAPITAL"
echo " Leverage:     ${LEVERAGE}x"
echo " Max Holdings: $MAX_POS (Max $MAX_PER_SECTOR per sector)"
echo " IBKR Port:    $IB_PORT"
echo " Timestamp:    $(date)"
echo "================================================================================"

# Step 1: Run Multi-Asset Scanner & Risk Management Engine
printf "\n[Step 1/3] Running Quantitative Scanner & Ralph Vince Optimal-f Sizing...\n"
Rscript daily_signal.R \
  --capital="$CAPITAL" \
  --symbols_file="$SYMBOLS_FILE" \
  --leverage="$LEVERAGE" \
  --max_pos="$MAX_POS" \
  --max_per_sector="$MAX_PER_SECTOR" \
  --portfolio_file="$PROJECT_DIR/portfolio.json" 2>&1 | tee "$LOG_FILE" "$LATEST_TICKET"

# Step 2: Check for Actionable Buy Tickets
N_ACTIONABLE=$(grep -c "Action: *BUY [0-9]" "$LATEST_TICKET" || true)
MACRO_GATE=$(grep -m 1 "Macro Gate:" "$LATEST_TICKET" | sed 's/.*Macro Gate: \([^ |]*\).*/\1/' || echo "Active")

printf "\n[Step 2/3] Scanner Finished. Macro Gate: [%s] | Qualifying Orders: %s\n" "$MACRO_GATE" "$N_ACTIONABLE"

if [ "$N_ACTIONABLE" -eq 0 ]; then
  echo "[Notice] No qualifying BUY signals generated today. Exiting without placing orders."
  exit 0
fi

# Step 3: Transmit Bracket Orders to Interactive Brokers
printf "\n[Step 3/3] Staging / Transmitting Bracket Orders to IBKR Gateway/TWS...\n"
python3 execute_ibkr.py --dry_run="$DRY_RUN" --port="$IB_PORT" --ticket_file="$LATEST_TICKET" 2>&1 | tee -a "$LOG_FILE"

# Post-Execution Portfolio Reconciliation (R2)
if [ "$DRY_RUN" = "false" ]; then
  printf "\n[Reconciliation] Synchronizing active IBKR account positions into portfolio.json...\n"
  python3 execute_broker.py --broker=ibkr --sync --port="$IB_PORT" --dry_run=false 2>&1 | tee -a "$LOG_FILE" || true
fi

printf "\n================================================================================\n"
echo "        INTERACTIVE BROKERS EXECUTION PIPELINE COMPLETED                        "
echo "================================================================================"
echo " Review full execution details in: $LOG_FILE"
echo "================================================================================"
