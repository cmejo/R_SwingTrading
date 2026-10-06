#!/usr/bin/env bash
# ==============================================================================
# Turnkey Automated Scanner & Robinhood Order Execution Pipeline
# ==============================================================================
# Usage:
#   # 1. Preview / Dry Run (Simulates scan and Robinhood order submissions):
#   ./auto_trade_robinhood.sh --dry_run=true
#
#   # 2. Live Execution (Places real market buy orders on Robinhood):
#   ./auto_trade_robinhood.sh --dry_run=false
#
#   # 3. Custom Watchlist & Leverage:
#   ./auto_trade_robinhood.sh --symbols_file=symbols_broad.txt --leverage=1.5 --dry_run=false
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
  esac
done

TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
LOG_FILE="$PROJECT_DIR/logs/auto_trade_robinhood_${TIMESTAMP}.log"
LATEST_TICKET="$PROJECT_DIR/LATEST_TICKET.txt"

mkdir -p "$PROJECT_DIR/logs"

echo "================================================================================"
echo "           AUTOMATED SCANNER & ROBINHOOD EXECUTION PIPELINE                     "
echo "================================================================================"
echo " Mode:         $([ "$DRY_RUN" = "false" ] && echo '🔴 LIVE EXECUTION' || echo '🟡 DRY RUN (Preview)')"
echo " Watchlist:    $SYMBOLS_FILE"
echo " Capital:      \$$CAPITAL"
echo " Leverage:     ${LEVERAGE}x"
echo " Max Holdings: $MAX_POS (Max $MAX_PER_SECTOR per sector)"
echo " Timestamp:    $(date)"
echo "================================================================================"

# Step 1: Run Multi-Asset Scanner & Risk Management Engine
echo "\n[Step 1/3] Running Quantitative Scanner & Ralph Vince Optimal-f Engine..."
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

# Step 3: Execute Buy Orders on Robinhood
printf "\n[Step 3/3] Staging / Executing Orders via Robinhood API Bridge...\n"
python3 execute_robinhood.py --dry_run="$DRY_RUN" --ticket_file="$LATEST_TICKET" 2>&1 | tee -a "$LOG_FILE"

printf "\n================================================================================\n"
echo "           ROBINHOOD EXECUTION COMPLETED SUCCESSFULLY                           "
echo "================================================================================"
echo " Active Positions Tracked in: robinhood_positions.json"
echo " To start background real-time stop-loss and profit target monitoring, run:"
echo "   nohup python3 poll_robinhood_exits.py --interval=30 --dry_run=$DRY_RUN > logs/exit_poller.log 2>&1 &"
echo "================================================================================"
