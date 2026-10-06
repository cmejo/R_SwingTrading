#!/usr/bin/env bash
# ==============================================================================
# Automated Daily Execution Script for Multi-Asset Swing Trading Model
# ==============================================================================

export PATH="/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin:$PATH"

# Resolve project directory from script location
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$SCRIPT_DIR"
cd "$PROJECT_DIR" || exit 1

# Find Rscript (handles both Apple Silicon and Intel Homebrew, plus CRAN installs)
RSCRIPT_BIN=$(command -v Rscript || echo "/usr/local/bin/Rscript")

TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
LOG_FILE="$PROJECT_DIR/logs/scan_${TIMESTAMP}.log"
LATEST_TICKET="$PROJECT_DIR/LATEST_TICKET.txt"
MONTHLY_EVAL="$PROJECT_DIR/output/MONTHLY_EVALUATION.txt"

mkdir -p "$PROJECT_DIR/logs" "$PROJECT_DIR/output"

echo "[$(date)] Reading watchlist from $PROJECT_DIR/symbols_broad.txt..." >> "$LOG_FILE"

# 1. Execute Daily Multi-Asset Scanner (Max Positions: 5 | Vince Safe f: 0.50 | Fractional: TRUE)
"$RSCRIPT_BIN" daily_signal.R --symbols_file="$PROJECT_DIR/symbols_broad.txt" --capital=10000 --max_pos=5 --sizing_mode=vince --safety_factor=0.50 --fractional=TRUE --portfolio_file="$PROJECT_DIR/portfolio.json" 2>&1 | tee -a "$LOG_FILE" > "$LATEST_TICKET"

# 2. Automated Monthly Strategy Evaluation (Runs every Friday or if evaluation file is missing)
DAY_OF_WEEK=$(date +%u) # 5 = Friday
if [ "$DAY_OF_WEEK" -eq 5 ] || [ ! -f "$MONTHLY_EVAL" ]; then
  echo "[$(date)] Automatically executing 30-day strategy health review..." >> "$LOG_FILE"
  "$RSCRIPT_BIN" monthly_review.R --symbol=SPY --days=30 > "$MONTHLY_EVAL" 2>&1
  echo "[$(date)] Monthly strategy review updated: $MONTHLY_EVAL" >> "$LOG_FILE"
fi

# 3. Extract summary for desktop notification
MACRO_STATUS=$(grep -m 1 "Macro Gate:" "$LATEST_TICKET" | sed 's/.*Macro Gate: \([^ |]*\).*/\1/')
MACRO_STATUS="${MACRO_STATUS:-Active}"
N_BUYS=$(grep -c "ORDER TICKET #" "$LATEST_TICKET" || true)
TOP_TICKET=$(grep -m 1 "ORDER TICKET #1:" "$LATEST_TICKET" | sed 's/--- //;s/ ---//' || true)

if [ "$DAY_OF_WEEK" -eq 5 ]; then
  NOTIF_TITLE="Swing Scanner: Friday Weekend Exit Check"
  if [ "$N_BUYS" -gt 0 ] 2>/dev/null && [ -n "$TOP_TICKET" ]; then
    NOTIF_MSG="Weekend review + ${N_BUYS} new signal(s) for Monday. Check LATEST_TICKET.txt"
  else
    NOTIF_MSG="Zero weekend risk enforced! Check LATEST_TICKET.txt to close open positions."
  fi
elif [ "$N_BUYS" -gt 0 ] 2>/dev/null && [ -n "$TOP_TICKET" ]; then
  NOTIF_TITLE="Swing Scanner [${MACRO_STATUS}]: ${N_BUYS} Order(s)"
  NOTIF_MSG="${TOP_TICKET} | Check LATEST_TICKET.txt"
else
  NOTIF_TITLE="Swing Scanner [${MACRO_STATUS}]: 100% Cash Buffer"
  NOTIF_MSG="No symbols triggered BUY criteria today. Maintain cash position."
fi

# Send native macOS notification banner
osascript -e "display notification \"${NOTIF_MSG}\" with title \"${NOTIF_TITLE}\" sound name \"Glass\""

echo "[$(date)] Daily run completed successfully. Latest ticket saved to ${LATEST_TICKET}" >> "$LOG_FILE"
