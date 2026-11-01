#!/usr/bin/env bash
# ==============================================================================
# Triple-Engine Momentum System (Option 6: 1.5x Margin + QLD Core + 22% VT -> GLD)
# ==============================================================================
# Usage:
#   ./run_coresat_30_70.sh                   # Daily monitoring / watchdog run
#   ./run_coresat_30_70.sh --reset           # Fresh portfolio setup with new tickets
#   ./run_coresat_30_70.sh --force_rebalance # Force immediate rebalance
#   ./run_coresat_30_70.sh --capital=8000    # Run with custom capital base
# ==============================================================================
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Ensure common Rscript binary paths are available on Linux / Mac
export PATH="/usr/local/bin:/usr/bin:/opt/homebrew/bin:$HOME/bin:$PATH"

Rscript core_satellite_signal.R "$@"
