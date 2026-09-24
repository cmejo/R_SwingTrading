#!/usr/bin/env bash
# ==============================================================================
# Uninstaller for macOS Daily Swing Trading Automation
# ==============================================================================

PLIST_NAME="com.swingtrading.sndk.plist"
TARGET_PLIST="$HOME/Library/LaunchAgents/$PLIST_NAME"

if [ -f "$TARGET_PLIST" ]; then
    launchctl unload "$TARGET_PLIST" 2>/dev/null || true
    rm -f "$TARGET_PLIST"
    echo "Service $PLIST_NAME has been unloaded and removed."
else
    echo "Service $PLIST_NAME was not installed."
fi
