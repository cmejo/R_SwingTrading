#!/usr/bin/env bash
# ==============================================================================
# Installer for macOS Daily Swing Trading Automation
# ==============================================================================

PLIST_NAME="com.swingtrading.sndk.plist"
SOURCE_PLIST="/Volumes/2TB.ssd/_a Development/swingtrading/$PLIST_NAME"
TARGET_DIR="$HOME/Library/LaunchAgents"
TARGET_PLIST="$TARGET_DIR/$PLIST_NAME"

mkdir -p "$TARGET_DIR"

# Unload existing if loaded
launchctl unload "$TARGET_PLIST" 2>/dev/null || true

# Copy plist to user LaunchAgents directory
cp "$SOURCE_PLIST" "$TARGET_PLIST"

# Load into launchd
launchctl load "$TARGET_PLIST"

echo "======================================================================"
echo " macOS Automation Installed Successfully!"
echo " Service:  $TARGET_PLIST"
echo " Schedule: Monday through Friday at 4:30 PM (16:30)"
echo " Logs:     /Volumes/2TB.ssd/_a Development/swingtrading/logs/"
echo " Ticket:   /Volumes/2TB.ssd/_a Development/swingtrading/LATEST_TICKET.txt"
echo "======================================================================"
