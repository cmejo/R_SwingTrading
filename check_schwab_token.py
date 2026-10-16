#!/usr/bin/env python3
"""
Lightweight Schwab OAuth Token Health & Expiration Warning Script.
Checks if schwab_token.json is valid or within 24-48 hours of expiration.
Dispatches alert via email (or webhook) with exact command to re-authenticate.

Usage:
  python3 check_schwab_token.py
  python3 check_schwab_token.py --warn_hours=48
"""

import os
import sys
import json
import time
import datetime
import smtplib
from email.mime.text import MIMEText
from email.mime.multipart import MIMEMultipart

TOKEN_FILE = "schwab_token.json"
ENV_FILE = ".env"

def load_env():
    if os.path.exists(ENV_FILE):
        with open(ENV_FILE, "r") as f:
            for line in f:
                line = line.strip()
                if not line or line.startswith("#") or "=" not in line:
                    continue
                k, v = line.split("=", 1)
                k = k.strip().replace("export ", "")
                v = v.strip().strip("\"'")
                if k not in os.environ:
                    os.environ[k] = v

load_env()

def send_notification(subject: str, body: str):
    # Print to console / stdout for cron logs
    print(f"\n================================================================================")
    print(f" [SCHWAB TOKEN ALERT] {subject}")
    print(f"================================================================================")
    print(body)
    print(f"================================================================================\n")

    # Send Email if SMTP configured
    smtp_server = os.environ.get("SMTP_SERVER")
    smtp_port = int(os.environ.get("SMTP_PORT", "587"))
    smtp_user = os.environ.get("SMTP_USER")
    smtp_pass = os.environ.get("SMTP_PASS")
    to_email = os.environ.get("ALERT_EMAIL_TO")
    from_email = os.environ.get("ALERT_EMAIL_FROM", smtp_user or "alerts@tradingbot.local")

    if smtp_server and to_email:
        try:
            msg = MIMEMultipart()
            msg["From"] = from_email
            msg["To"] = to_email
            msg["Subject"] = subject
            msg.attach(MIMEText(body, "plain"))

            server = smtplib.SMTP(smtp_server, smtp_port, timeout=10)
            server.ehlo()
            try:
                server.starttls()
            except Exception:
                pass
            if smtp_user and smtp_pass:
                server.login(smtp_user, smtp_pass)
            server.sendmail(from_email, [to_email], msg.as_string())
            server.quit()
            print(f"[Alert] Successfully dispatched email alert to {to_email}")
        except Exception as e:
            print(f"[Alert] Could not send email via SMTP ({smtp_server}): {e}")

    # Fallback: Telegram or Discord if configured
    discord_url = os.environ.get("DISCORD_WEBHOOK_URL")
    if discord_url:
        try:
            import urllib.request
            payload = json.dumps({"content": f"**{subject}**\n\n```\n{body}\n```"}).encode()
            req = urllib.request.Request(discord_url, data=payload, headers={"Content-Type": "application/json"})
            urllib.request.urlopen(req, timeout=10)
            print("[Alert] Dispatched Discord webhook alert.")
        except Exception as e:
            print(f"[Alert] Discord webhook failed: {e}")

def check_token_health(warn_hours: float = 48.0) -> int:
    if not os.path.exists(TOKEN_FILE):
        subject = "⚠️ URGENT: Schwab Token Missing - Action Required"
        body = (
            "No active schwab_token.json file was found on the system.\n\n"
            "Your automated trading system cannot place orders or sync balances at Charles Schwab.\n\n"
            "To authenticate your session right now, open your terminal and run:\n\n"
            "    python3 execute_broker.py --broker=schwab --auth\n\n"
            "Follow the prompt to log in and authorize the API."
        )
        send_notification(subject, body)
        return 1

    try:
        # Determine true refresh token creation and expiration time
        created_dt = None
        expires_dt = None
        if os.path.exists(TOKEN_FILE):
            try:
                with open(TOKEN_FILE, "r") as tf:
                    token_data = json.load(tf)
                if "refresh_token_created_at" in token_data:
                    created_dt = datetime.datetime.fromtimestamp(token_data["refresh_token_created_at"])
                    expires_dt = datetime.datetime.fromtimestamp(token_data.get("refresh_token_expires_at", token_data["refresh_token_created_at"] + 7 * 86400))
                elif "id_token" in token_data and "." in token_data["id_token"]:
                    import base64
                    part = token_data["id_token"].split(".")[1]
                    part += "=" * ((4 - len(part) % 4) % 4)
                    jwt_payload = json.loads(base64.urlsafe_b64decode(part))
                    iat = jwt_payload.get("iat")
                    if iat:
                        created_dt = datetime.datetime.fromtimestamp(iat)
                        expires_dt = created_dt + datetime.timedelta(days=7)
            except Exception:
                pass

        if not created_dt:
            mtime = os.path.getmtime(TOKEN_FILE)
            created_dt = datetime.datetime.fromtimestamp(mtime)
            expires_dt = created_dt + datetime.timedelta(days=7)

        now = datetime.datetime.now()
        remaining_seconds = (expires_dt - now).total_seconds()
        remaining_hours = remaining_seconds / 3600.0

        # Also test API call directly to detect revoked tokens
        app_key = os.environ.get("SCHWAB_APP_KEY", "")
        secret = os.environ.get("SCHWAB_SECRET", "")
        api_works = False
        error_msg = ""

        if app_key and secret:
            try:
                from execute_broker import SchwabTraderAPI
                schwab = SchwabTraderAPI(app_key, secret)
                schwab.get_account_hashes()
                api_works = True
            except Exception as e:
                api_works = False
                error_msg = str(e)
        else:
            api_works = False
            error_msg = "SCHWAB_APP_KEY or SCHWAB_SECRET missing in .env"

        # Condition 1: Token is already expired or revoked
        if not api_works or remaining_seconds <= 0:
            subject = "🚨 CRITICAL: Schwab Refresh Token Expired / Revoked - Immediate Action Required"
            body = (
                f"Your Charles Schwab API session is EXPIRED or REVOKED!\n"
                f"Reason: {error_msg}\n"
                f"Token Created: {created_dt.strftime('%Y-%m-%d %H:%M:%S')}\n\n"
                "Orders CANNOT be executed at market open until you re-authenticate.\n\n"
                "Run this exact command in your terminal right now:\n\n"
                "    python3 execute_broker.py --broker=schwab --auth\n\n"
                "1. Click the authorization URL in your terminal.\n"
                "2. Log in and approve access in Charles Schwab.\n"
                "3. Paste the redirect URL back into your terminal.\n"
            )
            send_notification(subject, body)
            return 2

        # Condition 2: Token is within warning window (24 - 48 hours remaining)
        elif remaining_hours <= warn_hours:
            subject = f"⚠️ WARNING: Schwab Refresh Token Expires in {remaining_hours:.1f} Hours"
            body = (
                f"Your Charles Schwab 7-day API refresh token will expire on:\n"
                f"    {expires_dt.strftime('%A, %B %d, %Y at %I:%M %p ET')}\n"
                f"Time Remaining: {remaining_hours:.1f} hours ({remaining_seconds / 86400.0:.1f} days)\n\n"
                "To ensure uninterrupted automated execution, re-authenticate before expiration:\n\n"
                "    python3 execute_broker.py --broker=schwab --auth\n\n"
                "This quick 30-second login resets your token for another full 7 days."
            )
            send_notification(subject, body)
            return 0

        else:
            print(f"[Schwab Token Health] Token is HEALTHY & ACTIVE.")
            print(f"  Created:   {created_dt.strftime('%Y-%m-%d %H:%M:%S')}")
            print(f"  Expires:   {expires_dt.strftime('%Y-%m-%d %H:%M:%S')}")
            print(f"  Remaining: {remaining_hours:.1f} hours ({remaining_seconds / 86400.0:.1f} days)")
            return 0

    except Exception as ex:
        print(f"[Schwab Token Health] Error checking token: {ex}")
        return 1

def set_token_creation_time(date_str: str) -> bool:
    if not os.path.exists(TOKEN_FILE):
        print(f"Error: {TOKEN_FILE} not found.")
        return False
    dt = None
    for fmt in ("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y-%m-%d", "%Y-%m-%dT%H:%M:%S"):
        try:
            dt = datetime.datetime.strptime(date_str, fmt)
            break
        except ValueError:
            pass
    if not dt:
        print(f"Error: Could not parse date string '{date_str}'. Expected format: 'YYYY-MM-DD' or 'YYYY-MM-DD HH:MM:SS'.")
        return False

    ts = dt.timestamp()
    exp_ts = ts + (7 * 86400)
    try:
        with open(TOKEN_FILE, "r") as f:
            data = json.load(f)
        data["refresh_token_created_at"] = ts
        data["refresh_token_expires_at"] = exp_ts
        with open(TOKEN_FILE, "w") as f:
            json.dump(data, f, indent=2)
        print(f"✓ Updated Schwab token metadata in {TOKEN_FILE}:")
        print(f"  Created: {dt.strftime('%Y-%m-%d %H:%M:%S')}")
        print(f"  Expires: {datetime.datetime.fromtimestamp(exp_ts).strftime('%Y-%m-%d %H:%M:%S')}\n")
        return True
    except Exception as e:
        print(f"Error updating {TOKEN_FILE}: {e}")
        return False

if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser(description="Check Charles Schwab OAuth Token Expiration")
    parser.add_argument("--warn_hours", type=float, default=48.0, help="Warning threshold in hours (default: 48.0)")
    parser.add_argument("--set_created", type=str, default=None, help="Set the true creation timestamp of the refresh token (e.g. '2026-10-14 13:10:33' or '2026-10-14')")
    args = parser.parse_args()

    if args.set_created:
        set_token_creation_time(args.set_created)

    sys.exit(check_token_health(warn_hours=args.warn_hours))
