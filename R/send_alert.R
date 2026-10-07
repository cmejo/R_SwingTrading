#!/usr/bin/env Rscript
#' Mobile Push Notification Module (Discord & Telegram Webhooks)
#'
#' Sends formatted alerts to Discord and Telegram for:
#'   - Monday 14:00 Actionable Weekly Order Tickets
#'   - Friday 15:30 Conditional Weekend Review (Hold vs Sell)
#'   - Stop-Loss & Take-Profit Trigger Alerts
#'
#' Configuration:
#'   Set environment variables or put them in .env:
#'     DISCORD_WEBHOOK_URL="https://discord.com/api/webhooks/..."
#'     TELEGRAM_BOT_TOKEN="123456789:ABCDEF..."
#'     TELEGRAM_CHAT_ID="987654321"

suppressPackageStartupMessages({
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required. Please install it with install.packages('jsonlite').")
  }
  library(jsonlite)
})

# Load .env if present
load_dot_env <- function(env_path = ".env") {
  if (file.exists(env_path)) {
    lines <- readLines(env_path, warn = FALSE)
    for (line in lines) {
      line <- trimws(line)
      # Strip comments and export prefixes
      line <- sub("#.*$", "", line)
      line <- sub("^export\\s+", "", line)
      line <- trimws(line)
      if (line == "") next
      eq_pos <- regexpr("=", line)
      if (eq_pos > 0) {
        key <- trimws(substr(line, 1, eq_pos - 1))
        val <- trimws(substr(line, eq_pos + 1, nchar(line)))
        val <- gsub("^[\"']|[\"']$", "", val)
        if (Sys.getenv(key) == "") {
          do.call(Sys.setenv, setNames(list(val), key))
        }
      }
    }
  }
}
load_dot_env()

send_discord_alert <- function(title, description, color = 3447003, webhook_url = Sys.getenv("DISCORD_WEBHOOK_URL")) {
  if (is.null(webhook_url) || webhook_url == "") {
    return(FALSE)
  }
  
  payload <- list(
    embeds = list(
      list(
        title = title,
        description = substr(description, 1, 4000),
        color = color,
        footer = list(text = paste("Quantitative Swing Trading System •", format(Sys.time(), "%Y-%m-%d %H:%M:%S ET")))
      )
    )
  )
  
  json_body <- jsonlite::toJSON(payload, auto_unbox = TRUE)
  tmp_file <- tempfile(fileext = ".json")
  writeLines(json_body, tmp_file)
  on.exit(unlink(tmp_file), add = TRUE)
  
  res <- system2("curl", args = c(
    "-s", "-o", "/dev/null", "-w", "%{http_code}",
    "-X", "POST",
    "-H", "Content-Type: application/json",
    "-d", paste0("@", tmp_file),
    webhook_url
  ), stdout = TRUE, stderr = FALSE)
  
  code <- as.integer(res[1])
  return(!is.na(code) && code >= 200 && code < 300)
}

send_telegram_alert <- function(text, 
                                bot_token = Sys.getenv("TELEGRAM_BOT_TOKEN"), 
                                chat_id = Sys.getenv("TELEGRAM_CHAT_ID")) {
  if (is.null(bot_token) || bot_token == "" || is.null(chat_id) || chat_id == "") {
    return(FALSE)
  }
  
  url <- sprintf("https://api.telegram.org/bot%s/sendMessage", bot_token)
  
  # Sanitize HTML tags for Telegram HTML parse_mode
  clean_text <- gsub("&", "&amp;", text)
  clean_text <- gsub("<", "&lt;", clean_text)
  clean_text <- gsub(">", "&gt;", clean_text)
  
  payload <- list(
    chat_id = chat_id,
    text = substr(clean_text, 1, 4000),
    parse_mode = "HTML"
  )
  
  json_body <- jsonlite::toJSON(payload, auto_unbox = TRUE)
  tmp_file <- tempfile(fileext = ".json")
  writeLines(json_body, tmp_file)
  on.exit(unlink(tmp_file), add = TRUE)
  
  res <- system2("curl", args = c(
    "-s", "-o", "/dev/null", "-w", "%{http_code}",
    "-X", "POST",
    "-H", "Content-Type: application/json",
    "-d", paste0("@", tmp_file),
    url
  ), stdout = TRUE, stderr = FALSE)
  
  code <- as.integer(res[1])
  return(!is.na(code) && code >= 200 && code < 300)
}

send_email_alert <- function(title, body,
                             smtp_server = Sys.getenv("SMTP_SERVER"),
                             smtp_port = Sys.getenv("SMTP_PORT", "587"),
                             smtp_user = Sys.getenv("SMTP_USER"),
                             smtp_pass = Sys.getenv("SMTP_PASS"),
                             to_email = Sys.getenv("ALERT_EMAIL_TO")) {
  if (is.null(smtp_server) || smtp_server == "" || is.null(to_email) || to_email == "") {
    return(FALSE)
  }
  
  from_email <- if (Sys.getenv("ALERT_EMAIL_FROM") != "") Sys.getenv("ALERT_EMAIL_FROM") else smtp_user
  mail_content <- sprintf(
    "From: <%s>\nTo: <%s>\nSubject: %s\nContent-Type: text/plain; charset=utf-8\n\n%s\n\n--\nQuantitative Swing Trading System\nTimestamp: %s ET",
    from_email, to_email, title, body, format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
  
  tmp_mail <- tempfile(fileext = ".eml")
  writeLines(mail_content, tmp_mail)
  on.exit(unlink(tmp_mail), add = TRUE)
  
  curl_args <- c(
    "--url", sprintf("smtp://%s:%s", smtp_server, smtp_port),
    "--mail-from", from_email,
    "--mail-rcpt", to_email,
    "--upload-file", tmp_mail,
    "--ssl-reqd"
  )
  if (smtp_user != "" && smtp_pass != "") {
    curl_args <- c(curl_args, "--user", sprintf("%s:%s", smtp_user, smtp_pass))
  }
  
  res <- tryCatch({
    system2("curl", args = c("-s", curl_args), stdout = TRUE, stderr = FALSE)
    TRUE
  }, error = function(e) FALSE)
  return(isTRUE(res))
}

send_signal_alert <- function(message,
                              signal_api_url = Sys.getenv("SIGNAL_API_URL"),
                              signal_sender = Sys.getenv("SIGNAL_SENDER"),
                              signal_recipient = Sys.getenv("SIGNAL_RECIPIENT")) {
  if (is.null(signal_recipient) || signal_recipient == "") {
    return(FALSE)
  }
  
  # Option A: signal-cli REST API (e.g., bbernhard/signal-cli-rest-api container)
  if (!is.null(signal_api_url) && signal_api_url != "") {
    payload <- list(
      message = message,
      number = signal_sender,
      recipients = list(signal_recipient)
    )
    json_body <- jsonlite::toJSON(payload, auto_unbox = TRUE)
    tmp_file <- tempfile(fileext = ".json")
    writeLines(json_body, tmp_file)
    on.exit(unlink(tmp_file), add = TRUE)
    
    res <- system2("curl", args = c(
      "-s", "-o", "/dev/null", "-w", "%{http_code}",
      "-X", "POST",
      "-H", "Content-Type: application/json",
      "-d", paste0("@", tmp_file),
      paste0(sub("/$", "", signal_api_url), "/v2/send")
    ), stdout = TRUE, stderr = FALSE)
    code <- as.integer(res[1])
    return(!is.na(code) && code >= 200 && code < 300)
  }
  
  # Option B: Local signal-cli command line
  res <- tryCatch({
    sender_arg <- if (signal_sender != "") c("-u", signal_sender) else character(0)
    system2("signal-cli", args = c(sender_arg, "send", "-m", message, signal_recipient), stdout = FALSE, stderr = FALSE)
    TRUE
  }, error = function(e) FALSE)
  return(isTRUE(res))
}

#' High-level Alert Dispatcher
#'
#' @param title Subject of alert
#' @param body Markdown content
#' @param level "INFO", "SUCCESS", "WARNING", "ALERT"
#' @export
broadcast_alert <- function(title, body, level = "INFO") {
  color <- switch(level,
    "SUCCESS" = 3066993,   # Green
    "WARNING" = 15844367,  # Yellow/Orange
    "ALERT"   = 15158332,  # Red
    3447003                # Blue default
  )
  
  discord_sent  <- send_discord_alert(title, body, color)
  tg_text       <- sprintf("<b>%s</b>\n\n%s", title, body)
  telegram_sent <- send_telegram_alert(tg_text)
  email_sent    <- send_email_alert(title, body)
  signal_sent   <- send_signal_alert(sprintf("[%s]\n%s", title, body))
  
  sent_channels <- c(
    if (discord_sent) "Discord",
    if (telegram_sent) "Telegram",
    if (email_sent) "Email",
    if (signal_sent) "Signal"
  )
  
  if (length(sent_channels) > 0) {
    cat(sprintf("[AlertSystem] Notification sent: '%s' via [%s]\n",
                title, paste(sent_channels, collapse = ", ")))
  } else {
    cat(sprintf("[AlertSystem] Notice: Notification channels not configured for '%s'.\n", title))
    cat("  (To enable alerts, set EMAIL (SMTP_SERVER, ALERT_EMAIL_TO), SIGNAL, DISCORD, or TELEGRAM in .env)\n")
  }
}

# CLI Invocation
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  test_mode <- FALSE
  msg_text <- NULL
  file_path <- NULL
  title_text <- "Quantitative Swing Trading Alert"
  level_val <- "INFO"
  
  for (arg in args) {
    if (arg == "--test") test_mode <- TRUE
    if (startsWith(arg, "--msg=")) msg_text <- sub("^--msg=", "", arg)
    if (startsWith(arg, "--title=")) title_text <- sub("^--title=", "", arg)
    if (startsWith(arg, "--file=")) file_path <- sub("^--file=", "", arg)
    if (startsWith(arg, "--level=")) level_val <- toupper(sub("^--level=", "", arg))
  }
  
  if (test_mode) {
    cat("[AlertSystem] Running Webhook Test...\n")
    broadcast_alert(
      title = "🔔 Swing Trading System: Connection Test",
      body = "System notifications are active and connected.\nReady for Monday 14:00 scans and Friday 15:30 weekend reviews.",
      level = "SUCCESS"
    )
  } else if (!is.null(file_path) && file.exists(file_path)) {
    content <- paste(readLines(file_path, warn = FALSE), collapse = "\n")
    broadcast_alert(title = title_text, body = content, level = level_val)
  } else if (!is.null(msg_text)) {
    broadcast_alert(title = title_text, body = msg_text, level = level_val)
  }
}
