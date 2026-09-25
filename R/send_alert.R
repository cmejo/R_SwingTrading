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
    install.packages("jsonlite", repos = "https://cloud.r-project.org")
  }
  library(jsonlite)
})

# Load .env if present
load_dot_env <- function(env_path = ".env") {
  if (file.exists(env_path)) {
    lines <- readLines(env_path, warn = FALSE)
    for (line in lines) {
      line <- trimws(line)
      if (line == "" || startsWith(line, "#")) next
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
    "-s", "-X", "POST",
    "-H", "Content-Type: application/json",
    "-d", paste0("@", tmp_file),
    webhook_url
  ), stdout = TRUE, stderr = TRUE)
  
  return(TRUE)
}

send_telegram_alert <- function(text, 
                                bot_token = Sys.getenv("TELEGRAM_BOT_TOKEN"), 
                                chat_id = Sys.getenv("TELEGRAM_CHAT_ID")) {
  if (is.null(bot_token) || bot_token == "" || is.null(chat_id) || chat_id == "") {
    return(FALSE)
  }
  
  url <- sprintf("https://api.telegram.org/bot%s/sendMessage", bot_token)
  
  payload <- list(
    chat_id = chat_id,
    text = substr(text, 1, 4000),
    parse_mode = "Markdown"
  )
  
  json_body <- jsonlite::toJSON(payload, auto_unbox = TRUE)
  tmp_file <- tempfile(fileext = ".json")
  writeLines(json_body, tmp_file)
  on.exit(unlink(tmp_file), add = TRUE)
  
  res <- system2("curl", args = c(
    "-s", "-X", "POST",
    "-H", "Content-Type: application/json",
    "-d", paste0("@", tmp_file),
    url
  ), stdout = TRUE, stderr = TRUE)
  
  return(TRUE)
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
  
  discord_sent <- send_discord_alert(title, body, color)
  tg_text <- sprintf("*%s*\n\n%s", title, body)
  telegram_sent <- send_telegram_alert(tg_text)
  
  if (discord_sent || telegram_sent) {
    cat(sprintf("[AlertSystem] Notification sent: '%s' (Discord: %s, Telegram: %s)\n",
                title, ifelse(discord_sent, "YES", "NO"), ifelse(telegram_sent, "YES", "NO")))
  } else {
    cat(sprintf("[AlertSystem] Notice: Webhooks not configured. Skipping push alert for '%s'.\n", title))
    cat("  (To enable mobile alerts, set DISCORD_WEBHOOK_URL or TELEGRAM_BOT_TOKEN & TELEGRAM_CHAT_ID in .env)\n")
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
