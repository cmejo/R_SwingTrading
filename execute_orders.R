#!/usr/bin/env Rscript
#' R CLI Wrapper for Multi-Broker Order Execution (IBKR & Charles Schwab)
#'
#' Usage:
#'   Rscript execute_orders.R --broker=ibkr --dry_run=TRUE
#'   Rscript execute_orders.R --broker=schwab --dry_run=TRUE
#'   Rscript execute_orders.R --broker=ibkr --dry_run=FALSE --port=7497

args <- commandArgs(trailingOnly = TRUE)

broker <- "ibkr"
dry_run <- "true"
port <- "7497"
ticket_file <- "LATEST_TICKET.txt"
symbols <- ""
interactive_flag <- FALSE
yes_flag <- FALSE
sync_flag <- FALSE

for (arg in args) {
  if (startsWith(arg, "--broker=")) broker <- tolower(sub("^--broker=", "", arg))
  if (startsWith(arg, "--dry_run=")) dry_run <- tolower(sub("^--dry_run=", "", arg))
  if (startsWith(arg, "--port=")) port <- sub("^--port=", "", arg)
  if (startsWith(arg, "--ticket_file=")) ticket_file <- sub("^--ticket_file=", "", arg)
  if (startsWith(arg, "--symbols=")) symbols <- sub("^--symbols=", "", arg)
  if (arg == "--interactive" || arg == "-i") interactive_flag <- TRUE
  if (arg == "--yes" || arg == "-y") yes_flag <- TRUE
  if (arg == "--sync") sync_flag <- TRUE
}

python_bin <- Sys.which("python3")
if (python_bin == "") {
  python_bin <- "/usr/bin/python3"
}

cmd_args <- c(
  "execute_broker.py",
  sprintf("--broker=%s", broker),
  sprintf("--dry_run=%s", dry_run),
  sprintf("--ticket_file=%s", ticket_file),
  sprintf("--port=%s", port)
)
if (symbols != "") {
  cmd_args <- c(cmd_args, sprintf("--symbols=%s", symbols))
}
if (interactive_flag) {
  cmd_args <- c(cmd_args, "--interactive")
}
if (yes_flag) {
  cmd_args <- c(cmd_args, "--yes")
}
if (sync_flag) {
  cmd_args <- c(cmd_args, "--sync")
}

exit_code <- system2(python_bin, args = cmd_args)
quit(save = "no", status = exit_code)
