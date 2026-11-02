#!/usr/bin/env Rscript
# ==============================================================================
# tests/smoke_core_satellite_signal.R
# End-to-end smoke test of core_satellite_signal.R on synthetic cached data in a
# SUPER-BULL regime (QQQ > SMA50 > SMA200, positive lmMA slope) -- the state in
# which the pre-fix script aborted with "object 'qqq_lm_slope' not found".
# Runs entirely offline in a temp sandbox; never touches real portfolio files.
#
# Usage (repo root):  Rscript tests/smoke_core_satellite_signal.R [path/to/core_satellite_signal.R]
# ==============================================================================
suppressPackageStartupMessages({ library(xts) })
script <- normalizePath(if (length(commandArgs(TRUE))) commandArgs(TRUE)[1] else "core_satellite_signal.R")
repo <- normalizePath(".")

sb <- tempfile("coresat_smoke_"); dir.create(file.path(sb, "data"), recursive = TRUE)
file.copy(file.path(repo, "R"), sb, recursive = TRUE)
file.copy(script, file.path(sb, "core_satellite_signal.R"))

set.seed(42)
dates <- seq(Sys.Date() - 900, Sys.Date(), by = "day")
dates <- dates[!weekdays(dates) %in% c("Saturday", "Sunday")]
n <- length(dates)
mk <- function(sym, drift, vol, mkt = NULL, beta = 1) {
  r <- rnorm(n, drift, vol) + if (is.null(mkt)) 0 else beta * mkt
  cl <- 100 * cumprod(1 + r)
  x <- xts(cbind(cl * 0.999, cl * 1.01, cl * 0.99, cl, 1e6, cl), order.by = dates)
  colnames(x) <- paste0(sym, c(".Open", ".High", ".Low", ".Close", ".Volume", ".Adjusted"))
  saveRDS(x, file.path(sb, "data", sprintf("data_%s.rds", tolower(sym))))
  r
}
mkt <- mk("QQQ", 0.0012, 0.006)                  # steady uptrend -> super-bull
mk("GLD", 0.0002, 0.008)
syms <- sprintf("ZZ%s", LETTERS[1:12])
for (i in seq_along(syms)) mk(syms[i], 0.0002 * (i - 6), 0.012, mkt, beta = 0.5 + i / 8)
writeLines(syms, file.path(sb, "symbols_smoke.txt"))

run <- function(tag, extra = character()) {
  pf <- sprintf("portfolio_%s.json", tag)
  out <- suppressWarnings(system2("Rscript", c("core_satellite_signal.R", "--symbols_file=symbols_smoke.txt",
                                               sprintf("--portfolio_file=%s", pf), "--capital=8000", extra),
                                  stdout = TRUE, stderr = TRUE, env = character()))
  st <- attr(out, "status"); if (is.null(st)) st <- 0L
  list(status = st, out = out)
}
owd <- setwd(sb); on.exit(setwd(owd), add = TRUE)

n_fail <- 0L
check <- function(desc, cond) { cat(if (isTRUE(cond)) "  PASS:" else { n_fail <<- n_fail + 1L; "  FAIL:" }, desc, "\n") }

a <- run("default")
check("default config runs to completion in a super-bull regime", a$status == 0)
check("turbo engages (super-bull leverage active)", any(grepl("SUPER-BULL LEVERAGE TURBO ACTIVE: [0-9.]+x", a$out)))
if (a$status != 0) cat(tail(a$out, 15), sep = "\n")

b <- run("features", c("--risk_target_lev=TRUE", "--sigma_target=0.30", "--mu_hat=0.30",
                       "--borrow_rate=0.12075", "--resid_mom=TRUE", "--skip_days=5"))
check("all three features enabled run to completion", b$status == 0)
check("Feature 1 block printed", any(grepl("FEATURE 1: RISK-TARGETED", b$out)))
check("residual ranking engine reported", any(grepl("RESIDUAL momentum IR", b$out)))
if (b$status != 0) cat(tail(b$out, 15), sep = "\n")

cat(sprintf("\nsandbox: %s\n%s\n", sb, if (n_fail == 0) "ALL PASS" else sprintf("%d FAILED", n_fail)))
if (n_fail > 0) quit(status = 1)
