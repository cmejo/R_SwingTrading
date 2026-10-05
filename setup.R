#!/usr/bin/env Rscript
#' Environment Setup Script for Quantitative Swing Trading System
#' Ensures all required CRAN packages are installed and available.

cat("========================================================================\n")
cat(" Setting up R environment for Quantitative Swing Trading System\n")
cat("========================================================================\n\n")

user_lib <- Sys.getenv("R_LIBS_USER")
if (user_lib != "") {
  dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
  .libPaths(c(user_lib, .libPaths()))
}

needed <- c("xts", "zoo", "quantmod", "TTR", "glmnet", "tseries", "jsonlite")
installed <- rownames(installed.packages())
to_install <- needed[!(needed %in% installed)]

if (length(to_install) > 0) {
  cat(sprintf("Installing %d missing R packages: %s\n", length(to_install), paste(to_install, collapse = ", ")))
  install.packages(to_install, repos = "https://cloud.r-project.org")
} else {
  cat("All required R packages are already installed:\n")
  for (pkg in needed) cat(sprintf("  ✓ %s\n", pkg))
}

cat("\nEnvironment verification:\n")
for (pkg in needed) {
  ok <- suppressWarnings(suppressPackageStartupMessages(require(pkg, character.only = TRUE)))
  cat(sprintf("  [%s] %s\n", ifelse(ok, "OK", "FAIL"), pkg))
}

cat("\nSetup complete.\n")
