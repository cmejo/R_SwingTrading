suppressPackageStartupMessages({
  library(xts)
})

files <- list.files("data/data_historical", pattern="data_.*\\.rds$", full.names=TRUE)
files <- files[!grepl("data_(\\^|vti|qqq|spy|vfinx|gspc|ndx|irx)", files)]

epochs <- list(
  "Epoch 5 (1988-1997)" = as.Date("1988-01-01"),
  "Epoch 1 (1995-2004)" = as.Date("1995-01-01"),
  "Epoch 2 (2000-2009)" = as.Date("2000-01-01"),
  "Epoch 3 (2007-2016)" = as.Date("2007-01-01"),
  "Epoch 4 (2012-2021)" = as.Date("2012-01-01")
)

cat(sprintf("Checking %d stock files...\n", length(files)))

for (ep in names(epochs)) {
  dt <- epochs[[ep]]
  active_count <- 0
  syms <- c()
  for (f in files) {
    d <- readRDS(f)
    idx <- as.Date(index(d))
    sub_bars <- sum(idx <= dt)
    if (sub_bars >= 500) {
      active_count <- active_count + 1
      s <- sub("^data_", "", sub("\\.rds$", "", basename(f)))
      syms <- c(syms, toupper(s))
    }
  }
  cat(sprintf("%s: %d symbols with >= 500 bars at start. Sample: %s\n", 
              ep, active_count, paste(head(syms, 10), collapse=", ")))
}
