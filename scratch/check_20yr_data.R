suppressPackageStartupMessages(library(xts))

files <- list.files("data/data_historical", pattern = "^data_.*\\.rds$", full.names = TRUE)
bench_patterns <- c("data_\\^", "data_vti", "data_qqq", "data_spy", "data_vfinx", "data_gspc", "data_ndx", "data_irx", "data_gld")
for (bp in bench_patterns) {
  files <- files[!grepl(bp, files)]
}

cat("Total stock files in data_historical:", length(files), "\n")

has_2006 <- 0
valid_syms <- c()

for (f in files) {
  d <- tryCatch(readRDS(f), error = function(e) NULL)
  if (!is.null(d) && nrow(d) > 0) {
    s_date <- as.Date(index(d)[1])
    e_date <- as.Date(tail(index(d), 1))
    if (s_date <= as.Date("2006-10-01") && e_date >= as.Date("2026-09-01")) {
      has_2006 <- has_2006 + 1
      valid_syms <- c(valid_syms, sub("^data_", "", sub("\\.rds$", "", basename(f))))
    }
  }
}

count_150 <- 0
for (f in files) {
  d <- tryCatch(readRDS(f), error = function(e) NULL)
  if (!is.null(d)) {
    sub_d <- d[index(d) <= as.Date("2006-10-02")]
    if (nrow(sub_d) >= 150) count_150 <- count_150 + 1
  }
}
cat("Stocks with >= 150 bars on 2006-10-02:", count_150, "\n")
