suppressPackageStartupMessages(library(xts))
# Check how many stocks qualify each day in 10yr test
sim_data <- list()
cache_files <- list.files("data/cache_10yr", pattern="_10yr_wf\\.rds$", full.names=TRUE)
for (cf in cache_files) {
  obj <- readRDS(cf)
  sim_data[[obj$symbol]] <- obj
}
qqq <- readRDS("data/data_10yr/data_qqq.rds")
eval_dates <- tail(sort(index(qqq)), 2520)

n_qual_per_day <- numeric(length(eval_dates))
for (t in seq_along(eval_dates)) {
  d <- eval_dates[t]
  cnt <- 0
  for (s in names(sim_data)) {
    obj <- sim_data[[s]]
    idx_p <- match(d, index(obj$pred_prob))
    idx_rs <- match(d, index(obj$rs_spy))
    if (!is.na(idx_p) && !is.na(idx_rs)) {
      if (as.numeric(obj$pred_prob)[idx_p] >= 0.58 && as.numeric(obj$rs_spy)[idx_rs] > 0) {
        cnt <- cnt + 1
      }
    }
  }
  n_qual_per_day[t] <- cnt
}

cat("Mean qualifying stocks per day:", mean(n_qual_per_day), "\n")
cat("Min qualifying:", min(n_qual_per_day), "Max qualifying:", max(n_qual_per_day), "\n")
cat("Days with 0 qualifying:", sum(n_qual_per_day == 0), "out of", length(eval_dates), "\n")
