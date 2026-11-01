source("R/08_metrics.R")
# Test calc_performance_metrics
x <- xts(rnorm(100, 0.001, 0.01), order.by = Sys.Date() - 100:1)
m <- calc_performance_metrics(x)$raw
print(names(m))
print(m)
