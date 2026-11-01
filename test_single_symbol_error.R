suppressPackageStartupMessages({
  library(xts)
  library(quantmod)
  library(TTR)
})

source("R/01_data_loader.R")
source("R/02_trend_lmMA.R")
source("R/03_volatility_garch.R")
source("R/04_feature_pipeline.R")

spy <- readRDS("data/data_10yr/data_spy.rds")
aapl <- readRDS("data/data_10yr/data_aapl.rds")

c_idx <- intersect(index(aapl), index(spy))
aapl_sub <- aapl[c_idx]
spy_sub  <- spy[c_idx]

pipe_res <- build_feature_dataset(aapl_sub, benchmark_ohlcv = spy_sub, look_ahead = 7, use_garch = FALSE)
df_model <- pipe_res$model_data
df_model <- na.omit(df_model)
cat("df_model rows:", nrow(df_model), "cols:", ncol(df_model), "\n")
cat("Colnames:", paste(names(df_model), collapse=", "), "\n")

# Check glm
fit_init <- tryCatch(glm(target ~ ., data = df_model[1:100, ], family = binomial), error = function(e) e)
if (inherits(fit_init, "error")) {
  cat("Error in glm:", fit_init$message, "\n")
} else {
  cat("glm success!\n")
}
