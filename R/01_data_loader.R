#' Data Loader for SNDK Stock
#'
#' Fetches daily OHLCV data for SNDK from Yahoo Finance (via quantmod)
#' or falls back to locally cached data / custom CSV files.

suppressMessages({
  library(xts)
  library(zoo)
  library(quantmod)
})

load_stock_data <- function(symbol = "SNDK", 
                            from = "2020-01-01", 
                            to = Sys.Date(), 
                            cache_file = NULL,
                            csv_file = NULL) {
  
  if (is.null(cache_file)) {
    cache_file <- file.path("data", sprintf("data_%s.rds", tolower(symbol)))
  }
  
  data_xts <- NULL
  
  # Option 1: Load from custom CSV file if specified
  if (!is.null(csv_file) && file.exists(csv_file)) {
    cat(sprintf("[DataLoader] Loading data from CSV: %s\n", csv_file))
    df <- read.csv(csv_file, stringsAsFactors = FALSE)
    # Check date column
    date_col <- grep("date|Date|DATE", names(df), value = TRUE)[1]
    if (is.na(date_col)) stop("CSV file must have a date column")
    dates <- as.Date(df[[date_col]])
    df_vals <- df[, setdiff(names(df), date_col), drop = FALSE]
    data_xts <- xts(df_vals, order.by = dates)
  }
  
  # Option 2: Try fetching live via quantmod getSymbols
  if (is.null(data_xts)) {
    cat(sprintf("[DataLoader] Attempting to fetch '%s' from Yahoo Finance (%s to %s)...\n", 
                symbol, from, as.character(to)))
    tryCatch({
      data_xts <- getSymbols(symbol, src = "yahoo", from = from, to = to, auto.assign = FALSE)
      cat(sprintf("[DataLoader] Successfully fetched %d bars from Yahoo Finance.\n", nrow(data_xts)))
      # Cache downloaded data
      if (!is.null(cache_file)) {
        saveRDS(data_xts, cache_file)
        cat(sprintf("[DataLoader] Cached data saved to %s\n", cache_file))
      }
    }, error = function(e) {
      cat(sprintf("[DataLoader] Live fetch failed (%s). Checking cache...\n", e$message))
    })
  }
  
  # Option 3: Fall back to cached RDS if live fetch failed
  if (is.null(data_xts) && !is.null(cache_file) && file.exists(cache_file)) {
    cat(sprintf("[DataLoader] Loading cached dataset from %s\n", cache_file))
    data_xts <- readRDS(cache_file)
  }
  
  if (is.null(data_xts) || nrow(data_xts) == 0) {
    stop(sprintf("Failed to obtain price data for '%s'. Provide valid CSV or network connection.", symbol))
  }
  
  # Ensure standard column naming
  colnames(data_xts) <- gsub(paste0("^", symbol, "\\."), "", colnames(data_xts))
  
  cat(sprintf("[DataLoader] Loaded %d bars for %s [%s to %s]\n",
              nrow(data_xts), symbol,
              as.character(index(first(data_xts))), 
              as.character(index(last(data_xts)))))
  
  return(data_xts)
}
