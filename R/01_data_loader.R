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

#' Retrieve Next Upcoming Earnings Date for Symbol
#'
#' Queries Yahoo Finance calendar events via authenticated session crumb
#' and caches the result locally in `data/earnings_cache.rds`.
#'
#' @param symbol Ticker symbol.
#' @param cache_file Path to cache RDS file.
#' @return Character string (YYYY-MM-DD) or NA if unavailable.
#' @export
get_upcoming_earnings_date <- function(symbol, cache_file = "data/earnings_cache.rds") {
  symbol <- toupper(symbol)
  today_str <- as.character(Sys.Date())
  
  # Check local daily cache first (use if valid non-NA date checked today)
  cache <- if (file.exists(cache_file)) tryCatch(readRDS(cache_file), error = function(e) list()) else list()
  if (!is.null(cache[[symbol]]) && !is.na(cache[[symbol]]$earnings_date) && identical(cache[[symbol]]$checked_on, today_str)) {
    return(cache[[symbol]]$earnings_date)
  }
  
  earn_date <- NA_character_
  
  tryCatch({
    cookie_file <- tempfile(fileext = ".txt")
    on.exit(unlink(cookie_file), add = TRUE)
    
    # Obtain initial session cookie
    cmd1 <- sprintf("curl -s -c %s https://fc.yahoo.com > /dev/null", shQuote(cookie_file))
    system(cmd1)
    
    # Fetch crumb
    cmd2 <- sprintf("curl -s -b %s -A %s https://query1.finance.yahoo.com/v1/test/getcrumb",
                    shQuote(cookie_file), shQuote("Mozilla/5.0"))
    crumb <- system(cmd2, intern = TRUE)
    
    if (length(crumb) > 0 && nchar(crumb[1]) > 0 && !grepl("Too Many|Unauthorized|error", crumb[1], ignore.case = TRUE)) {
      url <- sprintf("https://query2.finance.yahoo.com/v10/finance/quoteSummary/%s?modules=calendarEvents&crumb=%s",
                     symbol, crumb[1])
      cmd3 <- sprintf("curl -s -b %s -A %s %s",
                      shQuote(cookie_file), shQuote("Mozilla/5.0"), shQuote(url))
      res <- system(cmd3, intern = TRUE)
      parsed <- jsonlite::fromJSON(paste(res, collapse = ""))
      
      dates_df <- parsed$quoteSummary$result$calendarEvents$earnings$earningsDate[[1]]
      if (!is.null(dates_df) && "fmt" %in% names(dates_df) && length(dates_df$fmt) > 0) {
        earn_date <- as.character(dates_df$fmt[1])
      }
    }
  }, error = function(e) {
    # Non-blocking fallback
  })
  
  # Save to cache
  cache[[symbol]] <- list(earnings_date = earn_date, checked_on = today_str)
  dir.create(dirname(cache_file), showWarnings = FALSE, recursive = TRUE)
  tryCatch(saveRDS(cache, cache_file), error = function(e) NULL)
  
  return(earn_date)
}
