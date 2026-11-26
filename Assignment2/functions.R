# BDA400 Assignment 2 - Technical Analysis using R (Preliminary Stage)
# functions.R : load stock data and compute basic statistics

# install.packages(c("quantmod", "TTR"))   # run once
library(quantmod)
library(TTR)

# ---- Step 5: read portfolio.txt and import data for each symbol ----
load_stock_data <- function(file = "portfolio.txt",
                            from = Sys.Date() - 365, to = Sys.Date()) {
  symbols <- trimws(readLines(file, warn = FALSE))
  symbols <- symbols[symbols != ""]
  stocks <- list()
  for (sym in symbols) {
    xts_data <- getSymbols(sym, src = "yahoo", from = from, to = to,
                           auto.assign = FALSE)
    df <- data.frame(Date = index(xts_data), coredata(xts_data))
    colnames(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
    stocks[[sym]] <- df                       # separate data frame per symbol
    assign(paste0(sym, "_df"), df, envir = .GlobalEnv)
    cat("Loaded", sym, ":", nrow(df), "rows\n")
  }
  return(stocks)
}

# ---- helper: statistical mode ----
get_mode <- function(x) {
  x <- round(na.omit(x), 2)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# ---- Step 6: moving average, mean, mode, median, standard deviation ----
calculate_statistics <- function(df, ma_period = 20) {
  close <- df$Close
  df$SMA <- SMA(close, n = ma_period)        # moving average (TTR)
  stats <- data.frame(
    Mean            = mean(close, na.rm = TRUE),
    Mode            = get_mode(close),
    Median          = median(close, na.rm = TRUE),
    Std_Deviation   = sd(close, na.rm = TRUE),
    Latest_MA       = tail(na.omit(df$SMA), 1),
    MA_Period_Days  = ma_period
  )
  list(data = df, stats = stats)
}

# apply to all stocks, return one structured table
calculate_all_statistics <- function(stocks, ma_period = 20) {
  results <- lapply(stocks, calculate_statistics, ma_period = ma_period)
  table <- do.call(rbind, lapply(names(results), function(s)
    cbind(Symbol = s, results[[s]]$stats)))
  list(results = results, summary = table)
}
