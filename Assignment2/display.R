# BDA400 Assignment 2 - display.R : display data, statistics and visualizations
source("functions.R")

# ---- Step 7: display utilities ----
display_stock_data <- function(stocks, n = 6) {
  for (sym in names(stocks)) {
    cat("\n==========", sym, "==========\n")
    cat("Rows:", nrow(stocks[[sym]]), " From:", format(min(stocks[[sym]]$Date)),
        " To:", format(max(stocks[[sym]]$Date)), "\n")
    cat("-- First rows --\n"); print(head(stocks[[sym]], n))
    cat("-- Last rows --\n");  print(tail(stocks[[sym]], n))
    cat("-- Summary --\n");    print(summary(stocks[[sym]][, -1]))
  }
}

display_statistics <- function(summary_table) {
  cat("\n========== Basic Statistics (Close price) ==========\n")
  print(format(summary_table, digits = 4), row.names = FALSE)
}

# Yahoo-Finance style quote line for each stock
display_quote <- function(stocks) {
  for (sym in names(stocks)) {
    df <- stocks[[sym]]; last <- tail(df, 1); prev <- df[nrow(df) - 1, ]
    chg <- last$Close - prev$Close
    cat(sprintf("%-6s Close: %8.2f  Change: %+7.2f (%+.2f%%)  Day range: %.2f - %.2f  Volume: %s\n",
        sym, last$Close, chg, 100 * chg / prev$Close, last$Low, last$High,
        format(last$Volume, big.mark = ",")))
  }
}

plot_stock <- function(sym, ma_period = 20) {
  x <- getSymbols(sym, src = "yahoo", from = Sys.Date() - 365, auto.assign = FALSE)
  chartSeries(x, name = sym, theme = chartTheme("white"),
              TA = paste0("addSMA(n=", ma_period, ");addVo()"))
}

plot_close_with_ma <- function(res, sym) {
  df <- res$results[[sym]]$data
  plot(df$Date, df$Close, type = "l", col = "steelblue", xlab = "Date",
       ylab = "Price (USD)", main = paste(sym, "Close Price & Moving Average"))
  lines(df$Date, df$SMA, col = "red", lwd = 2)
  abline(h = res$results[[sym]]$stats$Mean, lty = 2, col = "darkgreen")
  legend("topleft", c("Close", "SMA", "Mean"),
         col = c("steelblue", "red", "darkgreen"), lty = c(1, 1, 2), bty = "n")
}

plot_statistics <- function(summary_table) {
  m <- t(as.matrix(summary_table[, c("Mean", "Median", "Mode", "Std_Deviation")]))
  colnames(m) <- summary_table$Symbol
  barplot(m, beside = TRUE, legend.text = rownames(m), main = "Statistics by Stock",
          ylab = "USD", col = c("steelblue", "orange", "grey", "red"))
}

# ---- run ----
stocks <- load_stock_data("portfolio.txt")
res    <- calculate_all_statistics(stocks, ma_period = 20)

display_stock_data(stocks)
display_quote(stocks)
display_statistics(res$summary)
View(res$summary)                      # table view in RStudio

for (sym in names(stocks)) {
  plot_stock(sym)                      # candlestick + SMA + volume
  plot_close_with_ma(res, sym)         # close vs moving average
}
plot_statistics(res$summary)
