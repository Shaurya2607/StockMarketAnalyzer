
packages <- c("quantmod", "ggplot2", "dplyr", "tidyr", "scales")

missing_packages <- packages[
  !packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

library(quantmod)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

options(stringsAsFactors = FALSE)

dir.create("data", showWarnings = FALSE)
dir.create("output", showWarnings = FALSE)

symbols <- c("RELIANCE.NS", "TCS.NS", "INFY.NS")

company_names <- c(
  "RELIANCE.NS" = "Reliance Industries",
  "TCS.NS" = "TCS",
  "INFY.NS" = "Infosys"
)

start_date <- Sys.Date() - 365 * 3
end_date <- Sys.Date()

stock_data <- list()
failed_symbols <- character()

for (symbol in symbols) {
  message("Downloading data for ", symbol)
  
  result <- tryCatch(
    suppressWarnings(
      getSymbols(
        symbol,
        src = "yahoo",
        from = start_date,
        to = end_date,
        auto.assign = FALSE
      )
    ),
    error = function(e) NULL
  )
  
  if (is.null(result) || NROW(result) == 0) {
    warning("Could not download data for ", symbol)
    failed_symbols <- c(failed_symbols, symbol)
    next
  }
  
  df <- data.frame(
    Date = as.Date(index(result)),
    Open = as.numeric(Op(result)),
    High = as.numeric(Hi(result)),
    Low = as.numeric(Lo(result)),
    Close = as.numeric(Cl(result)),
    Adjusted = as.numeric(Ad(result)),
    Volume = as.numeric(Vo(result))
  )
  
  df <- df %>%
    filter(
      !is.na(Date),
      is.finite(Adjusted),
      Adjusted > 0
    ) %>%
    arrange(Date) %>%
    distinct(Date, .keep_all = TRUE)
  
  if (nrow(df) < 2) {
    failed_symbols <- c(failed_symbols, symbol)
    next
  }
  
  df$Company <- unname(company_names[symbol])
  
  df$Daily_Return <- c(
    NA,
    diff(df$Adjusted) / head(df$Adjusted, -1) * 100
  )
  
  df$MA20 <- as.numeric(
    SMA(xts(df$Adjusted, order.by = df$Date), n = 20)
  )
  
  df$MA50 <- as.numeric(
    SMA(xts(df$Adjusted, order.by = df$Date), n = 50)
  )
  
  df$Cumulative_Return <- (
    df$Adjusted / df$Adjusted[1] - 1
  ) * 100
  
  df$Daily_Volatility <- zoo::rollapply(
    df$Daily_Return,
    width = 20,
    FUN = function(x) sd(x, na.rm = TRUE) * sqrt(252),
    fill = NA,
    align = "right",
    partial = FALSE
  )
  
  running_peak <- cummax(df$Adjusted)
  df$Drawdown <- (df$Adjusted / running_peak - 1) * 100
  
  stock_data[[symbol]] <- df
  
  write.csv(
    df,
    file.path("data", paste0(gsub("\\.", "_", symbol), ".csv")),
    row.names = FALSE
  )
}

if (length(stock_data) == 0) {
  stop("No stock data was downloaded. Check your internet connection and ticker symbols.")
}

if (length(failed_symbols) > 0) {
  message(
    "Unavailable symbols: ",
    paste(unique(failed_symbols), collapse = ", ")
  )
}

all_stocks <- bind_rows(stock_data)

write.csv(
  all_stocks,
  "output/all_stock_data.csv",
  row.names = FALSE
)

summary_table <- all_stocks %>%
  group_by(Company) %>%
  summarise(
    Start_Date = min(Date),
    End_Date = max(Date),
    Trading_Days = n(),
    Starting_Price = first(Adjusted),
    Latest_Price = last(Adjusted),
    Total_Return_Percent = (
      last(Adjusted) / first(Adjusted) - 1
    ) * 100,
    Average_Daily_Return = mean(Daily_Return, na.rm = TRUE),
    Daily_Return_StdDev = sd(Daily_Return, na.rm = TRUE),
    Annualized_Volatility = sd(Daily_Return, na.rm = TRUE) * sqrt(252),
    Best_Day_Percent = max(Daily_Return, na.rm = TRUE),
    Worst_Day_Percent = min(Daily_Return, na.rm = TRUE),
    Maximum_Drawdown_Percent = min(Drawdown, na.rm = TRUE),
    Average_Daily_Volume = mean(Volume, na.rm = TRUE),
    .groups = "drop"
  )

print(summary_table)

write.csv(
  summary_table,
  "output/stock_summary.csv",
  row.names = FALSE
)

price_plot <- ggplot(
  all_stocks,
  aes(x = Date, y = Adjusted, color = Company)
) +
  geom_line(linewidth = 0.7) +
  labs(
    title = "Historical Adjusted Stock Prices",
    x = "Date",
    y = "Adjusted Price (INR)",
    color = "Company"
  ) +
  scale_y_continuous(labels = label_number(prefix = "₹")) +
  theme_minimal(base_size = 12)

print(price_plot)

ggsave(
  "output/01_historical_prices.png",
  price_plot,
  width = 11,
  height = 6,
  dpi = 300
)

normalized_plot <- ggplot(
  all_stocks,
  aes(x = Date, y = Cumulative_Return, color = Company)
) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Cumulative Return Comparison",
    x = "Date",
    y = "Return Since Start (%)",
    color = "Company"
  ) +
  theme_minimal(base_size = 12)

print(normalized_plot)

ggsave(
  "output/02_cumulative_returns.png",
  normalized_plot,
  width = 11,
  height = 6,
  dpi = 300
)

selected_stock <- stock_data[[names(stock_data)[1]]]

moving_average_plot <- ggplot(
  selected_stock,
  aes(x = Date)
) +
  geom_line(aes(y = Adjusted, color = "Adjusted Price"), linewidth = 0.6) +
  geom_line(aes(y = MA20, color = "20-Day Average"), linewidth = 0.8, na.rm = TRUE) +
  geom_line(aes(y = MA50, color = "50-Day Average"), linewidth = 0.8, na.rm = TRUE) +
  labs(
    title = paste("Moving Average Analysis:", selected_stock$Company[1]),
    x = "Date",
    y = "Adjusted Price (INR)",
    color = "Series"
  ) +
  scale_y_continuous(labels = label_number(prefix = "₹")) +
  theme_minimal(base_size = 12)

print(moving_average_plot)

ggsave(
  "output/03_moving_averages.png",
  moving_average_plot,
  width = 11,
  height = 6,
  dpi = 300
)

return_plot <- ggplot(
  all_stocks %>% filter(!is.na(Daily_Return)),
  aes(x = Daily_Return, fill = Company)
) +
  geom_histogram(bins = 50, alpha = 0.6, position = "identity") +
  labs(
    title = "Distribution of Daily Returns",
    x = "Daily Return (%)",
    y = "Number of Trading Days",
    fill = "Company"
  ) +
  theme_minimal(base_size = 12)

print(return_plot)

ggsave(
  "output/04_daily_return_distribution.png",
  return_plot,
  width = 11,
  height = 6,
  dpi = 300
)

volume_plot <- ggplot(
  all_stocks,
  aes(x = Date, y = Volume, color = Company)
) +
  geom_line(linewidth = 0.5) +
  labs(
    title = "Daily Trading Volume",
    x = "Date",
    y = "Shares Traded",
    color = "Company"
  ) +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  theme_minimal(base_size = 12)

print(volume_plot)

ggsave(
  "output/05_trading_volume.png",
  volume_plot,
  width = 11,
  height = 6,
  dpi = 300
)

cat("\nSTOCK MARKET ANALYSIS COMPLETED\n")
cat("Companies downloaded:", length(stock_data), "\n")
cat("Output directory:", normalizePath("output"), "\n")
cat("Data directory:", normalizePath("data"), "\n")
cat("\nSummary table:\n")
print(summary_table)

if (nrow(summary_table) > 0) {
  best_stock <- summary_table$Company[
    which.max(summary_table$Total_Return_Percent)
  ]
  
  lowest_volatility <- summary_table$Company[
    which.min(summary_table$Annualized_Volatility)
  ]
  
  cat("\nHighest historical total return:", best_stock, "\n")
  cat("Lowest historical annualized volatility:", lowest_volatility, "\n")
}
