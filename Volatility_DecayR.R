
gen_daily_leveraged_returns <- function(prices, leverage) {
  # Initialize vectors
  leveraged_returns <- numeric(length(prices))
  value <- numeric(length(prices))
  
  # Set the initial value
  value[1] <- prices[1]
  
  # Loop through each day
  for (i in 2:length(prices)) {
    # Calculate the daily leveraged return
    leveraged_returns[i] <- ((prices[i] - prices[i - 1]) / prices[i - 1]) * leverage
    
    
    value[i] <- value[i - 1] * (1 + leveraged_returns[i])
  }
  
  return(value)
}



dates <- seq(1, 21)
stockA_values <- numeric(21)
stockA_values[1] <- 100
for (i in 2:21) {
  if (i %% 2 == 0) {  
    stockA_values[i] <- stockA_values[i-1] + 20
  } else { 
    stockA_values[i] <- stockA_values[i-1] - 10
  }
}
stockB_values <- seq(100, by = 5, length.out = 21)
stocks = data.frame(Date = dates, StockA = stockA_values, StockB = stockB_values)

ggplot(stocks) +
  geom_line(aes(x = Date, y = StockA, color = "StockA"), size = 1) +
  geom_line(aes(x = Date, y = StockB, color = "StockB"), size = 1) +
  labs(
    title = "StockA and StockB Over Time",
    x = "Time (Days)",
    y = "Price",
    color = "Stocks"  # Legends will show StockA and StockB
  ) +
  scale_color_manual(values = c("StockA" = "blue", "StockB" = "green")) +  # Custom colors for each stock
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )





