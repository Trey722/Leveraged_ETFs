update_time_data <- function(data, ticker)
{
  data$Year <- as.numeric(substr(data$Date, 1, 4))
  data$Month <- as.numeric(substr(data$Date, 6, 7))
  data$DecimalYear <- data$Year + ((data$Month - 1) / 12)
  data$Ticker <- ticker
  data$dividendYield <- data$Dividends / data$Open
  
  return(data)
}

update_for_base_year <- function(data1, baseTime) {
 
  base_open <- data1$Open[data1$DecimalYear == baseTime]
  base_high <- data1$High[data1$DecimalYear == baseTime]
  base_close <- data1$Close[data1$DecimalYear == baseTime]
  base_low <- data1$Low[data1$DecimalYear == baseTime]
  
  data1$Open <- (data1$Open / base_open) * 100
  data1$High <- (data1$High / base_high) * 100
  data1$Close <- (data1$Close / base_close) * 100
  data1$Low <- (data1$Low / base_low) * 100
  
  return(data1)
}

library(dplyr)
library(ggplot2)


UPRO_data <- update_time_data(read.csv("UPRO.csv"), "UPRO")
tqqq_data <- update_time_data(read.csv("TQQQ.csv"), "TQQQ")
qld_data <-  update_time_data(read.csv("QLD.csv"), "QLD")
sso_data <- update_time_data(read.csv("SSO.csv"), "SSO")
spy_data <- update_time_data(read.csv("SPY.csv"), "SPY")
qqq_data <- update_time_data(read.csv("QQQ.csv"), "QQQ")


combined_data <- rbind(UPRO_data, tqqq_data, qld_data, sso_data)

index_data <- data.frame( Year = qld_data$Year,
                          Month = qld_data$Month,
                          DecimalYear = qld_data$DecimalYear,
                          Volume = 0)


for (i in 1:nrow(index_data))
{
  index_data$Volume[i] = sum(combined_data$Volume[combined_data$Year == index_data$Year[i] & combined_data$Month == index_data$Month[i]])
}



ggplot() +
  geom_line(data = update_for_base_year(qld_data[qld_data$DecimalYear >= 2006.5, ], 2006.5), aes(x =DecimalYear, y =Open), color = "blue", size = 1) +
  geom_line(data = update_for_base_year(qqq_data[qqq_data$DecimalYear >= 2006.5, ], 2006.5), aes(x =DecimalYear, y =Open), color = "green", size = 1) +
  labs(
    title = "Monthly Prices Over Time",
    x = "Year",
    y = "Price"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )
