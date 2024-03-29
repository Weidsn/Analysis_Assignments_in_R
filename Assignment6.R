library(tidyverse)
library(forecast)

# load the data
df_retail <- read.csv("retail_data.csv")
glimpse(df_retail)
tail(df_retail)

# Create time series on Sales
ts_retail <- ts(df_retail$Sales,
                start = c(2018,1),
                end = c(2023,12),
                frequency = 12)
glimpse(ts_retail)
ts_retail_log <- log(ts_retail)
glimpse(ts_retail_log)

# Plot the decomposition
decompose(ts_retail_log) |> 
  plot()

# Apply ARIMA and display summary
arima_retail <- auto.arima(log(ts_retail))
summary(arima_retail)

# Forecast the next four months with summary, accuracy and plot
fc_retail_4 <- forecast(arima_retail, h = 4)
fc_retail_4
plot(fc_retail_4)


