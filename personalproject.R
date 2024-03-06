#BIG COMPANIES

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(caret)
library(randomForest)
library(zoo) # for na.approx()
library(reshape2)

# Load the dataset
stock <- read.csv("Stock Market Dataset.csv", stringsAsFactors = FALSE)

# Convert Date column to Date format
stock$Date <- dmy(stock$Date)

# Select columns for specified stocks
selected_stocks <- stock %>%
  select(Date, Amazon_Price, Microsoft_Price, Nvidia_Price, Meta_Price)

# Fill missing values with linear interpolation
selected_stocks_filled <- selected_stocks %>%
  mutate_at(vars(-Date), na.approx, na.rm = FALSE)

# Melt the dataset to long format for easier plotting
selected_stocks_melted <- melt(selected_stocks_filled, id.vars = "Date")

# Plotting time series of selected stocks
ggplot(selected_stocks_melted, aes(x = Date, y = value, color = variable)) +
  geom_line() +
  facet_wrap(~ variable, scales = "free_y", ncol = 1) +
  labs(title = "Time Series of Selected Stock Prices",
       y = "Price (USD)",
       color = "Stock") +
  theme_minimal()

#################### NEXT PART

# Fill missing values with linear interpolation
selected_stocks_filled <- selected_stocks %>%
  mutate_at(vars(-Date), na.approx, na.rm = FALSE)

# Melt the dataset to long format for easier plotting
selected_stocks_melted <- melt(selected_stocks_filled, id.vars = "Date")



# Function to predict future prices using linear regression
# Function to predict future prices using linear regression
predict_future <- function(stock_data, stock_name, future_date) {
  stock_data_subset <- stock_data %>%
    select(Date, matches(stock_name)) %>%
    rename(Price = matches(stock_name))
  
  lm_model <- lm(Price ~ Date, data = stock_data_subset)
  future_price <- predict(lm_model, newdata = data.frame(Date = future_date))
  return(future_price)
}

# Predict future prices for each stock for 2025, 2030, and 2050
future_dates_2025 <- as.Date("2025-12-31")
future_dates_2030 <- as.Date("2030-12-31")
future_dates_2050 <- as.Date("2050-12-31")

# Predictions for 2025
amazon_2025 <- predict_future(selected_stocks, "Amazon", future_dates_2025)
microsoft_2025 <- predict_future(selected_stocks, "Microsoft", future_dates_2025)
nvidia_2025 <- predict_future(selected_stocks, "Nvidia", future_dates_2025)
meta_2025 <- predict_future(selected_stocks, "Meta", future_dates_2025)

# Predictions for 2030
amazon_2030 <- predict_future(selected_stocks, "Amazon", future_dates_2030)
microsoft_2030 <- predict_future(selected_stocks, "Microsoft", future_dates_2030)
nvidia_2030 <- predict_future(selected_stocks, "Nvidia", future_dates_2030)
meta_2030 <- predict_future(selected_stocks, "Meta", future_dates_2030)

# Predictions for 2050
amazon_2050 <- predict_future(selected_stocks, "Amazon", future_dates_2050)
microsoft_2050 <- predict_future(selected_stocks, "Microsoft", future_dates_2050)
nvidia_2050 <- predict_future(selected_stocks, "Nvidia", future_dates_2050)
meta_2050 <- predict_future(selected_stocks, "Meta", future_dates_2050)

#paste("Predicted Price of Amazon on Dec 31st 2025: $", round(amazon_2025,2))
#paste("Predicted Price of Microsoft on Dec 31st 2025: $", round(microsoft_2025,2))
#paste("Predicted Price of Nvidia on Dec 31st 2025:$", round(nvidia_2025,2))
#paste("Predicted Price of Meta on Dec 31st 2025:$", round(meta_2025,2))

#paste("Predicted Price of Amazon on Dec 31st 2030: $", round(amazon_2030,2))
#paste("Predicted Price of Microsoft on Dec 31st 2030: $", round(microsoft_2030,2))
#paste("Predicted Price of Nvidia on Dec 31st 2030:$", round(nvidia_2030,2))
#paste("Predicted Price of Meta on Dec 31st 2030:$", round(meta_2030,2))

#paste("Predicted Price of Amazon on Dec 31st 2050: $", round(amazon_2050,2))
#paste("Predicted Price of Microsoft on Dec 31st 2050: $", round(microsoft_2050,2))
#paste("Predicted Price of Nvidia on Dec 31st 2050:$", round(nvidia_2050,2))
#paste("Predicted Price of Meta on Dec 31st 2050:$", round(meta_2050,2))
print("Prices on DEC 31st:")
future_prices <- data.frame(
  Year = c(2025, 2030, 2050),
  Amazon = c(round(amazon_2025, 2), round(amazon_2030, 2), round(amazon_2050, 2)),
  Microsoft = c(round(microsoft_2025, 2), round(microsoft_2030, 2), round(microsoft_2050, 2)),
  Nvidia = c(round(nvidia_2025, 2), round(nvidia_2030, 2), round(nvidia_2050, 2)),
  Meta = c(round(meta_2025, 2), round(meta_2030, 2), round(meta_2050, 2))
)

# Print the table
print(future_prices)
