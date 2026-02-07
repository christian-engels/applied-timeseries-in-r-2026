# Load Libraries
library(tidyverse)
library(tidyfinance)
library(tsibble)
library(fable)
library(feasts)
library(scales)

# Part A: Tidyverse

# Download Data
fred <- download_data("fred", series = c("GDP", "CPIAUCNS"))

# View Data
View(fred)
fred %>% View()

# Summary of Data
fred %>% glimpse()

# Filter CPIAUCNS Series
fred_cpi <- fred %>% filter(series == "CPIAUCNS")

# Summary Statistics for CPIAUCNS
fred_cpi_summary <- fred_cpi %>% 
  summarise(
    date_min = min(date),
    date_max = max(date),
    value_mean = mean(value)
  )

# Summary for All Series
fred_summary <- fred %>% 
  group_by(series) %>% 
  summarise(
    date_min = min(date),
    date_max = max(date),
    value_mean = mean(value)
  )

# Yearly Mean Value
fred_yearly_mean <- fred %>% 
  group_by(series, year = year(date)) %>% 
  summarise(value_mean = mean(value))

# Pivot Yearly Mean Data
fred_pivot <- fred_yearly_mean %>% 
  pivot_wider(
    id_cols = year,
    names_from = series,
    values_from = value_mean
  )

# Summarize and Reshape Yearly Data
fred_yearly <- fred_yearly_mean %>% 
  pivot_wider(
    id_cols = year,
    names_from = series,
    values_from = value_mean
  ) %>% 
  remove_missing()

# Inspect GDP Data
GDP <- fred_yearly %>% 
  select(year, GDP) %>% 
  glimpse()

# Plot GDP Over Time
GDP %>% 
  as_tsibble(index = year) %>% 
  autoplot(.vars = GDP)

# Logarithmic GDP Analysis
GDP %>% 
  as_tsibble(index = year) %>% 
  mutate(log_GDP = log(GDP)) %>% 
  autoplot(.vars = log_GDP)

# Part B: Time Series Data


# Download Apple Stock Data
AAPL <- download_data(
  "stock_prices", 
  symbols = "AAPL", 
  start = "2010-01-01", 
  end = "2020-01-01"
)
AAPL %>% glimpse()

# Prepare Closing Price Data
closing_price <- AAPL %>% 
  rename(price = adjusted_close) %>% 
  select(symbol, date, price) %>% 
  as_tsibble(index = date, regular = FALSE) %>% 
  glimpse()

# Calculate Logarithmic Returns
log_returns <- closing_price %>% 
  mutate(
    lprice = log(price),
    lreturn = difference(lprice, lag = 1, differences = 1)
  ) %>% 
  glimpse()

# Visualize Prices and Returns
# Price Over Time
log_returns %>% autoplot(.vars = price)

# Log Price Over Time
log_returns %>% autoplot(.vars = lprice)

# Log Returns Over Time
log_returns %>% autoplot(.vars = lreturn)

# Quantile Analysis
quantile_05 <- quantile(
  log_returns %>% 
    remove_missing() %>% 
    pull(lreturn), 
  probs = 0.05
)
quantile_05

# Plot Distribution of Daily Returns
log_returns %>% 
  ggplot(aes(x = lreturn)) +
  geom_histogram(bins = 100) +
  geom_vline(aes(xintercept = quantile_05),
             linetype = "dashed") +
  labs(
    x = NULL,
    y = NULL,
    title = "Distribution of daily Apple stock returns"
  ) +
  scale_x_continuous(labels = percent)

# Aggregate Weekly Log Returns
log_returns_weekly <- log_returns %>% 
  index_by(yearweek = ~yearweek(.)) %>% 
  summarise(lreturn = sum(lreturn)) %>% 
  glimpse()

log_returns_weekly %>% autoplot()

# Aggregate Monthly Log Returns
log_returns_monthly <- log_returns %>% 
  index_by(yearmonth = ~yearmonth(.)) %>% 
  summarise(lreturn = sum(lreturn)) %>% 
  glimpse()

log_returns_monthly %>% autoplot()
