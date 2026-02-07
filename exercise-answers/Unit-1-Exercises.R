# =============================================================================
# Unit 1 Exercise Solutions: Introduction to Time Series & Tidyverse
# =============================================================================

# Load Libraries
library(tidyverse)
library(tidyfinance)
library(tsibble)
library(fable)
library(feasts)
library(scales)

# =============================================================================
# Exercise 1: Macro Data
# =============================================================================
# (a) Download GDP and CPI from FRED

fred <- download_data("fred", series = c("GDP", "CPIAUCNS"))
fred %>% glimpse()

# (b) Compute yearly means with index_by() and summarise()

fred_tsibble <- fred %>%
  as_tsibble(index = date, key = series, regular = FALSE)

fred_yearly <- fred_tsibble %>%
  index_by(year = ~year(.)) %>%
  group_by(series) %>%
  summarise(value_mean = mean(value))

fred_yearly

# (c) Pivot to wide format

fred_wide <- fred_yearly %>%
  as_tibble() %>%
  pivot_wider(
    id_cols = year,
    names_from = series,
    values_from = value_mean
  ) %>%
  remove_missing()

fred_wide

# =============================================================================
# Exercise 2: Stock Data
# =============================================================================
# Download a ticker of your choice and convert to a tsibble

msft <- download_data(
  "stock_prices",
  symbols = "MSFT",
  start = "2010-01-01",
  end = "2020-01-01"
)
msft %>% glimpse()

msft_ts <- msft %>%
  rename(price = adjusted_close) %>%
  select(symbol, date, price) %>%
  as_tsibble(index = date, regular = FALSE)

msft_ts

# =============================================================================
# Exercise 3: Log Returns
# =============================================================================
# (a) Compute daily log returns with mutate() and difference()

msft_returns <- msft_ts %>%
  mutate(
    lprice = log(price),
    lreturn = difference(lprice)
  )

msft_returns %>% glimpse()

# (b) Aggregate to weekly and monthly with index_by()

msft_weekly <- msft_returns %>%
  index_by(yearweek = ~yearweek(.)) %>%
  summarise(lreturn = sum(lreturn, na.rm = TRUE))

msft_monthly <- msft_returns %>%
  index_by(yearmonth = ~yearmonth(.)) %>%
  summarise(lreturn = sum(lreturn, na.rm = TRUE))

# (c) Plot all three with autoplot()

msft_returns %>% autoplot(.vars = lreturn) +
  labs(title = "MSFT Daily Log Returns")

msft_weekly %>% autoplot(.vars = lreturn) +
  labs(title = "MSFT Weekly Log Returns")

msft_monthly %>% autoplot(.vars = lreturn) +
  labs(title = "MSFT Monthly Log Returns")

# =============================================================================
# Exercise 4: Distribution
# =============================================================================
# Plot a histogram of daily returns with geom_histogram()
# and add a 5th-percentile geom_vline()

q05 <- quantile(
  msft_returns %>% remove_missing() %>% pull(lreturn),
  probs = 0.05
)
q05

msft_returns %>%
  ggplot(aes(x = lreturn)) +
  geom_histogram(bins = 100) +
  geom_vline(aes(xintercept = q05), linetype = "dashed", color = "red") +
  labs(
    x = NULL,
    y = NULL,
    title = "Distribution of daily MSFT log returns",
    subtitle = paste0("Dashed line = 5th percentile (", percent(q05, accuracy = 0.01), ")")
  ) +
  scale_x_continuous(labels = percent)
