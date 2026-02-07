# Load Libraries
library(tidyverse)
library(tidyfinance)
library(tsibble)
library(fable)
library(feasts)
library(scales)
library(fabletools)

# Simulate ARIMA Processes
set.seed(42)
n <- 10^2
innovations <- tibble(
  time = 1:n, 
  e = rnorm(n),
  e_lag = lag(e, default = 0)
) %>% as_tsibble(index = time)

innovations

# Plot Innovations
innovations %>% autoplot(e)
innovations %>% ggplot(aes(e)) + 
  geom_histogram(aes(y = after_stat(density))) +
  stat_function(fun = dnorm, color = "red")

# Generate ARIMA Processes
arma_processes <- innovations %>% 
  mutate(
    random_walk = accumulate(e, (\(y_lag, e) y_lag + e)),
    arma10 = accumulate(e, (\(y_lag, e) 0.9 * y_lag + e)),
    arma01 = -0.9 * e_lag + e,
    arma11 = accumulate2(e, e_lag, .f = (\(y_lag, e, e_lag) 0.9 * y_lag + 0.9 * e_lag + e), .init = 0)[-1]
  )

arma_processes

# Display ARIMA Components
arma_processes %>% gg_tsdisplay(arma10, plot_type = "partial", lag_max = 12)
arma_processes %>% gg_tsdisplay(arma01, plot_type = "partial", lag_max = 12)
arma_processes %>% gg_tsdisplay(arma11, plot_type = "partial", lag_max = 12)

# SP500 Daily Returns
sp500_returns <- download_data(
  "stock_prices", 
  symbols = "^GSPC", 
  start = "2010-01-01", 
  end = "2019-12-31"
) %>% 
  rename(price = adjusted_close) %>% 
  select(symbol, date, price) %>% 
  mutate(
    date = row_number(),
    lprice = log(price),
    lreturn = difference(lprice)
  ) %>% 
  remove_missing() %>% 
  as_tsibble(index = date) %>% 
  glimpse()

# Explore Log Prices
sp500_returns %>% gg_tsdisplay(lprice, plot_type = "partial", lag_max = 12)
sp500_returns %>% autoplot(.vars = lprice)

# Perform Stationarity Tests
sp500_returns %>% features(lprice, unitroot_kpss)
sp500_returns %>% features(lprice, unitroot_ndiffs)

# Explore Log Returns
sp500_returns %>% autoplot(.vars = lreturn)
sp500_returns %>% features(lreturn, ljung_box, lag = 12)
sp500_returns %>% gg_tsdisplay(lreturn, plot_type = "partial", lag_max = 12)

# Fit ARIMA Models
# Manual ARIMA Model
model_manual <- sp500_returns %>% 
  remove_missing() %>% 
  model(arima = ARIMA(lreturn ~ pdq(5,0,1)))

model_manual %>% report()
model_manual %>% gg_tsresiduals(lag_max = 12)

# Automatic ARIMA Model
model_auto <- sp500_returns %>% 
  remove_missing() %>%
  model(arima = ARIMA(lreturn))

model_auto %>% report() %>% coef()
model_auto %>% gg_tsresiduals(lag_max = 12)

# Compare Data and Fitted Values
model_auto %>% 
  augment() %>% 
  ggplot(aes(date)) + 
  geom_line(aes(y = lreturn, color = "Data")) + 
  geom_line(aes(y = .fitted, color = "Fitted")) +
  scale_colour_manual(values = c(Data = "black", Fitted = "#D55E00")) +
  guides(colour = guide_legend(title = NULL))

# Forecast ARIMA Model
model_auto %>% 
  forecast(h = 4) %>% 
  autoplot(sp500_returns %>% tail(52))

# Additional Useful Functions
model_manual %>% coef()
model_manual %>% glance()
model_manual %>% report()
model_manual %>% fitted()
model_manual %>% residuals()
model_manual %>% accuracy()

# Preview of Volatility Processes
sp500_returns %>% 
  mutate(lreturn_sq = lreturn^2) %>% 
  autoplot(.vars = lreturn_sq)
