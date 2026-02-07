# =============================================================================
# Unit 3 Exercise Solutions: ARMA/ARIMA Models & Forecasting
# =============================================================================

# Load Libraries
library(tidyverse)
library(tidyfinance)
library(tsibble)
library(fable)
library(feasts)
library(scales)
library(fabletools)

# =============================================================================
# Exercise 1: Simulation
# =============================================================================
# (a) Simulate AR(1), MA(1) and ARMA(1,1) processes using accumulate()

set.seed(42)
n <- 200

innovations <- tibble(
  time = 1:n,
  e = rnorm(n),
  e_lag = lag(e, default = 0)
) %>% as_tsibble(index = time)

arma_processes <- innovations %>%
  mutate(
    ar1   = accumulate(e, \(y_lag, e) 0.9 * y_lag + e),
    ma1   = -0.9 * e_lag + e,
    arma11 = accumulate2(
      e, e_lag,
      .f = \(y_lag, e, e_lag) 0.9 * y_lag + 0.9 * e_lag + e,
      .init = 0
    )[-1]
  )

# (b) Plot ACF/PACF for each and verify the identification table

# AR(1): ACF decays gradually, PACF cuts off at lag 1
arma_processes %>% gg_tsdisplay(ar1, plot_type = "partial", lag_max = 20)

# MA(1): ACF cuts off at lag 1, PACF decays gradually
arma_processes %>% gg_tsdisplay(ma1, plot_type = "partial", lag_max = 20)

# ARMA(1,1): Both ACF and PACF decay gradually
arma_processes %>% gg_tsdisplay(arma11, plot_type = "partial", lag_max = 20)

# =============================================================================
# Exercise 2: Fit
# =============================================================================
# (a) Download log returns for a stock of your choice

tsla <- download_data(
  "stock_prices",
  symbols = "TSLA",
  start = "2015-01-01",
  end = "2020-01-01"
)

tsla_returns <- tsla %>%
  rename(price = adjusted_close) %>%
  select(symbol, date, price) %>%
  mutate(
    date = row_number(),
    lprice = log(price),
    lreturn = difference(lprice)
  ) %>%
  remove_missing() %>%
  as_tsibble(index = date)

tsla_returns %>% glimpse()

# (b) Fit an automatic ARIMA with ARIMA(lreturn)

model_auto <- tsla_returns %>%
  model(arima = ARIMA(lreturn))

# (c) Use report() to inspect the selected order

model_auto %>% report()
model_auto %>% coef()
model_auto %>% glance()

# =============================================================================
# Exercise 3: Diagnose
# =============================================================================
# (a) Run gg_tsresiduals()

model_auto %>% gg_tsresiduals(lag_max = 12)

# (b) Do the residuals resemble white noise?
# Check: ACF values should fall within the 95% confidence bands.
# The histogram should be approximately bell-shaped.
# The Ljung-Box test should fail to reject:

model_auto %>% augment() %>% features(.innov, ljung_box, lag = 12)

# =============================================================================
# Exercise 4: Forecast
# =============================================================================
# (a) Forecast 5 days ahead

fc <- model_auto %>% forecast(h = 5)

# (b) Plot the fan chart -- what happens to point forecasts?

fc %>% autoplot(tsla_returns %>% tail(60)) +
  labs(
    title = "TSLA Log Returns: 5-Day Forecast",
    y = "Log Return"
  )

# The point forecast quickly reverts to zero (the unconditional mean).
# The prediction intervals widen as the horizon increases.

# =============================================================================
# Exercise 5: Preview
# =============================================================================
# Plot squared log returns. Do you observe volatility clustering?

tsla_returns %>%
  mutate(lreturn_sq = lreturn^2) %>%
  autoplot(.vars = lreturn_sq) +
  labs(
    title = "TSLA Squared Log Returns",
    subtitle = "Clusters of high values indicate volatility clustering",
    y = "Squared Log Return"
  )

# Yes -- periods of large squared returns cluster together,
# indicating that high-volatility episodes persist over time.
# This motivates the GARCH models covered in Unit 4.
