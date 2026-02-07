library(tidyverse)
library(tidyfinance)
library(tsibble)
library(feasts)
library(rugarch)
library(PortfolioAnalytics)
library(xts)


# Download daily stock prices
if (!file.exists("sp500_download.csv")) {

  # Download constituents
  constituents <- download_data("constituents", index = "S&P 500")

  sp500_download <-
    download_data(
      "stock_prices",
      symbols = constituents %>% pull(symbol),
      start_date = "2014-11-18",
      end_date = "2024-11-18"
    )
  sp500_download %>% write_csv("sp500_download.csv")
} else {
  sp500_download <- read_csv("sp500_download.csv")
}

# Compute daily returns
sp500_returns <-
  sp500_download %>%
  select(symbol, date, adjusted_close) %>%
  group_by(symbol) %>%
  arrange(date) %>% # Ensure data is sorted by date
  mutate(
    return = adjusted_close / lag(adjusted_close) - 1
  ) %>%
  ungroup() %>%
  drop_na(return) %>% # Remove NAs resulting from lag
  group_by(symbol) %>%
  mutate(n_days = n()) %>%
  ungroup() %>%
  filter(n_days == max(n_days)) %>% # Keep symbols with full data
  select(symbol, date, return)

# Pivot to wide format using 'date'
sp500_wide <-
  sp500_returns %>%
  pivot_wider(
    id_cols = date,
    values_from = return,
    names_from = symbol
  ) %>%
  mutate(date = as_date(date)) %>%
  glimpse()

# Prepare returns data frame
returns <- sp500_wide %>%
  select(-date) %>%
  as.data.frame()
rownames(returns) <- sp500_wide$date
funds <- colnames(returns)

# Define the portfolio
init_portf <-
  portfolio.spec(assets = funds) %>%
  add.constraint(type = "weight_sum", min_sum = 0.99, max_sum = 1.01) %>%
  add.constraint(type = "box", min = 0, max = 1) %>%
  add.objective(type = "risk", name = "StdDev")
print(init_portf)

# Optimize the portfolio
min_sd <-
  optimize.portfolio(
    R = returns,
    portfolio = init_portf,
    optimize_method = "osqp",
    trace = TRUE
  )
min_sd

# Extract weights
weights <-
  tibble(
    symbol = names(min_sd$weights),
    weight = round(min_sd$weights, 2)
  )

# Compute portfolio returns using daily data
portfolio_returns <-
  sp500_returns %>%
  left_join(weights, by = "symbol") %>%
  group_by(date) %>%
  summarise(
    portfolio_return = sum(return * weight, na.rm = TRUE),
  ) %>%
  mutate(portfolio_return_sq = portfolio_return^2)

# Plot portfolio returns
portfolio_returns %>%
  pivot_longer(starts_with("portfolio")) %>%
  ggplot(aes(date, value)) +
  geom_line() +
  facet_wrap(~name, scales = "free", nrow = 2)

# ARIMA model on daily data
portfolio_returns %>%
  mutate(time = row_number()) %>%
  as_tsibble(index = time) %>%
  model(arima = fable::ARIMA(portfolio_return))

# Standard GARCH(1,1) model specification
spec <-
  ugarchspec(
    variance.model = list(
      model = "fGARCH",
      submodel = "TGARCH",
      garchOrder = c(1, 1)
    ),
    mean.model = list(
      armaOrder = c(2, 2),
      include.mean = TRUE
    ),
    distribution.model = "std"
  )

# Fit GARCH model
portfolio_fit <-
  ugarchfit(
    spec = spec,
    data = portfolio_returns$portfolio_return
  )
portfolio_fit

# Comparison of actual vs fitted returns
comparison_data <-
  tibble(
    date = portfolio_returns$date,
    return_actual = portfolio_returns$portfolio_return,
    return_fitted = fitted(portfolio_fit) %>% as.numeric()
  )
comparison_data

comparison_data %>%
  pivot_longer(-date) %>%
  ggplot(aes(x = date, y = value, color = name)) +
  geom_line() +
  theme_minimal() +
  labs(
    title = "Comparison of Actual vs Fitted Returns",
    y = "Return",
    x = "Date",
    color = "Type"
  ) +
  scale_color_brewer(
    labels = c("Actual", "Fitted"),
    palette = "Set1"
  )

# Extract and plot volatility (standard deviation)
volatility_data <-
  tibble(
    date = portfolio_returns$date,
    return_abs = abs(portfolio_returns$portfolio_return),
    sigma = sigma(portfolio_fit) %>% as.numeric()
  )

volatility_data %>%
  pivot_longer(
    cols = c(return_abs, sigma),
    names_to = "measure",
    values_to = "value"
  ) %>%
  ggplot(aes(x = date, y = value, color = measure)) +
  geom_line() +
  labs(
    title = "Fitted Daily Standard Deviation",
    y = "Standard Deviation",
    x = "Date"
  )

# Prepare data for rolling forecast and backtesting
df_portfolio <-
  portfolio_returns %>%
  select(date, portfolio_return)

# Convert to xts object
df_portfolio_xts <- xts(
  x = df_portfolio$portfolio_return,
  order.by = df_portfolio$date
)

# Rolling forecast and backtesting
roll_model <-
  ugarchroll(
    spec = spec,
    data = df_portfolio_xts,
    n.ahead = 1,
    forecast.length = 252, # Approximately one trading year
    refit.every = 50,
    refit.window = "moving",
    calculate.VaR = TRUE,
    VaR.alpha = c(0.01, 0.05)
  )

roll_model

# Generate reports and evaluate backtesting results
# VaR Backtest Report

report(
  roll_model,
  type = "VaR",
  VaR.alpha = 0.01,
  conf.level = 0.95
)

# Forecast Performance Measures Report
fpm_report <- report(roll_model, type = "fpm")
print(fpm_report)

# Visualization
# Plot rolling forecasts with actual values
# Reset to default single plot per window
par(mfrow = c(1, 1))
par(new = FALSE) # Reset plotting parameters
plot(roll_model, which = 1)

par(mfrow = c(1, 1))
par(new = FALSE) # Reset plotting parameters
plot(roll_model, which = 2)

par(mfrow = c(1, 1))
par(new = FALSE) # Reset plotting parameters
plot(roll_model, which = 3)

par(mfrow = c(1, 1))
par(new = FALSE) # Reset plotting parameters
plot(roll_model, which = 4)
