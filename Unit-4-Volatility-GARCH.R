library(tidyverse)
library(tidyfinance)
library(tsibble)
library(feasts)
library(tseries)
library(broom)
library(PortfolioAnalytics)
library(rugarch)
library(ggpubr)
library(fable)
library(forecast)

# Part A: Simulate GARCH Processes

n_sim <- 1000

# Define multiple GARCH specifications
volatility_specs <-
  list(
    # sGARCH (Standard GARCH) is the simplest GARCH model that models the conditional variance
    # as a function of past squared residuals and past variances. It is used to capture volatility clustering in financial data.
    sGARCH = ugarchspec(
      variance.model = list(model = "sGARCH", garchOrder = c(1, 0)),
      fixed.pars = c(mu = 0.1, ar1 = 0.9, omega = 0.1, alpha1 = 0.7),
      mean.model = list(armaOrder = c(1, 0)),
      distribution.model = "norm"
    ),
    # gjrGARCH (Glosten-Jagannathan-Runkle GARCH) introduces an asymmetry term to capture the leverage effect,
    # where negative shocks have a larger impact on volatility compared to positive shocks.
    gjrGARCH = ugarchspec(
      variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
      fixed.pars = c(mu = 0.1, ar1 = 0.9, omega = 0.1, alpha1 = 0.5, beta1 = 0.1, gamma1 = 0.1),
      mean.model = list(armaOrder = c(1, 0)),
      distribution.model = "norm"
    ),
    # eGARCH (Exponential GARCH) models the logarithm of the conditional variance,
    # allowing for both asymmetry and capturing the effect of past returns.
    # It ensures that the variance is always positive without imposing non-negativity constraints.
    eGARCH = ugarchspec(
      variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
      fixed.pars = c(mu = 0.1, ar1 = 0.9, omega = 0.1, alpha1 = 0.5, beta1 = 0.4, gamma1 = 0.1),
      mean.model = list(armaOrder = c(1, 0)),
      distribution.model = "norm"
    ),
    # iGARCH (Integrated GARCH) is a restricted version of GARCH where the persistence of volatility
    # is enforced to be unity, implying long memory behavior in the volatility process.
    iGARCH = ugarchspec(
      variance.model = list(model = "iGARCH", garchOrder = c(1, 1)),
      fixed.pars = c(mu = 0.1, ar1 = 0.9, omega = 0.1, alpha1 = 0.5),
      mean.model = list(armaOrder = c(1, 0)),
      distribution.model = "norm"
    ),
    # apARCH (Asymmetric Power ARCH) allows for asymmetric effects and models the conditional standard deviation
    # to a power 'delta', providing greater flexibility in capturing non-linear relationships in volatility.
    apARCH = ugarchspec(
      variance.model = list(model = "apARCH", garchOrder = c(1, 1)),
      fixed.pars = c(mu = 0.1, ar1 = 0.9, omega = 0.1, alpha1 = 0.5, beta1 = 0.4, gamma1 = 0.1, delta = 1),
      mean.model = list(armaOrder = c(1, 0)),
      distribution.model = "norm"
    )
  )

# Loop through each specification and simulate
sim_data <-
  map(
    names(volatility_specs),
    function(model_name) {
      spec <- volatility_specs[[model_name]]
      simulation <- ugarchpath(spec = spec, n.sim = n_sim)
      tibble(
        time = 1:n_sim,
        series = as.numeric(simulation@path$seriesSim),
        sigma = as.numeric(simulation@path$sigmaSim),
        model = model_name
      )
    }
  ) %>%
  list_rbind()

# Plot the simulations
sim_data %>%
  pivot_longer(c(series, sigma)) %>%
  ggplot(aes(time, value, color = model)) +
  geom_line() +
  facet_wrap(~name, scales = "free", nrow = 2) +
  theme(legend.position = "bottom")

# Display time series diagnostics for each model
plots <-
  sim_data %>%
  group_by(model) %>%
  nest() %>%
  mutate(
    data = map(data, ~ as_tsibble(., index = time)),
    plot = map(data, ~ gg_tsdisplay(., sigma, plot_type = "partial"))
  ) %>%
  ungroup() %>%
  glimpse()

models <- plots %>% pull(model)
for (m in models) {
  print(str_c("Model: ", m))
  plots %>%
    filter(model == m) %>%
    pull(plot) %>%
    print()

  readline("Press enter to continue")
}


# Part B: Fit GARCH Models to SP500 Returns


if (!file.exists("sp500_index.csv")) {
  sp500_download <-
    download_data(
      "stock_prices",
      symbols = "^GSPC",
      start_date = "2014-11-18",
      end_date = "2024-11-18"
    )
  sp500_download %>% write_csv("sp500_index.csv")
} else {
  sp500_download <- read_csv("sp500_index.csv")
}

sp500_returns <-
  sp500_download %>%
  select(symbol, date, price = adjusted_close) %>%
  mutate(return = price / lag(price) - 1) %>%
  remove_missing() %>%
  select(-price) %>%
  mutate(date = row_number()) %>%
  print()

# Plot the returns to visualize volatility clustering
sp500_returns %>%
  ggplot(aes(x = date, y = return)) +
  geom_line() +
  labs(
    title = "S&P 500 Monthly Returns",
    x = "Month",
    y = "Daily Return"
  )

# Add descriptive statistics
sp500_stats <-
  sp500_returns %>%
  summarise(
    mean_return = mean(return),
    std_dev = sd(return),
    skewness = moments::skewness(return),
    kurtosis = moments::kurtosis(return),
    jarque_bera = tseries::jarque.bera.test(return)$p.value
  )

sp500_stats

# mean_return: 0.000481 (0.0481%)
# This is the average daily return
# std_dev: 0.0112 (1.12%)
# Represents the daily volatility/risk
# skewness: -0.517
# Negative value indicates the distribution is left-skewed
# Returns have more extreme negative values than positive ones
# kurtosis: 17.5
# Much higher than 3 (normal distribution's kurtosis)
# Indicates heavy tails (more extreme values than a normal distribution)
# jarque_bera: 0
# P-value of 0 strongly rejects the null hypothesis of normality
# Confirms returns are not normally distributed

# Add return distribution analysis
sp500_returns %>%
  ggplot(aes(x = return)) +
  geom_histogram(aes(y = ..density..), bins = 50) +
  geom_density(color = "red") +
  stat_function(
    fun = dnorm,
    args = list(
      mean = mean(sp500_returns$return),
      sd = sd(sp500_returns$return)
    ),
    color = "blue"
  ) +
  labs(title = "Return Distribution vs Normal Distribution")

# Add ACF/PACF analysis
sp500_returns %>%
  as_tsibble(index = date) %>%
  gg_tsdisplay(return, plot_type = "partial")

fit_auto_arima <-
  sp500_returns %>%
  as_tsibble(index = date) %>%
  model(arima = ARIMA(return))
fit_auto_arima

garch_order <- c(2, 2)
arma_order <- c(2, 2)

# Define GARCH model specifications to fit
garch_specs <-
  list(
    # Standard GARCH(1,1) model specification
    sGARCH = ugarchspec(
      variance.model = list(
        model = "sGARCH",
        garchOrder = garch_order
      ), # Variance model with GARCH(1,1)
      mean.model = list(
        armaOrder = arma_order,
        include.mean = TRUE
      ), # Mean model with ARMA(1,4)
      distribution.model = "std" # t distribution for residuals
    ),
    # Expontential GARCH(1,1) model specification
    apARCH = ugarchspec(
      variance.model = list(
        model = "apARCH",
        garchOrder = garch_order
      ), # Variance model with eGARCH(1,1)
      mean.model = list(
        armaOrder = arma_order,
        include.mean = TRUE
      ), # Mean model with ARMA(1,4)
      distribution.model = "std" # Normal distribution for residuals
    ),
    # GJR-GARCH(1,1) model specification
    gjrGARCH = ugarchspec(
      variance.model = list(
        model = "gjrGARCH",
        garchOrder = garch_order
      ), # Variance model with GJR-GARCH(1,1)
      mean.model = list(
        armaOrder = arma_order,
        include.mean = TRUE
      ), # Mean model with ARMA(1,4)
      distribution.model = "std" # t distribution for residuals
    ),
    # TGARCH (Threshold GARCH) model specification
    tGARCH = ugarchspec(
      variance.model = list(
        model = "fGARCH",
        submodel = "TGARCH",
        garchOrder = garch_order
      ), # Variance model with TGARCH(1,1)
      mean.model = list(
        armaOrder = arma_order,
        include.mean = TRUE
      ), # Mean model with ARMA(1,4)
      distribution.model = "std" # t distribution for residuals
    )
  )

# Fit the models to the return data
fit_results <-
  map(
    garch_specs,
    ~ ugarchfit(spec = .x, data = sp500_returns$return)
  )

# Function to extract model diagnostics
get_model_diagnostics <- function(model) {
  tibble(
    aic = infocriteria(model)[1],
    bic = infocriteria(model)[2],
    log_likelihood = likelihood(model),
    persistence = persistence(model),
    half_life = -log(2) / log(persistence(model)),
    unconditional_variance = sigma(model)^2 %>% mean()
  )
}

# Extract fit statistics for model comparison
fit_stats <- map_dfr(fit_results, get_model_diagnostics, .id = "model")
fit_stats

# Model Comparison
# The table shows metrics for 4 different GARCH variants:

# sGARCH: Standard GARCH
# apARCH: Asymmetric Power ARCH
# gjrGARCH: Glosten-Jagannathan-Runkle GARCH
# tGARCH: Threshold GARCH
# Model Selection Criteria
# Information Criteria:

# AIC: tGARCH has lowest (-6.75), suggesting best fit
# BIC: tGARCH has lowest (-6.72), confirming best fit
# Higher log-likelihood for tGARCH (8500) indicates better fit
# Volatility Characteristics:

# Persistence: sGARCH shows highest (0.997)
# Half-life: Ranges from 11.5 days (tGARCH) to 201 days (sGARCH)
# Unconditional variance: Similar across models (~0.00013-0.00014)
# Best Model
# The tGARCH model appears optimal based on:

# Lowest AIC/BIC values
# Highest log-likelihood
# More reasonable half-life (11.5 days)

residuals <-
  imap_dfr(
    fit_results,
    ~ tibble(
      model = .y,
      date = sp500_returns$date,
      residuals = residuals(.x, standardize = TRUE) %>% as.numeric()
    )
  )
residuals

# plot residuals for sGARCH
residuals %>%
  ggplot(aes(x = date, y = residuals^2)) +
  geom_line() +
  labs(
    title = "Standardized Residuals for sGARCH Model",
    x = "Date",
    y = "Standardized Residuals Squared"
  ) +
  facet_wrap(~model)

residuals %>%
  filter(model == "tGARCH") %>%
  as_tsibble(index = date) %>%
  gg_tsdisplay(plot_type = "partial")

# Forecast future volatility using the fitted models
n_ahead <- 30 # Number of days to forecast
forecasts <- map(
  fit_results,
  ~ ugarchforecast(.x, n.ahead = n_ahead)
)

# Extract and prepare forecast data for plotting
forecast_data <-
  map2_df(
    forecasts, names(forecasts),
    ~ {
      sigma_forecast <- sigma(.x) %>% as.numeric()
      conf_intervals <- qnorm(c(0.025, 0.975)) * sigma_forecast
      tibble(
        Day = 1:n_ahead,
        Forecasted_Volatility = as.numeric(sigma_forecast),
        Lower_CI = as.numeric(sigma_forecast + conf_intervals[1]),
        Upper_CI = as.numeric(sigma_forecast + conf_intervals[2]),
        Model = .y
      )
    }
  )

# Plot the volatility forecasts from each model
ggplot(forecast_data, aes(x = Day, y = Forecasted_Volatility, color = Model)) +
  geom_line(linewidth = 1) +
  labs(
    title = "30-Day Ahead Volatility Forecasts",
    x = "Day",
    y = "Forecasted Volatility (Sigma)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


# Calculate VaR and ES using Student-t distribution
alpha <- 0.05 # 95% confidence level

# We need to iterate over both forecasts and the corresponding fitted models
var_forecasts <- map2_dfr(forecasts, fit_results, function(forecast, model) {
  # Extract mu and sigma from the forecast
  mu <- as.numeric(fitted(forecast))[1]
  sigma <- as.numeric(sigma(forecast))[1]

  # Extract the shape parameter (degrees of freedom) from the fitted model
  shape <- as.numeric(coef(model)["shape"])

  # Print debugging information
  print(sprintf("mu: %f, sigma: %f, shape: %f", mu, sigma, shape))

  # Calculate VaR using quantile of t-distribution
  var_95 <- mu + sigma * qt(alpha, df = shape)

  # Calculate ES (conditional expectation beyond VaR)
  qt_value <- qt(alpha, df = shape)
  dt_value <- dt(qt_value, df = shape)
  es_95 <- mu - sigma * (dt_value / alpha) * (shape + qt_value^2) / (shape - 1)

  tibble(
    VaR_95 = var_95,
    ES_95 = es_95
  )
}, .id = "model")

var_forecasts
