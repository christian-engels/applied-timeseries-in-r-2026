# =============================================================================
# Unit 2 Exercise Solutions: Random Walks, Stationarity & Unit Root Tests
# =============================================================================

# Load Libraries
library(tidyverse)
library(tidyfinance)
library(tsibble)
library(fable)
library(feasts)
library(scales)
library(tseries)

# =============================================================================
# Exercise 1: AR(1) Simulation
# =============================================================================
# Simulate 200 observations for each case and plot

set.seed(42)
steps <- 200
e <- rnorm(steps)

# Helper: simulate AR(1) with y_t = alpha + beta * y_{t-1} + e_t
simulate_ar1 <- function(alpha, beta, e) {
  n <- length(e)
  y <- numeric(n)
  y[1] <- e[1]
  for (i in 2:n) {
    y[i] <- alpha + beta * y[i - 1] + e[i]
  }
  return(y)
}

# (a) alpha = 2, beta = 1 (random walk with drift)
y_drift <- simulate_ar1(alpha = 2, beta = 1, e = e)

# (b) alpha = 0, beta = 0.8 (stationary)
y_stationary <- simulate_ar1(alpha = 0, beta = 0.8, e = e)

# (c) alpha = 0, beta = 1.05 (explosive)
y_explosive <- simulate_ar1(alpha = 0, beta = 1.05, e = e)

# Combine and plot
ar1_sims <- tibble(
  step = rep(1:steps, 3),
  y = c(y_drift, y_stationary, y_explosive),
  case = rep(
    c("(a) Drift: alpha=2, beta=1",
      "(b) Stationary: alpha=0, beta=0.8",
      "(c) Explosive: alpha=0, beta=1.05"),
    each = steps
  )
)

ar1_sims %>%
  ggplot(aes(x = step, y = y)) +
  geom_line() +
  facet_wrap(~case, scales = "free_y", ncol = 1) +
  labs(title = "AR(1) Simulations", x = "Time", y = "y")

# Classification:
# (a) Non-stationary (unit root with drift, variance grows with t)
# (b) Stationary (|beta| = 0.8 < 1, mean-reverting)
# (c) Non-stationary (explosive, |beta| = 1.05 > 1, diverges)

# =============================================================================
# Exercise 2: Theoretical Moments
# =============================================================================
# For beta = 0.8, compute E[y] and Var(y) and compare with simulation

alpha <- 0
beta <- 0.8
sigma2 <- 1  # Var(e) = 1 for rnorm()

# Theoretical moments
E_y <- alpha / (1 - beta)
Var_y <- sigma2 / (1 - beta^2)

cat("Theoretical E[y]:", E_y, "\n")
cat("Theoretical Var(y):", Var_y, "\n")

# Simulated moments (discard first 50 as burn-in)
cat("Simulated mean(y):", mean(y_stationary[51:steps]), "\n")
cat("Simulated var(y):", var(y_stationary[51:steps]), "\n")

# =============================================================================
# Exercise 3: Real Data
# =============================================================================
# (a) Download a stock of your choice

aapl <- download_data(
  "stock_prices",
  symbols = "AAPL",
  start = "2010-01-01",
  end = "2020-01-01"
)

# (b) Compute log returns

aapl_returns <- aapl %>%
  rename(price = adjusted_close) %>%
  select(symbol, date, price) %>%
  as_tsibble(index = date, regular = FALSE) %>%
  mutate(
    lprice = log(price),
    lreturn = difference(lprice)
  ) %>%
  remove_missing()

aapl_returns %>% glimpse()

# (c) Run KPSS and ADF tests

# KPSS test on log prices (expect: reject H0 of stationarity)
aapl_returns %>% features(lprice, unitroot_kpss)

# ADF test on log prices (expect: fail to reject H0 of unit root)
adf.test(aapl_returns %>% pull(lprice))

# KPSS test on log returns (expect: fail to reject H0 of stationarity)
aapl_returns %>% features(lreturn, unitroot_kpss)

# ADF test on log returns (expect: reject H0 of unit root)
adf.test(aapl_returns %>% pull(lreturn))

# Conclusion: Log prices are non-stationary (unit root).
#             Log returns are stationary.

# (d) Plot ACF/PACF

# ACF/PACF for log prices (slow decay -> non-stationary)
aapl_returns %>%
  mutate(date = row_number()) %>%
  as_tsibble(index = date) %>%
  gg_tsdisplay(lprice, plot_type = "partial", lag_max = 20)

# ACF/PACF for log returns (within bands -> stationary)
aapl_returns %>%
  mutate(date = row_number()) %>%
  as_tsibble(index = date) %>%
  gg_tsdisplay(lreturn, plot_type = "partial", lag_max = 20)
