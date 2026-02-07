# =============================================================================
# Unit 4 Exercise Solutions: Volatility Models (ARCH/GARCH)
# =============================================================================

# Load Libraries
library(tidyverse)
library(tidyfinance)
library(tsibble)
library(feasts)
library(rugarch)
library(moments)
library(tseries)

# =============================================================================
# Exercise 1: Simulate
# =============================================================================
# (a) Generate GARCH(1,1) paths with high and low persistence

set.seed(42)
n_sim <- 1000

# High persistence: alpha = 0.1, beta = 0.88  (persistence = 0.98)
spec_high <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  fixed.pars = list(mu = 0, omega = 0.01, alpha1 = 0.1, beta1 = 0.88),
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "norm"
)

# Low persistence: alpha = 0.3, beta = 0.2  (persistence = 0.5)
spec_low <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  fixed.pars = list(mu = 0, omega = 0.01, alpha1 = 0.3, beta1 = 0.2),
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "norm"
)

sim_high <- ugarchpath(spec = spec_high, n.sim = n_sim)
sim_low  <- ugarchpath(spec = spec_low,  n.sim = n_sim)

sim_data <- bind_rows(
  tibble(
    time = 1:n_sim,
    returns = as.numeric(sim_high@path$seriesSim),
    sigma   = as.numeric(sim_high@path$sigmaSim),
    model   = "High persistence (0.98)"
  ),
  tibble(
    time = 1:n_sim,
    returns = as.numeric(sim_low@path$seriesSim),
    sigma   = as.numeric(sim_low@path$sigmaSim),
    model   = "Low persistence (0.50)"
  )
)

# (b) Compare volatility plots

sim_data %>%
  pivot_longer(c(returns, sigma)) %>%
  ggplot(aes(time, value)) +
  geom_line() +
  facet_grid(name ~ model, scales = "free_y") +
  labs(title = "GARCH(1,1) Simulations: High vs Low Persistence")

# High persistence: volatility shocks take a long time to die out.
# Low persistence: volatility reverts quickly to its unconditional level.

# =============================================================================
# Exercise 2: Fit
# =============================================================================
# (a) Fit sGARCH(1,1) and GJR-GARCH(1,1) to S&P 500 returns

sp500 <- download_data(
  "stock_prices",
  symbols = "^GSPC",
  start = "2010-01-01",
  end = "2020-01-01"
)

sp500_returns <- sp500 %>%
  rename(price = adjusted_close) %>%
  select(date, price) %>%
  mutate(
    lprice = log(price),
    lreturn = c(NA, diff(lprice))
  ) %>%
  remove_missing()

sp500_returns %>% glimpse()

# sGARCH(1,1)
spec_sgarch <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
  distribution.model = "std"
)

fit_sgarch <- ugarchfit(spec = spec_sgarch, data = sp500_returns$lreturn)

# GJR-GARCH(1,1)
spec_gjr <- ugarchspec(
  variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
  distribution.model = "std"
)

fit_gjr <- ugarchfit(spec = spec_gjr, data = sp500_returns$lreturn)

# (b) Is gamma1 significant?

coef(fit_gjr)
# Check the gamma1 coefficient and its p-value in the output:
fit_gjr

# If gamma1 is positive and statistically significant (p < 0.05),
# the leverage effect is present: negative shocks increase volatility
# more than positive shocks of the same magnitude.

# =============================================================================
# Exercise 3: Compare
# =============================================================================
# Use infocriteria() to rank the models by AIC/BIC

ic_comparison <- tibble(
  model = c("sGARCH", "gjrGARCH"),
  aic = c(infocriteria(fit_sgarch)[1], infocriteria(fit_gjr)[1]),
  bic = c(infocriteria(fit_sgarch)[2], infocriteria(fit_gjr)[2]),
  persistence = c(persistence(fit_sgarch), persistence(fit_gjr)),
  half_life = -log(2) / log(c(persistence(fit_sgarch), persistence(fit_gjr)))
)

ic_comparison

# The model with the lower AIC/BIC is preferred.
# For equity returns, GJR-GARCH typically wins because it captures
# the leverage effect.

# =============================================================================
# Exercise 4: Risk
# =============================================================================
# (a) Forecast volatility 30 days ahead

fc_gjr <- ugarchforecast(fit_gjr, n.ahead = 30)

# Plot the volatility forecast
tibble(
  day = 1:30,
  sigma = as.numeric(sigma(fc_gjr))
) %>%
  ggplot(aes(x = day, y = sigma)) +
  geom_line(linewidth = 1) +
  labs(
    title = "GJR-GARCH(1,1): 30-Day Volatility Forecast",
    x = "Day Ahead",
    y = "Forecasted Sigma"
  )

# (b) Compute 1-day VaR and ES at 99% confidence

alpha <- 0.01
mu_fc    <- as.numeric(fitted(fc_gjr))[1]
sigma_fc <- as.numeric(sigma(fc_gjr))[1]
shape    <- as.numeric(coef(fit_gjr)["shape"])

# VaR using Student-t quantile
var_99 <- mu_fc + sigma_fc * qt(alpha, df = shape)

# ES using Student-t formula
qt_val <- qt(alpha, df = shape)
dt_val <- dt(qt_val, df = shape)
es_99  <- mu_fc - sigma_fc * (dt_val / alpha) * (shape + qt_val^2) / (shape - 1)

cat("1-day 99% VaR:", var_99, "\n")
cat("1-day 99% ES: ", es_99, "\n")

# Interpretation:
# VaR: With 99% confidence, the worst expected daily loss will not exceed |VaR|.
# ES:  Given that we are in the worst 1% of outcomes, the expected loss is |ES|.
