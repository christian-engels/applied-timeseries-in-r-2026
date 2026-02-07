# =============================================================================
# Unit 5 Exercise Solutions: Portfolio Optimisation & Risk Management
# =============================================================================

# Load Libraries
library(tidyverse)
library(tidyfinance)
library(tsibble)
library(feasts)
library(rugarch)
library(PortfolioAnalytics)
library(xts)

# =============================================================================
# Exercise 1: Data
# =============================================================================
# (a) Download daily prices for 5 stocks

symbols <- c("AAPL", "MSFT", "JNJ", "JPM", "XOM")

stock_prices <- download_data(
  "stock_prices",
  symbols = symbols,
  start = "2015-01-01",
  end = "2020-01-01"
)

stock_prices %>% glimpse()

# (b) Compute log returns

stock_returns <- stock_prices %>%
  select(symbol, date, adjusted_close) %>%
  group_by(symbol) %>%
  arrange(date) %>%
  mutate(
    lprice  = log(adjusted_close),
    lreturn = c(NA, diff(lprice))
  ) %>%
  ungroup() %>%
  drop_na(lreturn) %>%
  select(symbol, date, lreturn)

stock_returns %>% glimpse()

# (c) Reshape to wide format with pivot_wider()

returns_wide <- stock_returns %>%
  pivot_wider(
    id_cols = date,
    names_from = symbol,
    values_from = lreturn
  ) %>%
  drop_na()

returns_wide %>% glimpse()

# =============================================================================
# Exercise 2: Optimise
# =============================================================================
# (a) Define a min-variance portfolio with long-only constraints

# Prepare returns matrix for PortfolioAnalytics
returns_mat <- returns_wide %>%
  select(-date) %>%
  as.data.frame()
rownames(returns_mat) <- as.character(returns_wide$date)

init_portf <- portfolio.spec(assets = symbols) %>%
  add.constraint(type = "weight_sum", min_sum = 0.99, max_sum = 1.01) %>%
  add.constraint(type = "box", min = 0, max = 1) %>%
  add.objective(type = "risk", name = "StdDev")

# (b) Solve with osqp

opt <- optimize.portfolio(
  R = returns_mat,
  portfolio = init_portf,
  optimize_method = "osqp"
)
opt

# (c) Plot optimal weights

weights_df <- tibble(
  symbol = names(opt$weights),
  weight = round(opt$weights, 4)
)

weights_df %>%
  ggplot(aes(x = reorder(symbol, -weight), y = weight)) +
  geom_col() +
  labs(
    title = "Minimum Variance Portfolio: Optimal Weights",
    x = "Asset",
    y = "Weight"
  ) +
  scale_y_continuous(labels = scales::percent)

# =============================================================================
# Exercise 3: Portfolio Returns
# =============================================================================
# (a) Join weights to returns with left_join

port_ret <- stock_returns %>%
  left_join(weights_df, by = "symbol")

# (b) Compute weighted sum per date

port_ret <- port_ret %>%
  group_by(date) %>%
  summarise(
    ret = sum(lreturn * weight)
  )

port_ret %>% glimpse()

# (c) Plot returns and squared returns

port_ret %>%
  mutate(ret_sq = ret^2) %>%
  pivot_longer(c(ret, ret_sq)) %>%
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(~name, scales = "free_y", nrow = 2,
             labeller = as_labeller(c(ret = "Portfolio Return",
                                      ret_sq = "Squared Return"))) +
  labs(title = "Portfolio Returns", x = "Date", y = NULL)

# =============================================================================
# Exercise 4: GARCH
# =============================================================================
# (a) Fit GJR-GARCH(1,1)

spec <- ugarchspec(
  variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
  distribution.model = "std"
)

fit <- ugarchfit(spec = spec, data = port_ret$ret)

# (b) Extract coefficients with coef(fit)

coef(fit)

# (c) Check if gamma1 is significant

fit
# Inspect the gamma1 row in the output table.
# If p-value < 0.05, the leverage effect is statistically significant,
# meaning negative shocks increase portfolio volatility more than
# positive shocks.

# =============================================================================
# Exercise 5: Backtest
# =============================================================================
# (a) Run 252-day rolling VaR with ugarchroll

# Convert to xts
port_xts <- xts(port_ret$ret, order.by = port_ret$date)

roll <- ugarchroll(
  spec = spec,
  data = port_xts,
  n.ahead = 1,
  forecast.length = 252,
  refit.every = 50,
  refit.window = "moving",
  calculate.VaR = TRUE,
  VaR.alpha = c(0.01, 0.05)
)

# (b) Use report(roll, type = "VaR")

report(roll, type = "VaR", VaR.alpha = 0.05, conf.level = 0.95)

# (c) Compare violation rate to expected 5%

# Expected violations at 5%: 0.05 * 252 ~ 13 out of 252 days.
# The Kupiec test checks whether the observed violation rate
# is statistically different from 5%.
# The Christoffersen test checks whether violations are independent
# (i.e. they do not cluster).
# A good model passes both tests (fail to reject H0).
