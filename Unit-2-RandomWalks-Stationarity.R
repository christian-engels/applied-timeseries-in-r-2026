# Load Libraries
library(tidyverse)
library(tidyfinance)
library(tsibble)
library(fable)
library(feasts)
library(scales)
library(fixest)

# Illustrate a Random Walk
steps <- 100
e <- rnorm(steps)
y <- e
coef <- 1
drift = 0

plot(e, type="l")

for (i in 2:steps) {
  y[i] <- drift + coef * y[i-1] + e[i]
}

plot(y, type="l")

# Multiple Random Walks
n_random_walks <- 30

create_random_walk <- function(random_walk_no, steps = 100, n_random_walks = 30) {
  innovations <- tibble(
    random_walk_no, 
    step = 1:steps,
    e = rnorm(steps)
  )
  random_walk <- innovations %>% mutate(random_walk = cumsum(e))
  return(random_walk)
}

random_walks <- 1:n_random_walks %>% map(~create_random_walk(.)) %>% list_rbind()

random_walks %>% 
  mutate(random_walk_no = factor(random_walk_no, 1:n_random_walks)) %>% 
  ggplot(aes(x = step, y = random_walk, color = random_walk_no)) +
  geom_line()

# Analyze Single Random Walk
random_walk_1 <- random_walks %>% 
  filter(random_walk_no == 1) %>% 
  mutate(random_walk_lag = lag(random_walk)) %>% 
  remove_missing() %>% 
  print()

# Estimate Regression
est <- feols(random_walk ~ random_walk_lag - 1, data = random_walk_1)
etable(est)

# Monte Carlo Simulation of Random Walks
random_walks <- 1:10^3 %>%  
  map(~create_random_walk(.)) %>% 
  list_rbind() %>% 
  group_by(random_walk_no) %>% 
  mutate(random_walk_lag = lag(random_walk)) %>% 
  remove_missing() %>% 
  nest() %>% 
  mutate(
    est = map(data, ~feols(random_walk ~ random_walk_lag - 1, data = .)),
    coef = map(est, ~.$coefficients)
  ) %>% 
  unnest(cols = coef) %>% 
  ungroup()

random_walks

# Summary of Coefficients
coef_mean <- random_walks %>% summarise(coef_mean = mean(coef)) %>% pull(coef_mean)

random_walks %>% 
  ggplot(aes(coef)) + 
  geom_density() + 
  geom_vline(xintercept = coef_mean, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 1, linetype = "twodash", color = "navy") +
  scale_x_continuous(
    breaks = c(coef_mean, 1),
    labels = c("coef_mean" = "mean estimate", "1" = "1 (true)")
  )
