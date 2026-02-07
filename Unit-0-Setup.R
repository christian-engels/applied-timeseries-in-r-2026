# ==============================================================================
# Unit 0: Workshop Setup & Environment Verification
# Applied Timeseries in R — 7 Feb 2026
# Dr Christian Engels
# ==============================================================================

# --- Step 1: Install required packages (if not already installed) -------------
required_packages <- c(
  "tidyverse",
  "tidyfinance",
  "tsibble",
  "fable",
  "feasts",
  "scales",
  "fixest",
  "fabletools",
  "rugarch",
  "PortfolioAnalytics",
  "xts",
  "zoo",
  "moments",
  "tseries",
  "forecast",
  "broom",
  "ggpubr",
  "httr2"
)

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Installing", pkg, "..."))
    install.packages(pkg, repos = "https://cloud.r-project.org")
  } else {
    message(paste(pkg, "is already installed."))
  }
}

invisible(lapply(required_packages, install_if_missing))

# --- Step 2: Load all packages ------------------------------------------------
library(tidyverse)
library(tidyfinance)
library(tsibble)
library(fable)
library(feasts)
library(scales)
library(fixest)
library(fabletools)
library(rugarch)
library(PortfolioAnalytics)
library(xts)
library(zoo)
library(moments)
library(tseries)
library(forecast)
library(broom)
library(httr2)

# --- Step 3: Quick verification -----------------------------------------------
# Check that key functions are available
cat("\n========================================\n")
cat("Environment Verification\n")
cat("========================================\n\n")

cat("R version:", R.version.string, "\n\n")

# Test tidyverse
test_tibble <- tibble(x = 1:5, y = rnorm(5))
cat("tidyverse:           OK\n")

# Test tsibble
test_tsibble <- test_tibble %>%
  as_tsibble(index = x)
cat("tsibble:             OK\n")

# Test rugarch
test_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0)),
  distribution.model = "norm"
)
cat("rugarch:             OK\n")

# Test PortfolioAnalytics
cat("PortfolioAnalytics:  OK\n")

cat("\n========================================\n")
cat("All packages loaded successfully!\n")
cat("You are ready for the workshop.\n")
cat("========================================\n")
