# Applied Timeseries in R

One-day workshop on applied time series analysis using R, covering foundations through portfolio risk management.

**Instructor:** Dr Christian Engels 

**Programme:** Certificate in Engagement with Practice in Finance (CEPF) | Durham University Business School

## Getting Started

1. **Accept the GitHub Classroom assignment:** [classroom.github.com/a/lpdeYbn_](https://classroom.github.com/a/lpdeYbn_)
2. **Open your repository** under [github.com/orgs/Applied-Timeseries-in-R/repositories](https://github.com/orgs/Applied-Timeseries-in-R/repositories)
3. **Launch a Codespace** by clicking the green **Code** button, then **Codespaces** > **Create codespace on main**
4. **Verify your setup** by running `Rscript Unit-0-Setup.R` in the terminal

## Workshop Units

| Unit | Topic | Slides | R Script |
|------|-------|--------|----------|
| 0 | Setup & Environment | `Unit-0-Setup.tex` | `Unit-0-Setup.R` |
| 1 | Time Series & Tidyverse Foundations | `Unit-1-TimeSeries-Foundations.tex` | `Unit-1-TimeSeries-Foundations.R` |
| 2 | Random Walks, Stationarity & Unit Root Tests | `Unit-2-RandomWalks-Stationarity.tex` | `Unit-2-RandomWalks-Stationarity.R` |
| 3 | ARMA/ARIMA Models & Forecasting | `Unit-3-ARIMA-Forecasting.tex` | `Unit-3-ARIMA-Forecasting.R` |
| 4 | Volatility Models (ARCH/GARCH) | `Unit-4-Volatility-GARCH.tex` | `Unit-4-Volatility-GARCH.R` |
| 5 | Portfolio Optimisation & Risk Management | `Unit-5-Portfolio-Optimization.tex` | `Unit-5-Portfolio-Optimization.R` |
| 6 | Wrap-up & Resources | `Unit-6-Wrapup.tex` | |

Each unit pairs a slide deck (theory and equations) with a companion R script (worked examples and exercises). Exercise solutions are in `exercise-answers/`.

## Saving Your Work

Run the following in the terminal to save your progress to GitHub:

```bash
bash save-work.sh
```

## Key R Packages

| Area | Packages |
|------|----------|
| Data & wrangling | `tidyverse`, `tidyfinance`, `scales`, `broom` |
| Time series | `tsibble`, `fable`, `feasts`, `fabletools` |
| Econometrics | `fixest`, `tseries`, `moments` |
| Volatility | `rugarch`, `forecast` |
| Portfolios | `PortfolioAnalytics`, `xts`, `zoo` |
