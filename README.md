# oneMeanTest

An R package for performing a one-sample t-test for a population mean when the variance is unknown, developed as a STAT 181 (Statistical Computing) project at the Institute of Statistics, UP Diliman.

## Installation

You can install the development version from GitHub:

install.packages("devtools") # if needed
devtools::install_github("KingSSIBAL/oneMeantTest")


Then load the package:

library(oneMeanTest)



## Main functionality

- `one_mean_test()`  
  Perform a one-sample t-test for a population mean with unknown variance.  
  Returns:
  - test statistic and degrees of freedom
  - p-value
  - confidence interval for the mean
  - decision to reject or fail to reject \(H_0\)
  - plain-language interpretation
  - optional assumption checks

- `descriptive_stats()`  
  Basic descriptive statistics (n, mean, median, sd, se, variance, quantiles, IQR) for a numeric sample.

- `check_assumptions()`  
  Check normality and other basic assumptions for using the one-sample t-test.

- `bootstrap_ttest()`  
  Bootstrap-based one-sample t-test: bootstrap distribution of the t-statistic, bootstrap p-value, and percentile CI for the mean.

- `power_analysis_one_mean()`  
  Wrapper around `stats::power.t.test()` for one-sample t-test power analysis (solve for power or for required sample size).

- `plot()` methods for objects of class `oneMeanTest` and `oneMeanTest_bootstrap`  
  Visualizations including:
  - t-distribution with observed statistic and critical region
  - histogram and boxplot of the data with \(\mu_0\) and sample mean
  - normal Q-Q plot
  - confidence interval plot for the mean

## Quick example

library(oneMeanTest)

set.seed(123)
x <- rnorm(30, mean = 5, sd = 2)

res <- one_mean_test(
x,
mu0 = 5,
alternative = "two.sided",
alpha = 0.05,
conf.level = 0.95,
check_assumptions = TRUE
)

res # print main results
plot(res, "t") # t-distribution with test statistic
plot(res, "ci") # confidence interval for the mean

text

## When to use this package

Use `oneMeanTest` when:

- You have one numeric sample (independent observations).
- You want to test a hypothesis about a single population mean with unknown variance.
- The population is approximately normal (or the sample size is moderately large).
- You want classical t-test results, and optionally bootstrap-based inference and power analysis.

This package was created for STAT 181, 1st Semester 2025–2026, as an educational tool for understanding the one-sample t-test and related concepts.