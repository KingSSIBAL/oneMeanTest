# Introduction to oneMeanTest

knitr::opts_chunk\$set( collapse = TRUE, comment = “#\>” )

## Overview

The `oneMeanTest` package provides tools for performing a one-sample
t-test for a population mean when the variance is unknown, along with
descriptive statistics, assumption checks, bootstrap-based inference,
power analysis, and informative plots.

This vignette shows a complete analysis workflow using simulated data
that mimics a real application.

## Setup

library(oneMeanTest) set.seed(123)

## Example data

Suppose we measure a numeric outcome (e.g., exam scores, weights, or
waiting times) on $n = 30$ individuals, and we want to test whether the
population mean equals a hypothesized value $\mu_{0} = 5$.

Here we simulate such a dataset:

x \<- rnorm(30, mean = 5, sd = 2) head(x) length(x)

You can replace this simulated data with your own dataset when using the
package in practice.

## Descriptive statistics

Before running the hypothesis test, we examine the basic descriptive
statistics.

ds \<- descriptive_stats(x, digits = NULL) ds

The output includes the sample size, mean, median, standard deviation,
standard error, variance, range, quartiles, and IQR.

## One-sample t-test

We now perform a one-sample t-test of

$$H_{0}:\mu = 5\quad\text{vs.}\quad H_{a}:\mu \neq 5.$$

res \<- one_mean_test( x, mu0 = 5, alternative = “two.sided”, alpha =
0.05, conf.level = 0.95, check_assumptions = TRUE )

res

The printed result shows:

- the t statistic and degrees of freedom  
- the p-value  
- a confidence interval for the mean  
- the sample mean  
- the decision to reject or fail to reject $H_{0}$  
- a plain-language interpretation.

## Assumption checks

The one-sample t-test assumes that the data are approximately normally
distributed (or that the sample size is large enough for the central
limit theorem to apply).

The [`check_assumptions()`](../reference/check_assumptions.md) function,
which is called inside
[`one_mean_test()`](../reference/one_mean_test.md) when
`check_assumptions = TRUE`, provides a basic assessment:

assump \<- check_assumptions(x, alpha = 0.05, verbose = FALSE) assump

Interpret these results to decide whether the normality assumption is
reasonable.

## Graphical summaries

The [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method for
`oneMeanTest` objects provides several useful visualizations.

First, a t-distribution with the observed t statistic and critical
values:

plot(res, which = “t”)

Next, a confidence interval plot for the mean:

plot(res, which = “ci”)

If you store the raw data in the result object, you can also draw a
histogram, boxplot, and normal Q-Q plot:

attr(res, “data”) \<- x

plot(res, which = “hist”) plot(res, which = “box”) plot(res, which =
“qq”)

These plots can be used directly as screenshots in your presentation.

## Bootstrap-based inference

The [`bootstrap_ttest()`](../reference/bootstrap_ttest.md) function
implements a simple non-parametric bootstrap procedure for the
one-sample mean.

boot_res \<- bootstrap_ttest( x, mu0 = 5, nboot = 500, conf.level =
0.95, alternative = “two.sided”, seed = 123 )

boot_res

You can compare the bootstrap confidence interval and p-value to the
classical t-test results as part of your discussion.

## Power analysis

Finally, `power_analysis_one_mean()` wraps
[`stats::power.t.test()`](https://rdrr.io/r/stats/power.t.test.html) for
one-sample t-tests.

For example, to compute the power for detecting a true difference of 1
unit with $n = 30$, $sd = 2$, and $\alpha = 0.05$:

p_res \<- power_analysis_one_mean( n = 30, delta = 1, sd = 2, sig.level
= 0.05, power = NULL, alternative = “two.sided” )

p_res

To find the required sample size for 80% power:

n_res \<- power_analysis_one_mean( n = NULL, delta = 1, sd = 2,
sig.level = 0.05, power = 0.8, alternative = “two.sided” )

n_res

## Summary

In this vignette, we:

- simulated a numeric dataset;  
- computed descriptive statistics;  
- performed a one-sample t-test using
  [`one_mean_test()`](../reference/one_mean_test.md);  
- checked assumptions, created informative plots, and compared
  bootstrap-based inference;  
- explored power analysis using `power_analysis_one_mean()`.

You can adapt this workflow to your own data to analyze a single
population mean with unknown variance and to create materials for your
STAT 181 presentation.
