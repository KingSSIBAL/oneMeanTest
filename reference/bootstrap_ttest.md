# Bootstrap one-sample t-test

Perform a simple non-parametric bootstrap procedure for the one-sample
t-test on a population mean with unknown variance.

## Usage

``` r
bootstrap_ttest(
  x,
  mu0 = 0,
  nboot = 1000,
  conf.level = 0.95,
  alternative = c("two.sided", "less", "greater"),
  seed = NULL
)
```

## Arguments

- x:

  Numeric vector of observations.

- mu0:

  Hypothesized population mean under the null hypothesis.

- nboot:

  Number of bootstrap resamples (integer, usually at least 100).

- conf.level:

  Confidence level for the bootstrap confidence interval.

- alternative:

  Character string specifying the alternative hypothesis, one of
  `"two.sided"`, `"less"`, or `"greater"`.

- seed:

  Optional integer seed for reproducibility.

## Value

A list of class `"oneMeanTest_bootstrap"` with components:

- t.obs:

  Observed t-statistic.

- t.boot:

  Numeric vector of bootstrap t-statistics.

- mean.obs:

  Observed sample mean.

- mean.boot:

  Numeric vector of bootstrap means.

- conf.int:

  Bootstrap percentile confidence interval for the mean (numeric vector
  of length 2 with attribute `conf.level`).

- p.value:

  Bootstrap p-value.

- nboot:

  Number of bootstrap resamples.

- mu0:

  Hypothesized mean.

- alternative:

  Alternative hypothesis used.

- call:

  The matched function call.

## Details

The function resamples the observed data with replacement, computes the
t-statistic for each bootstrap sample, and uses the empirical
distribution of these statistics to obtain a bootstrap p-value and a
percentile confidence interval for the mean.

This is useful for illustrating resampling-based inference and for
comparing classical t-test results with bootstrap results.

## Examples

``` r
set.seed(123)
x <- rnorm(30, mean = 5, sd = 2)

boot_res <- bootstrap_ttest(
  x,
  mu0 = 5,
  nboot = 500,
  conf.level = 0.95,
  alternative = "two.sided",
  seed = 123
)
boot_res
#> Bootstrap one-sample t-test
#> 
#> Call:
#> bootstrap_ttest(x = x, mu0 = 5, nboot = 500, conf.level = 0.95, 
#>     alternative = "two.sided", seed = 123)
#> 
#> Observed t-statistic: -0.2630
#> Bootstrap p-value  : 0.82
#> 95.0000% bootstrap CI for the mean:
#>   [4.1919, 5.5519]
#> Number of resamples: 500
```
