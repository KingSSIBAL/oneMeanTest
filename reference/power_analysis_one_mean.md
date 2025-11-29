# Power analysis for one-sample t-test

Wrapper around
[`power.t.test`](https://rdrr.io/r/stats/power.t.test.html) for the
one-sample t-test on a population mean with unknown variance.

## Usage

``` r
power_analysis_one_mean(
  n = NULL,
  delta = NULL,
  sd = 1,
  sig.level = 0.05,
  power = NULL,
  alternative = c("two.sided", "one.sided")
)
```

## Arguments

- n:

  Sample size (or `NULL` if to be computed).

- delta:

  True difference in means (\\\mu - \mu_0\\).

- sd:

  Standard deviation (estimated or assumed).

- sig.level:

  Significance level (alpha).

- power:

  Desired power (or `NULL` if to be computed).

- alternative:

  Character string specifying the alternative hypothesis, either
  `"two.sided"` or `"one.sided"`.

## Value

An object of class `"power.htest"` as returned by
[`power.t.test`](https://rdrr.io/r/stats/power.t.test.html) with
`type = "one.sample"`.

## Details

You must supply exactly two of the three arguments `n`, `delta`, and
`power`; the third will be computed, as in
[`power.t.test`](https://rdrr.io/r/stats/power.t.test.html).

This function is useful for:

- computing the power of a one-sample t-test for a given sample size,
  effect size (`delta`), and significance level; or

- computing the required sample size to achieve a desired power.

## Examples

``` r
# Power for n = 30, true difference = 1, sd = 2
p_res <- power_analysis_one_mean(
  n = 30,
  delta = 1,
  sd = 2,
  sig.level = 0.05,
  power = NULL,
  alternative = "two.sided"
)
p_res
#> 
#>      One-sample t test power calculation 
#> 
#>               n = 30
#>           delta = 1
#>              sd = 2
#>       sig.level = 0.05
#>           power = 0.7539627
#>     alternative = two.sided
#> 

# Required n for 80% power
n_res <- power_analysis_one_mean(
  n = NULL,
  delta = 1,
  sd = 2,
  sig.level = 0.05,
  power = 0.8,
  alternative = "two.sided"
)
n_res
#> 
#>      One-sample t test power calculation 
#> 
#>               n = 33.3672
#>           delta = 1
#>              sd = 2
#>       sig.level = 0.05
#>           power = 0.8
#>     alternative = two.sided
#> 
```
