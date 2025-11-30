# Power Analysis Functions for One-Sample T-Test

Functions to calculate statistical power, required sample size, and
detectable effect sizes for one-sample t-tests

## Usage

``` r
power_t_test(
  n,
  delta,
  sd,
  alpha = 0.05,
  alternative = c("two.sided", "less", "greater")
)
```

## Arguments

- n:

  Sample size

- delta:

  True difference from null value (effect in original units)

- sd:

  Standard deviation

- alpha:

  Significance level (default 0.05)

- alternative:

  Alternative hypothesis: "two.sided", "less", or "greater"

## Value

Numeric power value (0 to 1)

## Examples

``` r
# Power to detect effect of 0.5 SD with n=30
power_t_test(n = 30, delta = 0.5, sd = 1, alpha = 0.05)
#> [1] 0.7539647
```
