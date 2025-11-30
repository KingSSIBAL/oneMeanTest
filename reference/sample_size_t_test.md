# Calculate required sample size for desired power

Determines the sample size needed to achieve specified power

## Usage

``` r
sample_size_t_test(
  power = 0.8,
  delta,
  sd,
  alpha = 0.05,
  alternative = c("two.sided", "less", "greater")
)
```

## Arguments

- power:

  Desired power (default 0.80)

- delta:

  True difference from null value

- sd:

  Standard deviation

- alpha:

  Significance level (default 0.05)

- alternative:

  Alternative hypothesis

## Value

Required sample size (integer)

## Examples

``` r
# Sample size needed for 80% power to detect effect of 0.5 SD
sample_size_t_test(power = 0.80, delta = 0.5, sd = 1)
#> [1] 34
```
