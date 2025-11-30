# Calculate detectable effect size for given power

Determines the effect size that can be detected with specified power.

## Usage

``` r
effect_size_t_test(
  n,
  power = 0.8,
  sd,
  alpha = 0.05,
  alternative = c("two.sided", "less", "greater")
)
```

## Arguments

- n:

  Sample size

- power:

  Desired power (default 0.80)

- sd:

  Standard deviation

- alpha:

  Significance level (default 0.05)

- alternative:

  Alternative hypothesis

## Value

Detectable effect size (in original units)

## Examples

``` r
# Effect size detectable with 80% power and n = 30
effect_size_t_test(n = 30, power = 0.80, sd = 1)
#> [1] 0.5270605
```
