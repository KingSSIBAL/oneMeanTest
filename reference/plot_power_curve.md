# Plot power curve

Creates a plot showing how power changes with sample size.

## Usage

``` r
plot_power_curve(
  delta,
  sd,
  alpha = 0.05,
  alternative = c("two.sided", "less", "greater"),
  n_range = 10:100
)
```

## Arguments

- delta:

  Effect size

- sd:

  Standard deviation

- alpha:

  Significance level (default 0.05)

- alternative:

  Alternative hypothesis

- n_range:

  Range of sample sizes to plot (default 10 to 100)

## Value

Invisible data frame with n and power values
