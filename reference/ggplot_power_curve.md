# Power curve plot with ggplot2

Power curve plot with ggplot2

## Usage

``` r
ggplot_power_curve(
  delta,
  sd,
  alpha = 0.05,
  n_range = 10:100,
  theme = ggplot2::theme_minimal()
)
```

## Arguments

- delta:

  Effect size

- sd:

  Standard deviation

- alpha:

  Significance level

- n_range:

  Range of sample sizes

- theme:

  ggplot2 theme

## Value

ggplot object
