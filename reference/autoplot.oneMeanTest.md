# ggplot2 method for oneMeanTest objects

Creates ggplot2 visualizations of test results

## Usage

``` r
autoplot.oneMeanTest(
  data,
  type = c("distribution", "ci", "qq", "diagnostic"),
  theme = ggplot2::theme_minimal(),
  ...
)
```

## Arguments

- data:

  Object of class oneMeanTest

- type:

  Type of plot: "distribution", "ci", "qq", "diagnostic"

- theme:

  ggplot2 theme to use (default theme_minimal)

- ...:

  Additional arguments

## Value

ggplot object

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)
x <- rnorm(30, mean = 5, sd = 2)
result <- one_mean_test(x, mu0 = 5)
autoplot(result, type = "distribution")
} # }
```
