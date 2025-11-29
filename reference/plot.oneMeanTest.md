# Plot methods for one-sample mean test objects

Plot methods for objects of class `"oneMeanTest"` produced by
[`one_mean_test`](one_mean_test.md). Different values of `which` produce
different visualizations:

## Usage

``` r
# S3 method for class 'oneMeanTest'
plot(x, which = c("t", "hist", "qq", "box", "ci"), ...)
```

## Arguments

- x:

  An object of class `"oneMeanTest"`.

- which:

  Character string specifying which plot to draw. One of `"t"`,
  `"hist"`, `"qq"`, `"box"`, or `"ci"`.

- ...:

  Additional arguments passed on to the underlying graphics functions
  (currently unused).

## Value

Invisibly returns `x` after producing a plot.

## Details

- `"t"`: t-distribution with the observed test statistic and critical
  values for the chosen `alpha`.

- `"hist"`: histogram of the data with vertical lines for the sample
  mean and hypothesized mean `mu0`.

- `"qq"`: normal Q-Q plot of the data with a reference line.

- `"box"`: boxplot of the data with a vertical line at `mu0`.

- `"ci"`: confidence interval plot for the mean.

For `"hist"`, `"qq"`, and `"box"`, the raw data must be stored in the
object via `attr(x, "data") <- your_data`.

## Examples

``` r
set.seed(123)
x <- rnorm(30, mean = 5, sd = 2)
res <- one_mean_test(x, mu0 = 5, check_assumptions = FALSE)
attr(res, "data") <- x

plot(res, which = "t")

plot(res, which = "ci")

plot(res, which = "hist")

plot(res, which = "qq")

plot(res, which = "box")

```
