# Wilcoxon Signed-Rank Test

Non-parametric alternative to one-sample t-test when normality
assumption is violated

## Usage

``` r
wilcoxon_test(
  x,
  mu0 = 0,
  alternative = c("two.sided", "less", "greater"),
  conf.level = 0.95,
  exact = NULL
)
```

## Arguments

- x:

  Numeric vector of observations

- mu0:

  Hypothesized median (default 0)

- alternative:

  Alternative hypothesis: "two.sided", "less", or "greater"

- conf.level:

  Confidence level for interval estimate (default 0.95)

- exact:

  Use exact p-value calculation (default TRUE for n \< 50)

## Value

Object of class oneMeanTest_wilcoxon

## Examples

``` r
# Non-normal data
x <- rexp(30, rate = 0.1)
wilcoxon_test(x, mu0 = 10)
#> 
#> Wilcoxon signed rank exact test
#> 
#> data: x
#> V = 197, n = 30, p-value = 0.4716
#> alternative hypothesis: true location is not equal to 10.0000
#> 95% confidence interval:
#>  [6.0994, 12.3825]
#> sample estimates:
#> pseudo-median 8.5284
```
