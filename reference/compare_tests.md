# Compare parametric and non-parametric tests

Runs t-test, Wilcoxon, and sign test side-by-side for comparison

## Usage

``` r
compare_tests(x, mu0 = 0, alternative = c("two.sided", "less", "greater"))
```

## Arguments

- x:

  Numeric vector

- mu0:

  Null hypothesis value

- alternative:

  Alternative hypothesis

## Value

Data frame comparing results
