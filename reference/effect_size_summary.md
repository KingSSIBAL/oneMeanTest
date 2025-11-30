# Calculate multiple effect sizes

Computes Cohen's d, Hedges' g, and provides comprehensive interpretation

## Usage

``` r
effect_size_summary(x, mu0 = 0, conf.level = 0.95)
```

## Arguments

- x:

  Numeric vector of observations

- mu0:

  Null hypothesis value

- conf.level:

  Confidence level for CIs

## Value

Data frame with multiple effect size measures

## Examples

``` r
x <- rnorm(30, mean = 5, sd = 2)
effect_size_summary(x, mu0 = 0)
#> 
#> Effect Size Summary
#> ====================================================================== 
#> 
#> Sample size: n = 30
#> Null value: mu0 = 0.0000
#> Confidence level: 95%
#> 
#>    Measure Estimate CI_Lower CI_Upper    Interpretation
#>  Cohen's d    2.825    2.012    3.626 large effect size
#>  Hedges' g    2.751    1.960    3.531 large effect size
```
