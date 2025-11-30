# Calculate Hedges' g (bias-corrected Cohen's d)

Hedges' g corrects for small sample bias in Cohen's d

## Usage

``` r
hedges_g(x, mu0 = 0, conf.level = 0.95)
```

## Arguments

- x:

  Numeric vector of observations

- mu0:

  Null hypothesis value (default 0)

- conf.level:

  Confidence level for CI (default 0.95)

## Value

List with g, CI, and interpretation

## Examples

``` r
x <- rnorm(20, mean = 5, sd = 2)
hedges_g(x, mu0 = 0)
#> 
#> Effect Size Calculation
#> ================================================== 
#> 
#> Hedges' g = 3.0123
#> Cohen's d = 3.1378 (uncorrected)
#> Correction factor (J) = 0.9600
#> 
#> 95% Confidence Interval:
#>   [1.9665, 4.0443]
#> 
#> Interpretation: large effect size
#> 
#> Note: Hedges' g corrects Cohen's d for small sample bias
```
