# Calculate Cohen's d with confidence interval

Computes Cohen's d effect size with bootstrap confidence interval

## Usage

``` r
cohens_d_ci(
  x,
  mu0 = 0,
  conf.level = 0.95,
  method = c("bootstrap", "nct"),
  nboot = 1000
)
```

## Arguments

- x:

  Numeric vector of observations

- mu0:

  Null hypothesis value (default 0)

- conf.level:

  Confidence level for CI (default 0.95)

- method:

  Method for CI: "bootstrap" or "nct" (noncentral t)

- nboot:

  Number of bootstrap samples (if method = "bootstrap")

## Value

List with d, CI, and interpretation

## Examples

``` r
x <- rnorm(30, mean = 5, sd = 2)
cohens_d_ci(x, mu0 = 0)
#> 
#> Effect Size Calculation
#> ================================================== 
#> 
#> Cohen's d = 3.2069
#> 
#> 95% Confidence Interval:
#>   [2.5650, 4.5035]
#> 
#> Interpretation: large effect size
```
