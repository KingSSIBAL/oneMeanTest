# Adjust p-values for multiple testing

Applies various multiple testing correction methods

## Usage

``` r
adjust_p_values(
  p_values,
  method = c("bonferroni", "holm", "hochberg", "BH", "BY", "fdr", "none"),
  alpha = 0.05
)
```

## Arguments

- p_values:

  Numeric vector of p-values

- method:

  Correction method: "bonferroni", "holm", "hochberg", "BH"
  (Benjamini-Hochberg), "BY" (Benjamini-Yekutieli), "fdr"

- alpha:

  Family-wise error rate or FDR level (default 0.05)

## Value

Data frame with original and adjusted p-values

## Examples

``` r
# Multiple tests
p_vals <- c(0.001, 0.01, 0.04, 0.15, 0.50)
adjust_p_values(p_vals, method = "bonferroni")
#> 
#> Multiple Testing Correction
#> ============================================================ 
#> 
#> Method: bonferroni
#> Number of tests: 5
#> Significance level: 0.0500
#> Number rejected: 1 (20.0%)
#> 
#>  Test P_value P_adjusted Reject
#>     1   0.001      0.005   TRUE
#>     2   0.010      0.050  FALSE
#>     3   0.040      0.200  FALSE
#>     4   0.150      0.750  FALSE
#>     5   0.500      1.000  FALSE
```
