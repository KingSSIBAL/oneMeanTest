# Perform multiple one-sample t-tests with correction

Conducts multiple one-sample t-tests and adjusts p-values

## Usage

``` r
multiple_t_tests(
  data_list,
  mu0_list,
  method = c("bonferroni", "holm", "BH", "BY"),
  alpha = 0.05,
  alternative = "two.sided"
)
```

## Arguments

- data_list:

  List of numeric vectors

- mu0_list:

  Vector of null values (same length as data_list)

- method:

  Multiple testing correction method

- alpha:

  Family-wise error rate (default 0.05)

- alternative:

  Alternative hypothesis

## Value

Object with all test results and corrections

## Examples

``` r
# Multiple datasets
data_list <- list(
  group1 = rnorm(30, mean = 5, sd = 2),
  group2 = rnorm(30, mean = 6, sd = 2),
  group3 = rnorm(30, mean = 5.5, sd = 2)
)

multiple_t_tests(data_list, mu0_list = c(5, 5, 5), method = "holm")
#> 
#> Multiple One-Sample T-Tests
#> ====================================================================== 
#> 
#> Number of tests: 3
#> Correction method: holm
#> Family-wise error rate: 0.0500
#> Tests rejected: 1 (33.3%)
#> 
#>    Test  n  Mean Mu0 t_statistic df   P_value P_adjusted Reject
#>  group1 30 4.999   5   -0.002964 29 1.0000000   1.000000  FALSE
#>  group2 30 6.212   5    3.800457 29 0.0006858   0.002058   TRUE
#>  group3 30 5.261   5    0.730362 29 0.4710699   0.942140  FALSE
```
