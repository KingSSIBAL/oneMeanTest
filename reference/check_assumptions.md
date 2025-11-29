# Assumption checks for one-sample t-test

Check normality and outliers for a numeric sample, as typically required
for a one-sample t-test on a population mean with unknown variance.

## Usage

``` r
check_assumptions(x, alpha = 0.05, verbose = TRUE)
```

## Arguments

- x:

  A numeric vector of observations.

- alpha:

  Significance level for the Shapiro–Wilk normality test.

- verbose:

  Logical; if `TRUE`, prints a short textual summary of the results to
  the console.

## Value

A list of class `"oneMeanTest_assumptions"` with components:

- shapiro:

  List with W statistic, p-value, `alpha`, and a logical flag `normal`
  indicating if normality is acceptable (or `NA` if the test was not
  run).

- outliers:

  Numeric vector of detected outliers (possibly empty).

- n:

  Sample size after removing NA values.

- normal:

  Logical flag indicating if normality is acceptable (`TRUE` / `FALSE`),
  or `FALSE` if the Shapiro–Wilk test rejects at the chosen `alpha`.

- message:

  Textual summary of the diagnostics.

## Details

The function performs a Shapiro–Wilk normality test (when the sample
size is between 3 and 5000) and detects outliers using the 1.5\*IQR
rule. It is used inside [`one_mean_test`](one_mean_test.md) when
`check_assumptions = TRUE`.

## Examples

``` r
set.seed(123)
x <- rnorm(30, mean = 5, sd = 2)

# Verbose output
check_assumptions(x, alpha = 0.05, verbose = TRUE)
#> Normality appears acceptable based on the Shapiro-Wilk test. No outliers detected by the 1.5*IQR rule. 
#> $shapiro
#> $shapiro$W
#> [1] 0.9789351
#> 
#> $shapiro$p.value
#> [1] 0.7965839
#> 
#> $shapiro$alpha
#> [1] 0.05
#> 
#> $shapiro$normal
#> [1] TRUE
#> 
#> 
#> $outliers
#> numeric(0)
#> 
#> $n
#> [1] 30
#> 
#> $normal
#> [1] TRUE
#> 
#> $message
#> [1] "Normality appears acceptable based on the Shapiro-Wilk test. No outliers detected by the 1.5*IQR rule."
#> 
#> attr(,"class")
#> [1] "oneMeanTest_assumptions"

# Silent output (used internally by one_mean_test)
a_res <- check_assumptions(x, alpha = 0.05, verbose = FALSE)
a_res
#> $shapiro
#> $shapiro$W
#> [1] 0.9789351
#> 
#> $shapiro$p.value
#> [1] 0.7965839
#> 
#> $shapiro$alpha
#> [1] 0.05
#> 
#> $shapiro$normal
#> [1] TRUE
#> 
#> 
#> $outliers
#> numeric(0)
#> 
#> $n
#> [1] 30
#> 
#> $normal
#> [1] TRUE
#> 
#> $message
#> [1] "Normality appears acceptable based on the Shapiro-Wilk test. No outliers detected by the 1.5*IQR rule."
#> 
#> attr(,"class")
#> [1] "oneMeanTest_assumptions"
```
