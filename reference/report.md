# Report Methods for oneMeanTest Objects

Generate formatted reports of t-test results in different output
formats. This provides an alternative to format_ttest_report() with
support for multiple output formats.

## Usage

``` r
report(x, format = "console", ...)

# S3 method for class 'oneMeanTest'
report(x, format = "console", ...)
```

## Arguments

- x:

  Object of class oneMeanTest

- format:

  Output format: "console", "markdown", or "latex"

- ...:

  Additional arguments (currently unused)

## Value

Formatted report as character string

## Examples

``` r
if (FALSE) { # \dontrun{
set.seed(123)
x <- rnorm(30, mean = 5, sd = 2)
result <- one_mean_test(x, mu0 = 5)

# Console format (default)
cat(report(result, format = "console"))

# Markdown format
cat(report(result, format = "markdown"))

# LaTeX format
cat(report(result, format = "latex"))
} # }
```
