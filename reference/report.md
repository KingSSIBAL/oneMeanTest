# Report Methods for One-Sample T-Test

Functions to generate formatted reports of test results in various
formats (text, markdown, LaTeX) Generate report from test results

## Usage

``` r
report(x, format = c("text", "markdown", "latex"), file = NULL, ...)
```

## Arguments

- x:

  Object of class oneMeanTest

- format:

  Output format: "text", "markdown", or "latex"

- file:

  Optional file path to save report

- ...:

  Additional arguments

## Value

Character string with formatted report (invisible if file specified)

## Details

Creates a formatted report of one-sample t-test results
