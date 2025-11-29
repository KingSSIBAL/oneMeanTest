# Descriptive statistics for a numeric sample

Compute basic descriptive statistics for a numeric vector, including the
sample size, mean, median, standard deviation, variance, standard error,
quartiles, range, and interquartile range (IQR).

## Usage

``` r
descriptive_stats(x, digits = 3)
```

## Arguments

- x:

  A numeric vector of observations.

- digits:

  Optional integer; number of digits to round in the printed output. If
  `NULL`, no rounding is applied.

## Value

A data frame with one row of summary statistics:

- `n`: sample size (number of non-missing observations).

- `mean`: sample mean.

- `median`: sample median.

- `sd`: sample standard deviation.

- `variance`: sample variance.

- `se`: standard error of the mean.

- `min`, `q1`, `q3`, `max`: minimum, first quartile, third quartile, and
  maximum.

- `iqr`: interquartile range (`q3 - q1`).

## Details

This function is intended for numeric data such as measurements or
scores, typically the same type of data used in the one-sample t-test.

## Examples

``` r
x <- c(1, 2, 3, 4, 5)
descriptive_stats(x)
#>  n mean median    sd variance    se min q1 q3 max iqr
#>  5    3      3 1.581      2.5 0.707   1  2  4   5   2

# Without rounding
descriptive_stats(x, digits = NULL)
#>  n mean median       sd variance        se min q1 q3 max iqr
#>  5    3      3 1.581139      2.5 0.7071068   1  2  4   5   2
```
