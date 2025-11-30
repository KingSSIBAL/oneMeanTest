# One-sample t-test for a population mean (unknown variance)

Perform a one-sample t-test on a population mean when the population
variance is unknown, using the Student's t-distribution. The function
returns the usual test statistic, p-value, confidence interval, and a
plain-language interpretation, and can optionally run assumption checks.

## Usage

``` r
one_mean_test(
  x,
  mu0 = 0,
  alternative = c("two.sided", "less", "greater"),
  alpha = 0.05,
  conf.level = 0.95,
  check_assumptions = TRUE
)
```

## Arguments

- x:

  A numeric vector of observations.

- mu0:

  The hypothesized population mean under the null hypothesis.

- alternative:

  Character string specifying the alternative hypothesis, one of
  `"two.sided"`, `"less"`, or `"greater"`.

- alpha:

  Significance level used for the hypothesis decision.

- conf.level:

  Confidence level for the confidence interval.

- check_assumptions:

  Logical; if `TRUE`, performs assumption checks via
  [`check_assumptions`](check_assumptions.md) and stores the result in
  the returned object.

## Value

An object of class `"oneMeanTest"` containing:

- statistic:

  Named numeric vector with the t statistic.

- parameter:

  Named numeric vector with the degrees of freedom.

- p.value:

  P-value of the test.

- conf.int:

  Numeric vector with the confidence interval for the mean, with
  attribute `conf.level`.

- estimate:

  Named numeric vector with the sample mean.

- null.value:

  Named numeric vector with the hypothesized mean (`mu0`).

- alternative:

  Character string with the alternative hypothesis.

- t.critical:

  Named vector with critical value(s) for the test.

- method:

  Description of the method used.

- data.name:

  Character string with the name of the data vector.

- sample.stats:

  List with `n`, `mean`, `sd`, and `se`.

- alpha:

  Significance level used.

- assumptions:

  Result from [`check_assumptions()`](check_assumptions.md) or `NULL`.

- decision:

  Character string: `"reject H0"` or `"fail to reject H0"`.

- interpretation:

  Plain-language interpretation of the result.

## Examples

``` r
set.seed(123)
x <- rnorm(30, mean = 5, sd = 2)

# Basic two-sided test with default alpha and conf.level
res <- one_mean_test(x, mu0 = 5)
res
#> One-sample t-test for a population mean (unknown variance) 
#> Data: c(3.87904870689558, 4.53964502103344, 8.11741662829825, 5.14101678284915,  5.25857547032189, 8.43012997376656, 5.9218324119784, 2.46987753078693,  3.62629429621295, 4.10867605980008, 7.44816359487892, 5.71962765411473,  5.8015429011881, 5.22136543189024, 3.88831773049185, 8.57382627360616,  5.99570095645848, 1.06676568674072, 6.40271180312737, 4.05441718454413,  2.86435258802631, 4.56405017068341, 2.94799110338552, 3.54221754141772,  3.74992146430149, 1.62661337851517, 6.67557408898905, 5.30674623567303,  2.7237261259761, 7.50762984213985) 
#> 
#> Hypothesized mean (H0): 5 
#> Alternative hypothesis: two.sided 
#> 
#> Sample statistics:
#>   n    = 30
#>   mean = 4.9058
#>   sd   = 1.9621
#> 
#> Test results:
#>   t     = -0.2630
#>   df    = 29
#>   p-val = 0.7952
#>   t-crit = ±2.0452
#>   alpha = 0.0500
#> 
#> 95.0000% confidence interval for the mean:
#>   [4.1731, 5.6384]
#> 
#> Decision: fail to reject H0 
#> Interpretation:
#>   At alpha = 0.0500, we fail to reject the null hypothesis that the population mean equals 5.0000. The sample mean (4.9058) is different from 5.0000 (t = -0.2630, df = 29, p-value = 0.7952). 

# One-sided alternative and different confidence level
res_less <- one_mean_test(x, mu0 = 5, alternative = "less",
                          alpha = 0.01, conf.level = 0.99)
res_less
#> One-sample t-test for a population mean (unknown variance) 
#> Data: c(3.87904870689558, 4.53964502103344, 8.11741662829825, 5.14101678284915,  5.25857547032189, 8.43012997376656, 5.9218324119784, 2.46987753078693,  3.62629429621295, 4.10867605980008, 7.44816359487892, 5.71962765411473,  5.8015429011881, 5.22136543189024, 3.88831773049185, 8.57382627360616,  5.99570095645848, 1.06676568674072, 6.40271180312737, 4.05441718454413,  2.86435258802631, 4.56405017068341, 2.94799110338552, 3.54221754141772,  3.74992146430149, 1.62661337851517, 6.67557408898905, 5.30674623567303,  2.7237261259761, 7.50762984213985) 
#> 
#> Hypothesized mean (H0): 5 
#> Alternative hypothesis: less 
#> 
#> Sample statistics:
#>   n    = 30
#>   mean = 4.9058
#>   sd   = 1.9621
#> 
#> Test results:
#>   t     = -0.2630
#>   df    = 29
#>   p-val = 0.3976
#>   t-crit = -2.4620
#>   alpha = 0.0100
#> 
#> 99.0000% confidence interval for the mean:
#>   [3.9184, 5.8932]
#> 
#> Decision: fail to reject H0 
#> Interpretation:
#>   At alpha = 0.0100, we fail to reject the null hypothesis that the population mean equals 5.0000. The sample mean (4.9058) is less than 5.0000 (t = -0.2630, df = 29, p-value = 0.3976). 
```
