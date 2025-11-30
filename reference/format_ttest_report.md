# Format T-test Report

Formats the one-sample t-test results according to STAT 181 project
guidelines. Generates a professional, human-readable report with all
essential components including descriptive statistics, hypotheses, test
statistics, critical values, p-value, confidence interval, decision, and
interpretation.

## Usage

``` r
format_ttest_report(test_result, include_sections = c("all"))
```

## Arguments

- test_result:

  Object of class oneMeanTest from one_mean_test()

- include_sections:

  Character vector of sections to include: "descriptives", "hypotheses",
  "test_statistic", "critical_value", "p_value", "ci", "conclusion".
  Default is "all" which includes everything.

## Value

A formatted report as a character string that can be printed with cat()

## Examples

``` r
set.seed(123)
x <- rnorm(30, mean = 5, sd = 2)
result <- one_mean_test(x, mu0 = 5)
report <- format_ttest_report(result)
cat(report)
#> ======================================================================
#> ONE-SAMPLE T-TEST REPORT
#> ======================================================================
#> 
#> DESCRIPTIVE STATISTICS
#> ----------------------------------------------------------------------
#> Sample size (n):       30
#> Sample mean:           4.9058
#> Standard deviation:    1.9621
#> Standard error:        0.3582
#> 
#> HYPOTHESES
#> ----------------------------------------------------------------------
#> H0: μ = 5.0000
#> Ha: μ ≠ 5.0000
#> Significance level:    α = 0.0500
#> 
#> TEST STATISTICS
#> ----------------------------------------------------------------------
#> Test statistic (t):    -0.2630
#> Degrees of freedom:    29
#> Critical value:        ±2.0452
#> P-value:               0.7952
#> 
#> CONFIDENCE INTERVAL (95.0%)
#> ----------------------------------------------------------------------
#> [4.1731, 5.6384]
#> 
#> DECISION
#> ----------------------------------------------------------------------
#> At α = 0.0500: fail to reject H0
#> 
#> INTERPRETATION
#> ----------------------------------------------------------------------
#> At alpha = 0.0500, we fail to reject the null hypothesis that the
#> population mean equals 5.0000. The sample mean (4.9058) is different
#> from 5.0000 (t = -0.2630, df = 29, p-value = 0.7952).
#> 
#> ASSUMPTION CHECKS
#> ----------------------------------------------------------------------
#> 
#> ======================================================================

# Save report to file
writeLines(report, "ttest_report.txt")
```
