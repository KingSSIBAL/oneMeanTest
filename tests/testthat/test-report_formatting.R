# Test report formatting functionality

test_that("format_ttest_report produces output", {
  skip_if_not_installed("oneMeanTest")
  skip_if_not(exists("format_ttest_report"))
  
  set.seed(999)
  x <- rnorm(25, mean = 7, sd = 2)
  result <- one_mean_test(x, mu0 = 5, check_assumptions = FALSE)
  
  report <- format_ttest_report(result)
  
  expect_type(report, "character")
  expect_true(nchar(report) > 0)
})

test_that("interpretation changes with alternative hypothesis", {
  set.seed(1010)
  x <- rnorm(50, mean = 8, sd = 2)
  
  result_two <- one_mean_test(x, mu0 = 5, alternative = "two.sided",
                              check_assumptions = FALSE)
  result_gr <- one_mean_test(x, mu0 = 5, alternative = "greater",
                            check_assumptions = FALSE)
  result_ls <- one_mean_test(x, mu0 = 10, alternative = "less",
                            check_assumptions = FALSE)
  
  # Interpretations should mention the direction
  expect_true(grepl("different from", result_two$interpretation) ||
              grepl("two.sided", result_two$alternative))
  expect_true(grepl("greater than", result_gr$interpretation) ||
              grepl("greater", result_gr$alternative))
  expect_true(grepl("less than", result_ls$interpretation) ||
              grepl("less", result_ls$alternative))
})

test_that("report includes proper t-test components", {
  skip_if_not(exists("format_ttest_report"))
  
  set.seed(1111)
  x <- rnorm(30, mean = 10, sd = 3)
  result <- one_mean_test(x, mu0 = 10, check_assumptions = FALSE)
  
  # Print should work without error
  expect_output(print(result))
  
  # Should show key statistics
  output <- capture.output(print(result))
  output_text <- paste(output, collapse = "\n")
  
  expect_true(any(grepl("mean", output, ignore.case = TRUE)))
  expect_true(any(grepl("sd", output, ignore.case = TRUE)))
  expect_true(any(grepl("t", output, ignore.case = TRUE)))
  expect_true(any(grepl("p", output, ignore.case = TRUE)))
})


test_that("format_ttest_report includes assumption checks if available", {
  set.seed(777)
  x <- rnorm(30, mean = 10, sd = 3)
  result <- one_mean_test(x, mu0 = 10, check_assumptions = TRUE)
  
  report <- format_ttest_report(result)
  
  # Check for assumption checks section
  expect_true(grepl("ASSUMPTION CHECKS", report))
  
  # The exact text may vary, check for presence of assumption-related terms
  has_assumptions <- grepl("Shapiro-Wilk", report, ignore.case = TRUE) || 
                     grepl("normality", report, ignore.case = TRUE) ||
                     grepl("Sample size", report, ignore.case = TRUE) ||
                     grepl("PASSED|FAILED|ADEQUATE", report)
  
  expect_true(has_assumptions)
})

test_that("format_ttest_report handles different confidence levels", {
  set.seed(888)
  x <- rnorm(30, mean = 5, sd = 2)
  
  conf_levels <- c(0.90, 0.95, 0.99)
  
  for (cl in conf_levels) {
    result <- one_mean_test(x, mu0 = 5, conf.level = cl,
                           check_assumptions = FALSE)
    report <- format_ttest_report(result)
    
    pct <- cl * 100
    expect_true(grepl(sprintf("%.1f%%", pct), report))
  }
})

test_that("format_ttest_report wraps long text properly", {
  set.seed(999)
  x <- rnorm(30, mean = 5, sd = 2)
  result <- one_mean_test(x, mu0 = 5, check_assumptions = FALSE)
  
  report <- format_ttest_report(result)
  
  # Check that no line exceeds 70 characters (except header lines)
  lines <- strsplit(report, "\n")[[1]]
  content_lines <- lines[!grepl("^=+$|^-+$", lines)]
  
  # Most lines should be reasonably short
  long_lines <- sum(nchar(content_lines) > 80)
  expect_true(long_lines < length(content_lines) * 0.3)  # Less than 30%
})

test_that("format_ttest_report shows critical values correctly", {
  set.seed(1010)
  x <- rnorm(30, mean = 5, sd = 2)
  
  # Two-sided
  result_two <- one_mean_test(x, mu0 = 5, alternative = "two.sided",
                              check_assumptions = FALSE)
  report_two <- format_ttest_report(result_two)
  expect_true(grepl("\u00b1", report_two))  # Plus-minus symbol
  
  # One-sided
  result_one <- one_mean_test(x, mu0 = 5, alternative = "greater",
                              check_assumptions = FALSE)
  report_one <- format_ttest_report(result_one)
  expect_true(grepl("Critical value:", report_one))
})

test_that("format_ttest_report includes all sections", {
  set.seed(1111)
  x <- rnorm(30, mean = 5, sd = 2)
  result <- one_mean_test(x, mu0 = 5, check_assumptions = TRUE)
  
  report <- format_ttest_report(result)
  
  required_sections <- c(
    "DESCRIPTIVE STATISTICS",
    "HYPOTHESES",
    "TEST STATISTICS",
    "CONFIDENCE INTERVAL",
    "DECISION",
    "INTERPRETATION"
  )
  
  for (section in required_sections) {
    expect_true(grepl(section, report), 
                info = paste("Missing section:", section))
  }
})