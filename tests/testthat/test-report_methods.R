# Tests for report methods

test_that("report generic function works", {
  set.seed(123)
  x <- rnorm(30, mean = 5, sd = 2)
  result <- one_mean_test(x, mu0 = 5, check_assumptions = FALSE)
  
  # Should work without error
  expect_error(report(result), NA)
})

test_that("report.oneMeanTest console format works", {
  set.seed(456)
  x <- rnorm(25, mean = 10, sd = 3)
  result <- one_mean_test(x, mu0 = 10, check_assumptions = FALSE)
  
  console_report <- report(result, format = "console")
  
  expect_type(console_report, "character")
  expect_true(nchar(console_report) > 0)
  expect_true(grepl("ONE-SAMPLE T-TEST", console_report, ignore.case = TRUE))
})

test_that("report.oneMeanTest markdown format works", {
  set.seed(789)
  x <- rnorm(30, mean = 7, sd = 2)
  result <- one_mean_test(x, mu0 = 7, check_assumptions = FALSE)
  
  md_report <- report(result, format = "markdown")
  
  expect_type(md_report, "character")
  expect_true(grepl("#", md_report))  # Markdown headers
  expect_true(grepl("\\|", md_report))  # Markdown tables
  expect_true(grepl("Hypotheses", md_report))
})

test_that("report.oneMeanTest latex format works", {
  set.seed(111)
  x <- rnorm(20, mean = 15, sd = 4)
  result <- one_mean_test(x, mu0 = 15, check_assumptions = FALSE)
  
  latex_report <- report(result, format = "latex")
  
  expect_type(latex_report, "character")
  expect_true(grepl("\\\\", latex_report))  # LaTeX commands
  expect_true(grepl("tabular", latex_report))  # LaTeX tables
  expect_true(grepl("subsection", latex_report))
})

test_that("report validates format argument", {
  set.seed(222)
  x <- rnorm(30, mean = 5, sd = 2)
  result <- one_mean_test(x, mu0 = 5, check_assumptions = FALSE)
  
  expect_error(
    report(result, format = "invalid"),
    "'arg' should be one of"
  )
})

test_that("report markdown includes all required sections", {
  set.seed(444)
  x <- rnorm(40, mean = 12, sd = 3)
  result <- one_mean_test(x, mu0 = 10, alternative = "greater",
                         check_assumptions = FALSE)
  
  md <- report(result, format = "markdown")
  
  # Check for required sections
  expect_true(grepl("Hypotheses", md))
  expect_true(grepl("Descriptive Statistics", md))
  expect_true(grepl("Test Results", md))
  expect_true(grepl("Confidence Interval", md))
  expect_true(grepl("Decision", md))
  expect_true(grepl("Interpretation", md))
})

test_that("report latex includes mathematical notation", {
  set.seed(555)
  x <- rnorm(35, mean = 8, sd = 2)
  result <- one_mean_test(x, mu0 = 8, alternative = "less",
                         check_assumptions = FALSE)
  
  latex <- report(result, format = "latex")
  
  # Check for LaTeX math
  expect_true(grepl("\\$", latex))  # Dollar signs for math
  expect_true(grepl("mu", latex))  # Greek letters
  expect_true(grepl("alpha", latex))
})

test_that("report handles different alternatives correctly", {
  set.seed(666)
  x <- rnorm(30, mean = 5, sd = 2)
  
  alternatives <- c("two.sided", "greater", "less")
  
  for (alt in alternatives) {
    result <- one_mean_test(x, mu0 = 5, alternative = alt,
                           check_assumptions = FALSE)
    
    md <- report(result, format = "markdown")
    latex <- report(result, format = "latex")
    
    expect_type(md, "character")
    expect_type(latex, "character")
    expect_true(nchar(md) > 100)
    expect_true(nchar(latex) > 100)
  }
})

test_that("report console uses format_ttest_report", {
  set.seed(777)
  x <- rnorm(30, mean = 5, sd = 2)
  result <- one_mean_test(x, mu0 = 5, check_assumptions = FALSE)
  
  console <- report(result, format = "console")
  direct <- format_ttest_report(result)
  
  # Should be the same
  expect_equal(console, direct)
})

test_that("report markdown creates proper tables", {
  set.seed(888)
  x <- rnorm(25, mean = 10, sd = 2)
  result <- one_mean_test(x, mu0 = 10, check_assumptions = FALSE)
  
  md <- report(result, format = "markdown")
  
  # Check for table syntax
  expect_true(grepl("\\|.*\\|.*\\|", md))  # Table rows
  expect_true(grepl("\\|-+\\|-+\\|", md))  # Table separator
})

test_that("report latex creates proper tables", {
  set.seed(999)
  x <- rnorm(30, mean = 7, sd = 2)
  result <- one_mean_test(x, mu0 = 7, check_assumptions = FALSE)
  
  latex <- report(result, format = "latex")
  
  # Check for LaTeX table elements
  expect_true(grepl("\\\\begin\\{tabular\\}", latex))
  expect_true(grepl("\\\\end\\{tabular\\}", latex))
  expect_true(grepl("\\\\hline", latex))
})

test_that("report formats critical values correctly", {
  set.seed(1010)
  x <- rnorm(30, mean = 5, sd = 2)
  
  # Two-sided
  result_two <- one_mean_test(x, mu0 = 5, alternative = "two.sided",
                              check_assumptions = FALSE)
  md_two <- report(result_two, format = "markdown")
  expect_true(grepl("Critical value", md_two))
  
  # One-sided
  result_one <- one_mean_test(x, mu0 = 5, alternative = "greater",
                              check_assumptions = FALSE)
  md_one <- report(result_one, format = "markdown")
  expect_true(grepl("Critical value", md_one))
})
