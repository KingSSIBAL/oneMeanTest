# ============================================================
# TEST SUITE 12: Report Methods
# ============================================================

test_that("report.oneMeanTest generates complete report", {
  set.seed(123)
  x <- rnorm(30, mean = 5, sd = 2)
  result <- one_mean_test(x, mu0 = 5, check_assumptions = FALSE)
  
  rpt <- report(result)
  
  expect_type(rpt, "character")
  expect_true(nchar(rpt) > 100)
})

test_that("summary.oneMeanTest provides concise summary", {
  set.seed(456)
  x <- rnorm(40, mean = 100, sd = 15)
  result <- one_mean_test(x, mu0 = 100, check_assumptions = FALSE)
  
  expect_output(summary(result))
})

test_that("report includes all sections", {
  set.seed(789)
  x <- rnorm(30, mean = 5, sd = 2)
  result <- one_mean_test(x, mu0 = 5, check_assumptions = TRUE)
  
  rpt <- report(result)
  
  expect_true(grepl("Descriptive|DESCRIPTIVE", rpt, ignore.case = TRUE))
  expect_true(grepl("Test Statistic|TEST STATISTIC", rpt, ignore.case = TRUE))
  expect_true(grepl("P-value|P-VALUE", rpt, ignore.case = TRUE))
  expect_true(grepl("Confidence Interval|CONFIDENCE INTERVAL", rpt, ignore.case = TRUE))
  expect_true(grepl("Decision|DECISION", rpt, ignore.case = TRUE))
})

test_that("report can be saved to file", {
  set.seed(111)
  x <- rnorm(30, mean = 5, sd = 2)
  result <- one_mean_test(x, mu0 = 5, check_assumptions = FALSE)
  
  temp_file <- tempfile(fileext = ".txt")
  
  expect_message(report(result, file = temp_file), "Report saved")
  expect_true(file.exists(temp_file))
  
  # Clean up
  unlink(temp_file)
})

test_that("markdown report generation", {
  set.seed(222)
  x <- rnorm(30, mean = 5, sd = 2)
  result <- one_mean_test(x, mu0 = 5, check_assumptions = FALSE)
  
  md_report <- report(result, format = "markdown")
  
  expect_type(md_report, "character")
  expect_true(grepl("#", md_report))
})

test_that("LaTeX report generation", {
  set.seed(333)
  x <- rnorm(30, mean = 5, sd = 2)
  result <- one_mean_test(x, mu0 = 5, check_assumptions = FALSE)
  
  tex_report <- report(result, format = "latex")
  
  expect_type(tex_report, "character")
  expect_true(grepl("\\\\", tex_report))
})
