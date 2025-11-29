# Test print and display methods

test_that("print.oneMeanTest displays all essential information", {
  set.seed(1212)
  x <- rnorm(25, mean = 12, sd = 3)
  result <- one_mean_test(x, mu0 = 10, check_assumptions = FALSE)
  
  output <- capture.output(print(result))
  output_text <- paste(output, collapse = "\n")
  
  # Check for essential components
  expect_true(grepl("One-sample t-test", output_text))
  expect_true(grepl("mean", output_text, ignore.case = TRUE))
  expect_true(grepl("sd", output_text, ignore.case = TRUE))
  expect_true(grepl("df", output_text, ignore.case = TRUE))
  expect_true(grepl("p-val", output_text, ignore.case = TRUE))
  expect_true(grepl("confidence interval", output_text, ignore.case = TRUE))
  expect_true(grepl("Decision", output_text))
  expect_true(grepl("Interpretation", output_text))
})

test_that("print shows t-critical value when present", {
  set.seed(1313)
  x <- rnorm(30, mean = 5, sd = 2)
  result <- one_mean_test(x, mu0 = 5, check_assumptions = FALSE)
  
  output <- capture.output(print(result))
  output_text <- paste(output, collapse = "\n")
  
  # If t.critical is in result, should be printed
  if ("t.critical" %in% names(result)) {
    expect_true(grepl("t-crit", output_text, ignore.case = TRUE) ||
                grepl("critical", output_text, ignore.case = TRUE))
  }
})

test_that("print formatting is clean and readable", {
  set.seed(1414)
  x <- rnorm(20, mean = 8, sd = 1.5)
  result <- one_mean_test(x, mu0 = 8, check_assumptions = FALSE)
  
  output <- capture.output(print(result))
  
  # Should have reasonable number of lines
  expect_true(length(output) > 5)
  expect_true(length(output) < 50)
  
  # Should not have excessive blank lines
  blank_lines <- sum(output == "")
  expect_true(blank_lines < length(output) / 2)
})

test_that("summary method works", {
  set.seed(1515)
  x <- rnorm(30, mean = 15, sd = 3)
  result <- one_mean_test(x, mu0 = 14, check_assumptions = FALSE)
  
  expect_output(summary(result))
  expect_invisible(summary(result))
})
