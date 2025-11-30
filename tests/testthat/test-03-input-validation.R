# ============================================================
# TEST SUITE 03: Input Validation & Error Handling
# ============================================================
# Tests error handling for invalid inputs
# Tests edge cases and boundary conditions
# ============================================================

test_that("one_mean_test rejects non-numeric input", {
  expect_error(one_mean_test(c("a", "b", "c"), mu0 = 0),
               "must be a numeric vector")
})

test_that("one_mean_test rejects empty vectors", {
  expect_error(one_mean_test(numeric(0), mu0 = 0),
               "must have length >= 1")
})

test_that("one_mean_test handles NA values correctly", {
  x_with_na <- c(1, 2, NA, 4, 5)
  
  expect_warning(result <- one_mean_test(x_with_na, mu0 = 3,
                                          check_assumptions = FALSE),
                 "Removed 1 NA value")
  
  expect_equal(result$sample.stats$n, 4)
})

test_that("one_mean_test rejects all-NA input", {
  x_all_na <- as.numeric(c(NA, NA, NA))
  
  # Fixed: Match the actual error message
  expect_error(suppressWarnings(one_mean_test(x_all_na, mu0 = 0)),
               "All values.*are NA")
})

test_that("one_mean_test requires at least 2 values", {
  expect_error(suppressWarnings(one_mean_test(c(5, NA), mu0 = 5)),
               "at least 2 non-missing values")
})

test_that("Invalid alpha values are rejected", {
  x <- rnorm(20)
  
  expect_error(one_mean_test(x, mu0 = 0, alpha = 0),
               "alpha must be strictly between 0 and 1")
  expect_error(one_mean_test(x, mu0 = 0, alpha = 1),
               "alpha must be strictly between 0 and 1")
  expect_error(one_mean_test(x, mu0 = 0, alpha = -0.05),
               "alpha must be strictly between 0 and 1")
  expect_error(one_mean_test(x, mu0 = 0, alpha = 1.5),
               "alpha must be strictly between 0 and 1")
})

test_that("Invalid conf.level values are rejected", {
  x <- rnorm(20)
  
  expect_error(one_mean_test(x, mu0 = 0, conf.level = 0),
               "conf.level must be strictly between 0 and 1")
  expect_error(one_mean_test(x, mu0 = 0, conf.level = 1),
               "conf.level must be strictly between 0 and 1")
})

test_that("Invalid alternative hypotheses are rejected", {
  x <- rnorm(20)
  
  expect_error(one_mean_test(x, mu0 = 0, alternative = "not.valid"),
               "'arg' should be one of")
})

test_that("Edge case: sample size of 2", {
  x <- c(5, 7)
  
  expect_silent(result <- one_mean_test(x, mu0 = 6, check_assumptions = FALSE))
  expect_equal(as.numeric(result$parameter), 1)
})

test_that("Edge case: zero variance (constant data)", {
  x <- rep(5, 10)
  
  # Test when data equals mu0
  result_equal <- one_mean_test(x, mu0 = 5, check_assumptions = FALSE)
  expect_s3_class(result_equal, "oneMeanTest")
  expect_equal(result_equal$decision, "fail to reject H0")
  
  # Test when data differs from mu0
  result_diff <- one_mean_test(x, mu0 = 10, check_assumptions = FALSE)
  expect_s3_class(result_diff, "oneMeanTest")
  expect_equal(result_diff$decision, "reject H0")
  
  # Both should complete without error
  expect_true(is.numeric(result_equal$p.value))
  expect_true(is.numeric(result_diff$p.value))
})

test_that("Edge case: very large sample", {
  set.seed(999)
  x <- rnorm(1000, mean = 100, sd = 15)
  
  expect_silent(result <- one_mean_test(x, mu0 = 100, check_assumptions = FALSE))
  expect_equal(result$sample.stats$n, 1000)
})
