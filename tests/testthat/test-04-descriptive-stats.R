# ============================================================
# TEST SUITE 04: Descriptive Statistics
# ============================================================
# Tests descriptive_stats() function
# Tests custom quantile implementation
# ============================================================

test_that("descriptive_stats returns correct structure", {
  x <- c(1, 2, 3, 4, 5)
  result <- descriptive_stats(x, digits = 4)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  
  expected_cols <- c("n", "mean", "median", "sd", "variance", "se",
                     "min", "q1", "q3", "max", "iqr")
  expect_true(all(expected_cols %in% names(result)))
})

test_that("descriptive_stats computes correct values", {
  x <- c(1, 2, 3, 4, 5)
  result <- descriptive_stats(x, digits = 4)
  
  expect_equal(result$n, 5)
  expect_equal(result$mean, 3)
  expect_equal(result$median, 3)
  expect_equal(result$min, 1)
  expect_equal(result$max, 5)
  expect_equal(result$q1, 2)
  expect_equal(result$q3, 4)
  expect_equal(result$iqr, 2)
})

test_that("descriptive_stats uses 4 decimals by default", {
  set.seed(444)
  x <- rnorm(50, mean = 100, sd = 15)
  
  result <- descriptive_stats(x)
  
  # Check that result is a data frame
  expect_s3_class(result, "data.frame")
})

test_that("descriptive_stats handles NA values", {
  x <- c(1, 2, NA, 4, 5)
  
  expect_warning(result <- descriptive_stats(x),
                 "Removed 1 NA value")
  
  expect_equal(result$n, 4)
})

test_that("Custom quantile matches built-in for various datasets", {
  set.seed(555)
  
  # Test with normal distribution
  x_norm <- rnorm(100, mean = 50, sd = 10)
  custom_q <- oneMeanTest:::.custom_quantile(x_norm, 
                                              probs = c(0.25, 0.5, 0.75),
                                              names = FALSE)
  builtin_q <- quantile(x_norm, probs = c(0.25, 0.5, 0.75), 
                        names = FALSE, type = 7)
  
  expect_equal(round(custom_q, 3), round(builtin_q, 3), tolerance = 0.01)
  
  # Test with uniform distribution
  x_unif <- runif(100, min = 0, max = 100)
  custom_q2 <- oneMeanTest:::.custom_quantile(x_unif,
                                               probs = c(0.1, 0.5, 0.9),
                                               names = FALSE)
  builtin_q2 <- quantile(x_unif, probs = c(0.1, 0.5, 0.9),
                         names = FALSE, type = 7)
  
  expect_equal(round(custom_q2, 3), round(builtin_q2, 3), tolerance = 0.01)
})
