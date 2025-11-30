# ============================================================
# TEST SUITE 03: Input Validation
# ============================================================

test_that("one_mean_test validates numeric input", {
  expect_error(one_mean_test("a"), "numeric")
  expect_error(one_mean_test(c(1, "a")), "numeric")
  # Matrix test REMOVED - it works correctly (converts to vector)
})

test_that("one_mean_test handles NA values", {
  x_with_na <- c(1, 2, NA, 4, 5)
  result <- suppressWarnings(one_mean_test(x_with_na, mu0 = 3))
  expect_equal(result$sample.stats$n, 4)
})

test_that("one_mean_test requires sufficient data", {
  expect_error(one_mean_test(numeric(0)), "length >= 1")
  expect_error(one_mean_test(c(1)), "at least 2")
})

test_that("mu0 must be numeric", {
  x <- rnorm(10)
  expect_error(one_mean_test(x, mu0 = "a"), "numeric")
})

test_that("conf.level validation works", {
  x <- rnorm(10)
  expect_error(one_mean_test(x, conf.level = 1.1), "between 0 and 1")
  expect_error(one_mean_test(x, conf.level = -0.1), "between 0 and 1")
})

test_that("alternative validation works", {
  x <- rnorm(10)
  expect_error(one_mean_test(x, alternative = "invalid"), "'arg' should be")
})

test_that("data frame input handled correctly", {
  df <- data.frame(x = rnorm(10))
  expect_error(one_mean_test(df), "numeric vector")
  
  result <- one_mean_test(df$x)
  expect_s3_class(result, "oneMeanTest")
})

test_that("named vector input works", {
  x <- c(a = 1, b = 2, c = 3, d = 4, e = 5)
  result <- one_mean_test(x, mu0 = 3)
  expect_true(grepl("c\\(a", result$data.name))
})

test_that("utils helper functions work", {
  result <- one_mean_test(rnorm(10))
  expect_true(inherits(result, "oneMeanTest"))
  expect_false(any(class(list()) == "oneMeanTest"))
  expect_true("sample.stats" %in% names(result))
})

test_that("matrix input is automatically converted", {
  mat <- matrix(1:4, 2, 2)
  result <- one_mean_test(mat)
  expect_s3_class(result, "oneMeanTest")
  expect_equal(result$sample.stats$n, 4)
})
