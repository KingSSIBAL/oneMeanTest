# ============================================================
# TEST SUITE 10: Plotting Functions
# ============================================================

test_that("base plot.oneMeanTest works", {
  x <- rnorm(30)
  result <- one_mean_test(x, mu0 = 0, check_assumptions = FALSE)
  
  expect_silent(plot(result))
})

test_that("plot types work individually", {
  x <- rnorm(30)
  result <- one_mean_test(x, mu0 = 0, check_assumptions = FALSE)
  
  expect_silent(plot(result, type = "distribution"))
  expect_silent(plot(result, type = "ci"))
  expect_silent(plot(result, type = "diagnostic"))
})

test_that("plot all types works", {
  x <- rnorm(30)
  result <- one_mean_test(x, mu0 = 0, check_assumptions = FALSE)
  
  expect_silent(plot(result, type = "all"))
})

test_that("plot handles small samples", {
  x <- rnorm(5)
  result <- one_mean_test(x, mu0 = 0, check_assumptions = FALSE)
  
  expect_silent(plot(result))
})

test_that("plot functions handle edge cases", {
  x <- rnorm(5)
  result <- one_mean_test(x, mu0 = 0, check_assumptions = FALSE)
  
  expect_silent(plot(result, type = "diagnostic"))
  expect_silent(plot(result, type = "ci"))
})

test_that("all plot types work", {
  x <- rnorm(30)
  result <- one_mean_test(x, mu0 = 0)
  
  expect_silent(plot(result))
  expect_silent(plot(result, type = "distribution"))
  expect_silent(plot(result, type = "ci"))
  expect_silent(plot(result, type = "diagnostic"))
})
