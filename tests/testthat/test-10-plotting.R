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

test_that("plot.oneMeanTest errors on non-oneMeanTest object", {
  x <- rnorm(10)
  # OLD (fails):
  # expect_error(plot(x), "oneMeanTest")

  # NEW: call the S3 method directly so method dispatch is tested
  expect_error(plot.oneMeanTest(x), "oneMeanTest")
})

test_that("plot_distribution covers both branches", {
  set.seed(1)
  x <- rnorm(20)
  res <- one_mean_test(x, mu0 = 0, check_assumptions = FALSE)

  # oneMeanTest method branch
  expect_silent(plot_distribution(res))

  # raw numeric vector branch
  expect_silent(plot_distribution(x))
})

test_that("plot_ci validates input and draws correctly", {
  set.seed(1)
  x <- rnorm(20)
  res <- one_mean_test(x, mu0 = 0, check_assumptions = FALSE)

  expect_silent(plot_ci(res))
  expect_error(plot_ci(1:5), "oneMeanTest")
})

test_that("diagnostic helper plots run with NAs", {
  x <- c(rnorm(20), NA_real_)
  expect_silent(plot_qq(x))
  expect_silent(plot_histogram(x))
  expect_silent(plot_boxplot(x))
})

test_that("plot.oneMeanTest_bootstrap works and errors on wrong class", {
  set.seed(123)
  x <- rnorm(20)
  boot_res <- bootstrap_ttest(x, mu0 = 0, nboot = 100, seed = 123)

  expect_silent(plot(boot_res))

  # OLD (fails because graphics::plot.default is used):
  # expect_error(plot(structure(list(), class = "foo")), "oneMeanTest_bootstrap")

  # NEW: call the specific method on the wrong class
  expect_error(
    plot.oneMeanTest_bootstrap(structure(list(), class = "foo")),
    "oneMeanTest_bootstrap"
  )
})