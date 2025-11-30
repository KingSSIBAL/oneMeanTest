# ============================================================
# TEST SUITE 10: Plotting Functions
# ============================================================

test_that("plot.oneMeanTest creates plot without error", {
  set.seed(123)
  x <- rnorm(30, mean = 5, sd = 2)
  result <- one_mean_test(x, mu0 = 5, check_assumptions = FALSE)
  
  # Should create plot without error (returns NULL invisibly)
  expect_silent(p <- plot(result))
  expect_null(p)
})

test_that("plot handles different types", {
  set.seed(456)
  x <- rnorm(30, mean = 5, sd = 2)
  result <- one_mean_test(x, mu0 = 5, check_assumptions = FALSE)
  
  expect_silent(plot(result, type = "distribution"))
  expect_silent(plot(result, type = "ci"))
})

test_that("plot_diagnostic creates diagnostic plots", {
  set.seed(111)
  x <- rnorm(30, mean = 5, sd = 2)
  result <- one_mean_test(x, mu0 = 5, check_assumptions = TRUE)
  
  expect_silent(plot(result, type = "diagnostic"))
})

test_that("plot_bootstrap shows bootstrap distribution", {
  set.seed(222)
  x <- rnorm(30, mean = 50, sd = 10)
  result <- bootstrap_ttest(x, mu0 = 50, nboot = 500)
  
  expect_silent(plot(result))
})

test_that("qqnorm plot is created for assumptions", {
  set.seed(333)
  x <- rnorm(50, mean = 100, sd = 15)
  
  expect_silent(plot_qq(x))
})

test_that("histogram plot is created", {
  set.seed(444)
  x <- rnorm(50, mean = 100, sd = 15)
  
  expect_silent(plot_histogram(x))
})

test_that("boxplot is created with outliers", {
  x <- c(rnorm(50, mean = 100, sd = 10), 200, 250)
  
  expect_silent(plot_boxplot(x))
})
