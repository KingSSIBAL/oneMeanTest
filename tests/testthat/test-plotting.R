test_that("plot.oneMeanTest draws t and ci plots without error", {
  set.seed(123)
  x <- rnorm(30, mean = 5, sd = 2)

  res <- one_mean_test(x, mu0 = 5, check_assumptions = FALSE)

  # attach data so hist/qq/box can work as well later
  attr(res, "data") <- x

  # these should just run without error
  expect_silent(plot(res, which = "t"))
  expect_silent(plot(res, which = "ci"))
})

test_that("plot.oneMeanTest draws hist, qq, and box when data is stored", {
  set.seed(456)
  x <- rnorm(25, mean = 10, sd = 3)

  res <- one_mean_test(x, mu0 = 10, check_assumptions = FALSE)
  attr(res, "data") <- x

  expect_silent(plot(res, which = "hist"))
  expect_silent(plot(res, which = "qq"))
  expect_silent(plot(res, which = "box"))
})

test_that("plot.oneMeanTest errors for hist/qq/box without raw data", {
  set.seed(789)
  x <- rnorm(20)

  res <- one_mean_test(x, mu0 = 0, check_assumptions = FALSE)

  expect_error(plot(res, which = "hist"), "Raw data not stored")
  expect_error(plot(res, which = "qq"), "Raw data not stored")
  expect_error(plot(res, which = "box"), "Raw data not stored")
})
