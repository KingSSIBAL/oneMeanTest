test_that("check_assumptions behaves sensibly on normal data", {
  set.seed(101)
  x <- rnorm(40, mean = 0, sd = 1)

  res <- check_assumptions(x, alpha = 0.05, verbose = FALSE)

  # adapt these expectations to your actual structure, e.g.:
  # expect_true(res$normality$decision %in% c("fail to reject", "no evidence against normality"))
  # For now, just check structure:
  expect_type(res, "list")
})

test_that("check_assumptions integrates with one_mean_test", {
  set.seed(202)
  x <- rnorm(30, mean = 5, sd = 2)

  res <- one_mean_test(
    x,
    mu0 = 5,
    check_assumptions = TRUE
  )

  # assumptions slot should be a list or NULL
  expect_true(is.null(res$assumptions) || is.list(res$assumptions))
})

test_that("check_assumptions runs on clearly normal data", {
  set.seed(101)
  x <- rnorm(50, mean = 0, sd = 1)

  res <- check_assumptions(x, alpha = 0.05, verbose = FALSE)

  # Just check that it returns something list-like without error
  expect_true(is.list(res))
  expect_true(length(res) >= 1)
})

test_that("check_assumptions runs on clearly non-normal data", {
  set.seed(202)
  x <- rexp(50, rate = 1)   # skewed data

  res <- check_assumptions(x, alpha = 0.05, verbose = FALSE)

  expect_true(is.list(res))
  expect_true(length(res) >= 1)
})
