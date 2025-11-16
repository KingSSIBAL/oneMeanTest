test_that("descriptive_stats returns correct summaries", {
  x <- c(1, 2, 3, 4, 5)

  # turn off rounding so we compare raw numbers
  ds <- descriptive_stats(x, digits = NULL)

  expect_equal(ds$n, 5L)
  expect_equal(ds$mean, mean(x))
  expect_equal(ds$sd, sd(x))
  expect_equal(ds$se, sd(x) / sqrt(5))
})

test_that("descriptive_stats handles NA appropriately", {
  x <- c(1, 2, NA, 4, 5)

  # expect a warning about removing NA values, but still get correct stats
  expect_warning(
    ds <- descriptive_stats(x, digits = NULL),
    "Removed"
  )

  expect_equal(ds$n, 4L)  # if NA are dropped
  expect_equal(ds$mean, mean(x, na.rm = TRUE))
})
