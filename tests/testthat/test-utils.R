test_that(".check_numeric_vector enforces numeric input", {
  f <- oneMeanTest:::.check_numeric_vector

  expect_error(f("a"), "numeric")
  expect_error(f(list(1, 2, 3)), "numeric")
  expect_silent(f(c(1, 2, 3)))
})

test_that(".remove_na_with_warning drops NA and checks length", {
  f <- oneMeanTest:::.remove_na_with_warning

  # vector with some NA
  x <- c(1, 2, NA, 4)
  expect_warning(
    out <- f(x, "x"),
    regexp = "Removed"
  )
  expect_equal(out, c(1, 2, 4))

  # all NA -> some error (message text may vary)
  expect_error(
    f(c(NA, NA), "x")
  )

  # only one non-missing value
  expect_error(
    suppressWarnings(f(c(1, NA), "x"))
  )
})
