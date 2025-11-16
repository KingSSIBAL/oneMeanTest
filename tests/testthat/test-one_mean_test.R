test_that("one_mean_test matches t.test for basic two-sided case", {
  set.seed(123)
  x <- rnorm(30, mean = 5, sd = 2)

  ours <- one_mean_test(
    x,
    mu0 = 5,
    alternative = "two.sided",
    alpha = 0.05,
    conf.level = 0.95
  )

  theirs <- t.test(
    x,
    mu = 5,
    alternative = "two.sided",
    conf.level = 0.95
  )

  expect_equal(unname(ours$statistic["t"]),
               unname(theirs$statistic),
               tolerance = 1e-6)
  expect_equal(ours$p.value,
               theirs$p.value,
               tolerance = 1e-6)
  expect_equal(unname(ours$conf.int),
               unname(theirs$conf.int),
               tolerance = 1e-6)
})

test_that("one_mean_test works for 'less' and 'greater' alternatives", {
  set.seed(456)
  x <- rnorm(25, mean = 10, sd = 3)

  for (alt in c("less", "greater")) {
    ours <- one_mean_test(
      x,
      mu0 = 10,
      alternative = alt,
      alpha = 0.05,
      conf.level = 0.95
    )

    theirs <- t.test(
      x,
      mu = 10,
      alternative = alt,
      conf.level = 0.95
    )

    expect_equal(unname(ours$statistic["t"]),
                 unname(theirs$statistic),
                 tolerance = 1e-6)
    expect_equal(ours$p.value,
                 theirs$p.value,
                 tolerance = 1e-6)
  }
})

test_that("one_mean_test errors on clearly bad input", {
  # non-numeric x
  expect_error(
    one_mean_test("not numeric", mu0 = 0),
    "numeric"
  )

  # all NA -> some error (message may vary depending on your helpers)
  expect_error(
    one_mean_test(c(NA, NA, NA), mu0 = 0)
  )

  # only one non-missing value after removing NA
  expect_error(
    suppressWarnings(one_mean_test(c(1, NA), mu0 = 0))
  )
})
