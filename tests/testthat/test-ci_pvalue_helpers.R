test_that(".t_confint_one_mean matches manual t.test CI", {
  set.seed(404)
  x <- rnorm(20, mean = 2, sd = 1)
  n <- length(x)
  m <- mean(x)
  s <- sd(x)

  # helper CI for mean, conf.level = 0.95
  ci_helper <- oneMeanTest:::.t_confint_one_mean(
    mean = m,
    sd = s,
    n = n,
    conf.level = 0.95
  )

  # CI from t.test with same data and conf.level
  ci_ttest <- t.test(x, mu = 0, conf.level = 0.95)$conf.int

  # compare numeric values only, ignore names/attributes
  expect_equal(
    unname(ci_helper),
    unname(ci_ttest),
    tolerance = 1e-6,
    check.attributes = FALSE
  )
})
