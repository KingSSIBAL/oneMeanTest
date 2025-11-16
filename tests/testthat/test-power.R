test_that("power_analysis_one_mean returns sensible power and reacts to n", {
  # same effect size and sd, but different n
  p_small <- power_analysis_one_mean(
    n = 10,
    delta = 1,
    sd = 1,
    sig.level = 0.05,
    power = NULL,
    alternative = "two.sided"
  )

  p_large <- power_analysis_one_mean(
    n = 100,
    delta = 1,
    sd = 1,
    sig.level = 0.05,
    power = NULL,
    alternative = "two.sided"
  )

  # returns power.t.test object
  expect_s3_class(p_small, "power.htest")
  expect_s3_class(p_large, "power.htest")

  # power in [0,1], and larger n gives higher power
  expect_true(p_small$power >= 0 && p_small$power <= 1)
  expect_true(p_large$power >= 0 && p_large$power <= 1)
  expect_gt(p_large$power, p_small$power)
})

test_that("power_analysis_one_mean can solve for n given target power", {
  res <- power_analysis_one_mean(
    n = NULL,
    delta = 1,
    sd = 1,
    sig.level = 0.05,
    power = 0.8,
    alternative = "two.sided"
  )

  expect_s3_class(res, "power.htest")
  expect_true(res$n > 0)
  expect_true(res$power > 0.79 && res$power < 0.81)  # around 0.8
})
