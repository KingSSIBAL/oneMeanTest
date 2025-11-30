# ============================================================
# TEST SUITE 11: Power Analysis
# ============================================================

test_that("power_t_test basic functionality", {
  power_val <- power_t_test(n = 30, delta = 1, sd = 2, alpha = 0.05)
  expect_true(power_val >= 0 && power_val <= 1)
})

test_that("power_t_test zero effect gives alpha", {
  expect_equal(
    power_t_test(n = 30, delta = 0, sd = 1, alpha = 0.05),
    0.05,
    tolerance = 0.01
  )
})

test_that("power increases with sample size", {
  set.seed(123)
  power_small <- power_t_test(10, 1, 2, 0.05)
  power_large <- power_t_test(100, 1, 2, 0.05)
  expect_true(power_large > power_small)
})

test_that("power increases with effect size", {
  set.seed(123)
  power_small_effect <- power_t_test(30, 0.1, 2, 0.05)
  power_large_effect <- power_t_test(30, 1, 2, 0.05)
  expect_true(power_large_effect > power_small_effect)
})

test_that("power_t_test handles edge cases", {
  expect_equal(
    power_t_test(n = 10, delta = 0, sd = 1, alpha = 0.05),
    0.05,
    tolerance = 0.01
  )

  expect_error(power_t_test(n = 1, delta = 1, sd = 1, alpha = 0.05), "at least 2")
  expect_error(power_t_test(n = -1, delta = 1, sd = 1, alpha = 0.05), "at least 2")
})

test_that("power_t_test one-sided vs two-sided", {
  set.seed(123)
  power_two <- power_t_test(30, 1, 2, 0.05, alternative = "two.sided")
  power_greater <- power_t_test(30, 1, 2, 0.05, alternative = "greater")
  expect_true(power_greater > power_two)
})

test_that("power functions cover all branches", {
  set.seed(123)

  expect_equal(
    power_t_test(30, 0, 1, 0.05, alternative = "two.sided"),
    0.05,
    tolerance = 0.01
  )

  expect_gt(
    power_t_test(100, 5, 10, 0.05, alternative = "two.sided"),
    0.8
  )

  p_two <- power_t_test(30, 1, 2, 0.05, alternative = "two.sided")
  p_greater <- power_t_test(30, 1, 2, 0.05, alternative = "greater")
  p_less <- power_t_test(30, -1, 2, 0.05, alternative = "less")
  expect_true(p_greater > p_two)
  expect_true(p_less > p_two)

  expect_error(power_t_test(n = 30, delta = 1, sd = 0, alpha = 0.05), "positive")
  expect_error(power_t_test(n = 30, delta = 1, sd = -1, alpha = 0.05), "positive")
})

test_that("power_t_test validates alpha and sd", {
  expect_error(power_t_test(n = 30, delta = 1, sd = 1, alpha = 0), "between 0 and 1")
  expect_error(power_t_test(n = 30, delta = 1, sd = 1, alpha = 1), "between 0 and 1")
})

test_that("sample_size_t_test works and gives target power", {
  n <- sample_size_t_test(delta = 0.5, sd = 1, power = 0.8, alpha = 0.05)
  expect_true(is.numeric(n))
  expect_true(n >= 2)

  achieved_power <- power_t_test(n = n, delta = 0.5, sd = 1, alpha = 0.05)
  expect_true(abs(achieved_power - 0.8) < 0.05)
})


test_that("effect_size_t_test works and matches target power", {
  n <- 30
  sd <- 2
  target_power <- 0.8

  delta <- effect_size_t_test(n = n, power = target_power, sd = sd, alpha = 0.05)
  achieved_power <- power_t_test(n = n, delta = delta, sd = sd, alpha = 0.05)

  expect_true(abs(achieved_power - target_power) < 0.05)
})

test_that("sample_size_t_test input validation", {
  expect_error(sample_size_t_test(power = 0, delta = 0.5, sd = 1), "between 0 and 1")
  expect_error(sample_size_t_test(power = 1, delta = 0.5, sd = 1), "between 0 and 1")
  expect_error(sample_size_t_test(power = 0.8, delta = 0.5, sd = 0), "positive")

  # OLD:
  # expect_error(sample_size_t_test(power = 0.8, delta = 0, sd = 1), "cannot be zero")

  # NEW:
  expect_error(
    sample_size_t_test(power = 0.8, delta = 0, sd = 1),
    "Effect size \\(delta\\) must be a non-zero numeric value"
  )
})


test_that("plot_power_curve returns data and handles n_range", {
  res <- plot_power_curve(delta = 0.5, sd = 1, alpha = 0.05, n_range = 5:8)
  expect_s3_class(res, "data.frame")
  expect_true(all(c("n", "power") %in% names(res)))
  expect_equal(res$n, 5:8)

  expect_error(
    plot_power_curve(delta = 0.5, sd = 1, alpha = 0.05, n_range = c(0, 1)),
    "at least 2"
  )
})