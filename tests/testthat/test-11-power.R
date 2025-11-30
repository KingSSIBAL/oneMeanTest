# ============================================================
# TEST SUITE 11: Power Analysis Functions
# ============================================================

test_that("power_t_test calculates power correctly", {
  # Calculate power for given parameters
  pwr <- power_t_test(n = 30, delta = 0.5, sd = 1, alpha = 0.05)
  
  expect_true(is.numeric(pwr))
  expect_true(pwr >= 0 && pwr <= 1)
})

test_that("sample_size_t_test calculates required n", {
  # Calculate required sample size
  n <- sample_size_t_test(power = 0.8, delta = 0.5, sd = 1, alpha = 0.05)
  
  expect_true(is.numeric(n))
  expect_true(n >= 2)
})

test_that("effect_size_t_test calculates detectable effect", {
  # Calculate detectable effect size
  d <- effect_size_t_test(n = 30, power = 0.8, sd = 1, alpha = 0.05)
  
  expect_true(is.numeric(d))
  expect_true(d > 0)
})

test_that("power increases with sample size", {
  pwr_30 <- power_t_test(n = 30, delta = 0.5, sd = 1, alpha = 0.05)
  pwr_100 <- power_t_test(n = 100, delta = 0.5, sd = 1, alpha = 0.05)
  
  expect_true(pwr_100 > pwr_30)
})

test_that("power increases with effect size", {
  pwr_small <- power_t_test(n = 30, delta = 0.2, sd = 1, alpha = 0.05)
  pwr_large <- power_t_test(n = 30, delta = 0.8, sd = 1, alpha = 0.05)
  
  expect_true(pwr_large > pwr_small)
})

test_that("power plot is created", {
  expect_silent(plot_power_curve(delta = 0.5, sd = 1, alpha = 0.05))
})
