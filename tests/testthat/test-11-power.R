# ============================================================
# TEST SUITE 11: Power Analysis
# ============================================================

test_that("power_t_test basic functionality", {
  power_val <- power_t_test(n = 30, delta = 1, sd = 2, alpha = 0.05)
  expect_true(power_val >= 0 && power_val <= 1)
})

test_that("power_t_test zero effect gives alpha", {
  expect_equal(power_t_test(n = 30, delta = 0, sd = 1, alpha = 0.05), 0.05, tolerance = 0.01)
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
  # Zero effect should give alpha power
  expect_equal(power_t_test(n = 10, delta = 0, sd = 1, alpha = 0.05), 0.05, tolerance = 0.01)
  
  # n=1 should error
  expect_error(power_t_test(n = 1, delta = 1, sd = 1, alpha = 0.05), "at least 2")
  
  # Negative n should error  
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
  
  # Zero effect
  expect_equal(power_t_test(30, 0, 1, 0.05, alternative = "two.sided"), 0.05, tolerance = 0.01)
  
  # Large effect
  expect_gt(power_t_test(100, 5, 10, 0.05, alternative = "two.sided"), 0.8)
  
  # Different alternatives
  p_two <- power_t_test(30, 1, 2, 0.05, alternative = "two.sided")
  p_greater <- power_t_test(30, 1, 2, 0.05, alternative = "greater")
  p_less <- power_t_test(30, -1, 2, 0.05, alternative = "less")
  expect_true(p_greater > p_two)
  expect_true(p_less > p_two)
  
  # Edge cases - proper error messages
  expect_error(power_t_test(n = 1, delta = 1, sd = 1, alpha = 0.05), "at least 2")
  expect_error(power_t_test(n = 30, delta = 1, sd = 0, alpha = 0.05), "positive")
  expect_error(power_t_test(n = 30, delta = 1, sd = -1, alpha = 0.05), "positive")
})

test_that("sample_size_t_test works", {
  n <- sample_size_t_test(delta = 0.5, sd = 1, power = 0.8, alpha = 0.05)
  expect_true(n >= 2)
  expect_true(is.numeric(n))
})
