# ============================================================
# TEST SUITE 02: Core Statistical Functionality
# ============================================================
# Tests main one_mean_test() function
# Tests statistical correctness, decisions, and interpretations
# ============================================================

test_that("one_mean_test basic two-sided test works", {
  set.seed(123)
  x <- rnorm(30, mean = 5, sd = 2)
  
  result <- one_mean_test(x, mu0 = 5, check_assumptions = FALSE)
  
  expect_s3_class(result, "oneMeanTest")
  expect_true(is.numeric(result$statistic))
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_equal(as.numeric(result$parameter), 29)
  expect_equal(result$alternative, "two.sided")
})

test_that("one_mean_test one-sided greater test works", {
  set.seed(456)
  x <- rnorm(50, mean = 10, sd = 3)
  
  result <- one_mean_test(x, mu0 = 9, alternative = "greater", 
                          check_assumptions = FALSE)
  
  expect_equal(result$alternative, "greater")
  expect_true(result$p.value < 1)
  
  # Should have single critical value for one-sided
  expect_true(length(result$t.critical) == 1)
})

test_that("one_mean_test one-sided less test works", {
  set.seed(789)
  x <- rnorm(40, mean = 100, sd = 15)
  
  result <- one_mean_test(x, mu0 = 105, alternative = "less",
                          check_assumptions = FALSE)
  
  expect_equal(result$alternative, "less")
  expect_true(is.numeric(result$t.critical))
})

test_that("one_mean_test decision logic is correct", {
  set.seed(111)
  
  # Create data that should reject H0
  x_reject <- rnorm(100, mean = 10, sd = 1)
  result_reject <- one_mean_test(x_reject, mu0 = 5, alpha = 0.05,
                                  check_assumptions = FALSE)
  expect_equal(result_reject$decision, "reject H0")
  expect_true(result_reject$p.value < 0.05)
  
  # Create data that should fail to reject H0
  x_fail <- rnorm(100, mean = 5, sd = 10)
  result_fail <- one_mean_test(x_fail, mu0 = 5, alpha = 0.05,
                                check_assumptions = FALSE)
  expect_equal(result_fail$decision, "fail to reject H0")
  expect_true(result_fail$p.value >= 0.05)
})

test_that("Confidence intervals are correct", {
  set.seed(222)
  x <- rnorm(50, mean = 20, sd = 5)
  
  result95 <- one_mean_test(x, mu0 = 20, conf.level = 0.95,
                            check_assumptions = FALSE)
  result99 <- one_mean_test(x, mu0 = 20, conf.level = 0.99,
                            check_assumptions = FALSE)
  
  # 99% CI should be wider than 95% CI
  width95 <- result95$conf.int[2] - result95$conf.int[1]
  width99 <- result99$conf.int[2] - result99$conf.int[1]
  
  expect_true(width99 > width95)
  
  # CI should contain the sample mean
  expect_true(result95$estimate >= result95$conf.int[1] &&
              result95$estimate <= result95$conf.int[2])
})

test_that("Critical values are correctly computed", {
  set.seed(333)
  x <- rnorm(30, mean = 50, sd = 10)
  
  # Two-sided: should have two critical values
  result_two <- one_mean_test(x, mu0 = 50, alternative = "two.sided",
                               check_assumptions = FALSE)
  expect_true("lower" %in% names(result_two$t.critical))
  expect_true("upper" %in% names(result_two$t.critical))
  expect_true(result_two$t.critical["upper"] > 0)
  expect_true(result_two$t.critical["lower"] < 0)
  
  # One-sided: should have single critical value
  result_greater <- one_mean_test(x, mu0 = 50, alternative = "greater",
                                   check_assumptions = FALSE)
  expect_equal(length(result_greater$t.critical), 1)
})

test_that("Sample statistics are correct", {
  x <- c(1, 2, 3, 4, 5)
  result <- one_mean_test(x, mu0 = 3, check_assumptions = FALSE)
  
  expect_equal(result$sample.stats$n, 5)
  expect_equal(result$sample.stats$mean, 3)
  expect_equal(round(result$sample.stats$sd, 4), round(sd(x), 4))
  expect_equal(round(result$sample.stats$se, 4), round(sd(x)/sqrt(5), 4))
})
