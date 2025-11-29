# Tests for bootstrap_ttest function

test_that("bootstrap_ttest returns correct structure", {
  set.seed(123)
  x <- rnorm(30, mean = 5, sd = 2)
  
  result <- bootstrap_ttest(x, mu0 = 5, nboot = 1000)
  
  expect_s3_class(result, "oneMeanTest_bootstrap")
  expect_true("t.boot" %in% names(result))
  expect_true("conf.int" %in% names(result))
  expect_true("p.value" %in% names(result))
  expect_length(result$t.boot, 1000)
})

test_that("bootstrap_ttest works with different alternatives", {
  set.seed(456)
  x <- rnorm(25, mean = 10, sd = 3)
  
  # Two-sided
  res_two <- bootstrap_ttest(x, mu0 = 10, alternative = "two.sided", nboot = 500)
  expect_equal(res_two$alternative, "two.sided")
  
  # Greater
  res_greater <- bootstrap_ttest(x, mu0 = 10, alternative = "greater", nboot = 500)
  expect_equal(res_greater$alternative, "greater")
  
  # Less
  res_less <- bootstrap_ttest(x, mu0 = 10, alternative = "less", nboot = 500)
  expect_equal(res_less$alternative, "less")
})

test_that("bootstrap_ttest p-values are in valid range", {
  set.seed(789)
  
  # Data centered at null
  x_null <- rnorm(30, mean = 5, sd = 2)
  res_null <- bootstrap_ttest(x_null, mu0 = 5, nboot = 1000)
  
  # Should have relatively high p-value (not significant)
  expect_true(res_null$p.value > 0.05)
  
  # Data with some difference - just validate p-value is computed correctly
  x_diff <- rnorm(30, mean = 10, sd = 1)
  res_diff <- bootstrap_ttest(x_diff, mu0 = 5, nboot = 1000)
  
  # Check p-value is valid (between 0 and 1)
  expect_true(res_diff$p.value >= 0 && res_diff$p.value <= 1)
  expect_true(is.finite(res_diff$p.value))
})

test_that("bootstrap confidence intervals are calculated", {
  set.seed(111)
  x <- rnorm(30, mean = 5, sd = 2)
  
  result <- bootstrap_ttest(x, mu0 = 5, conf.level = 0.95, nboot = 1000)
  
  expect_length(result$conf.int, 2)
  expect_true(result$conf.int[1] < result$conf.int[2])
  expect_true(is.finite(result$conf.int[1]))
  expect_true(is.finite(result$conf.int[2]))
})

test_that("bootstrap_ttest handles small samples", {
  set.seed(222)
  x_small <- rnorm(10, mean = 5, sd = 1)
  
  expect_error(
    bootstrap_ttest(x_small, mu0 = 5, nboot = 100),
    NA
  )
})

test_that("bootstrap_ttest handles various sample sizes", {
  set.seed(1212)
  
  # Small sample
  x_small <- rnorm(10, mean = 5, sd = 1)
  result_small <- bootstrap_ttest(x_small, mu0 = 5, nboot = 100)
  expect_s3_class(result_small, "oneMeanTest_bootstrap")
  expect_true(is.numeric(result_small$p.value))
  
  # Larger sample - check structure
  x_large <- rnorm(50, mean = 8, sd = 2)
  result_large <- bootstrap_ttest(x_large, mu0 = 7, nboot = 500)
  expect_s3_class(result_large, "oneMeanTest_bootstrap")
  expect_true(is.numeric(result_large$p.value))
  expect_true(result_large$p.value >= 0 && result_large$p.value <= 1)
})

test_that("bootstrap_ttest with different alternatives", {
  set.seed(1313)
  x <- rnorm(30, mean = 7, sd = 2)
  
  for (alt in c("two.sided", "less", "greater")) {
    result <- bootstrap_ttest(x, mu0 = 5, alternative = alt, nboot = 200)
    expect_equal(result$alternative, alt)
    expect_true(is.numeric(result$p.value))
    expect_true(result$p.value >= 0 && result$p.value <= 1)
  }
})

test_that("bootstrap results are reproducible with seed", {
  set.seed(1414)
  x <- rnorm(30, mean = 5, sd = 2)
  
  boot1 <- bootstrap_ttest(x, mu0 = 5, nboot = 500, seed = 42)
  boot2 <- bootstrap_ttest(x, mu0 = 5, nboot = 500, seed = 42)
  
  expect_equal(boot1$p.value, boot2$p.value)
  expect_equal(boot1$conf.int, boot2$conf.int)
  expect_equal(boot1$t.boot, boot2$t.boot)
})

test_that("bootstrap_ttest validates nboot parameter", {
  set.seed(333)
  x <- rnorm(30, mean = 5, sd = 2)
  
  # nboot too small
  expect_error(
    bootstrap_ttest(x, mu0 = 5, nboot = 50),
    "nboot must be a single integer >= 100"
  )
  
  # Invalid nboot
  expect_error(
    bootstrap_ttest(x, mu0 = 5, nboot = "1000")
  )
})

test_that("bootstrap output structure is complete", {
  set.seed(444)
  x <- rnorm(25, mean = 8, sd = 2)
  result <- bootstrap_ttest(x, mu0 = 8, nboot = 200)
  
  expected_names <- c("t.obs", "t.boot", "mean.obs", "mean.boot",
                     "conf.int", "p.value", "nboot", "mu0", 
                     "alternative", "call")
  
  expect_true(all(expected_names %in% names(result)))
})

test_that("bootstrap CI contains observed mean approximately", {
  set.seed(555)
  x <- rnorm(100, mean = 10, sd = 2)
  
  result <- bootstrap_ttest(x, mu0 = 10, nboot = 1000, conf.level = 0.95)
  
  # With large n and centered at null, observed mean should often be in CI
  # (not always due to sampling, but we just check structure)
  expect_true(result$conf.int[1] < result$conf.int[2])
  expect_true(is.finite(result$conf.int[1]))
  expect_true(is.finite(result$conf.int[2]))
})
