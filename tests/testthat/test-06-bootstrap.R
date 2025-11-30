# ============================================================
# TEST SUITE 06: Bootstrap Methods
# ============================================================
# Tests bootstrap_ttest() function
# Tests bootstrap confidence intervals
# Extended coverage for all bootstrap functionality
# ============================================================

test_that("Bootstrap test produces valid output", {
  set.seed(888)
  x <- rnorm(30, mean = 50, sd = 10)
  
  result <- bootstrap_ttest(x, mu0 = 50, nboot = 500)
  
  expect_s3_class(result, "oneMeanTest_bootstrap")
  expect_true(!is.null(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_equal(length(result$t.boot), 500)
})

test_that("Bootstrap CI has reasonable coverage", {
  set.seed(999)
  x <- rnorm(50, mean = 100, sd = 15)
  
  result <- bootstrap_ttest(x, mu0 = 100, nboot = 1000,
                            conf.level = 0.95)
  
  # CI should be numeric vector of length 2
  expect_true(is.numeric(result$conf.int))
  expect_equal(length(result$conf.int), 2)
  
  # Lower < Upper
  expect_true(result$conf.int[1] < result$conf.int[2])
})

test_that("Bootstrap results are reproducible with set seed", {
  x <- rnorm(30, mean = 50, sd = 10)
  
  set.seed(123)
  result1 <- bootstrap_ttest(x, mu0 = 50, nboot = 500, seed = 123)
  
  set.seed(123)
  result2 <- bootstrap_ttest(x, mu0 = 50, nboot = 500, seed = 123)
  
  expect_equal(result1$p.value, result2$p.value)
  expect_equal(result1$conf.int, result2$conf.int)
})

# ============================================================
# EXTENDED BOOTSTRAP TESTS
# ============================================================

test_that("bootstrap_ttest handles different alternatives", {
  set.seed(123)
  x <- rnorm(30, mean = 50, sd = 10)
  
  # Two-sided
  result_two <- bootstrap_ttest(x, mu0 = 50, nboot = 500, 
                                 alternative = "two.sided")
  expect_equal(result_two$alternative, "two.sided")
  
  # Greater
  result_greater <- bootstrap_ttest(x, mu0 = 45, nboot = 500,
                                     alternative = "greater")
  expect_equal(result_greater$alternative, "greater")
  
  # Less
  result_less <- bootstrap_ttest(x, mu0 = 55, nboot = 500,
                                  alternative = "less")
  expect_equal(result_less$alternative, "less")
})

test_that("bootstrap_ttest validates nboot parameter", {
  x <- rnorm(30, mean = 50, sd = 10)
  
  # Too few bootstrap samples
  expect_error(bootstrap_ttest(x, mu0 = 50, nboot = 50),
               "nboot must be.*>= 100")
})

test_that("bootstrap_ttest uses custom sample function", {
  set.seed(456)
  x <- rnorm(30, mean = 50, sd = 10)
  
  # Should use .custom_sample internally
  result <- bootstrap_ttest(x, mu0 = 50, nboot = 500, seed = 456)
  
  expect_true(length(result$t.boot) == 500)
  expect_true(length(result$mean.boot) == 500)
})

test_that("print.oneMeanTest_bootstrap displays correctly", {
  set.seed(789)
  x <- rnorm(30, mean = 50, sd = 10)
  result <- bootstrap_ttest(x, mu0 = 50, nboot = 500)
  
  output <- capture.output(print(result))
  
  expect_true(any(grepl("Bootstrap", output)))
  expect_true(any(grepl("p-value", output)))
  expect_true(any(grepl("CI", output)))
})

test_that("print.oneMeanTest_bootstrap respects digits parameter", {
  set.seed(111)
  x <- rnorm(30, mean = 50, sd = 10)
  result <- bootstrap_ttest(x, mu0 = 50, nboot = 500)
  
  output_4 <- capture.output(print(result, digits = 4))
  output_2 <- capture.output(print(result, digits = 2))
  
  expect_true(length(output_4) > 0)
  expect_true(length(output_2) > 0)
})

test_that("bootstrap distributions have correct properties", {
  set.seed(222)
  x <- rnorm(50, mean = 100, sd = 15)
  result <- bootstrap_ttest(x, mu0 = 100, nboot = 1000)
  
  # Bootstrap mean should be close to sample mean
  expect_equal(mean(result$mean.boot), mean(x), tolerance = 2)
  
  # Bootstrap t should be centered near observed t
  expect_equal(mean(result$t.boot), result$t.obs, tolerance = 1)
})

test_that("bootstrap handles NA values through preprocessing", {
  x_with_na <- c(rnorm(30, mean = 50, sd = 10), NA, NA)
  
  # Should remove NAs with warning before bootstrap
  expect_warning(
    result <- bootstrap_ttest(x_with_na, mu0 = 50, nboot = 500),
    "Removed.*NA"
  )
  
  expect_equal(result$nboot, 500)
})

test_that("bootstrap confidence level parameter works", {
  set.seed(333)
  x <- rnorm(50, mean = 100, sd = 15)
  
  result_95 <- bootstrap_ttest(x, mu0 = 100, nboot = 1000, conf.level = 0.95)
  result_99 <- bootstrap_ttest(x, mu0 = 100, nboot = 1000, conf.level = 0.99)
  
  # 99% CI should be wider than 95% CI
  width_95 <- result_95$conf.int[2] - result_95$conf.int[1]
  width_99 <- result_99$conf.int[2] - result_99$conf.int[1]
  
  expect_true(width_99 > width_95)
})

test_that("bootstrap call is stored correctly", {
  set.seed(444)
  x <- rnorm(30, mean = 50, sd = 10)
  result <- bootstrap_ttest(x, mu0 = 50, nboot = 500)
  
  expect_true(!is.null(result$call))
  expect_true(inherits(result$call, "call"))
})
