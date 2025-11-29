# Edge cases

test_that("handles minimum sample size n=2", {
  x <- c(1, 2)
  
  result <- one_mean_test(x, mu0 = 1.5, check_assumptions = FALSE)
  
  expect_s3_class(result, "oneMeanTest")
  expect_equal(result$parameter, c(df = 1))
  expect_equal(result$sample.stats$n, 2)
})

test_that("handles large samples", {
  set.seed(1919)
  x <- rnorm(1000, mean = 100, sd = 15)
  
  expect_error(
    result <- one_mean_test(x, mu0 = 100, check_assumptions = FALSE),
    NA
  )
})

test_that("handles extreme p-values", {
  set.seed(2020)
  
  # Strong evidence
  x_extreme <- rnorm(100, mean = 100, sd = 1)
  result <- one_mean_test(x_extreme, mu0 = 50, check_assumptions = FALSE)
  
  expect_true(result$p.value < 0.001)
  expect_equal(result$decision, "reject H0")
})

test_that("different confidence levels work", {
  set.seed(2121)
  x <- rnorm(30, mean = 10, sd = 2)
  
  result_90 <- one_mean_test(x, mu0 = 10, conf.level = 0.90,
                            check_assumptions = FALSE)
  result_95 <- one_mean_test(x, mu0 = 10, conf.level = 0.95,
                            check_assumptions = FALSE)
  result_99 <- one_mean_test(x, mu0 = 10, conf.level = 0.99,
                            check_assumptions = FALSE)
  
  # Higher confidence = wider interval
  width_90 <- diff(result_90$conf.int)
  width_95 <- diff(result_95$conf.int)
  width_99 <- diff(result_99$conf.int)
  
  expect_true(width_90 < width_95)
  expect_true(width_95 < width_99)
})

test_that("handles different alpha levels", {
  set.seed(2222)
  x <- rnorm(40, mean = 7, sd = 1.5)
  
  alphas <- c(0.01, 0.05, 0.10)
  
  for (alpha in alphas) {
    result <- one_mean_test(x, mu0 = 7, alpha = alpha,
                           check_assumptions = FALSE)
    
    expect_equal(result$alpha, alpha)
  }
})
