test_that("critical values match qt() for two-sided tests", {
  set.seed(555)
  x <- rnorm(30, mean = 5, sd = 2)
  df <- 29
  alpha <- 0.05
  
  result <- one_mean_test(x, mu0 = 5, alpha = alpha, 
                         alternative = "two.sided",
                         check_assumptions = FALSE)
  
  expected_upper <- qt(1 - alpha/2, df)
  expected_lower <- qt(alpha/2, df)
  
  # FIX: Use unname() to remove names before comparison
  expect_true("t.critical" %in% names(result))
  expect_equal(unname(result$t.critical["upper"]), expected_upper, tolerance = 1e-6)
  expect_equal(unname(result$t.critical["lower"]), expected_lower, tolerance = 1e-6)
})

test_that("critical values correct for greater alternative", {
  set.seed(666)
  x <- rnorm(25, mean = 10, sd = 3)
  df <- 24
  alpha <- 0.01
  
  result <- one_mean_test(x, mu0 = 10, alpha = alpha,
                         alternative = "greater",
                         check_assumptions = FALSE)
  
  expected_crit <- qt(1 - alpha, df)
  
  # FIX: Use unname()
  expect_equal(unname(result$t.critical["critical"]), expected_crit, tolerance = 1e-6)
})

test_that("critical values correct for less alternative", {
  set.seed(777)
  x <- rnorm(20, mean = 7, sd = 1.5)
  df <- 19
  alpha <- 0.10
  
  result <- one_mean_test(x, mu0 = 7, alpha = alpha,
                         alternative = "less",
                         check_assumptions = FALSE)
  
  expected_crit <- qt(alpha, df)
  
  expect_equal(unname(result$t.critical["critical"]), expected_crit, tolerance = 1e-6)
})
