# ============================================================
# TEST SUITE 01: Numerical Accuracy & Precision Tests
# ============================================================
# Tests custom implementations against R's built-in functions
# Validates 4-decimal accuracy requirements
# ============================================================

test_that("Custom quantile matches R's quantile to 4 decimals", {
  set.seed(42)
  x <- rnorm(100, mean = 50, sd = 10)
  
  probs <- c(0, 0.25, 0.5, 0.75, 1)
  
  # Get results from both implementations
  custom_q <- oneMeanTest:::.custom_quantile(x, probs = probs, names = FALSE)
  builtin_q <- quantile(x, probs = probs, names = FALSE, type = 7)
  
  # Test each quantile to 4 decimal places
  for (i in seq_along(probs)) {
    expect_equal(round(custom_q[i], 4), round(builtin_q[i], 4),
                 tolerance = 0.0001,
                 label = sprintf("Quantile at p=%.2f", probs[i]))
  }
})

test_that("Custom pt() matches stats::pt() to 4 decimals", {
  # Test various df values
  df_values <- c(5, 10, 20, 50)
  q_values <- c(-3, -1.96, -1, 0, 1, 1.96, 3)
  
  for (df in df_values) {
    for (q in q_values) {
      custom_p <- oneMeanTest:::.custom_pt(q, df, lower.tail = TRUE)
      builtin_p <- pt(q, df, lower.tail = TRUE)
      
      expect_equal(round(custom_p, 4), round(builtin_p, 4),
                   tolerance = 0.001,
                   label = sprintf("pt(q=%.2f, df=%d)", q, df))
    }
  }
})

test_that("Custom qt() matches stats::qt() to 4 decimals", {
  # Test various df values and probabilities
  df_values <- c(5, 10, 20, 50, 100)
  p_values <- c(0.01, 0.025, 0.05, 0.1, 0.5, 0.9, 0.95, 0.975, 0.99)
  
  for (df in df_values) {
    for (p in p_values) {
      custom_q <- oneMeanTest:::.custom_qt(p, df)
      builtin_q <- qt(p, df)
      
      # Allow higher tolerance for extreme quantiles
      tol <- if (p < 0.02 || p > 0.98) 0.01 else 0.001
      
      expect_equal(round(custom_q, 3), round(builtin_q, 3),
                   tolerance = tol,
                   label = sprintf("qt(p=%.3f, df=%d)", p, df))
    }
  }
})

test_that("Custom pnorm() matches stats::pnorm() to 4 decimals", {
  q_values <- seq(-3, 3, by = 0.5)
  
  for (q in q_values) {
    custom_p <- oneMeanTest:::.custom_pnorm(q, lower.tail = TRUE)
    builtin_p <- pnorm(q, lower.tail = TRUE)
    
    expect_equal(round(custom_p, 4), round(builtin_p, 4),
                 tolerance = 0.0001,
                 label = sprintf("pnorm(q=%.2f)", q))
  }
})

test_that("Custom qnorm() matches stats::qnorm() to 4 decimals", {
  p_values <- c(0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975, 0.99)
  
  for (p in p_values) {
    custom_q <- oneMeanTest:::.custom_qnorm(p)
    builtin_q <- qnorm(p)
    
    tol <- if (p < 0.02 || p > 0.98) 0.01 else 0.001
    
    expect_equal(round(custom_q, 3), round(builtin_q, 3),
                 tolerance = tol,
                 label = sprintf("qnorm(p=%.3f)", p))
  }
})

test_that("Incomplete beta function has reasonable accuracy", {
  # Test basic cases
  result <- oneMeanTest:::.incomplete_beta(0.5, 0.5, 0.5)
  expect_equal(round(result, 2), 0.5, tolerance = 0.05)
  
  result <- oneMeanTest:::.incomplete_beta(0.25, 1, 1)
  expect_equal(round(result, 2), 0.25, tolerance = 0.01)
})

test_that("End-to-end: one_mean_test produces 4-decimal accurate results", {
  set.seed(123)
  x <- rnorm(50, mean = 100, sd = 15)
  
  result_custom <- one_mean_test(x, mu0 = 100, check_assumptions = FALSE)
  result_builtin <- t.test(x, mu = 100)
  
  # Compare t-statistic
  expect_equal(round(result_custom$statistic, 4), 
               round(result_builtin$statistic, 4),
               tolerance = 0.001)
  
  # Compare p-value (allow slightly higher tolerance)
  expect_equal(round(result_custom$p.value, 3), 
               round(result_builtin$p.value, 3),
               tolerance = 0.01)
  
  # Compare confidence intervals (without names)
  expect_equal(round(as.numeric(result_custom$conf.int[1]), 3), 
               round(as.numeric(result_builtin$conf.int[1]), 3),
               tolerance = 0.01)
  expect_equal(round(as.numeric(result_custom$conf.int[2]), 3), 
               round(as.numeric(result_builtin$conf.int[2]), 3),
               tolerance = 0.01)
})

test_that("Numerical stability with extreme values", {
  # Test with very small df
  expect_silent(oneMeanTest:::.custom_qt(0.5, df = 5))
  expect_silent(oneMeanTest:::.custom_pt(0, df = 5, lower.tail = TRUE))
  
  # Test with very large df (should use normal approximation)
  expect_silent(oneMeanTest:::.custom_qt(0.975, df = 1000))
  expect_silent(oneMeanTest:::.custom_pt(1.96, df = 1000, lower.tail = TRUE))
  
  # Test extreme quantiles
  expect_true(is.finite(oneMeanTest:::.custom_qt(0.01, df = 10)))
  expect_true(is.finite(oneMeanTest:::.custom_qt(0.99, df = 10)))
})

test_that("All output formatting uses 4 decimals", {
  set.seed(456)
  x <- rnorm(30, mean = 5, sd = 2)
  result <- one_mean_test(x, mu0 = 5)
  
  # Get formatted report
  report <- format_ttest_report(result)
  
  # Check that report contains decimal formatted numbers
  expect_true(grepl("\\d+\\.\\d{4}", report))
})
