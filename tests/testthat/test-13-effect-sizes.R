# ============================================================
# TEST SUITE 13: Effect Size Calculations
# ============================================================
# Tests for Cohen's d, Hedges' g, Glass's delta, and CIs

test_that("cohens_d_ci calculates correctly with bootstrap", {
  set.seed(123)
  x <- rnorm(30, mean = 5, sd = 2)
  
  result <- cohens_d_ci(x, mu0 = 0, method = "bootstrap", nboot = 500)
  
  expect_s3_class(result, "effect_size")
  expect_true(is.numeric(result$d))
  expect_equal(length(result$ci), 2)
  expect_true(result$ci[1] < result$ci[2])
  expect_equal(result$method, "bootstrap")
})

test_that("cohens_d_ci calculates correctly with nct method", {
  set.seed(456)
  x <- rnorm(30, mean = 5, sd = 2)
  
  result <- cohens_d_ci(x, mu0 = 0, method = "nct", conf.level = 0.95)
  
  expect_s3_class(result, "effect_size")
  expect_true(is.numeric(result$d))
  expect_equal(length(result$ci), 2)
  expect_equal(result$method, "nct")
})

test_that("hedges_g applies bias correction", {
  set.seed(789)
  x <- rnorm(20, mean = 5, sd = 2)
  
  result <- hedges_g(x, mu0 = 0)
  
  expect_s3_class(result, "effect_size")
  expect_true(is.numeric(result$g))
  expect_true(is.numeric(result$d))
  expect_true(is.numeric(result$correction_factor))
  expect_true(result$correction_factor < 1)  # Correction reduces estimate
  expect_true(abs(result$g) <= abs(result$d))  # g <= d for small samples
})

test_that("hedges_g correction factor decreases with sample size", {
  set.seed(111)
  
  # Small sample
  x_small <- rnorm(10, mean = 5, sd = 2)
  g_small <- hedges_g(x_small, mu0 = 0)
  
  # Large sample
  x_large <- rnorm(100, mean = 5, sd = 2)
  g_large <- hedges_g(x_large, mu0 = 0)
  
  # Correction factor should be smaller (closer to 1) for larger samples
  expect_true(g_small$correction_factor < g_large$correction_factor)
})

test_that("glass_delta calculates correctly", {
  set.seed(222)
  x <- rnorm(30, mean = 5, sd = 2)
  
  # With known sigma
  delta1 <- glass_delta(x, mu0 = 0, sigma = 2)
  expect_true(is.numeric(delta1))
  
  # Without sigma (uses sample sd)
  expect_warning(delta2 <- glass_delta(x, mu0 = 0, sigma = NULL))
  expect_true(is.numeric(delta2))
})

test_that("glass_delta handles zero variance", {
  x <- rep(5, 10)
  
  expect_warning(delta <- glass_delta(x, mu0 = 0, sigma = 0))
  expect_true(is.na(delta))
})

test_that("effect_size_summary creates comparison", {
  set.seed(333)
  x <- rnorm(30, mean = 5, sd = 2)
  
  summary <- effect_size_summary(x, mu0 = 0, conf.level = 0.95)
  
  expect_s3_class(summary, "effect_size_summary")
  expect_s3_class(summary, "data.frame")
  expect_equal(nrow(summary), 2)  # Cohen's d and Hedges' g
  expect_true(all(c("Measure", "Estimate", "CI_Lower", "CI_Upper", "Interpretation") %in% names(summary)))
  expect_equal(attr(summary, "conf.level"), 0.95)
})

test_that("effect_size_summary has correct attributes", {
  set.seed(444)
  x <- rnorm(25, mean = 10, sd = 3)
  
  summary <- effect_size_summary(x, mu0 = 5)
  
  expect_equal(attr(summary, "n"), 25)
  expect_equal(attr(summary, "mu0"), 5)
  expect_true(!is.null(attr(summary, "conf.level")))
})

test_that("print.effect_size displays correctly", {
  set.seed(555)
  x <- rnorm(30, mean = 5, sd = 2)
  
  es <- cohens_d_ci(x, mu0 = 0, nboot = 100)
  
  expect_output(print(es), "Effect Size")
  expect_output(print(es), "Cohen's d")
  expect_output(print(es), "Confidence Interval")
})

test_that("print.effect_size_summary displays correctly", {
  set.seed(666)
  x <- rnorm(30, mean = 5, sd = 2)
  
  summary <- effect_size_summary(x, mu0 = 0)
  
  expect_output(print(summary), "Effect Size Summary")
  expect_output(print(summary), "Sample size")
})

test_that("effect sizes handle edge cases", {
  set.seed(555)
  
  # Very small effect - use larger sample for more stable estimate
  x_small <- rnorm(100, mean = 0.1, sd = 2)
  es_small <- cohens_d_ci(x_small, mu0 = 0, nboot = 100)
  expect_true(abs(es_small$d) < 0.3)  # More realistic threshold
  
  # Large effect
  x_large <- rnorm(30, mean = 3, sd = 2)
  es_large <- cohens_d_ci(x_large, mu0 = 0, nboot = 100)
  expect_true(abs(es_large$d) > 0.5)  # More realistic threshold
})

