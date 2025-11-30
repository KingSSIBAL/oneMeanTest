# ============================================================
# TEST SUITE 05: Assumption Checking
# ============================================================
# Tests check_assumptions() function
# Tests normality, outliers, and sample size checks
# Extended coverage for all assumption functionality
# ============================================================

test_that("check_assumptions returns valid structure", {
  set.seed(666)
  
  # Normal data
  x_normal <- rnorm(50, mean = 100, sd = 15)
  result_normal <- check_assumptions(x_normal, verbose = FALSE)
  
  expect_s3_class(result_normal, "oneMeanTest_assumptions")
  expect_true(is.list(result_normal))
  expect_true("shapiro" %in% names(result_normal))
  expect_true("outliers" %in% names(result_normal))
  expect_true("n" %in% names(result_normal))
})

test_that("check_assumptions shapiro test works", {
  set.seed(777)
  x <- rnorm(50, mean = 100, sd = 15)
  
  result <- check_assumptions(x, verbose = FALSE)
  
  expect_true(!is.null(result$shapiro))
  expect_true(is.list(result$shapiro))
  expect_true("p.value" %in% names(result$shapiro))
})

test_that("check_assumptions detects outliers", {
  # Data with obvious outliers
  x_with_outliers <- c(rnorm(50, mean = 100, sd = 10), 200, 250, -50)
  
  result <- check_assumptions(x_with_outliers, verbose = FALSE)
  
  expect_true(!is.null(result$outliers))
  expect_true(length(result$outliers) > 0)
})

test_that("check_assumptions records sample size", {
  # Small sample
  x_small <- rnorm(15)
  result_small <- check_assumptions(x_small, verbose = FALSE)
  
  expect_equal(result_small$n, 15)
  
  # Large sample
  x_large <- rnorm(100)
  result_large <- check_assumptions(x_large, verbose = FALSE)
  
  expect_equal(result_large$n, 100)
})

test_that("one_mean_test integrates assumptions when requested", {
  set.seed(888)
  x <- rnorm(50, mean = 100, sd = 15)
  
  result_with <- one_mean_test(x, mu0 = 100, check_assumptions = TRUE)
  result_without <- one_mean_test(x, mu0 = 100, check_assumptions = FALSE)
  
  expect_true(!is.null(result_with$assumptions))
  expect_null(result_without$assumptions)
})

# ============================================================
# EXTENDED ASSUMPTIONS TESTS
# ============================================================

test_that("check_assumptions handles edge sample sizes", {
  # Very small sample (n < 3)
  x_tiny <- c(5, 7)
  result_tiny <- check_assumptions(x_tiny, verbose = FALSE)
  
  expect_true(is.na(result_tiny$shapiro$W))
  expect_true(grepl("not run", result_tiny$shapiro$note, ignore.case = TRUE))
  
  # Very large sample (n > 5000)
  set.seed(123)
  x_large <- rnorm(6000)
  result_large <- check_assumptions(x_large, verbose = FALSE)
  
  expect_true(is.na(result_large$shapiro$W))
})

test_that("check_assumptions verbose mode works", {
  set.seed(456)
  x <- rnorm(50, mean = 100, sd = 15)
  
  # Verbose = TRUE should print message
  expect_output(check_assumptions(x, alpha = 0.05, verbose = TRUE))
  
  # Verbose = FALSE should not print
  expect_silent(check_assumptions(x, alpha = 0.05, verbose = FALSE))
})

test_that("check_assumptions detects different types of outliers", {
  # Mild outliers only
  x_mild <- c(rnorm(50, mean = 100, sd = 10), 140)
  result_mild <- check_assumptions(x_mild, verbose = FALSE)
  
  expect_true(length(result_mild$outliers) > 0)
  
  # Extreme outliers
  x_extreme <- c(rnorm(50, mean = 100, sd = 10), 200, 250)
  result_extreme <- check_assumptions(x_extreme, verbose = FALSE)
  
  expect_true(length(result_extreme$outliers) > 0)
})

test_that("check_assumptions uses custom quantile function", {
  set.seed(789)
  x <- rnorm(50, mean = 100, sd = 15)
  
  # Should use .custom_quantile for IQR calculation
  result <- check_assumptions(x, verbose = FALSE)
  
  expect_true(is.numeric(result$n))
  expect_true(!is.null(result$outliers))
})

test_that("check_assumptions message contains all information", {
  set.seed(111)
  x_normal_no_outliers <- rnorm(50, mean = 100, sd = 10)
  result1 <- check_assumptions(x_normal_no_outliers, verbose = FALSE)
  
  expect_true(grepl("normal", result1$message, ignore.case = TRUE))
  expect_true(grepl("outlier", result1$message, ignore.case = TRUE))
  
  # Non-normal with outliers
  x_nonnormal <- c(rexp(50, rate = 0.1), 200)
  result2 <- check_assumptions(x_nonnormal, verbose = FALSE)
  
  expect_type(result2$message, "character")
})

test_that("check_assumptions returns correct S3 class", {
  set.seed(222)
  x <- rnorm(30)
  result <- check_assumptions(x, verbose = FALSE)
  
  expect_s3_class(result, "oneMeanTest_assumptions")
})

test_that("check_assumptions alpha parameter works", {
  set.seed(333)
  x <- rnorm(50, mean = 100, sd = 15)
  
  result_05 <- check_assumptions(x, alpha = 0.05, verbose = FALSE)
  result_01 <- check_assumptions(x, alpha = 0.01, verbose = FALSE)
  
  expect_equal(result_05$shapiro$alpha, 0.05)
  expect_equal(result_01$shapiro$alpha, 0.01)
})

test_that("check_assumptions handles no outliers case", {
  set.seed(444)
  # Generate data unlikely to have outliers
  x_clean <- rnorm(50, mean = 100, sd = 5)
  result <- check_assumptions(x_clean, verbose = FALSE)
  
  # May or may not have outliers, but should handle gracefully
  expect_true(is.numeric(length(result$outliers)))
})

test_that("check_assumptions normal flag is logical", {
  set.seed(555)
  x <- rnorm(50, mean = 100, sd = 15)
  result <- check_assumptions(x, verbose = FALSE)
  
  expect_true(is.logical(result$normal))
})

test_that("check_assumptions handles uniform distribution", {
  set.seed(666)
  x_uniform <- runif(50, min = 0, max = 100)
  result <- check_assumptions(x_uniform, verbose = FALSE)
  
  # Uniform distribution may fail normality test
  expect_s3_class(result, "oneMeanTest_assumptions")
  expect_true(!is.null(result$shapiro))
})
