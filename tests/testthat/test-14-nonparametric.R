# ============================================================
# TEST SUITE 14: Nonparametric Tests
# ============================================================
# Tests for Wilcoxon signed-rank test and sign test

test_that("wilcoxon_test works correctly", {
  set.seed(123)
  x <- rexp(30, rate = 0.1)
  
  result <- wilcoxon_test(x, mu0 = 10)
  
  expect_s3_class(result, "oneMeanTest_wilcoxon")
  expect_true(is.numeric(result$statistic))
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
  expect_equal(result$n, 30)
})

test_that("wilcoxon_test handles different alternatives", {
  set.seed(456)
  x <- rnorm(30, mean = 5, sd = 2)
  
  # Two-sided
  result_two <- wilcoxon_test(x, mu0 = 0, alternative = "two.sided")
  expect_equal(result_two$alternative, "two.sided")
  
  # Greater
  result_greater <- wilcoxon_test(x, mu0 = 0, alternative = "greater")
  expect_equal(result_greater$alternative, "greater")
  
  # Less
  result_less <- wilcoxon_test(x, mu0 = 0, alternative = "less")
  expect_equal(result_less$alternative, "less")
})

test_that("wilcoxon_test removes zeros", {
  x <- c(0, 0, 1, 2, 3, 4, 5)
  
  result <- wilcoxon_test(x, mu0 = 0)
  
  expect_equal(result$n, 7)
  expect_equal(result$n.used, 5)  # 2 zeros removed
})

test_that("wilcoxon_test handles all zeros", {
  x <- rep(5, 10)
  
  expect_warning(result <- wilcoxon_test(x, mu0 = 5), "trivial")
  expect_equal(result$p.value, 1)
})

test_that("wilcoxon_test computes pseudomedian", {
  set.seed(789)
  x <- rnorm(30, mean = 5, sd = 2)
  
  result <- wilcoxon_test(x, mu0 = 0)
  
  expect_true(is.numeric(result$pseudomedian))
  expect_true(!is.na(result$pseudomedian))
})

test_that("wilcoxon_test computes confidence interval", {
  set.seed(111)
  x <- rnorm(30, mean = 5, sd = 2)
  
  result <- wilcoxon_test(x, mu0 = 0, conf.level = 0.95)
  
  expect_true(is.numeric(result$conf.int))
  expect_equal(length(result$conf.int), 2)
  expect_true(result$conf.int[1] < result$conf.int[2])
})

test_that("wilcoxon_test exact vs approximate", {
  set.seed(222)
  x <- rnorm(20, mean = 5, sd = 2)
  
  # Small sample - should use exact
  result_exact <- wilcoxon_test(x, mu0 = 0, exact = TRUE)
  expect_true(result_exact$exact)
  
  # Large sample - should use approximate
  x_large <- rnorm(60, mean = 5, sd = 2)
  result_approx <- wilcoxon_test(x_large, mu0 = 0, exact = FALSE)
  expect_false(result_approx$exact)
})

test_that("sign_test works correctly", {
  set.seed(333)
  x <- c(1, 3, 5, 7, 9, 11, 6, 4, 8)
  
  result <- sign_test(x, mu0 = 5)
  
  expect_true(is.list(result))
  expect_true(is.numeric(result$statistic))
  expect_true(is.numeric(result$p.value))
  expect_true(result$p.value >= 0 && result$p.value <= 1)
})

test_that("sign_test counts correctly", {
  x <- c(1, 3, 5, 7, 9)  # All > 0
  
  result <- sign_test(x, mu0 = 0)
  
  expect_equal(result$n.plus, 5)
  expect_equal(result$n.minus, 0)
  expect_equal(result$n.zero, 0)
  expect_equal(result$n.used, 5)
})

test_that("sign_test handles zeros", {
  x <- c(-1, 0, 0, 1, 2, 3)
  
  result <- sign_test(x, mu0 = 0)
  
  expect_equal(result$n, 6)
  expect_equal(result$n.zero, 2)
  expect_equal(result$n.used, 4)
})

test_that("sign_test different alternatives", {
  set.seed(444)
  x <- rnorm(30, mean = 5, sd = 2)
  
  # Two-sided
  result_two <- sign_test(x, mu0 = 0, alternative = "two.sided")
  expect_equal(result_two$alternative, "two.sided")
  
  # Greater
  result_greater <- sign_test(x, mu0 = 0, alternative = "greater")
  expect_equal(result_greater$alternative, "greater")
  
  # Less
  result_less <- sign_test(x, mu0 = 0, alternative = "less")
  expect_equal(result_less$alternative, "less")
})

test_that("compare_tests runs all three tests", {
  set.seed(555)
  x <- rnorm(30, mean = 5, sd = 2)
  
  comparison <- compare_tests(x, mu0 = 0)
  
  expect_s3_class(comparison, "test_comparison")
  expect_s3_class(comparison, "data.frame")
  expect_equal(nrow(comparison), 3)  # Three tests
  expect_true(all(c("Test", "Statistic", "P_value", "Decision") %in% names(comparison)))
})

test_that("compare_tests shows agreement", {
  set.seed(666)
  
  # Strong effect - all should reject
  x_strong <- rnorm(30, mean = 10, sd = 2)
  comp_strong <- compare_tests(x_strong, mu0 = 0)
  
  expect_true(all(grepl("reject", comp_strong$Decision, ignore.case = TRUE)))
})

test_that("print.oneMeanTest_wilcoxon displays correctly", {
  set.seed(777)
  x <- rnorm(30, mean = 5, sd = 2)
  
  result <- wilcoxon_test(x, mu0 = 0)
  
  expect_output(print(result), "Wilcoxon")
  expect_output(print(result), "p-value")
  expect_output(print(result), "pseudo-median")
})
