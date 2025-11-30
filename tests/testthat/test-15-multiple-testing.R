# ============================================================
# TEST SUITE 15: Multiple Testing Corrections
# ============================================================
# Tests for multiple comparison adjustments

test_that("adjust_p_values applies Bonferroni correction", {
  p_vals <- c(0.001, 0.01, 0.04, 0.15, 0.50)
  
  bonf <- adjust_p_values(p_vals, method = "bonferroni", alpha = 0.05)
  
  expect_s3_class(bonf, "multiple_testing")
  expect_s3_class(bonf, "data.frame")
  expect_true(all(bonf$P_adjusted >= bonf$P_value))
  expect_equal(attr(bonf, "method"), "bonferroni")
})

test_that("adjust_p_values applies Holm correction", {
  p_vals <- c(0.001, 0.01, 0.04, 0.15, 0.50)
  
  holm <- adjust_p_values(p_vals, method = "holm", alpha = 0.05)
  
  expect_s3_class(holm, "multiple_testing")
  expect_true(all(holm$P_adjusted >= holm$P_value))
  expect_equal(attr(holm, "method"), "holm")
})

test_that("adjust_p_values applies BH correction", {
  p_vals <- c(0.001, 0.01, 0.04, 0.15, 0.50)
  
  bh <- adjust_p_values(p_vals, method = "BH", alpha = 0.05)
  
  expect_s3_class(bh, "multiple_testing")
  expect_equal(attr(bh, "method"), "BH")
})

test_that("adjust_p_values handles all methods", {
  p_vals <- c(0.01, 0.02, 0.03, 0.04, 0.05)
  
  methods <- c("bonferroni", "holm", "hochberg", "BH", "BY", "fdr")
  
  for (method in methods) {
    result <- adjust_p_values(p_vals, method = method)
    expect_s3_class(result, "multiple_testing")
    expect_equal(nrow(result), length(p_vals))
  }
})

test_that("adjust_p_values validates input", {
  # Invalid p-values
  expect_error(adjust_p_values(c(-0.1, 0.5)), "between 0 and 1")
  expect_error(adjust_p_values(c(0.5, 1.5)), "between 0 and 1")
  expect_error(adjust_p_values(c("a", "b")), "numeric")
})

test_that("bonferroni_alpha calculates correctly", {
  alpha_adj <- bonferroni_alpha(alpha = 0.05, m = 10)
  expect_equal(alpha_adj, 0.005)
  
  alpha_adj2 <- bonferroni_alpha(alpha = 0.01, m = 5)
  expect_equal(alpha_adj2, 0.002)
})

test_that("multiple_t_tests conducts multiple tests", {
  set.seed(123)
  data_list <- list(
    group1 = rnorm(30, mean = 5, sd = 2),
    group2 = rnorm(30, mean = 6, sd = 2),
    group3 = rnorm(30, mean = 5.5, sd = 2)
  )
  
  result <- multiple_t_tests(data_list, mu0_list = c(5, 5, 5), method = "holm")
  
  expect_s3_class(result, "multiple_t_tests")
  expect_equal(nrow(result$summary), 3)
  expect_equal(length(result$tests), 3)
  expect_equal(result$method, "holm")
  expect_equal(result$m, 3)
})

test_that("multiple_t_tests uses test names", {
  set.seed(456)
  data_list <- list(
    test_a = rnorm(20, mean = 5, sd = 2),
    test_b = rnorm(20, mean = 6, sd = 2)
  )
  
  result <- multiple_t_tests(data_list, mu0_list = c(5, 5), method = "BH")
  
  expect_true("test_a" %in% result$summary$Test)
  expect_true("test_b" %in% result$summary$Test)
})

test_that("multiple_t_tests validates input", {
  data_list <- list(rnorm(20), rnorm(20))
  
  # Mismatched lengths
  expect_error(multiple_t_tests(data_list, mu0_list = c(0)), "same length")
})

test_that("multiple_t_tests works with different alternatives", {
  set.seed(789)
  data_list <- list(
    g1 = rnorm(20, mean = 5, sd = 2),
    g2 = rnorm(20, mean = 6, sd = 2)
  )
  
  result <- multiple_t_tests(data_list, mu0_list = c(5, 5), 
                              alternative = "greater", method = "bonferroni")
  
  expect_s3_class(result, "multiple_t_tests")
})

test_that("calculate_fdr estimates FDR", {
  p_vals <- c(0.001, 0.01, 0.02, 0.05, 0.10, 0.50)
  
  fdr <- calculate_fdr(p_vals, alpha = 0.05)
  
  expect_true(is.list(fdr))
  expect_true(is.numeric(fdr$n_rejected))
  expect_true(fdr$n_rejected <= length(p_vals))
  expect_equal(fdr$method, "Benjamini-Hochberg")
  expect_true(fdr$fdr_controlled)
})

test_that("calculate_fdr handles no rejections", {
  p_vals <- c(0.5, 0.6, 0.7, 0.8, 0.9)
  
  fdr <- calculate_fdr(p_vals, alpha = 0.05)
  
  expect_equal(fdr$n_rejected, 0)
  expect_equal(fdr$threshold, 0)
})

test_that("calculate_fdr handles all rejections", {
  p_vals <- c(0.001, 0.002, 0.003, 0.004, 0.005)
  
  fdr <- calculate_fdr(p_vals, alpha = 0.10)
  
  expect_true(fdr$n_rejected > 0)
})

test_that("print.multiple_testing displays correctly", {
  p_vals <- c(0.01, 0.04, 0.10)
  adj <- adjust_p_values(p_vals, method = "bonferroni")
  
  expect_output(print(adj), "Multiple Testing")
  expect_output(print(adj), "bonferroni")
  expect_output(print(adj), "Number of tests")
})

test_that("print.multiple_t_tests displays correctly", {
  set.seed(111)
  data_list <- list(
    g1 = rnorm(20, mean = 5, sd = 2),
    g2 = rnorm(20, mean = 6, sd = 2)
  )
  
  result <- multiple_t_tests(data_list, mu0_list = c(5, 5), method = "BH")
  
  expect_output(print(result), "Multiple One-Sample")
  expect_output(print(result), "BH")
  expect_output(print(result), "Family-wise")
})
