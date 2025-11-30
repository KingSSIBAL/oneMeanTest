# ============================================================
# TEST SUITE 16: ggplot2 Visualizations
# ============================================================
# Tests for ggplot2-based plotting functions

test_that("autoplot works for oneMeanTest objects", {
  skip_if_not_installed("ggplot2")
  
  set.seed(123)
  x <- rnorm(30, mean = 5, sd = 2)
  result <- one_mean_test(x, mu0 = 5, check_assumptions = FALSE)
  
  # Use oneMeanTest namespace explicitly
  p <- oneMeanTest::autoplot.oneMeanTest(result, type = "distribution")
  expect_s3_class(p, "ggplot")
  
  # CI plot
  p_ci <- oneMeanTest::autoplot.oneMeanTest(result, type = "ci")
  expect_s3_class(p_ci, "ggplot")
  
  # QQ plot
  p_qq <- oneMeanTest::autoplot.oneMeanTest(result, type = "qq")
  expect_s3_class(p_qq, "ggplot")
})


test_that("ggplot_distribution creates plot", {
  skip_if_not_installed("ggplot2")
  
  set.seed(456)
  x <- rnorm(30, mean = 5, sd = 2)
  
  p <- ggplot_distribution(x)
  expect_s3_class(p, "ggplot")
})

test_that("ggplot_ci creates confidence interval plot", {
  skip_if_not_installed("ggplot2")
  
  set.seed(789)
  x <- rnorm(30, mean = 5, sd = 2)
  result <- one_mean_test(x, mu0 = 5, check_assumptions = FALSE)
  
  p <- ggplot_ci(result)
  expect_s3_class(p, "ggplot")
})

test_that("ggplot_qq creates Q-Q plot", {
  skip_if_not_installed("ggplot2")
  
  set.seed(111)
  x <- rnorm(30, mean = 5, sd = 2)
  
  p <- ggplot_qq(x)
  expect_s3_class(p, "ggplot")
})

test_that("ggplot_boxplot creates boxplot", {
  skip_if_not_installed("ggplot2")
  
  set.seed(222)
  x <- rnorm(30, mean = 5, sd = 2)
  
  p <- ggplot_boxplot(x)
  expect_s3_class(p, "ggplot")
  
  # With mu0
  p_mu0 <- ggplot_boxplot(x, mu0 = 5)
  expect_s3_class(p_mu0, "ggplot")
})

test_that("ggplot_diagnostic creates diagnostic plots", {
  skip_if_not_installed("ggplot2")
  
  set.seed(333)
  x <- rnorm(30, mean = 5, sd = 2)
  result <- one_mean_test(x, mu0 = 5, check_assumptions = FALSE)
  
  diag <- ggplot_diagnostic(x, result)
  
  # Returns either patchwork object or list
  expect_true(inherits(diag, "patchwork") || is.list(diag))
})

test_that("ggplot_power_curve creates power plot", {
  skip_if_not_installed("ggplot2")
  
  p <- ggplot_power_curve(delta = 5, sd = 15, alpha = 0.05)
  expect_s3_class(p, "ggplot")
})

test_that("ggplot_effect_sizes plots effect sizes", {
  skip_if_not_installed("ggplot2")
  
  set.seed(444)
  x <- rnorm(30, mean = 5, sd = 2)
  summary <- effect_size_summary(x, mu0 = 0)
  
  p <- ggplot_effect_sizes(summary)
  expect_s3_class(p, "ggplot")
})

test_that("ggplot_bootstrap plots bootstrap results", {
  skip_if_not_installed("ggplot2")
  
  set.seed(555)
  x <- rnorm(30, mean = 5, sd = 2)
  boot_result <- bootstrap_ttest(x, mu0 = 5, nboot = 500)
  
  plots <- ggplot_bootstrap(boot_result)
  
  # Returns either patchwork or list
  expect_true(inherits(plots, "patchwork") || is.list(plots))
})

test_that("ggplot_multiple_tests plots multiple test results", {
  skip_if_not_installed("ggplot2")
  
  set.seed(666)
  data_list <- list(
    g1 = rnorm(20, mean = 5, sd = 2),
    g2 = rnorm(20, mean = 6, sd = 2),
    g3 = rnorm(20, mean = 5.5, sd = 2)
  )
  
  result <- multiple_t_tests(data_list, mu0_list = c(5, 5, 5), method = "holm")
  
  p <- ggplot_multiple_tests(result)
  expect_s3_class(p, "ggplot")
})

test_that("ggplot functions error when ggplot2 not available", {
  # Mock ggplot2 not being available
  skip("Difficult to test without mocking")
})
