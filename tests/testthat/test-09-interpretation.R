# ============================================================
# TEST SUITE 09: Interpretation Helper Functions (EXTENDED)
# ============================================================

test_that("interpret_result works with reject decision", {
  set.seed(123)
  x <- rnorm(30, mean = 10, sd = 2)
  result <- one_mean_test(x, mu0 = 5, check_assumptions = FALSE)
  
  interp <- interpret_result(result)
  
  expect_type(interp, "character")
  expect_true(grepl("reject", interp, ignore.case = TRUE))
})

test_that("interpret_result works with fail to reject", {
  set.seed(456)
  x <- rnorm(30, mean = 5, sd = 2)
  result <- one_mean_test(x, mu0 = 5, check_assumptions = FALSE)
  
  interp <- interpret_result(result)
  expect_type(interp, "character")
})

test_that("format_decision works correctly", {
  result1 <- format_decision("reject H0", 0.05)
  expect_true(grepl("Reject", result1))
  
  result2 <- format_decision("fail to reject H0", 0.01)
  expect_true(grepl("Fail to reject", result2))
})

test_that("get_alternative_text returns all alternatives", {
  expect_equal(get_alternative_text("two.sided"), "different from")
  expect_equal(get_alternative_text("greater"), "greater than")
  expect_equal(get_alternative_text("less"), "less than")
})

test_that("interpret_p_value categorizes correctly", {
  expect_true(grepl("very strong", interpret_p_value(0.0005), ignore.case = TRUE))
  expect_true(grepl("strong", interpret_p_value(0.005), ignore.case = TRUE))
  expect_true(grepl("moderate", interpret_p_value(0.03), ignore.case = TRUE))
  expect_true(grepl("weak", interpret_p_value(0.08), ignore.case = TRUE))
  expect_true(grepl("little|no", interpret_p_value(0.5), ignore.case = TRUE))
})

test_that("interpret_p_value validates input", {
  expect_error(interpret_p_value(-0.1), "between 0 and 1")
  expect_error(interpret_p_value(1.5), "between 0 and 1")
  expect_error(interpret_p_value("text"), "between 0 and 1")
})

test_that("interpret_effect_size categorizes all sizes", {
  expect_true(grepl("negligible", interpret_effect_size(0.1), ignore.case = TRUE))
  expect_true(grepl("small", interpret_effect_size(0.3), ignore.case = TRUE))
  expect_true(grepl("medium", interpret_effect_size(0.6), ignore.case = TRUE))
  expect_true(grepl("large", interpret_effect_size(1.0), ignore.case = TRUE))
})

test_that("interpret_effect_size handles negative values", {
  expect_true(grepl("small", interpret_effect_size(-0.3), ignore.case = TRUE))
})

test_that("interpret_effect_size validates input", {
  expect_error(interpret_effect_size("text"), "must be numeric")
})

test_that("cohens_d calculates correctly", {
  x <- c(10, 12, 14, 16, 18)
  d <- cohens_d(x, mu0 = 10)
  expect_true(is.numeric(d))
  expect_true(d > 0)
})

test_that("cohens_d handles zero variance", {
  x <- c(5, 5, 5, 5)
  expect_warning(d <- cohens_d(x, mu0 = 10), "zero")
  expect_true(is.na(d))
})

test_that("interpret_ci works without mu0", {
  ci <- c(2, 8)
  result <- interpret_ci(ci, conf_level = 0.95)
  expect_type(result, "character")
  expect_true(grepl("95%", result))
})

test_that("interpret_ci works with mu0 inside CI", {
  ci <- c(2, 8)
  result <- interpret_ci(ci, conf_level = 0.95, mu0 = 5)
  expect_true(grepl("plausible", result, ignore.case = TRUE))
})

test_that("interpret_ci works with mu0 below CI", {
  ci <- c(5, 10)
  result <- interpret_ci(ci, conf_level = 0.95, mu0 = 2)
  expect_true(grepl("greater", result, ignore.case = TRUE))
})

test_that("interpret_ci works with mu0 above CI", {
  ci <- c(2, 5)
  result <- interpret_ci(ci, conf_level = 0.95, mu0 = 10)
  expect_true(grepl("less", result, ignore.case = TRUE))
})

test_that("interpret_ci validates input", {
  expect_error(interpret_ci(c(1, 2, 3)), "exactly 2 values")
})

test_that("interpret_assumptions works", {
  set.seed(123)
  x <- rnorm(50)
  assumptions <- check_assumptions(x, verbose = FALSE)
  
  result <- interpret_assumptions(assumptions)
  expect_type(result, "character")
  expect_true(grepl("normal", result, ignore.case = TRUE))
})

test_that("interpret_assumptions handles non-normal", {
  x <- rexp(50)
  assumptions <- check_assumptions(x, verbose = FALSE)
  
  result <- interpret_assumptions(assumptions)
  expect_type(result, "character")
})

test_that("interpret_assumptions handles outliers", {
  x <- c(rnorm(50), 100, -100)
  assumptions <- check_assumptions(x, verbose = FALSE)
  
  result <- interpret_assumptions(assumptions)
  expect_true(grepl("outlier", result, ignore.case = TRUE))
})

test_that("interpret_assumptions handles small sample", {
  x <- rnorm(15)
  assumptions <- check_assumptions(x, verbose = FALSE)
  
  result <- interpret_assumptions(assumptions)
  expect_true(grepl("small|15", result, ignore.case = TRUE))
})

test_that("interpret_assumptions validates input", {
  expect_error(interpret_assumptions(list()), "must be from check_assumptions")
})
