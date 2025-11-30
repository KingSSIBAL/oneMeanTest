# ============================================================
# TEST SUITE 09: Interpretation Helper Functions
# ============================================================

test_that("interpret_test_result generates correct text for rejection", {
  set.seed(123)
  x <- rnorm(30, mean = 10, sd = 2)
  result <- one_mean_test(x, mu0 = 5, check_assumptions = FALSE)
  
  interp <- interpret_result(result)
  
  expect_type(interp, "character")
  expect_true(grepl("reject", interp, ignore.case = TRUE))
  expect_true(grepl("significant", interp, ignore.case = TRUE))
})

test_that("interpret_test_result generates correct text for non-rejection", {
  set.seed(456)
  x <- rnorm(30, mean = 5, sd = 2)
  result <- one_mean_test(x, mu0 = 5, check_assumptions = FALSE)
  
  interp <- interpret_result(result)
  
  expect_type(interp, "character")
  expect_true(grepl("fail to reject|insufficient", interp, ignore.case = TRUE))
})

test_that("format_decision creates readable decision text", {
  result1 <- format_decision("reject H0", 0.05)
  expect_true(grepl("Reject the null hypothesis", result1))
  expect_true(grepl("0.05", result1))
  
  result2 <- format_decision("fail to reject H0", 0.01)
  expect_true(grepl("Fail to reject the null hypothesis", result2))
  expect_true(grepl("0.01", result2))
})

test_that("get_alternative_text returns correct direction", {
  expect_equal(get_alternative_text("two.sided"), "different from")
  expect_equal(get_alternative_text("greater"), "greater than")
  expect_equal(get_alternative_text("less"), "less than")
})

test_that("interpret_p_value provides context", {
  expect_true(grepl("strong", interpret_p_value(0.001), ignore.case = TRUE))
  expect_true(grepl("moderate", interpret_p_value(0.03), ignore.case = TRUE))
  expect_true(grepl("weak|no", interpret_p_value(0.15), ignore.case = TRUE))
})

test_that("interpret_effect_size categorizes effect", {
  # Small effect
  expect_true(grepl("small", interpret_effect_size(0.2), ignore.case = TRUE))
  
  # Medium effect
  expect_true(grepl("medium", interpret_effect_size(0.5), ignore.case = TRUE))
  
  # Large effect
  expect_true(grepl("large", interpret_effect_size(0.8), ignore.case = TRUE))
})

# Test what IS available - the interpretation field in results
test_that("one_mean_test generates interpretation text", {
  set.seed(123)
  x <- rnorm(30, mean = 5, sd = 2)
  result <- one_mean_test(x, mu0 = 5, check_assumptions = FALSE)
  
  expect_true(!is.null(result$interpretation))
  expect_type(result$interpretation, "character")
  expect_true(nchar(result$interpretation) > 20)
})

test_that("interpretation contains decision information", {
  set.seed(456)
  
  # Test reject case
  x_reject <- rnorm(100, mean = 10, sd = 1)
  result_reject <- one_mean_test(x_reject, mu0 = 5, check_assumptions = FALSE)
  expect_true(grepl("reject", result_reject$interpretation, ignore.case = TRUE))
  
  # Test fail to reject case
  x_fail <- rnorm(100, mean = 5, sd = 10)
  result_fail <- one_mean_test(x_fail, mu0 = 5, check_assumptions = FALSE)
  expect_true(grepl("fail to reject", result_fail$interpretation, ignore.case = TRUE))
})
