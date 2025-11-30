# ============================================================
# TEST SUITE 08: Data Preparation Functions
# ============================================================

test_that("prepare_data handles numeric vectors", {
  x <- c(1, 2, 3, 4, 5)
  result <- oneMeanTest:::prepare_data(x)
  expect_equal(result, x)
})

test_that("prepare_data extracts from data frame", {
  df <- data.frame(x = c(1, 2, 3, 4, 5), y = c(6, 7, 8, 9, 10))
  result <- oneMeanTest:::prepare_data(df, "x")
  expect_equal(result, c(1, 2, 3, 4, 5))
})

test_that("prepare_data errors on missing variable", {
  df <- data.frame(x = 1:5)
  expect_error(oneMeanTest:::prepare_data(df, "z"), "not found")
})

test_that("prepare_data handles lists", {
  lst <- list(a = c(1, 2, 3), b = "text")
  result <- oneMeanTest:::prepare_data(lst)
  expect_equal(result, c(1, 2, 3))
})

test_that("prepare_data coerces when possible", {
  result <- oneMeanTest:::prepare_data(c("1", "2", "3"))
  expect_equal(result, c(1, 2, 3))
})

test_that("extract_numeric handles numeric input", {
  x <- c(1, 2, 3)
  result <- oneMeanTest:::extract_numeric(x)
  expect_equal(result, x)
})

test_that("extract_numeric handles factors", {
  x <- factor(c("1", "2", "3"))
  expect_warning(result <- oneMeanTest:::extract_numeric(x), "factor")
  expect_true(is.numeric(result))
})

test_that("extract_numeric handles characters", {
  x <- c("1", "2", "3")
  result <- oneMeanTest:::extract_numeric(x)
  expect_equal(result, c(1, 2, 3))
})

test_that("extract_numeric errors on non-convertible", {
  x <- c("a", "b", "c")
  expect_error(oneMeanTest:::extract_numeric(x), "Cannot convert")
})

test_that("extract_numeric handles logical", {
  x <- c(TRUE, FALSE, TRUE)
  result <- oneMeanTest:::extract_numeric(x)
  expect_equal(result, c(1, 0, 1))
})

test_that("validate_data_input accepts valid data", {
  x <- c(1, 2, 3, 4, 5)
  expect_silent(oneMeanTest:::validate_data_input(x))
})

test_that("validate_data_input rejects non-numeric", {
  expect_error(oneMeanTest:::validate_data_input("text"), "must be numeric")
})

test_that("validate_data_input rejects empty", {
  expect_error(oneMeanTest:::validate_data_input(numeric(0)), "at least one")
})

test_that("validate_data_input rejects all NA", {
  # Test with numeric NA
  expect_error(oneMeanTest:::validate_data_input(c(NA_real_, NA_real_)), 
               "All values are NA")
})

test_that("validate_data_input rejects insufficient n", {
  expect_error(oneMeanTest:::validate_data_input(c(1, NA_real_), min_n = 2), 
               "at least 2")
})

test_that("validate_data_input warns on infinite", {
  expect_warning(oneMeanTest:::validate_data_input(c(1, 2, Inf)), "infinite")
})

test_that("validate_data_input warns on zero variance", {
  expect_warning(oneMeanTest:::validate_data_input(c(5, 5, 5)), "zero variance")
})

test_that("extract_variable_name works with formula", {
  f <- ~ variable
  result <- oneMeanTest:::extract_variable_name(f)
  expect_equal(result, "variable")
})

test_that("extract_variable_name errors on non-formula", {
  expect_error(oneMeanTest:::extract_variable_name("not a formula"), 
               "must be a formula")
})

test_that("prepare_data_formula works correctly", {
  df <- data.frame(x = 1:5, y = 6:10)
  f <- ~ x
  result <- oneMeanTest:::prepare_data_formula(f, df)
  expect_equal(result, 1:5)
})

test_that("prepare_data_formula errors on missing variable", {
  df <- data.frame(x = 1:5)
  f <- ~ z
  expect_error(oneMeanTest:::prepare_data_formula(f, df), "not found")
})

test_that("prepare_data_formula errors on non-formula", {
  df <- data.frame(x = 1:5)
  expect_error(oneMeanTest:::prepare_data_formula("not formula", df), 
               "must be a formula")
})

test_that("prepare_data_formula errors on non-data.frame", {
  f <- ~ x
  expect_error(oneMeanTest:::prepare_data_formula(f, c(1, 2, 3)), 
               "must be a data frame")
})
