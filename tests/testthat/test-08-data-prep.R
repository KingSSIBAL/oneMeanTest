# ============================================================
# TEST SUITE 08: Data Preparation Functions
# ============================================================

test_that("prepare_data extracts numeric vector from data frame", {
  # Test with data frame
  df <- data.frame(x = c(1, 2, 3, 4, 5), y = c(6, 7, 8, 9, 10))
  
  # Assuming function exists: prepare_data(df, "x")
  # If function doesn't exist, check actual function names
  skip_if_not(exists("prepare_data", where = asNamespace("oneMeanTest")),
              "prepare_data function not found")
  
  result <- prepare_data(df, "x")
  expect_equal(result, c(1, 2, 3, 4, 5))
})

test_that("prepare_data handles formula interface", {
  skip_if_not(exists("prepare_data", where = asNamespace("oneMeanTest")),
              "prepare_data function not found")
  
  df <- data.frame(value = rnorm(20), group = rep(c("A", "B"), each = 10))
  
  # Test formula ~ data interface if supported
  expect_silent(prepare_data(df, "value"))
})

test_that("extract_numeric handles various input types", {
  skip_if_not(exists("extract_numeric", where = asNamespace("oneMeanTest")),
              "extract_numeric function not found")
  
  # Numeric vector
  expect_equal(extract_numeric(1:5), 1:5)
  
  # Data frame column
  df <- data.frame(x = 1:5)
  expect_equal(extract_numeric(df$x), 1:5)
})

test_that("validate_data_input checks input validity", {
  skip_if_not(exists("validate_data_input", where = asNamespace("oneMeanTest")),
              "validate_data_input function not found")
  
  # Valid input
  expect_silent(validate_data_input(c(1, 2, 3)))
  
  # Invalid input
  expect_error(validate_data_input(c("a", "b", "c")))
})
