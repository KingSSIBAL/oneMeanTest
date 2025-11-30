# ============================================================
# Test Configuration & Helpers
# ============================================================

# Set global tolerance for numerical comparisons
options(testthat.tolerance = 1e-4)

# Helper: Generate test data with known properties
generate_test_data <- function(n = 50, mean = 100, sd = 15, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  rnorm(n, mean = mean, sd = sd)
}

# Helper: Check if value is formatted to k decimals
check_decimal_places <- function(x, k = 4) {
  x_str <- as.character(x)
  decimal_part <- sub(".*\\.", "", x_str)
  nchar(decimal_part) == k
}

# Helper: Compare results to 4 decimals
expect_equal_4d <- function(actual, expected, label = NULL) {
  testthat::expect_equal(
    round(actual, 4),
    round(expected, 4),
    tolerance = 0.0001,
    label = label
  )
}
