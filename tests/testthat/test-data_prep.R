# Tests for data preparation and example generators

test_that("generate_example_scores works correctly", {
  # Test basic generation
  scores <- generate_example_scores(n = 30, mean = 80, sd = 10, seed = 123)
  
  expect_length(scores, 30)
  expect_type(scores, "double")
  expect_true(all(is.finite(scores)))
  
  # Test reproducibility with seed
  scores1 <- generate_example_scores(n = 20, mean = 75, sd = 5, seed = 42)
  scores2 <- generate_example_scores(n = 20, mean = 75, sd = 5, seed = 42)
  expect_equal(scores1, scores2)
})

test_that("generate_example_scores validates input", {
  # Invalid n
  expect_error(
    generate_example_scores(n = 1, mean = 80, sd = 10),
    "n must be a single integer >= 2"
  )
  
  expect_error(
    generate_example_scores(n = "30", mean = 80, sd = 10)
  )
  
  expect_error(
    generate_example_scores(n = c(10, 20), mean = 80, sd = 10)
  )
})

test_that("generate_example_scores produces reasonable distributions", {
  set.seed(999)
  scores <- generate_example_scores(n = 1000, mean = 80, sd = 10)
  
  # Check that mean and sd are close to specified values
  expect_true(abs(mean(scores) - 80) < 2)  # Within 2 units
  expect_true(abs(sd(scores) - 10) < 2)    # Within 2 units
})

test_that("generate_example_weights works correctly", {
  weights <- generate_example_weights(n = 30, mean = 500, sd = 5, seed = 123)
  
  expect_length(weights, 30)
  expect_type(weights, "double")
  expect_true(all(is.finite(weights)))
})

test_that("generate_example_weights validates input", {
  expect_error(
    generate_example_weights(n = 1, mean = 500, sd = 5)
  )
  
  expect_error(
    generate_example_weights(n = -5, mean = 500, sd = 5)
  )
})

test_that("generate_example_weights produces reasonable distributions", {
  set.seed(888)
  weights <- generate_example_weights(n = 1000, mean = 500, sd = 5)
  
  expect_true(abs(mean(weights) - 500) < 1)
  expect_true(abs(sd(weights) - 5) < 1)
})

test_that("example_one_mean_pipeline works end-to-end", {
  result <- example_one_mean_pipeline(
    n = 30, 
    true_mean = 80, 
    sd = 10, 
    mu0 = 75, 
    seed = 123
  )
  
  expect_s3_class(result, "oneMeanTest")
  expect_equal(result$null.value, c(mu = 75))
  expect_true(!is.null(attr(result, "data")))
  expect_length(attr(result, "data"), 30)
})

test_that("example_one_mean_pipeline is reproducible", {
  result1 <- example_one_mean_pipeline(n = 25, seed = 456)
  result2 <- example_one_mean_pipeline(n = 25, seed = 456)
  
  expect_equal(result1$statistic, result2$statistic)
  expect_equal(result1$p.value, result2$p.value)
})

test_that("example_one_mean_pipeline with different parameters", {
  # Test with different means
  result_high <- example_one_mean_pipeline(
    n = 50, true_mean = 100, mu0 = 80, seed = 789
  )
  
  expect_s3_class(result_high, "oneMeanTest")
  expect_true(result_high$sample.stats$mean > 80)
})

test_that("example_one_mean_pipeline stores data attribute", {
  result <- example_one_mean_pipeline(n = 30, seed = 111)
  
  stored_data <- attr(result, "data")
  expect_false(is.null(stored_data))
  expect_length(stored_data, 30)
  expect_type(stored_data, "double")
})
