# Integration tests

test_that("complete workflow executes without error", {
  set.seed(1616)
  x <- rnorm(40, mean = 15, sd = 3)
  
  # Run test
  result <- one_mean_test(
    x,
    mu0 = 14,
    alternative = "greater",
    alpha = 0.05,
    conf.level = 0.95,
    check_assumptions = TRUE
  )
  
  # Should print
  expect_output(print(result))
  
  # Should have assumptions if checked
  expect_false(is.null(result$assumptions))
  
  # Should be able to access components
  expect_true(is.numeric(result$statistic))
  expect_true(is.numeric(result$p.value))
  expect_true(is.character(result$decision))
})

test_that("assumptions checking completes properly", {
  set.seed(1717)
  x <- rnorm(50, mean = 20, sd = 4)
  
  result <- one_mean_test(x, mu0 = 20, check_assumptions = TRUE)
  
  expect_false(is.null(result$assumptions))
  expect_type(result$assumptions, "list")
})

test_that("all three alternatives work in full workflow", {
  set.seed(1818)
  x <- rnorm(35, mean = 12, sd = 2.5)
  
  alternatives <- c("two.sided", "less", "greater")
  
  for (alt in alternatives) {
    result <- one_mean_test(
      x, 
      mu0 = 12, 
      alternative = alt,
      check_assumptions = FALSE
    )
    
    expect_s3_class(result, "oneMeanTest")
    expect_equal(result$alternative, alt)
    expect_true(result$decision %in% c("reject H0", "fail to reject H0"))
  }
})
