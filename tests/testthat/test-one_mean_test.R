test_that("one_mean_test matches t.test for basic two-sided case", {
  set.seed(123)
  x <- rnorm(20, mean = 5, sd = 2)
  
  ours <- one_mean_test(
    x,
    mu0 = 4.5,
    alternative = "two.sided",
    alpha = 0.05,
    conf.level = 0.95,
    check_assumptions = FALSE
  )
  
  theirs <- t.test(x, mu = 4.5, alternative = "two.sided", conf.level = 0.95)
  
  expect_equal(ours$statistic, theirs$statistic)
  expect_equal(ours$parameter, theirs$parameter)
  expect_equal(ours$p.value, theirs$p.value, tolerance = 0.001)
  expect_equal(unname(ours$estimate), unname(theirs$estimate))  # Remove names
  expect_equal(unname(ours$conf.int), unname(theirs$conf.int), tolerance = 0.0001)
  expect_equal(unname(ours$null.value), unname(theirs$null.value))  # Remove names
  expect_equal(ours$alternative, theirs$alternative)
})

test_that("one_mean_test works for 'less' and 'greater' alternatives", {
  set.seed(456)
  x <- rnorm(15, mean = 10, sd = 3)
  
  for (alt in c("less", "greater")) {
    ours <- one_mean_test(
      x,
      mu0 = 11,
      alternative = alt,
      alpha = 0.05,
      check_assumptions = FALSE
    )
    
    theirs <- t.test(x, mu = 11, alternative = alt)
    
    expect_equal(ours$statistic, theirs$statistic)
    expect_equal(ours$p.value, theirs$p.value, tolerance = 0.00001)
    expect_equal(unname(ours$estimate), unname(theirs$estimate))  # Remove names
    expect_equal(ours$alternative, theirs$alternative)
  }
})

test_that("one_mean_test returns all expected components", {
  set.seed(789)
  x <- rnorm(30, mean = 15, sd = 4)
  
  res <- one_mean_test(x, mu0 = 14, check_assumptions = FALSE)
  
  expect_s3_class(res, "oneMeanTest")
  expect_named(res, c(
    "statistic", "parameter", "p.value", "conf.int", "estimate",
    "null.value", "alternative", "t.critical", "method", "data.name",
    "sample.stats", "alpha", "assumptions", "decision", "interpretation"
  ))
  
  expect_type(res$statistic, "double")
  expect_type(as.numeric(res$parameter), "double")  # Convert to numeric first
  expect_type(res$p.value, "double")
  expect_type(res$conf.int, "double")
  expect_type(res$estimate, "double")
  expect_type(res$null.value, "double")
  expect_type(res$alternative, "character")
  expect_type(res$t.critical, "double")
  expect_type(res$method, "character")
  expect_type(res$data.name, "character")
  expect_type(res$sample.stats, "list")
  expect_type(res$alpha, "double")
  expect_null(res$assumptions)
  expect_type(res$decision, "character")
  expect_type(res$interpretation, "character")
})

test_that("one_mean_test decision matches p-value and alpha", {
  set.seed(111)
  x <- rnorm(25, mean = 100, sd = 15)
  
  res1 <- one_mean_test(x, mu0 = 100, alpha = 0.05, check_assumptions = FALSE)
  if (res1$p.value <= 0.05) {
    expect_equal(res1$decision, "reject H0")
  } else {
    expect_equal(res1$decision, "fail to reject H0")
  }
  
  res2 <- one_mean_test(x, mu0 = 80, alpha = 0.01, check_assumptions = FALSE)
  if (res2$p.value <= 0.01) {
    expect_equal(res2$decision, "reject H0")
  } else {
    expect_equal(res2$decision, "fail to reject H0")
  }
})

test_that("one_mean_test includes assumptions when requested", {
  set.seed(222)
  x <- rnorm(30, mean = 50, sd = 10)
  
  res_with <- one_mean_test(x, mu0 = 50, check_assumptions = TRUE)
  expect_s3_class(res_with$assumptions, "oneMeanTest_assumptions")
  
  res_without <- one_mean_test(x, mu0 = 50, check_assumptions = FALSE)
  expect_null(res_without$assumptions)
})

test_that("one_mean_test handles different confidence levels", {
  set.seed(333)
  x <- rnorm(20, mean = 25, sd = 5)
  
  res90 <- one_mean_test(x, mu0 = 25, conf.level = 0.90, check_assumptions = FALSE)
  res95 <- one_mean_test(x, mu0 = 25, conf.level = 0.95, check_assumptions = FALSE)
  res99 <- one_mean_test(x, mu0 = 25, conf.level = 0.99, check_assumptions = FALSE)
  
  expect_equal(attr(res90$conf.int, "conf.level"), 0.90)
  expect_equal(attr(res95$conf.int, "conf.level"), 0.95)
  expect_equal(attr(res99$conf.int, "conf.level"), 0.99)
  
  width90 <- diff(res90$conf.int)
  width95 <- diff(res95$conf.int)
  width99 <- diff(res99$conf.int)
  
  expect_true(width90 < width95)
  expect_true(width95 < width99)
})
