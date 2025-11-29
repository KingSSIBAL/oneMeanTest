# Test one_mean_test core functionality

test_that("one_mean_test matches t.test for basic two-sided case", {
  set.seed(123)
  x <- rnorm(30, mean = 5, sd = 2)

  ours <- one_mean_test(
    x,
    mu0 = 5,
    alternative = "two.sided",
    alpha = 0.05,
    conf.level = 0.95,
    check_assumptions = FALSE
  )

  theirs <- t.test(
    x,
    mu = 5,
    alternative = "two.sided",
    conf.level = 0.95
  )

  expect_equal(unname(ours$statistic["t"]),
               unname(theirs$statistic),
               tolerance = 1e-6)
  expect_equal(ours$p.value,
               theirs$p.value,
               tolerance = 1e-6)
  expect_equal(unname(ours$conf.int),
               unname(theirs$conf.int),
               tolerance = 1e-6)
})

test_that("one_mean_test works for 'less' and 'greater' alternatives", {
  set.seed(456)
  x <- rnorm(25, mean = 10, sd = 3)

  for (alt in c("less", "greater")) {
    ours <- one_mean_test(
      x,
      mu0 = 10,
      alternative = alt,
      alpha = 0.05,
      conf.level = 0.95,
      check_assumptions = FALSE
    )

    theirs <- t.test(
      x,
      mu = 10,
      alternative = alt,
      conf.level = 0.95
    )

    expect_equal(unname(ours$statistic["t"]),
                 unname(theirs$statistic),
                 tolerance = 1e-6)
    expect_equal(ours$p.value,
                 theirs$p.value,
                 tolerance = 1e-6)
  }
})

test_that("one_mean_test returns correct object structure", {
  set.seed(789)
  x <- rnorm(20, mean = 0, sd = 1)
  
  result <- one_mean_test(x, mu0 = 0, check_assumptions = FALSE)
  
  expect_s3_class(result, "oneMeanTest")
  
  # FIX: Add "t.critical" to expected names
  expect_named(result, c("statistic", "parameter", "p.value", "conf.int",
                         "estimate", "null.value", "alternative", "t.critical",
                         "method", "data.name", "sample.stats", "alpha", 
                         "assumptions", "decision", "interpretation"))
  
  expect_type(result$statistic, "double")
  expect_type(result$p.value, "double")
  expect_type(result$decision, "character")
  expect_type(result$interpretation, "character")
  
  expect_true(is.numeric(result$parameter))
})

test_that("one_mean_test decision logic is correct", {
  set.seed(111)
  
  # Create data that will clearly reject H0
  x_reject <- rnorm(100, mean = 10, sd = 1)
  result_reject <- one_mean_test(x_reject, mu0 = 5, alpha = 0.05, 
                                 check_assumptions = FALSE)
  expect_equal(result_reject$decision, "reject H0")
  expect_true(result_reject$p.value < 0.05)
  
  # Create data that will fail to reject H0
  x_fail <- rnorm(100, mean = 5, sd = 1)
  result_fail <- one_mean_test(x_fail, mu0 = 5, alpha = 0.05,
                               check_assumptions = FALSE)
  expect_equal(result_fail$decision, "fail to reject H0")
  expect_true(result_fail$p.value >= 0.05)
})

test_that("one_mean_test errors on invalid input", {
  # non-numeric x
  expect_error(
    one_mean_test("not numeric", mu0 = 0),
    "numeric"
  )

  # all NA
  expect_error(
    one_mean_test(c(NA, NA, NA), mu0 = 0)
  )

  # only one non-missing value
  expect_error(
    suppressWarnings(one_mean_test(c(1, NA), mu0 = 0))
  )
  
  # invalid alpha
  expect_error(
    one_mean_test(rnorm(10), mu0 = 0, alpha = -0.05)
  )
  expect_error(
    one_mean_test(rnorm(10), mu0 = 0, alpha = 1.5)
  )
  
  # invalid conf.level
  expect_error(
    one_mean_test(rnorm(10), mu0 = 0, conf.level = 0)
  )
  expect_error(
    one_mean_test(rnorm(10), mu0 = 0, conf.level = 1)
  )
})

test_that("one_mean_test handles NA values with warning", {
  set.seed(222)
  x_with_na <- c(rnorm(15, mean = 3, sd = 1), NA, NA)
  
  expect_warning(
    result <- one_mean_test(x_with_na, mu0 = 3, check_assumptions = FALSE),
    "NA"
  )
  
  expect_equal(result$sample.stats$n, 15)
})

test_that("one_mean_test confidence intervals have correct coverage", {
  set.seed(333)
  n_sims <- 100
  coverage <- 0
  conf_level <- 0.95
  true_mu <- 5
  
  for (i in seq_len(n_sims)) {
    x <- rnorm(30, mean = true_mu, sd = 2)
    result <- one_mean_test(x, mu0 = true_mu, conf.level = conf_level,
                           check_assumptions = FALSE)
    if (result$conf.int[1] <= true_mu && result$conf.int[2] >= true_mu) {
      coverage <- coverage + 1
    }
  }
  
  coverage_rate <- coverage / n_sims
  # Allow some tolerance (binomial variability)
  expect_true(coverage_rate >= 0.90 && coverage_rate <= 1.0)
})

test_that("one_mean_test interpretation text is informative", {
  set.seed(444)
  x <- rnorm(30, mean = 7, sd = 2)
  
  result <- one_mean_test(x, mu0 = 5, alpha = 0.05, check_assumptions = FALSE)
  
  interp <- result$interpretation
  
  # Should mention key elements
  expect_true(grepl("alpha", interp))
  expect_true(grepl("0.050", interp))
  expect_true(grepl("mean", interp))
  expect_true(grepl("p-value", interp))
})
