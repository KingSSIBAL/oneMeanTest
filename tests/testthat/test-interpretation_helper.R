# Tests for interpretation helper functions

test_that(".generate_interpretation works for reject H0 two-sided", {
  interp <- .generate_interpretation(
    decision = "reject H0",
    alternative = "two.sided",
    mu0 = 10,
    xbar = 15,
    ci = c(13, 17),
    alpha = 0.05,
    p_value = 0.001
  )
  
  expect_type(interp, "character")
  expect_true(nchar(interp) > 0)
  expect_true(grepl("sufficient", interp, ignore.case = TRUE))
  expect_true(grepl("reject", interp, ignore.case = TRUE))
  expect_true(grepl("15", interp))  # Sample mean mentioned
  expect_true(grepl("10", interp))  # Null value mentioned
})

test_that(".generate_interpretation works for reject H0 greater", {
  interp <- .generate_interpretation(
    decision = "reject H0",
    alternative = "greater",
    mu0 = 10,
    xbar = 15,
    ci = c(13, 17),
    alpha = 0.05,
    p_value = 0.01
  )
  
  expect_type(interp, "character")
  expect_true(grepl("greater", interp, ignore.case = TRUE))
  expect_true(grepl("sufficient", interp, ignore.case = TRUE))
  expect_true(grepl("reject", interp, ignore.case = TRUE))
})

test_that(".generate_interpretation works for reject H0 less", {
  interp <- .generate_interpretation(
    decision = "reject H0",
    alternative = "less",
    mu0 = 10,
    xbar = 5,
    ci = c(3, 7),
    alpha = 0.05,
    p_value = 0.02
  )
  
  expect_type(interp, "character")
  expect_true(grepl("less", interp, ignore.case = TRUE))
  expect_true(grepl("sufficient", interp, ignore.case = TRUE))
  expect_true(grepl("reject", interp, ignore.case = TRUE))
})

test_that(".generate_interpretation works for fail to reject two-sided", {
  interp <- .generate_interpretation(
    decision = "fail to reject H0",
    alternative = "two.sided",
    mu0 = 10,
    xbar = 10.5,
    ci = c(9, 12),
    alpha = 0.05,
    p_value = 0.3
  )
  
  expect_type(interp, "character")
  expect_true(grepl("insufficient", interp, ignore.case = TRUE))
  expect_true(grepl("not significantly", interp, ignore.case = TRUE) ||
              grepl("consistent with", interp, ignore.case = TRUE))
})

test_that(".generate_interpretation works for fail to reject greater", {
  interp <- .generate_interpretation(
    decision = "fail to reject H0",
    alternative = "greater",
    mu0 = 10,
    xbar = 10.5,
    ci = c(9, 12),
    alpha = 0.05,
    p_value = 0.25
  )
  
  expect_type(interp, "character")
  expect_true(grepl("insufficient", interp, ignore.case = TRUE))
  expect_true(grepl("greater", interp, ignore.case = TRUE))
  expect_true(grepl("does not provide", interp, ignore.case = TRUE))
})

test_that(".generate_interpretation works for fail to reject less", {
  interp <- .generate_interpretation(
    decision = "fail to reject H0",
    alternative = "less",
    mu0 = 10,
    xbar = 9.5,
    ci = c(8, 11),
    alpha = 0.05,
    p_value = 0.20
  )
  
  expect_type(interp, "character")
  expect_true(grepl("insufficient", interp, ignore.case = TRUE))
  expect_true(grepl("less", interp, ignore.case = TRUE))
  expect_true(grepl("does not provide", interp, ignore.case = TRUE))
})

test_that(".format_alternative_text works for all alternatives", {
  expect_equal(.format_alternative_text("two.sided"), "not equal to")
  expect_equal(.format_alternative_text("greater"), "greater than")
  expect_equal(.format_alternative_text("less"), "less than")
})

test_that(".generate_short_interpretation for reject", {
  short <- .generate_short_interpretation("reject H0", 0.01, 0.05)
  
  expect_type(short, "character")
  expect_true(grepl("reject", short, ignore.case = TRUE))
  expect_true(grepl("0.01", short))
})

test_that(".generate_short_interpretation for fail to reject", {
  short <- .generate_short_interpretation("fail to reject H0", 0.20, 0.05)
  
  expect_type(short, "character")
  expect_true(grepl("Not statistically significant", short, ignore.case = TRUE))
  expect_true(grepl("0.20", short))
})

test_that(".generate_interpretation includes alpha and p-value", {
  interp <- .generate_interpretation(
    decision = "reject H0",
    alternative = "two.sided",
    mu0 = 5,
    xbar = 8,
    ci = c(6, 10),
    alpha = 0.01,
    p_value = 0.005
  )
  
  expect_true(grepl("0.01", interp))
  expect_true(grepl("0.005", interp))
})

test_that(".generate_interpretation mentions sample mean and null value", {
  interp <- .generate_interpretation(
    decision = "fail to reject H0",
    alternative = "greater",
    mu0 = 100,
    xbar = 102,
    ci = c(95, 109),
    alpha = 0.05,
    p_value = 0.15
  )
  
  expect_true(grepl("100", interp))
  expect_true(grepl("102", interp))
})
