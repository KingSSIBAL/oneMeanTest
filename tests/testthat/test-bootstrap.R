test_that("bootstrap_ttest returns sensible results", {
  set.seed(303)
  x <- rnorm(50, mean = 5, sd = 2)

  res <- bootstrap_ttest(
    x,
    mu0 = 5,
    nboot = 200,          # smaller for speed
    conf.level = 0.95,
    alternative = "two.sided",
    seed = 123
  )

  # structure checks
  expect_s3_class(res, "oneMeanTest_bootstrap")
  expect_true(is.numeric(res$t.obs))
  expect_true(is.numeric(res$p.value))
  expect_true(res$p.value >= 0 && res$p.value <= 1)
  expect_length(res$t.boot, 200)
  expect_length(res$mean.boot, 200)

  # CI should be length 2, and contain the sample mean
  expect_length(res$conf.int, 2)
  expect_true(res$conf.int[1] <= res$mean.obs &&
              res$mean.obs <= res$conf.int[2])
})
