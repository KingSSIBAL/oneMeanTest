# Internal helpers for one-sample t-test calculations (not exported)
#
# These functions compute the core quantities for the one-sample t-test:
# the t statistic, its p-value under different alternatives, and the
# confidence interval for the mean. They are used internally by
# \code{one_mean_test()} and are not exported.

.t_statistic_one_mean <- function(x, mu0) {
  n <- length(x)
  xbar <- mean(x)
  s <- stats::sd(x)
  se <- s / sqrt(n)
  t_stat <- (xbar - mu0) / se
  list(
    t = t_stat,
    mean = xbar,
    sd = s,
    se = se,
    n = n
  )
}

.t_pvalue_one_mean <- function(t_stat, df, alternative = c("two.sided", "less", "greater")) {
  alternative <- match.arg(alternative)
  if (alternative == "two.sided") {
    p <- 2 * stats::pt(abs(t_stat), df = df, lower.tail = FALSE)
  } else if (alternative == "greater") {
    p <- stats::pt(t_stat, df = df, lower.tail = FALSE)
  } else { # "less"
    p <- stats::pt(t_stat, df = df, lower.tail = TRUE)
  }
  p
}

.t_confint_one_mean <- function(mean, sd, n, conf.level = 0.95) {
  alpha <- 1 - conf.level
  df <- n - 1L
  se <- sd / sqrt(n)
  t_crit <- stats::qt(1 - alpha / 2, df = df)
  margin <- t_crit * se
  c(lower = mean - margin, upper = mean + margin)
}
