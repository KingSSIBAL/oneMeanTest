# Bootstrap-based inference for one-sample mean

#' Bootstrap one-sample t-test
#'
#' Perform a simple non-parametric bootstrap procedure for the one-sample
#' t-test on a population mean with unknown variance.
#'
#' The function resamples the observed data with replacement, computes the
#' t-statistic for each bootstrap sample, and uses the empirical distribution
#' of these statistics to obtain a bootstrap p-value and a percentile
#' confidence interval for the mean.
#'
#' This is useful for illustrating resampling-based inference and for
#' comparing classical t-test results with bootstrap results.
#'
#' @param x Numeric vector of observations.
#' @param mu0 Hypothesized population mean under the null hypothesis.
#' @param nboot Number of bootstrap resamples (integer, usually at least 100).
#' @param conf.level Confidence level for the bootstrap confidence interval.
#' @param alternative Character string specifying the alternative hypothesis,
#'   one of \code{"two.sided"}, \code{"less"}, or \code{"greater"}.
#' @param seed Optional integer seed for reproducibility.
#'
#' @return A list of class \code{"oneMeanTest_bootstrap"} with components:
#'   \item{t.obs}{Observed t-statistic.}
#'   \item{t.boot}{Numeric vector of bootstrap t-statistics.}
#'   \item{mean.obs}{Observed sample mean.}
#'   \item{mean.boot}{Numeric vector of bootstrap means.}
#'   \item{conf.int}{Bootstrap percentile confidence interval for the mean
#'     (numeric vector of length 2 with attribute \code{conf.level}).}
#'   \item{p.value}{Bootstrap p-value.}
#'   \item{nboot}{Number of bootstrap resamples.}
#'   \item{mu0}{Hypothesized mean.}
#'   \item{alternative}{Alternative hypothesis used.}
#'   \item{call}{The matched function call.}
#'
#' @examples
#' set.seed(123)
#' x <- rnorm(30, mean = 5, sd = 2)
#'
#' boot_res <- bootstrap_ttest(
#'   x,
#'   mu0 = 5,
#'   nboot = 500,
#'   conf.level = 0.95,
#'   alternative = "two.sided",
#'   seed = 123
#' )
#' boot_res
#'
#' @export
bootstrap_ttest <- function(
    x,
    mu0 = 0,
    nboot = 1000,
    conf.level = 0.95,
    alternative = c("two.sided", "less", "greater"),
    seed = NULL
) {
  .check_numeric_vector(x, "x")
  x <- .remove_na_with_warning(x, "x")
  .check_conf_level(conf.level)
  alternative <- .match_alternative(alternative)

  if (!is.null(seed)) {
    set.seed(seed)
  }
  if (!is.numeric(nboot) || length(nboot) != 1L || nboot < 100) {
    stop("nboot must be a single integer >= 100.", call. = FALSE)
  }
  nboot <- as.integer(nboot)

  # Observed statistics
  core <- .t_statistic_one_mean(x, mu0 = mu0)
  t_obs <- core$t
  mean_obs <- core$mean
  n <- core$n

  # Bootstrap
  t_boot <- numeric(nboot)
  mean_boot <- numeric(nboot)

  for (b in seq_len(nboot)) {
    xb <- sample(x, size = n, replace = TRUE)
    n_b <- length(xb)
    mean_b <- mean(xb)
    sd_b <- stats::sd(xb)
    se_b <- sd_b / sqrt(n_b)
    t_boot[b] <- (mean_b - mu0) / se_b
    mean_boot[b] <- mean_b
  }

  # Bootstrap p-value based on empirical t distribution
  if (alternative == "two.sided") {
    p_boot <- mean(abs(t_boot) >= abs(t_obs))
  } else if (alternative == "greater") {
    p_boot <- mean(t_boot >= t_obs)
  } else {
    p_boot <- mean(t_boot <= t_obs)
  }

  # Percentile CI for the mean
  alpha <- 1 - conf.level
  ci_boot <- stats::quantile(
    mean_boot,
    probs = c(alpha / 2, 1 - alpha / 2),
    names = FALSE
  )
  names(ci_boot) <- c("lower", "upper")

  res <- list(
    t.obs = t_obs,
    t.boot = t_boot,
    mean.obs = mean_obs,
    mean.boot = mean_boot,
    conf.int = structure(ci_boot, conf.level = conf.level),
    p.value = p_boot,
    nboot = nboot,
    mu0 = mu0,
    alternative = alternative,
    call = match.call()
  )
  class(res) <- "oneMeanTest_bootstrap"
  res
}

#' Print method for bootstrap one-sample t-test
#'
#' Pretty printing for objects of class \code{"oneMeanTest_bootstrap"} produced
#' by \code{\link{bootstrap_ttest}}.
#'
#' @param x An object of class \code{"oneMeanTest_bootstrap"}.
#' @param digits Number of digits to use for printing numeric values.
#' @param ... Unused; included for method compatibility.
#'
#' @return Invisibly returns \code{x}.
#'
#' @export
print.oneMeanTest_bootstrap <- function(x, digits = 4, ...) {
  cat("Bootstrap one-sample t-test\n\n")
  cat("Call:\n")
  print(x$call)
  cat("\n")

  cat(sprintf("Observed t-statistic: %.*f\n", digits, x$t.obs))
  cat(sprintf("Bootstrap p-value  : %.*g\n", digits, x$p.value))
  ci <- x$conf.int
  cl <- attr(ci, "conf.level")
  cat(sprintf("%.*f%% bootstrap CI for the mean:\n", digits, cl * 100))
  cat(sprintf("  [%.4f, %.4f]\n", ci[1L], ci[2L]))
  cat(sprintf("Number of resamples: %d\n", x$nboot))

  invisible(x)
}
