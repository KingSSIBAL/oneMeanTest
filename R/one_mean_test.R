#' One-sample t-test for a population mean (unknown variance)
#'
#' Perform a one-sample t-test on a population mean when the population
#' variance is unknown, using the Student's t-distribution.
#'
#' @param x A numeric vector of observations.
#' @param mu0 The hypothesized population mean under the null hypothesis.
#' @param alternative Character string specifying the alternative hypothesis,
#'   one of \code{"two.sided"}, \code{"less"}, or \code{"greater"}.
#' @param alpha Significance level used for the hypothesis decision.
#' @param conf.level Confidence level for the confidence interval.
#' @param check_assumptions Logical; if TRUE, performs assumption checks.
#'
#' @return An object of class \code{"oneMeanTest"} containing:
#'   \item{statistic}{The t statistic.}
#'   \item{parameter}{Degrees of freedom.}
#'   \item{p.value}{P-value of the test.}
#'   \item{conf.int}{Numeric vector with confidence interval.}
#'   \item{estimate}{Sample mean.}
#'   \item{null.value}{Hypothesized mean (mu0).}
#'   \item{alternative}{The alternative hypothesis.}
#'   \item{method}{Description of the method.}
#'   \item{data.name}{Name of the data vector.}
#'   \item{sample.stats}{List with n, mean, sd, and se.}
#'   \item{alpha}{Significance level used.}
#'   \item{assumptions}{Result from \code{check_assumptions()} or \code{NULL}.}
#'   \item{decision}{Character string: "reject H0" or "fail to reject H0".}
#'   \item{interpretation}{Plain-language interpretation of the result.}
#'
#' @examples
#' x <- rnorm(30, mean = 5, sd = 2)
#' res <- one_mean_test(x, mu0 = 5)
#' res
#'
#' @export
one_mean_test <- function(
    x,
    mu0 = 0,
    alternative = c("two.sided", "less", "greater"),
    alpha = 0.05,
    conf.level = 0.95,
    check_assumptions = TRUE
) {
  .check_numeric_vector(x, "x")
  x <- .remove_na_with_warning(x, "x")
  .check_alpha(alpha)
  .check_conf_level(conf.level)
  alternative <- .match_alternative(alternative)

  data_name <- deparse(substitute(x))

  # Core statistics
  core <- .t_statistic_one_mean(x, mu0 = mu0)
  t_stat <- core$t
  n <- core$n
  df <- n - 1L

  # p-value & confidence interval
  p_val <- .t_pvalue_one_mean(t_stat, df = df, alternative = alternative)
  ci <- .t_confint_one_mean(core$mean, core$sd, n, conf.level = conf.level)

  # Decision rule
  decision <- if (p_val <= alpha) "reject H0" else "fail to reject H0"

  # Plain-language interpretation
  alt_text <- switch(
    alternative,
    "two.sided" = "different from",
    "greater"   = "greater than",
    "less"      = "less than"
  )
  interp <- sprintf(
    "At alpha = %.3f, we %s the null hypothesis that the population mean equals %.3f. The sample mean (%.3f) is %s %.3f (t = %.3f, df = %d, p-value = %.4f).",
    alpha,
    if (decision == "reject H0") "reject" else "fail to reject",
    mu0,
    core$mean,
    alt_text,
    mu0,
    t_stat,
    df,
    p_val
  )

  # Optional assumption checks
  assumptions_res <- NULL
  if (isTRUE(check_assumptions)) {
    assumptions_res <- check_assumptions(x, alpha = alpha, verbose = FALSE)
  }

  result <- list(
    statistic = c(t = t_stat),
    parameter = c(df = df),
    p.value = p_val,
    conf.int = structure(ci, conf.level = conf.level),
    estimate = c(mean = core$mean),
    null.value = c(mu = mu0),
    alternative = alternative,
    method = "One-sample t-test for a population mean (unknown variance)",
    data.name = data_name,
    sample.stats = list(
      n = n,
      mean = core$mean,
      sd = core$sd,
      se = core$se
    ),
    alpha = alpha,
    assumptions = assumptions_res,
    decision = decision,
    interpretation = interp
  )

  class(result) <- "oneMeanTest"
  result
}
