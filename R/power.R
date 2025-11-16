# Power analysis for one-sample t-test

#' Power analysis for one-sample t-test
#'
#' Wrapper around \code{\link[stats]{power.t.test}} for the one-sample t-test
#' on a population mean with unknown variance.
#'
#' You must supply exactly two of the three arguments \code{n}, \code{delta},
#' and \code{power}; the third will be computed, as in
#' \code{\link[stats]{power.t.test}}.
#'
#' This function is useful for:
#' \itemize{
#'   \item computing the power of a one-sample t-test for a given sample size,
#'         effect size (\code{delta}), and significance level; or
#'   \item computing the required sample size to achieve a desired power.
#' }
#'
#' @param n Sample size (or \code{NULL} if to be computed).
#' @param delta True difference in means (\eqn{\mu - \mu_0}).
#' @param sd Standard deviation (estimated or assumed).
#' @param sig.level Significance level (alpha).
#' @param power Desired power (or \code{NULL} if to be computed).
#' @param alternative Character string specifying the alternative hypothesis,
#'   either \code{"two.sided"} or \code{"one.sided"}.
#'
#' @return An object of class \code{"power.htest"} as returned by
#'   \code{\link[stats]{power.t.test}} with \code{type = "one.sample"}.
#'
#' @examples
#' # Power for n = 30, true difference = 1, sd = 2
#' p_res <- power_analysis_one_mean(
#'   n = 30,
#'   delta = 1,
#'   sd = 2,
#'   sig.level = 0.05,
#'   power = NULL,
#'   alternative = "two.sided"
#' )
#' p_res
#'
#' # Required n for 80% power
#' n_res <- power_analysis_one_mean(
#'   n = NULL,
#'   delta = 1,
#'   sd = 2,
#'   sig.level = 0.05,
#'   power = 0.8,
#'   alternative = "two.sided"
#' )
#' n_res
#'
#' @export
power_analysis_one_mean <- function(
    n = NULL,
    delta = NULL,
    sd = 1,
    sig.level = 0.05,
    power = NULL,
    alternative = c("two.sided", "one.sided")
) {
  alternative <- match.arg(alternative)
  res <- stats::power.t.test(
    n = n,
    delta = delta,
    sd = sd,
    sig.level = sig.level,
    power = power,
    type = "one.sample",
    alternative = alternative
  )
  res
}
