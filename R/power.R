# Power analysis for one-sample t-test

#' Power analysis for one-sample t-test
#'
#' Wrapper around \code{\link[stats]{power.t.test}} for the one-sample t-test
#' on a population mean with unknown variance.
#'
#' You must supply exactly two of the three arguments \code{n}, \code{delta},
#' and \code{power}; the third will be computed.
#'
#' @param n Sample size (or \code{NULL} if to be computed).
#' @param delta True difference in means (\eqn{\mu - \mu_0}).
#' @param sd Standard deviation (estimated or assumed).
#' @param sig.level Significance level (alpha).
#' @param power Desired power (or \code{NULL} if to be computed).
#' @param alternative \code{"two.sided"} or \code{"one.sided"}.
#'
#' @return The result of \code{stats::power.t.test()} with \code{type = "one.sample"}.
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
