#' One-sample t-test for a population mean (unknown variance)
#'
#' Perform a one-sample t-test on a population mean when the population
#' variance is unknown, using the Student's t-distribution. The function
#' returns the usual test statistic, p-value, confidence interval, and a
#' plain-language interpretation, and can optionally run assumption checks.
#'
#' @param x A numeric vector of observations.
#' @param mu0 The hypothesized population mean under the null hypothesis.
#' @param alternative Character string specifying the alternative hypothesis,
#'   one of \code{"two.sided"}, \code{"less"}, or \code{"greater"}.
#' @param alpha Significance level used for the hypothesis decision.
#' @param conf.level Confidence level for the confidence interval.
#' @param check_assumptions Logical; if \code{TRUE}, performs assumption checks
#'   via \code{\link{check_assumptions}} and stores the result in the returned
#'   object.
#'
#' @return An object of class \code{"oneMeanTest"} containing:
#'   \item{statistic}{Named numeric vector with the t statistic.}
#'   \item{parameter}{Named numeric vector with the degrees of freedom.}
#'   \item{p.value}{P-value of the test.}
#'   \item{conf.int}{Numeric vector with the confidence interval for the mean,
#'     with attribute \code{conf.level}.}
#'   \item{estimate}{Named numeric vector with the sample mean.}
#'   \item{null.value}{Named numeric vector with the hypothesized mean (\code{mu0}).}
#'   \item{alternative}{Character string with the alternative hypothesis.}
#'   \item{t.critical}{Named vector with critical value(s) for the test.}
#'   \item{method}{Description of the method used.}
#'   \item{data.name}{Character string with the name of the data vector.}
#'   \item{sample.stats}{List with \code{n}, \code{mean}, \code{sd}, and \code{se}.}
#'   \item{alpha}{Significance level used.}
#'   \item{assumptions}{Result from \code{check_assumptions()} or \code{NULL}.}
#'   \item{decision}{Character string: \code{"reject H0"} or
#'     \code{"fail to reject H0"}.}
#'   \item{interpretation}{Plain-language interpretation of the result.}
#'
#' @examples
#' set.seed(123)
#' x <- rnorm(30, mean = 5, sd = 2)
#'
#' # Basic two-sided test with default alpha and conf.level
#' res <- one_mean_test(x, mu0 = 5)
#' res
#'
#' # One-sided alternative and different confidence level
#' res_less <- one_mean_test(x, mu0 = 5, alternative = "less",
#'                           alpha = 0.01, conf.level = 0.99)
#' res_less
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
  # ============================================================
  # INPUT VALIDATION AND PREPARATION
  # ============================================================
  
  # Ensure x is numeric and non-empty
  .check_numeric_vector(x, "x")
  
  # Remove NA values with warning if any are present
  x <- .remove_na_with_warning(x, "x")
  
  # Validate alpha is between 0 and 1
  .check_alpha(alpha)
  
  # Validate confidence level is between 0 and 1
  .check_conf_level(conf.level)
  
  # Match and validate alternative hypothesis argument
  alternative <- .match_alternative(alternative)

  # Capture the name of the data for reporting
  data_name <- deparse(substitute(x))

  # ============================================================
  # CALCULATE TEST STATISTIC
  # ============================================================
  
  # Compute t-statistic using formula: t = (x̄ - μ₀) / (s/√n)
  # Also returns sample mean, standard deviation, and standard error
  core <- .t_statistic_one_mean(x, mu0 = mu0)
  t_stat <- core$t      # The calculated t-statistic
  n <- core$n           # Sample size (after NA removal)
  df <- n - 1L          # Degrees of freedom (n - 1 for one-sample t-test)

  # ============================================================
  # CALCULATE P-VALUE
  # ============================================================
  
  # Compute p-value based on the alternative hypothesis:
  # - two.sided: P(|T| > |t_obs|) = 2 * P(T > |t_obs|)
  # - greater: P(T > t_obs)
  # - less: P(T < t_obs)
  p_val <- .t_pvalue_one_mean(t_stat, df = df, alternative = alternative)
  
  # ============================================================
  # CONSTRUCT CONFIDENCE INTERVAL
  # ============================================================
  
  # Calculate CI using formula: x̄ ± t_(α/2, df) * SE
  # where t_(α/2, df) is the critical value from t-distribution
  ci <- .t_confint_one_mean(core$mean, core$sd, n, conf.level = conf.level)

  # ============================================================
  # MAKE STATISTICAL DECISION
  # ============================================================
  
  # Decision rule: Reject H0 if p-value ≤ α
  # This is the classical Neyman-Pearson approach
  decision <- if (p_val <= alpha) "reject H0" else "fail to reject H0"

  # ============================================================
  # GENERATE INTERPRETATION TEXT
  # ============================================================
  
  # Create human-readable text describing the direction of the test
  alt_text <- switch(
    alternative,
    "two.sided" = "different from",
    "greater"   = "greater than",
    "less"      = "less than"
  )
  
  # Build plain-language interpretation of the test results
  # Includes: decision, sample mean, hypothesized mean, test statistic, p-value
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
  
  # ============================================================
  # CALCULATE CRITICAL VALUES
  # ============================================================
  
  # Determine the critical value(s) from t-distribution
  # Used to define the rejection region for the test
  # - Two-sided: ±t_(α/2, df)
  # - One-sided: t_(α, df) with appropriate sign
  t_critical <- .calculate_t_critical(alternative, alpha, df)

  # ============================================================
  # RUN ASSUMPTION CHECKS (OPTIONAL)
  # ============================================================
  
  # Perform diagnostic checks if requested:
  # - Shapiro-Wilk test for normality
  # - Outlier detection using IQR rule
  assumptions_res <- NULL
  if (isTRUE(check_assumptions)) {
    assumptions_res <- check_assumptions(x, alpha = alpha, verbose = FALSE)
  }

  # ============================================================
  # BUILD AND RETURN RESULT OBJECT
  # ============================================================
  
  # Construct S3 object with all test components
  # This follows the structure of stats::t.test() for compatibility
  result <- list(
    statistic = c(t = t_stat),                          # Test statistic
    parameter = c(df = df),                             # Degrees of freedom
    p.value = p_val,                                    # P-value
    conf.int = structure(ci, conf.level = conf.level),  # Confidence interval
    estimate = c(mean = core$mean),                     # Sample mean
    null.value = c(mu = mu0),                           # Hypothesized mean
    alternative = alternative,                          # Alternative hypothesis
    t.critical = t_critical,                            # Critical value(s)
    method = "One-sample t-test for a population mean (unknown variance)",
    data.name = data_name,                              # Original data name
    sample.stats = list(                                # Summary statistics
      n = n,
      mean = core$mean,
      sd = core$sd,
      se = core$se
    ),
    alpha = alpha,                                      # Significance level
    assumptions = assumptions_res,                      # Assumption check results
    decision = decision,                                # Statistical decision
    interpretation = interp                             # Plain-language summary
  )

  # Assign S3 class for custom print/plot methods
  class(result) <- "oneMeanTest"
  
  result
}
