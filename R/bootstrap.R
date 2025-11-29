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
  # ============================================================
  # INPUT VALIDATION
  # ============================================================
  
  # Ensure x is numeric and non-empty
  .check_numeric_vector(x, "x")
  
  # Remove NA values with warning if present
  x <- .remove_na_with_warning(x, "x")
  
  # Validate confidence level
  .check_conf_level(conf.level)
  
  # Match and validate alternative hypothesis
  alternative <- .match_alternative(alternative)

  # Set random seed for reproducibility if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Validate number of bootstrap resamples
  # Require at least 100 for reasonable approximation of sampling distribution
  if (!is.numeric(nboot) || length(nboot) != 1L || nboot < 100) {
    stop("nboot must be a single integer >= 100.", call. = FALSE)
  }
  nboot <- as.integer(nboot)

  # ============================================================
  # CALCULATE OBSERVED STATISTICS
  # ============================================================
  
  # Compute the observed t-statistic from the original sample
  # This serves as the reference point for the bootstrap distribution
  core <- .t_statistic_one_mean(x, mu0 = mu0)
  t_obs <- core$t        # Observed t-statistic
  mean_obs <- core$mean  # Observed sample mean
  n <- core$n            # Sample size

  # ============================================================
  # BOOTSTRAP RESAMPLING LOOP
  # ============================================================
  
  # Pre-allocate vectors to store bootstrap results
  # This is more efficient than growing vectors in the loop
  t_boot <- numeric(nboot)     # Bootstrap t-statistics
  mean_boot <- numeric(nboot)  # Bootstrap sample means

  # Perform bootstrap resampling
  # Each iteration:
  # 1. Draw n observations from x WITH replacement (key bootstrap principle)
  # 2. Calculate t-statistic for the bootstrap sample
  # 3. Store results for later analysis
  for (b in seq_len(nboot)) {
    # Draw bootstrap sample: sample with replacement from original data
    # Size equals original sample size to preserve sample size variability
    xb <- sample(x, size = n, replace = TRUE)
    
    # Calculate statistics for this bootstrap sample
    n_b <- length(xb)
    mean_b <- mean(xb)              # Bootstrap sample mean
    sd_b <- stats::sd(xb)           # Bootstrap sample SD
    se_b <- sd_b / sqrt(n_b)        # Bootstrap standard error
    
    # Compute bootstrap t-statistic using same null hypothesis
    # t* = (x̄* - μ₀) / (s*/√n*)
    t_boot[b] <- (mean_b - mu0) / se_b
    
    # Store bootstrap mean for percentile CI calculation
    mean_boot[b] <- mean_b
  }

  # ============================================================
  # CALCULATE BOOTSTRAP P-VALUE
  # ============================================================
  
  # Compute p-value from empirical bootstrap distribution
  # The p-value is the proportion of bootstrap statistics
  # as extreme or more extreme than the observed statistic
  
  if (alternative == "two.sided") {
    # Two-tailed test: count |t*| >= |t_obs|
    # Proportion of bootstrap statistics with absolute value
    # greater than or equal to observed absolute value
    p_boot <- mean(abs(t_boot) >= abs(t_obs))
    
  } else if (alternative == "greater") {
    # Right-tailed test: count t* >= t_obs
    # Proportion of bootstrap statistics greater than observed
    p_boot <- mean(t_boot >= t_obs)
    
  } else {
    # Left-tailed test: count t* <= t_obs
    # Proportion of bootstrap statistics less than observed
    p_boot <- mean(t_boot <= t_obs)
  }

  # ============================================================
  # CALCULATE PERCENTILE CONFIDENCE INTERVAL
  # ============================================================
  
  # Construct confidence interval using percentile method
  # Take α/2 and 1-α/2 quantiles of bootstrap mean distribution
  # This is the bootstrap percentile interval, one of several bootstrap CI methods
  
  alpha <- 1 - conf.level
  ci_boot <- stats::quantile(
    mean_boot,
    probs = c(alpha / 2, 1 - alpha / 2),  # Lower and upper percentiles
    names = FALSE
  )
  names(ci_boot) <- c("lower", "upper")

  # ============================================================
  # BUILD AND RETURN RESULT OBJECT
  # ============================================================
  
  # Construct S3 object containing all bootstrap results
  res <- list(
    t.obs = t_obs,                                      # Observed t-statistic
    t.boot = t_boot,                                    # Bootstrap t-distribution
    mean.obs = mean_obs,                                # Observed mean
    mean.boot = mean_boot,                              # Bootstrap mean distribution
    conf.int = structure(ci_boot, conf.level = conf.level),  # Percentile CI
    p.value = p_boot,                                   # Bootstrap p-value
    nboot = nboot,                                      # Number of resamples
    mu0 = mu0,                                          # Null hypothesis value
    alternative = alternative,                          # Alternative hypothesis
    call = match.call()                                 # Function call for reference
  )
  
  # Assign S3 class for custom print/plot methods
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
