# Internal helpers for one-sample t-test calculations (not exported)
#
# These functions compute the core quantities for the one-sample t-test:
# the t statistic, its p-value under different alternatives, and the
# confidence interval for the mean. They are used internally by
# \code{one_mean_test()} and are not exported.

#' Calculate t-statistic and related statistics
#' 
#' Computes the one-sample t-statistic: t = (x̄ - μ₀) / (s / √n)
#' 
#' @param x Numeric vector of observations
#' @param mu0 Hypothesized population mean
#' @return List with t-statistic, sample statistics
#' @keywords internal
#' @noRd
.t_statistic_one_mean <- function(x, mu0) {
  # Sample size
  n <- length(x)
  
  # Sample mean: x̄ = (Σx) / n
  xbar <- mean(x)
  
  # Sample standard deviation: s = √[Σ(x - x̄)² / (n-1)]
  s <- stats::sd(x)
  
  # Standard error of the mean: SE = s / √n
  se <- s / sqrt(n)
  
  # t-statistic: t = (x̄ - μ₀) / SE
  # Measures how many standard errors the sample mean is from μ₀
  t_stat <- (xbar - mu0) / se
  
  # Return all computed statistics
  list(
    t = t_stat,
    mean = xbar,
    sd = s,
    se = se,
    n = n
  )
}

#' Calculate p-value for t-test
#' 
#' Computes the p-value based on the t-statistic and alternative hypothesis
#' 
#' @param t_stat Test statistic value
#' @param df Degrees of freedom (n - 1)
#' @param alternative Type of alternative hypothesis
#' @return P-value
#' @keywords internal
#' @noRd
.t_pvalue_one_mean <- function(t_stat, df, alternative = c("two.sided", "less", "greater")) {
  alternative <- match.arg(alternative)
  
  if (alternative == "two.sided") {
    # Two-tailed: P(|T| > |t_obs|) = 2 * P(T > |t_obs|)
    # BUILT-IN (commented out):
    # p <- 2 * stats::pt(abs(t_stat), df = df, lower.tail = FALSE)
    
    # CUSTOM IMPLEMENTATION:
    p <- 2 * .custom_pt(abs(t_stat), df = df, lower.tail = FALSE)
    
  } else if (alternative == "greater") {
    # Right-tailed: P(T > t_obs)
    # BUILT-IN (commented out):
    # p <- stats::pt(t_stat, df = df, lower.tail = FALSE)
    
    # CUSTOM IMPLEMENTATION:
    p <- .custom_pt(t_stat, df = df, lower.tail = FALSE)
    
  } else { # "less"
    # Left-tailed: P(T < t_obs)
    # BUILT-IN (commented out):
    # p <- stats::pt(t_stat, df = df, lower.tail = TRUE)
    
    # CUSTOM IMPLEMENTATION:
    p <- .custom_pt(t_stat, df = df, lower.tail = TRUE)
  }
  
  p
}

#' Calculate confidence interval for the mean
#' 
#' Computes CI using: x̄ ± t_(α/2, n-1) * SE
#' 
#' @param mean Sample mean
#' @param sd Sample standard deviation
#' @param n Sample size
#' @param conf.level Confidence level (e.g., 0.95 for 95%)
#' @return Named vector with lower and upper bounds
#' @keywords internal
#' @noRd
.t_confint_one_mean <- function(mean, sd, n, conf.level = 0.95) {
  # Significance level: α = 1 - confidence level
  alpha <- 1 - conf.level
  
  # Degrees of freedom
  df <- n - 1L
  
  # Standard error
  se <- sd / sqrt(n)
  
  # Critical value from t-distribution (two-tailed)
  # BUILT-IN (commented out):
  # t_crit <- stats::qt(1 - alpha / 2, df = df)
  
  # CUSTOM IMPLEMENTATION:
  t_crit <- .custom_qt(1 - alpha / 2, df = df)
  
  # Margin of error: ME = t_crit * SE
  margin <- t_crit * se
  
  # Confidence interval: (x̄ - ME, x̄ + ME)
  c(lower = mean - margin, upper = mean + margin)
}
