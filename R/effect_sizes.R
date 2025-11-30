#' Effect Size Calculations for One-Sample T-Test
#'
#' Functions to calculate and interpret various effect size measures
#' including Cohen's d, Hedges' g, Glass's delta, and confidence intervals
#' for effect sizes.
#'
#' @name effect_sizes
NULL

#' Calculate Cohen's d with confidence interval
#'
#' Computes Cohen's d effect size with bootstrap confidence interval
#'
#' @param x Numeric vector of observations
#' @param mu0 Null hypothesis value (default 0)
#' @param conf.level Confidence level for CI (default 0.95)
#' @param method Method for CI: "bootstrap" or "nct" (noncentral t)
#' @param nboot Number of bootstrap samples (if method = "bootstrap")
#' @return List with d, CI, and interpretation
#' @export
#' @examples
#' x <- rnorm(30, mean = 5, sd = 2)
#' cohens_d_ci(x, mu0 = 0)
cohens_d_ci <- function(x, mu0 = 0, conf.level = 0.95, 
                        method = c("bootstrap", "nct"), nboot = 1000) {
  method <- match.arg(method)
  
  x <- x[!is.na(x)]
  n <- length(x)
  
  # Calculate point estimate
  d <- cohens_d(x, mu0)
  
  if (method == "bootstrap") {
    # Bootstrap CI
    boot_d <- replicate(nboot, {
      x_boot <- sample(x, n, replace = TRUE)
      cohens_d(x_boot, mu0)
    })
    
    ci <- stats::quantile(boot_d, probs = c((1 - conf.level)/2, 
                                              1 - (1 - conf.level)/2))
  } else {
    # Noncentral t CI (more accurate for small samples)
    t_stat <- (mean(x) - mu0) / (stats::sd(x) / sqrt(n))
    df <- n - 1
    
    # Confidence limits for noncentrality parameter
    alpha <- 1 - conf.level
    lambda_lower <- .ncp_lower(t_stat, df, alpha/2)
    lambda_upper <- .ncp_upper(t_stat, df, alpha/2)
    
    # Convert to Cohen's d
    ci <- c(lambda_lower / sqrt(n), lambda_upper / sqrt(n))
  }
  
  result <- list(
    d = d,
    ci = ci,
    conf.level = conf.level,
    interpretation = interpret_effect_size(d),
    method = method
  )
  
  class(result) <- "effect_size"
  result
}

#' Calculate Hedges' g (bias-corrected Cohen's d)
#'
#' Hedges' g corrects for small sample bias in Cohen's d
#'
#' @param x Numeric vector of observations
#' @param mu0 Null hypothesis value (default 0)
#' @param conf.level Confidence level for CI (default 0.95)
#' @return List with g, CI, and interpretation
#' @export
#' @examples
#' x <- rnorm(20, mean = 5, sd = 2)
#' hedges_g(x, mu0 = 0)
hedges_g <- function(x, mu0 = 0, conf.level = 0.95) {
  x <- x[!is.na(x)]
  n <- length(x)
  
  # Calculate Cohen's d
  d <- cohens_d(x, mu0)
  
  # Correction factor (J)
  # J = 1 - 3/(4*df - 1)
  df <- n - 1
  J <- 1 - (3 / (4 * df - 1))
  
  # Hedges' g = J * d
  g <- J * d
  
  # Approximate CI (using same factor for CI)
  d_result <- cohens_d_ci(x, mu0, conf.level, method = "nct")
  ci <- d_result$ci * J
  
  result <- list(
    g = g,
    d = d,
    correction_factor = J,
    ci = ci,
    conf.level = conf.level,
    interpretation = interpret_effect_size(g),
    note = "Hedges' g corrects Cohen's d for small sample bias"
  )
  
  class(result) <- "effect_size"
  result
}

#' Calculate Glass's delta
#'
#' Glass's delta uses the population standard deviation (if known)
#' or treats mu0 as the control group mean
#'
#' @param x Numeric vector of observations
#' @param mu0 Null hypothesis value
#' @param sigma Population standard deviation (if known)
#' @return Numeric Glass's delta value
#' @export
glass_delta <- function(x, mu0, sigma = NULL) {
  x <- x[!is.na(x)]
  xbar <- mean(x)
  
  if (is.null(sigma)) {
    warning("Population SD unknown. Using sample SD as approximation.", 
            call. = FALSE)
    sigma <- stats::sd(x)
  }
  
  if (sigma == 0) {
    warning("Standard deviation is zero. Effect size is undefined.", 
            call. = FALSE)
    return(NA_real_)
  }
  
  (xbar - mu0) / sigma
}

#' Calculate multiple effect sizes
#'
#' Computes Cohen's d, Hedges' g, and provides comprehensive interpretation
#'
#' @param x Numeric vector of observations
#' @param mu0 Null hypothesis value
#' @param conf.level Confidence level for CIs
#' @return Data frame with multiple effect size measures
#' @export
#' @examples
#' x <- rnorm(30, mean = 5, sd = 2)
#' effect_size_summary(x, mu0 = 0)
effect_size_summary <- function(x, mu0 = 0, conf.level = 0.95) {
  x <- x[!is.na(x)]
  
  # Cohen's d
  d_result <- cohens_d_ci(x, mu0, conf.level, method = "nct")
  
  # Hedges' g
  g_result <- hedges_g(x, mu0, conf.level)
  
  # Create summary data frame
  summary_df <- data.frame(
    Measure = c("Cohen's d", "Hedges' g"),
    Estimate = c(d_result$d, g_result$g),
    CI_Lower = c(d_result$ci[1], g_result$ci[1]),
    CI_Upper = c(d_result$ci[2], g_result$ci[2]),
    Interpretation = c(d_result$interpretation, g_result$interpretation),
    stringsAsFactors = FALSE
  )
  
  attr(summary_df, "conf.level") <- conf.level
  attr(summary_df, "n") <- length(x)
  attr(summary_df, "mu0") <- mu0
  
  class(summary_df) <- c("effect_size_summary", "data.frame")
  summary_df
}

#' Print method for effect size objects
#'
#' @param x Effect size object
#' @param digits Number of decimal places (default 4)
#' @param ... Additional arguments
#' @export
print.effect_size <- function(x, digits = 4, ...) {
  cat("\nEffect Size Calculation\n")
  cat(strrep("=", 50), "\n\n")
  
  if (!is.null(x$g)) {
    cat(sprintf("Hedges' g = %.4f\n", x$g))
    cat(sprintf("Cohen's d = %.4f (uncorrected)\n", x$d))
    cat(sprintf("Correction factor (J) = %.4f\n\n", x$correction_factor))
  } else {
    cat(sprintf("Cohen's d = %.4f\n\n", x$d))
  }
  
  cat(sprintf("%.0f%% Confidence Interval:\n", x$conf.level * 100))
  cat(sprintf("  [%.4f, %.4f]\n\n", x$ci[1], x$ci[2]))
  
  cat(sprintf("Interpretation: %s\n", x$interpretation))
  
  if (!is.null(x$note)) {
    cat(sprintf("\nNote: %s\n", x$note))
  }
  
  invisible(x)
}

#' Print method for effect size summary
#'
#' @param x Effect size summary object
#' @param digits Number of decimal places (default 4)
#' @param ... Additional arguments
#' @export
print.effect_size_summary <- function(x, digits = 4, ...) {
  cat("\nEffect Size Summary\n")
  cat(strrep("=", 70), "\n\n")
  
  cat(sprintf("Sample size: n = %d\n", attr(x, "n")))
  cat(sprintf("Null value: mu0 = %.4f\n", attr(x, "mu0")))
  cat(sprintf("Confidence level: %.0f%%\n\n", attr(x, "conf.level") * 100))
  
  print(as.data.frame(x), row.names = FALSE, digits = digits)
  
  invisible(x)
}

# Helper functions for noncentral t CI
.ncp_lower <- function(t, df, alpha) {
  if (t <= 0) return(-Inf)
  
  f <- function(ncp) stats::pt(t, df, ncp = ncp) - (1 - alpha)
  
  suppressWarnings(
    tryCatch(
      stats::uniroot(f, c(0, t * 5), tol = 0.001)$root,
      error = function(e) 0
    )
  )
}

.ncp_upper <- function(t, df, alpha) {
  if (t <= 0) return(0)
  
  f <- function(ncp) stats::pt(t, df, ncp = ncp) - alpha
  
  suppressWarnings(
    tryCatch(
      stats::uniroot(f, c(0, t * 5), tol = 0.001)$root,
      error = function(e) t * 2
    )
  )
}

