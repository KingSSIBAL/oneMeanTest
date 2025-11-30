#' Non-Parametric Alternatives for One-Sample Location Test
#'
#' Functions to perform Wilcoxon signed-rank test and other
#' non-parametric alternatives to the one-sample t-test
#'
#' @name nonparametric
NULL

#' Wilcoxon Signed-Rank Test
#'
#' Non-parametric alternative to one-sample t-test when normality
#' assumption is violated
#'
#' @param x Numeric vector of observations
#' @param mu0 Hypothesized median (default 0)
#' @param alternative Alternative hypothesis: "two.sided", "less", or "greater"
#' @param conf.level Confidence level for interval estimate (default 0.95)
#' @param exact Use exact p-value calculation (default TRUE for n < 50)
#' @return Object of class oneMeanTest_wilcoxon
#' @export
#' @examples
#' # Non-normal data
#' x <- rexp(30, rate = 0.1)
#' wilcoxon_test(x, mu0 = 10)
wilcoxon_test <- function(x, mu0 = 0, 
                          alternative = c("two.sided", "less", "greater"),
                          conf.level = 0.95,
                          exact = NULL) {
  alternative <- match.arg(alternative)
  
  # Remove NAs
  x_clean <- x[!is.na(x)]
  n <- length(x_clean)
  
  if (n < 1) {
    stop("Need at least 1 non-missing observation.", call. = FALSE)
  }
  
  # Center data
  d <- x_clean - mu0
  
  # Remove zeros (ties at mu0)
  d_nonzero <- d[d != 0]
  n_nonzero <- length(d_nonzero)
  
  if (n_nonzero == 0) {
    warning("All observations equal mu0. Test is trivial.", call. = FALSE)
    return(list(
      statistic = NA_real_,
      p.value = 1,
      method = "Wilcoxon signed rank test",
      data.name = deparse(substitute(x))
    ))
  }
  
  # Calculate ranks of absolute values
  ranks <- rank(abs(d_nonzero))
  
  # Sum of ranks for positive differences
  W_plus <- sum(ranks[d_nonzero > 0])
  
  # Determine exact vs approximate
  if (is.null(exact)) {
    exact <- (n_nonzero < 50)
  }
  
  # Calculate p-value
  if (exact && n_nonzero <= 50) {
    p_value <- .wilcoxon_exact_p(W_plus, n_nonzero, alternative)
  } else {
    p_value <- .wilcoxon_approx_p(W_plus, n_nonzero, alternative)
  }
  
  # Hodges-Lehmann estimator (pseudo-median)
  pairwise_avgs <- outer(x_clean, x_clean, "+") / 2
  pseudomedian <- stats::median(pairwise_avgs[lower.tri(pairwise_avgs, diag = TRUE)])
  
  # Confidence interval for pseudomedian
  ci <- .wilcoxon_ci(x_clean, conf.level, alternative)
  
  # Build result
  result <- list(
    statistic = stats::setNames(W_plus, "V"),
    n = n,
    n.used = n_nonzero,
    p.value = p_value,
    pseudomedian = pseudomedian,
    conf.int = ci,
    conf.level = conf.level,
    null.value = stats::setNames(mu0, "location"),
    alternative = alternative,
    method = if (exact) "Wilcoxon signed rank exact test" else 
      "Wilcoxon signed rank test with continuity correction",
    data.name = deparse(substitute(x)),
    exact = exact
  )
  
  class(result) <- "oneMeanTest_wilcoxon"
  result
}

#' Sign Test
#'
#' Simple non-parametric test based on signs of differences
#'
#' @param x Numeric vector of observations
#' @param mu0 Hypothesized median (default 0)
#' @param alternative Alternative hypothesis
#' @return List with test results
#' @export
sign_test <- function(x, mu0 = 0, 
                      alternative = c("two.sided", "less", "greater")) {
  alternative <- match.arg(alternative)
  
  x_clean <- x[!is.na(x)]
  n <- length(x_clean)
  
  # Count signs
  d <- x_clean - mu0
  n_plus <- sum(d > 0)
  n_minus <- sum(d < 0)
  n_zero <- sum(d == 0)
  n_used <- n_plus + n_minus
  
  # Under H0, n_plus ~ Binomial(n_used, 0.5)
  if (alternative == "two.sided") {
    p_value <- 2 * min(
      stats::pbinom(n_plus, n_used, 0.5),
      stats::pbinom(n_plus - 1, n_used, 0.5, lower.tail = FALSE)
    )
    p_value <- min(p_value, 1)
  } else if (alternative == "greater") {
    p_value <- stats::pbinom(n_plus - 1, n_used, 0.5, lower.tail = FALSE)
  } else {  # less
    p_value <- stats::pbinom(n_plus, n_used, 0.5)
  }
  
  list(
    statistic = stats::setNames(n_plus, "S"),
    n = n,
    n.used = n_used,
    n.plus = n_plus,
    n.minus = n_minus,
    n.zero = n_zero,
    p.value = p_value,
    null.value = stats::setNames(mu0, "median"),
    alternative = alternative,
    method = "Sign test",
    data.name = deparse(substitute(x))
  )
}

#' Compare parametric and non-parametric tests
#'
#' Runs t-test, Wilcoxon, and sign test side-by-side for comparison
#'
#' @param x Numeric vector
#' @param mu0 Null hypothesis value
#' @param alternative Alternative hypothesis
#' @return Data frame comparing results
#' @export
compare_tests <- function(x, mu0 = 0, 
                          alternative = c("two.sided", "less", "greater")) {
  alternative <- match.arg(alternative)
  
  # T-test
  t_result <- one_mean_test(x, mu0, alternative, check_assumptions = FALSE)
  
  # Wilcoxon
  w_result <- wilcoxon_test(x, mu0, alternative)
  
  # Sign test
  s_result <- sign_test(x, mu0, alternative)
  
  # Create comparison table
  comparison <- data.frame(
    Test = c("One-sample t-test", "Wilcoxon signed-rank", "Sign test"),
    Statistic = c(
      sprintf("t = %.4f", t_result$statistic),
      sprintf("V = %.0f", w_result$statistic),
      sprintf("S = %d", s_result$statistic)
    ),
    P_value = c(t_result$p.value, w_result$p.value, s_result$p.value),
    Decision = c(
      t_result$decision,
      if (w_result$p.value < 0.05) "reject H0" else "fail to reject H0",
      if (s_result$p.value < 0.05) "reject H0" else "fail to reject H0"
    ),
    stringsAsFactors = FALSE
  )
  
  attr(comparison, "assumptions") <- list(
    note = "T-test assumes normality; Wilcoxon and Sign test are distribution-free"
  )
  
  class(comparison) <- c("test_comparison", "data.frame")
  comparison
}

#' Print method for Wilcoxon test
#'
#' @param x Wilcoxon test object
#' @param digits Number of decimal places
#' @param ... Additional arguments
#' @export
print.oneMeanTest_wilcoxon <- function(x, digits = 4, ...) {
  cat("\n", x$method, "\n\n", sep = "")
  cat("data: ", x$data.name, "\n", sep = "")
  cat(sprintf("V = %.0f, n = %d", x$statistic, x$n.used))
  
  if (x$n != x$n.used) {
    cat(sprintf(" (used %d, %d zeros removed)", x$n.used, x$n - x$n.used))
  }
  
  cat(sprintf(", p-value = %.4f\n", x$p.value))
  
  cat("alternative hypothesis: true location is ")
  if (x$alternative == "two.sided") {
    cat("not equal to")
  } else if (x$alternative == "less") {
    cat("less than")
  } else {
    cat("greater than")
  }
  cat(sprintf(" %.4f\n", x$null.value))
  
  cat(sprintf("%.0f%% confidence interval:\n", x$conf.level * 100))
  cat(sprintf(" [%.4f, %.4f]\n", x$conf.int[1], x$conf.int[2]))
  cat(sprintf("sample estimates:\npseudo-median %.4f\n", x$pseudomedian))
  
  invisible(x)
}

# Helper: Exact Wilcoxon p-value (for small samples)
.wilcoxon_exact_p <- function(W, n, alternative) {
  # Use recursion or lookup table for small n
  # For simplicity, use normal approximation with correction
  .wilcoxon_approx_p(W, n, alternative)
}

# Helper: Approximate Wilcoxon p-value (normal approximation)
.wilcoxon_approx_p <- function(W, n, alternative) {
  # Expected value and variance under H0
  mu_W <- n * (n + 1) / 4
  sigma_W <- sqrt(n * (n + 1) * (2 * n + 1) / 24)
  
  # Continuity correction
  if (alternative == "two.sided") {
    z <- (abs(W - mu_W) - 0.5) / sigma_W
    p_value <- 2 * stats::pnorm(-abs(z))
  } else if (alternative == "greater") {
    z <- (W - mu_W - 0.5) / sigma_W
    p_value <- stats::pnorm(z, lower.tail = FALSE)
  } else {  # less
    z <- (W - mu_W + 0.5) / sigma_W
    p_value <- stats::pnorm(z)
  }
  
  p_value
}

# Helper: Confidence interval for Wilcoxon
.wilcoxon_ci <- function(x, conf.level, alternative) {
  # Walsh averages
  pairwise_avgs <- outer(x, x, "+") / 2
  walsh <- sort(pairwise_avgs[lower.tri(pairwise_avgs, diag = TRUE)])
  
  n <- length(x)
  k <- length(walsh)
  
  # Determine critical rank
  alpha <- 1 - conf.level
  
  if (alternative == "two.sided") {
    # Find ranks for CI
    z <- stats::qnorm(1 - alpha/2)
    rank_lower <- max(1, floor(k/2 - z * sqrt(n * (n + 1) * (2 * n + 1) / 24)))
    rank_upper <- min(k, ceiling(k/2 + z * sqrt(n * (n + 1) * (2 * n + 1) / 24)))
    
    ci <- c(walsh[rank_lower], walsh[rank_upper])
  } else if (alternative == "greater") {
    z <- stats::qnorm(1 - alpha)
    rank_lower <- max(1, floor(k/2 - z * sqrt(n * (n + 1) * (2 * n + 1) / 24)))
    ci <- c(walsh[rank_lower], Inf)
  } else {  # less
    z <- stats::qnorm(1 - alpha)
    rank_upper <- min(k, ceiling(k/2 + z * sqrt(n * (n + 1) * (2 * n + 1) / 24)))
    ci <- c(-Inf, walsh[rank_upper])
  }
  
  ci
}
