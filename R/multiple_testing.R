#' Multiple Testing Corrections
#'
#' Functions to adjust p-values for multiple comparisons
#' when conducting multiple one-sample tests
#'
#' @name multiple_testing
NULL

#' Adjust p-values for multiple testing
#'
#' Applies various multiple testing correction methods
#'
#' @param p_values Numeric vector of p-values
#' @param method Correction method: "bonferroni", "holm", "hochberg", 
#'   "BH" (Benjamini-Hochberg), "BY" (Benjamini-Yekutieli), "fdr"
#' @param alpha Family-wise error rate or FDR level (default 0.05)
#' @return Data frame with original and adjusted p-values
#' @export
#' @examples
#' # Multiple tests
#' p_vals <- c(0.001, 0.01, 0.04, 0.15, 0.50)
#' adjust_p_values(p_vals, method = "bonferroni")
adjust_p_values <- function(p_values, 
                            method = c("bonferroni", "holm", "hochberg", 
                                       "BH", "BY", "fdr", "none"),
                            alpha = 0.05) {
  method <- match.arg(method)
  
  if (!is.numeric(p_values) || any(p_values < 0) || any(p_values > 1)) {
    stop("p_values must be numeric between 0 and 1.", call. = FALSE)
  }
  
  m <- length(p_values)
  
  if (method == "fdr") {
    method <- "BH"  # FDR is same as BH
  }
  
  # Calculate adjusted p-values
  p_adjusted <- if (method == "none") {
    p_values
  } else {
    stats::p.adjust(p_values, method = method)
  }
  
  # Determine rejection decisions
  reject <- p_adjusted < alpha
  
  # Create summary
  result <- data.frame(
    Test = seq_along(p_values),
    P_value = p_values,
    P_adjusted = p_adjusted,
    Reject = reject,
    stringsAsFactors = FALSE
  )
  
  attr(result, "method") <- method
  attr(result, "alpha") <- alpha
  attr(result, "m") <- m
  
  class(result) <- c("multiple_testing", "data.frame")
  result
}

#' Bonferroni correction
#'
#' Simple Bonferroni correction: adjust alpha by dividing by number of tests
#'
#' @param alpha Original significance level
#' @param m Number of tests
#' @return Adjusted alpha level
#' @export
bonferroni_alpha <- function(alpha = 0.05, m) {
  alpha / m
}

#' Perform multiple one-sample t-tests with correction
#'
#' Conducts multiple one-sample t-tests and adjusts p-values
#'
#' @param data_list List of numeric vectors
#' @param mu0_list Vector of null values (same length as data_list)
#' @param method Multiple testing correction method
#' @param alpha Family-wise error rate (default 0.05)
#' @param alternative Alternative hypothesis
#' @return Object with all test results and corrections
#' @export
#' @examples
#' # Multiple datasets
#' data_list <- list(
#'   group1 = rnorm(30, mean = 5, sd = 2),
#'   group2 = rnorm(30, mean = 6, sd = 2),
#'   group3 = rnorm(30, mean = 5.5, sd = 2)
#' )
#' 
#' multiple_t_tests(data_list, mu0_list = c(5, 5, 5), method = "holm")
multiple_t_tests <- function(data_list, mu0_list, 
                              method = c("bonferroni", "holm", "BH", "BY"),
                              alpha = 0.05,
                              alternative = "two.sided") {
  method <- match.arg(method)
  
  if (length(data_list) != length(mu0_list)) {
    stop("data_list and mu0_list must have same length.", call. = FALSE)
  }
  
  m <- length(data_list)
  
  # Conduct all tests
  test_results <- lapply(seq_len(m), function(i) {
    one_mean_test(data_list[[i]], 
                  mu0 = mu0_list[i], 
                  alternative = alternative,
                  check_assumptions = FALSE)
  })
  
  # Extract p-values
  p_values <- sapply(test_results, function(x) x$p.value)
  
  # Adjust p-values
  adjusted <- adjust_p_values(p_values, method = method, alpha = alpha)
  
  # Create summary table
  test_names <- names(data_list)
  if (is.null(test_names)) {
    test_names <- paste0("Test_", seq_len(m))
  }
  
  summary_table <- data.frame(
    Test = test_names,
    n = sapply(data_list, function(x) length(x[!is.na(x)])),
    Mean = sapply(data_list, mean, na.rm = TRUE),
    Mu0 = mu0_list,
    t_statistic = sapply(test_results, function(x) x$statistic),
    df = sapply(test_results, function(x) x$parameter),
    P_value = p_values,
    P_adjusted = adjusted$P_adjusted,
    Reject = adjusted$Reject,
    stringsAsFactors = FALSE
  )
  
  result <- list(
    summary = summary_table,
    tests = test_results,
    method = method,
    alpha = alpha,
    m = m,
    n_rejected = sum(adjusted$Reject)
  )
  
  class(result) <- "multiple_t_tests"
  result
}

#' Calculate False Discovery Rate
#'
#' Estimates the false discovery rate for a set of tests
#'
#' @param p_values Numeric vector of p-values
#' @param alpha FDR level (default 0.05)
#' @return List with FDR estimates
#' @export
calculate_fdr <- function(p_values, alpha = 0.05) {
  m <- length(p_values)
  p_sorted <- sort(p_values)
  ranks <- seq_len(m)
  
  # BH critical values
  bh_critical <- (ranks / m) * alpha
  
  # Find largest k where p(k) <= (k/m)*alpha
  reject_indices <- which(p_sorted <= bh_critical)
  
  if (length(reject_indices) > 0) {
    k <- max(reject_indices)
    threshold <- p_sorted[k]
    n_reject <- k
  } else {
    k <- 0
    threshold <- 0
    n_reject <- 0
  }
  
  list(
    m = m,
    alpha = alpha,
    n_rejected = n_reject,
    threshold = threshold,
    method = "Benjamini-Hochberg",
    fdr_controlled = TRUE
  )
}

#' Print method for multiple testing
#'
#' @param x Multiple testing object
#' @param digits Number of decimal places
#' @param ... Additional arguments
#' @export
print.multiple_testing <- function(x, digits = 4, ...) {
  cat("\nMultiple Testing Correction\n")
  cat(strrep("=", 60), "\n\n")
  
  cat(sprintf("Method: %s\n", attr(x, "method")))
  cat(sprintf("Number of tests: %d\n", attr(x, "m")))
  cat(sprintf("Significance level: %.4f\n", attr(x, "alpha")))
  cat(sprintf("Number rejected: %d (%.1f%%)\n\n", 
              sum(x$Reject), 
              100 * sum(x$Reject) / attr(x, "m")))
  
  print(as.data.frame(x), digits = digits, row.names = FALSE)
  
  invisible(x)
}

#' Print method for multiple t-tests
#'
#' @param x Multiple t-tests object
#' @param digits Number of decimal places
#' @param ... Additional arguments
#' @export
print.multiple_t_tests <- function(x, digits = 4, ...) {
  cat("\nMultiple One-Sample T-Tests\n")
  cat(strrep("=", 70), "\n\n")
  
  cat(sprintf("Number of tests: %d\n", x$m))
  cat(sprintf("Correction method: %s\n", x$method))
  cat(sprintf("Family-wise error rate: %.4f\n", x$alpha))
  cat(sprintf("Tests rejected: %d (%.1f%%)\n\n", 
              x$n_rejected, 
              100 * x$n_rejected / x$m))
  
  print(x$summary, digits = digits, row.names = FALSE)
  
  invisible(x)
}
