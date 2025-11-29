#' Print Method for oneMeanTest Objects
#' 
#' Prints a formatted summary of one-sample t-test results
#' 
#' @param x An object of class oneMeanTest
#' @param digits Number of digits to display
#' @param ... Additional arguments (not currently used)
#' @export
print.oneMeanTest <- function(x, digits = 4, ...) {
  cat(x$method, "\n")
  cat("Data:", x$data.name, "\n\n")

  cat("Hypothesized mean (H0):", x$null.value, "\n")
  cat("Alternative hypothesis:", x$alternative, "\n\n")

  cat("Sample statistics:\n")
  ss <- x$sample.stats
  cat(sprintf("  n    = %d\n", ss$n))
  cat(sprintf("  mean = %.*f\n", digits, ss$mean))
  cat(sprintf("  sd   = %.*f\n\n", digits, ss$sd))

  cat("Test results:\n")
  cat(sprintf("  t     = %.*f\n", digits, x$statistic))
  cat(sprintf("  df    = %d\n", x$parameter))
  cat(sprintf("  p-val = %.*g\n", digits, x$p.value))
  
  # Display critical value (using Unicode \u00b1 for ±)
  if (!is.null(x$t.critical)) {
    if (x$alternative == "two.sided") {
      cat(sprintf("  t-crit = \u00b1%.*f\n", digits, x$t.critical["upper"]))
    } else {
      cat(sprintf("  t-crit = %.*f\n", digits, x$t.critical["critical"]))
    }
  }
  
  cat(sprintf("  alpha = %.*f\n\n", digits, x$alpha))

  ci <- x$conf.int
  cl <- attr(ci, "conf.level")
  cat(sprintf("%.*f%% confidence interval for the mean:\n", digits, cl * 100))
  cat(sprintf("  [%.4f, %.4f]\n\n", ci[1L], ci[2L]))

  cat("Decision:", x$decision, "\n")
  cat("Interpretation:\n")
  cat(" ", x$interpretation, "\n")

  invisible(x)
}

#' Summary Method for oneMeanTest Objects
#' 
#' Provides a summary of one-sample t-test results
#' 
#' @param object An object of class oneMeanTest
#' @param ... Additional arguments passed to print method
#' @export
summary.oneMeanTest <- function(object, ...) {
  print(object, ...)
  invisible(object)
}
