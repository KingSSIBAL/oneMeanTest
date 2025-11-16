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

#' @export
summary.oneMeanTest <- function(object, ...) {
  # For now, summary returns the object itself, but you could
  # enrich this with more detailed breakdown.
  print(object)
  invisible(object)
}
