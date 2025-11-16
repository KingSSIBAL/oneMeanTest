#' Descriptive statistics for a numeric sample
#'
#' Compute basic descriptive statistics for a numeric vector, including
#' mean, median, standard deviation, variance, standard error, and
#' selected quantiles.
#'
#' @param x A numeric vector.
#' @param digits Number of digits to round in the printed output.
#'
#' @return A data frame with one row of summary statistics.
#' @export
descriptive_stats <- function(x, digits = 3) {
  .check_numeric_vector(x, "x")
  x <- .remove_na_with_warning(x, "x")

  n <- length(x)
  mean_x <- mean(x)
  sd_x <- stats::sd(x)
  var_x <- stats::var(x)
  se_x <- sd_x / sqrt(n)
  median_x <- stats::median(x)
  qs <- stats::quantile(x, probs = c(0, 0.25, 0.5, 0.75, 1), names = FALSE)
  names(qs) <- c("min", "q1", "median", "q3", "max")
  iqr_x <- qs["q3"] - qs["q1"]

  res <- data.frame(
    n = n,
    mean = mean_x,
    median = median_x,
    sd = sd_x,
    variance = var_x,
    se = se_x,
    min = qs["min"],
    q1 = qs["q1"],
    q3 = qs["q3"],
    max = qs["max"],
    iqr = iqr_x,
    row.names = ""
  )

  if (!is.null(digits)) {
    res[] <- lapply(res, round, digits = digits)
  }

  res
}
