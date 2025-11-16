#' Descriptive statistics for a numeric sample
#'
#' Compute basic descriptive statistics for a numeric vector, including
#' the sample size, mean, median, standard deviation, variance, standard
#' error, quartiles, range, and interquartile range (IQR).
#'
#' This function is intended for numeric data such as measurements or
#' scores, typically the same type of data used in the one-sample t-test.
#'
#' @param x A numeric vector of observations.
#' @param digits Optional integer; number of digits to round in the printed
#'   output. If \code{NULL}, no rounding is applied.
#'
#' @return A data frame with one row of summary statistics:
#' \itemize{
#'   \item \code{n}: sample size (number of non-missing observations).
#'   \item \code{mean}: sample mean.
#'   \item \code{median}: sample median.
#'   \item \code{sd}: sample standard deviation.
#'   \item \code{variance}: sample variance.
#'   \item \code{se}: standard error of the mean.
#'   \item \code{min}, \code{q1}, \code{q3}, \code{max}: minimum, first
#'     quartile, third quartile, and maximum.
#'   \item \code{iqr}: interquartile range (\code{q3 - q1}).
#' }
#'
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' descriptive_stats(x)
#'
#' # Without rounding
#' descriptive_stats(x, digits = NULL)
#'
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
