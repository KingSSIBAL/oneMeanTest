#' Assumption checks for one-sample t-test
#'
#' Check normality and outliers for a numeric sample, as typically required
#' for a one-sample t-test on a population mean with unknown variance.
#'
#' @param x A numeric vector.
#' @param alpha Significance level for the Shapiro-Wilk normality test.
#' @param verbose Logical; if TRUE, prints a short textual summary.
#'
#' @return A list with components:
#'   \item{shapiro}{List with W statistic, p-value, and decision.}
#'   \item{outliers}{Numeric vector of detected outliers (possibly empty).}
#'   \item{n}{Sample size after removing NA values.}
#'   \item{normal}{Logical flag indicating if normality is acceptable.}
#'   \item{message}{Textual summary of the diagnostics.}
#' @export
check_assumptions <- function(x, alpha = 0.05, verbose = TRUE) {
  .check_numeric_vector(x, "x")
  .check_alpha(alpha)
  x <- .remove_na_with_warning(x, "x")

  n <- length(x)

  # Shapiro-Wilk normality test (only meaningful for 3 <= n <= 5000)
  shapiro_res <- NULL
  shapiro_ok <- NA
  if (n >= 3 && n <= 5000) {
    sw <- stats::shapiro.test(x)
    shapiro_ok <- (sw$p.value > alpha)
    shapiro_res <- list(
      W = unname(sw$statistic),
      p.value = sw$p.value,
      alpha = alpha,
      normal = shapiro_ok
    )
  } else {
    shapiro_res <- list(
      W = NA_real_,
      p.value = NA_real_,
      alpha = alpha,
      normal = NA,
      note = "Shapiro-Wilk test not run (n < 3 or n > 5000)."
    )
  }

  # Outlier detection using IQR rule
  qs <- stats::quantile(x, probs = c(0.25, 0.75), names = FALSE)
  iqr_x <- qs[2L] - qs[1L]
  lower_bound <- qs[1L] - 1.5 * iqr_x
  upper_bound <- qs[2L] + 1.5 * iqr_x
  outliers <- x[x < lower_bound | x > upper_bound]

  # Simple messages
  normal_msg <- if (isTRUE(shapiro_ok)) {
    "Normality appears acceptable based on the Shapiro-Wilk test."
  } else if (isFALSE(shapiro_ok)) {
    "Normality may be questionable (Shapiro-Wilk test rejects at the chosen alpha)."
  } else {
    "Normality test not performed (sample size outside valid range)."
  }

  outlier_msg <- if (length(outliers) == 0L) {
    "No outliers detected by the 1.5*IQR rule."
  } else {
    sprintf("Detected %d outlier(s) by the 1.5*IQR rule.", length(outliers))
  }

  overall_msg <- paste(normal_msg, outlier_msg, sep = " ")

  res <- list(
    shapiro = shapiro_res,
    outliers = outliers,
    n = n,
    normal = isTRUE(shapiro_ok),
    message = overall_msg
  )

  class(res) <- "oneMeanTest_assumptions"

  if (isTRUE(verbose)) {
    cat(overall_msg, "\n")
  }

  res
}
