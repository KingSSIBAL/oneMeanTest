#' Assumption checks for one-sample t-test
#'
#' Check normality and outliers for a numeric sample, as typically required
#' for a one-sample t-test on a population mean with unknown variance.
#'
#' The function performs a Shapiro–Wilk normality test (when the sample size
#' is between 3 and 5000) and detects outliers using the 1.5*IQR rule. It is
#' used inside \code{\link{one_mean_test}} when \code{check_assumptions = TRUE}.
#'
#' @param x A numeric vector of observations.
#' @param alpha Significance level for the Shapiro–Wilk normality test.
#' @param verbose Logical; if \code{TRUE}, prints a short textual summary of
#'   the results to the console.
#'
#' @return A list of class \code{"oneMeanTest_assumptions"} with components:
#'   \item{shapiro}{List with W statistic, p-value, \code{alpha}, and a logical
#'     flag \code{normal} indicating if normality is acceptable (or \code{NA}
#'     if the test was not run).}
#'   \item{outliers}{Numeric vector of detected outliers (possibly empty).}
#'   \item{n}{Sample size after removing NA values.}
#'   \item{normal}{Logical flag indicating if normality is acceptable
#'     (\code{TRUE} / \code{FALSE}), or \code{FALSE} if the Shapiro–Wilk test
#'     rejects at the chosen \code{alpha}.}
#'   \item{message}{Textual summary of the diagnostics.}
#'
#' @examples
#' set.seed(123)
#' x <- rnorm(30, mean = 5, sd = 2)
#'
#' # Verbose output
#' check_assumptions(x, alpha = 0.05, verbose = TRUE)
#'
#' # Silent output (used internally by one_mean_test)
#' a_res <- check_assumptions(x, alpha = 0.05, verbose = FALSE)
#' a_res
#'
#' @export
check_assumptions <- function(x, alpha = 0.05, verbose = TRUE) {
  # ============================================================
  # INPUT VALIDATION
  # ============================================================
  
  # Ensure x is numeric and non-empty
  .check_numeric_vector(x, "x")
  
  # Validate alpha is between 0 and 1
  .check_alpha(alpha)
  
  # Remove NA values with warning if present
  x <- .remove_na_with_warning(x, "x")

  # Get sample size after NA removal
  n <- length(x)

  # ============================================================
  # NORMALITY TEST (SHAPIRO-WILK)
  # ============================================================
  
  # Shapiro-Wilk test is only valid for sample sizes between 3 and 5000
  # For n < 3: insufficient data for meaningful test
  # For n > 5000: test becomes too sensitive, rejecting for trivial deviations
  
  shapiro_res <- NULL
  shapiro_ok <- NA  # Initialize as NA (unknown)
  
  if (n >= 3 && n <= 5000) {
    # Perform Shapiro-Wilk test of normality
    # H0: Data come from a normal distribution
    # Ha: Data do not come from a normal distribution
    sw <- stats::shapiro.test(x)
    
    # Decision: normality is "acceptable" if we fail to reject H0
    # i.e., if p-value > alpha
    shapiro_ok <- (sw$p.value > alpha)
    
    # Store test results
    shapiro_res <- list(
      W = unname(sw$statistic),  # Test statistic (W)
      p.value = sw$p.value,      # P-value
      alpha = alpha,             # Significance level used
      normal = shapiro_ok        # TRUE if normality acceptable
    )
  } else {
    # Sample size outside valid range for Shapiro-Wilk
    # Store NA values and explanatory note
    shapiro_res <- list(
      W = NA_real_,
      p.value = NA_real_,
      alpha = alpha,
      normal = NA,
      note = "Shapiro-Wilk test not run (n < 3 or n > 5000)."
    )
  }

  # ============================================================
  # OUTLIER DETECTION (TUKEY'S FENCES / IQR RULE)
  # ============================================================
  
  # Use the 1.5*IQR rule (Tukey's method) to identify potential outliers
  # This is a robust method based on the interquartile range
  
  # Calculate first and third quartiles (Q1 and Q3)
  qs <- stats::quantile(x, probs = c(0.25, 0.75), names = FALSE)
  
  # Calculate interquartile range: IQR = Q3 - Q1
  # IQR represents the middle 50% of the data
  iqr_x <- qs[2L] - qs[1L]
  
  # Define outlier boundaries (Tukey's fences):
  # Lower fence = Q1 - 1.5*IQR
  # Upper fence = Q3 + 1.5*IQR
  # Values outside these fences are considered potential outliers
  lower_bound <- qs[1L] - 1.5 * iqr_x
  upper_bound <- qs[2L] + 1.5 * iqr_x
  
  # Identify outliers: values below lower fence OR above upper fence
  outliers <- x[x < lower_bound | x > upper_bound]

  # ============================================================
  # GENERATE DIAGNOSTIC MESSAGES
  # ============================================================
  
  # Create human-readable message about normality
  normal_msg <- if (isTRUE(shapiro_ok)) {
    "Normality appears acceptable based on the Shapiro-Wilk test."
  } else if (isFALSE(shapiro_ok)) {
    "Normality may be questionable (Shapiro-Wilk test rejects at the chosen alpha)."
  } else {
    "Normality test not performed (sample size outside valid range)."
  }

  # Create message about outliers
  outlier_msg <- if (length(outliers) == 0L) {
    "No outliers detected by the 1.5*IQR rule."
  } else {
    sprintf("Detected %d outlier(s) by the 1.5*IQR rule.", length(outliers))
  }

  # Combine messages into overall summary
  overall_msg <- paste(normal_msg, outlier_msg, sep = " ")

  # ============================================================
  # BUILD AND RETURN RESULT OBJECT
  # ============================================================
  
  res <- list(
    shapiro = shapiro_res,        # Shapiro-Wilk test results
    outliers = outliers,          # Vector of detected outliers
    n = n,                        # Sample size
    normal = isTRUE(shapiro_ok),  # Overall normality flag
    message = overall_msg         # Diagnostic summary message
  )

  # Assign S3 class for potential custom methods
  class(res) <- "oneMeanTest_assumptions"

  # Print message if verbose mode is enabled
  if (isTRUE(verbose)) {
    cat(overall_msg, "\n")
  }

  res
}
