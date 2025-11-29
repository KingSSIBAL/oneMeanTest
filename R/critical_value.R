#' Calculate T-Critical Value
#' 
#' Internal function to calculate critical values for t-tests
#' based on alternative hypothesis and significance level.
#' 
#' For two-sided tests, returns both upper and lower critical values.
#' For one-sided tests, returns a single critical value.
#' 
#' @param alternative Alternative hypothesis type: "two.sided", "less", or "greater"
#' @param alpha Significance level (between 0 and 1)
#' @param df Degrees of freedom
#' @return Named numeric vector with critical value(s)
#' @importFrom stats qt
#' @keywords internal
#' @noRd
.calculate_t_critical <- function(alternative, alpha, df) {
  switch(alternative,
    "two.sided" = {
      # For two-sided test, split alpha between both tails
      # Reject if |t| > t_(α/2, df)
      crit <- stats::qt(1 - alpha/2, df)
      c(lower = -crit, upper = crit)
    },
    "greater" = {
      # For right-tailed test
      # Reject if t > t_(α, df)
      c(critical = stats::qt(1 - alpha, df))
    },
    "less" = {
      # For left-tailed test
      # Reject if t < -t_(α, df) or equivalently t < t_(α, df) from lower tail
      c(critical = stats::qt(alpha, df))
    }
  )
}
