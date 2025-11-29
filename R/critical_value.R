#' Calculate T-Critical Value
#' 
#' Internal function to calculate critical values for t-tests
#' based on alternative hypothesis and significance level.
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
      crit <- stats::qt(1 - alpha/2, df)
      c(lower = -crit, upper = crit)
    },
    "greater" = {
      c(critical = stats::qt(1 - alpha, df))
    },
    "less" = {
      c(critical = stats::qt(alpha, df))
    }
  )
}
