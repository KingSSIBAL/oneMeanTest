#' Generate Proper Interpretation
#' 
#' Creates interpretation following the guideline: 
#' 
#' 
#' @keywords internal
.generate_interpretation <- function(decision, alternative, mu0, xbar, 
                                     ci, alpha, p_value) {
  if (decision == "reject H0") {
    switch(alternative,
      "two.sided" = sprintf(
        "At the %g%% significance level, there is sufficient evidence to conclude that the population mean is significantly different from %g. The sample mean of %g falls outside the hypothesized value.",
        alpha * 100, mu0, xbar
      ),
      "greater" = sprintf(
        "At the %g%% significance level, there is sufficient evidence to conclude that the population mean is significantly greater than %g. The sample mean of %g exceeds the hypothesized value.",
        alpha * 100, mu0, xbar
      ),
      "less" = sprintf(
        "At the %g%% significance level, there is sufficient evidence to conclude that the population mean is significantly less than %g. The sample mean of %g is below the hypothesized value.",
        alpha * 100, mu0, xbar
      )
    )
  } else {
    # Fail to reject interpretations
  }
}
