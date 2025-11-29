#' Generate Proper Interpretation Text
#' 
#' Creates interpretation text following STAT 181 guidelines,
#' modifying language based on the alternative hypothesis direction.
#' This function generates human-readable conclusions based on the
#' statistical test results.
#' 
#' @param decision Decision string: "reject H0" or "fail to reject H0"
#' @param alternative Alternative hypothesis: "two.sided", "less", or "greater"
#' @param mu0 Null hypothesis value (hypothesized population mean)
#' @param xbar Sample mean
#' @param ci Confidence interval vector (lower, upper)
#' @param alpha Significance level (e.g., 0.05)
#' @param p_value P-value from the test
#' 
#' @return Character string with appropriate interpretation
#' 
#' @details
#' The interpretation language varies based on:
#' - Decision: reject vs fail to reject H0
#' - Alternative: two-sided, greater, or less
#' - Statistical significance level (alpha)
#' 
#' The function ensures the interpretation matches the hypothesis direction
#' and provides context about the strength of evidence.
#' 
#' @keywords internal
#' @noRd
.generate_interpretation <- function(decision, alternative, mu0, xbar, 
                                     ci, alpha, p_value) {
  
  # Determine the comparison phrase based on alternative hypothesis
  comparison <- switch(alternative,
                      "two.sided" = "different from",
                      "greater" = "greater than",
                      "less" = "less than")
  
  # Convert alpha to confidence level percentage for reporting
  conf_pct <- (1 - alpha) * 100
  
  if (decision == "reject H0") {
    # ========================================
    # REJECT NULL HYPOTHESIS - Sufficient Evidence
    # ========================================
    
    switch(alternative,
      "two.sided" = {
        # Two-sided: mean is significantly different
        sprintf(
          paste0(
            "At the %.1f%% confidence level (alpha = %.4f), ",
            "there is sufficient statistical evidence to reject ",
            "the null hypothesis that the population mean equals %.4f. ",
            "The sample mean of %.4f is significantly %s %.4f ",
            "(p-value = %.4f < alpha = %.4f). ",
            "The data suggest the true population mean differs from the hypothesized value."
          ),
          conf_pct, alpha, mu0, xbar, comparison, mu0, p_value, alpha
        )
      },
      
      "greater" = {
        # Right-tailed: mean is significantly greater
        sprintf(
          paste0(
            "At the %.1f%% confidence level (alpha = %.4f), ",
            "there is sufficient statistical evidence to reject ",
            "the null hypothesis and conclude that the population mean ",
            "is significantly %s %.4f. ",
            "The sample mean of %.4f provides strong evidence that the true mean ",
            "exceeds the hypothesized value (p-value = %.4f < alpha = %.4f)."
          ),
          conf_pct, alpha, comparison, mu0, xbar, p_value, alpha
        )
      },
      
      "less" = {
        # Left-tailed: mean is significantly less
        sprintf(
          paste0(
            "At the %.1f%% confidence level (alpha = %.4f), ",
            "there is sufficient statistical evidence to reject ",
            "the null hypothesis and conclude that the population mean ",
            "is significantly %s %.4f. ",
            "The sample mean of %.4f provides strong evidence that the true mean ",
            "is below the hypothesized value (p-value = %.4f < alpha = %.4f)."
          ),
          conf_pct, alpha, comparison, mu0, xbar, p_value, alpha
        )
      }
    )
    
  } else {
    # ========================================
    # FAIL TO REJECT NULL HYPOTHESIS - Insufficient Evidence
    # ========================================
    
    switch(alternative,
      "two.sided" = {
        # Two-sided: not significantly different
        sprintf(
          paste0(
            "At the %.1f%% confidence level (alpha = %.4f), ",
            "there is insufficient statistical evidence to reject ",
            "the null hypothesis that the population mean equals %.4f. ",
            "The sample mean of %.4f is not significantly %s %.4f ",
            "(p-value = %.4f >= alpha = %.4f). ",
            "The data are consistent with the null hypothesis."
          ),
          conf_pct, alpha, mu0, xbar, comparison, mu0, p_value, alpha
        )
      },
      
      "greater" = {
        # Right-tailed: not significantly greater
        sprintf(
          paste0(
            "At the %.1f%% confidence level (alpha = %.4f), ",
            "there is insufficient statistical evidence to conclude ",
            "that the population mean is %s %.4f. ",
            "The sample mean of %.4f does not provide strong enough evidence ",
            "to support the claim that the true mean exceeds the hypothesized value ",
            "(p-value = %.4f >= alpha = %.4f)."
          ),
          conf_pct, alpha, comparison, mu0, xbar, p_value, alpha
        )
      },
      
      "less" = {
        # Left-tailed: not significantly less
        sprintf(
          paste0(
            "At the %.1f%% confidence level (alpha = %.4f), ",
            "there is insufficient statistical evidence to conclude ",
            "that the population mean is %s %.4f. ",
            "The sample mean of %.4f does not provide strong enough evidence ",
            "to support the claim that the true mean is below the hypothesized value ",
            "(p-value = %.4f >= alpha = %.4f)."
          ),
          conf_pct, alpha, comparison, mu0, xbar, p_value, alpha
        )
      }
    )
  }
}


#' Format Direction Text for Alternative Hypothesis
#' 
#' Helper function to convert alternative hypothesis types into
#' readable text descriptions.
#' 
#' @param alternative Alternative hypothesis type
#' @return Character string describing the direction
#' @keywords internal
#' @noRd
.format_alternative_text <- function(alternative) {
  switch(alternative,
    "two.sided" = "not equal to",
    "greater" = "greater than",
    "less" = "less than",
    "two.sided"  # default
  )
}


#' Generate Short Interpretation
#' 
#' Creates a brief, one-sentence interpretation of test results.
#' Useful for summaries and quick reports.
#' 
#' @param decision Decision string
#' @param p_value P-value
#' @param alpha Significance level
#' @return Short interpretation string
#' @keywords internal
#' @noRd
.generate_short_interpretation <- function(decision, p_value, alpha) {
  if (decision == "reject H0") {
    sprintf(
      "Statistically significant result (p = %.4f < \u03b1 = %.4f): reject H0.",
      p_value, alpha
    )
  } else {
    sprintf(
      "Not statistically significant (p = %.4f >= \u03b1 = %.4f): fail to reject H0.",
      p_value, alpha
    )
  }
}
