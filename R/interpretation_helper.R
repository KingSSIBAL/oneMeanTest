#' Interpretation Helper Functions
#'
#' Functions to generate human-readable interpretations of statistical results
#' These provide context and plain-language explanations of test outcomes
#'
#' @keywords internal

#' Generate interpretation from test result
#'
#' Creates a comprehensive plain-language interpretation of test results
#'
#' @param result Object of class oneMeanTest
#' @return Character string with interpretation
#' @export
interpret_result <- function(result) {
  if (!inherits(result, "oneMeanTest")) {
    stop("Input must be an oneMeanTest object.", call. = FALSE)
  }
  
  # Extract key information
  decision <- result$decision
  p_value <- result$p.value
  alpha <- result$alpha
  alternative <- result$alternative
  mu0 <- result$null.value
  xbar <- result$estimate
  
  # Generate interpretation
  .get_interpretation_language(
    alternative = alternative,
    decision = decision,
    mu0 = mu0,
    xbar = xbar,
    alpha = alpha,
    p_value = p_value
  )
}

#' Format decision for display
#'
#' Creates readable text for statistical decision
#'
#' @param decision Decision string ("reject H0" or "fail to reject H0")
#' @param alpha Significance level
#' @return Formatted decision text
#' @export
format_decision <- function(decision, alpha) {
  if (decision == "reject H0") {
    sprintf("Reject the null hypothesis at alpha = %.4f", alpha)
  } else {
    sprintf("Fail to reject the null hypothesis at alpha = %.4f", alpha)
  }
}

#' Get alternative hypothesis text
#'
#' Returns readable text for alternative hypothesis direction
#'
#' @param alternative Alternative hypothesis type
#' @return Character string describing direction
#' @export
get_alternative_text <- function(alternative) {
  switch(alternative,
         "two.sided" = "different from",
         "greater" = "greater than",
         "less" = "less than",
         alternative)  # fallback
}

#' Interpret p-value strength
#'
#' Categorizes p-value to describe strength of evidence
#'
#' @param p_value Numeric p-value
#' @return Character description of evidence strength
#' @export
interpret_p_value <- function(p_value) {
  if (!is.numeric(p_value) || p_value < 0 || p_value > 1) {
    stop("p_value must be between 0 and 1.", call. = FALSE)
  }
  
  if (p_value < 0.001) {
    "very strong evidence against the null hypothesis"
  } else if (p_value < 0.01) {
    "strong evidence against the null hypothesis"
  } else if (p_value < 0.05) {
    "moderate evidence against the null hypothesis"
  } else if (p_value < 0.1) {
    "weak evidence against the null hypothesis"
  } else {
    "little to no evidence against the null hypothesis"
  }
}

#' Interpret effect size
#'
#' Categorizes effect size using Cohen's conventions
#'
#' @param d Cohen's d effect size (standardized mean difference)
#' @return Character description of effect size magnitude
#' @export
interpret_effect_size <- function(d) {
  if (!is.numeric(d)) {
    stop("Effect size must be numeric.", call. = FALSE)
  }
  
  d_abs <- abs(d)
  
  if (d_abs < 0.2) {
    "negligible effect size"
  } else if (d_abs < 0.5) {
    "small effect size"
  } else if (d_abs < 0.8) {
    "medium effect size"
  } else {
    "large effect size"
  }
}

#' Calculate Cohen's d effect size
#'
#' Computes standardized effect size for one-sample t-test
#'
#' @param x Numeric vector of observations
#' @param mu0 Null hypothesis value
#' @return Numeric Cohen's d value
#' @export
cohens_d <- function(x, mu0 = 0) {
  xbar <- mean(x, na.rm = TRUE)
  s <- stats::sd(x, na.rm = TRUE)
  
  if (s == 0) {
    warning("Standard deviation is zero. Effect size is undefined.", 
            call. = FALSE)
    return(NA_real_)
  }
  
  (xbar - mu0) / s
}

#' Interpret confidence interval
#'
#' Provides plain-language interpretation of confidence interval
#'
#' @param ci Numeric vector with lower and upper bounds
#' @param conf_level Confidence level (e.g., 0.95)
#' @param mu0 Null hypothesis value (optional)
#' @return Character string with interpretation
#' @export
interpret_ci <- function(ci, conf_level = 0.95, mu0 = NULL) {
  if (length(ci) != 2) {
    stop("CI must have exactly 2 values (lower and upper bounds).", 
         call. = FALSE)
  }
  
  lower <- ci[1]
  upper <- ci[2]
  pct <- conf_level * 100
  
  base_text <- sprintf(
    "We are %.0f%% confident that the true population mean lies between %.4f and %.4f.",
    pct, lower, upper
  )
  
  # Add interpretation relative to null value if provided
  if (!is.null(mu0)) {
    if (mu0 < lower) {
      add_text <- sprintf(
        " Since %.4f is below the entire confidence interval, this suggests the true mean is greater than %.4f.",
        mu0, mu0
      )
    } else if (mu0 > upper) {
      add_text <- sprintf(
        " Since %.4f is above the entire confidence interval, this suggests the true mean is less than %.4f.",
        mu0, mu0
      )
    } else {
      add_text <- sprintf(
        " Since %.4f falls within the confidence interval, this value is plausible for the population mean.",
        mu0
      )
    }
    base_text <- paste0(base_text, add_text)
  }
  
  base_text
}

#' Generate assumption check interpretation
#'
#' Interprets results of assumption checks
#'
#' @param assumptions Object from check_assumptions()
#' @return Character string with interpretation
#' @export
interpret_assumptions <- function(assumptions) {
  if (!inherits(assumptions, "oneMeanTest_assumptions")) {
    stop("Input must be from check_assumptions().", call. = FALSE)
  }
  
  messages <- character()
  
  # Normality
  if (!is.null(assumptions$shapiro)) {
    if (isTRUE(assumptions$normal)) {
      messages <- c(messages, 
                    "Data appear to be normally distributed (normality assumption satisfied).")
    } else if (isFALSE(assumptions$normal)) {
      messages <- c(messages,
                    "Data may not be normally distributed (consider using bootstrap or non-parametric tests).")
    } else {
      messages <- c(messages,
                    "Normality could not be assessed (sample size outside valid range).")
    }
  }
  
  # Outliers
  n_outliers <- length(assumptions$outliers)
  if (n_outliers > 0) {
    messages <- c(messages,
                  sprintf("Detected %d potential outlier(s). Consider investigating these values.",
                          n_outliers))
  } else {
    messages <- c(messages,
                  "No outliers detected.")
  }
  
  # Sample size
  if (assumptions$n < 30) {
    messages <- c(messages,
                  sprintf("Small sample size (n = %d). Results may be less reliable if normality is violated.",
                          assumptions$n))
  }
  
  paste(messages, collapse = " ")
}
