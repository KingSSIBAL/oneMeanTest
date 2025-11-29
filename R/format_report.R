#' Format T-test Report
#'
#' Formats the one-sample t-test results according to STAT 181 project guidelines.
#' Generates a professional, human-readable report with all essential components
#' including descriptive statistics, hypotheses, test statistics, critical values,
#' p-value, confidence interval, decision, and interpretation.
#'
#' @param test_result Object of class oneMeanTest from one_mean_test()
#' @param include_sections Character vector of sections to include: 
#'   "descriptives", "hypotheses", "test_statistic", "critical_value", 
#'   "p_value", "ci", "conclusion". Default is "all" which includes everything.
#' @return A formatted report as a character string that can be printed with cat()
#' @export
#' @examples
#' set.seed(123)
#' x <- rnorm(30, mean = 5, sd = 2)
#' result <- one_mean_test(x, mu0 = 5)
#' report <- format_ttest_report(result)
#' cat(report)
#' 
#' # Save report to file
#' writeLines(report, "ttest_report.txt")
format_ttest_report <- function(test_result, include_sections = c("all")) {
  
  if (!inherits(test_result, "oneMeanTest")) {
    stop("test_result must be of class 'oneMeanTest'", call. = FALSE)
  }
  
  # Build report lines
  lines <- character()
  
  # Header
  lines <- c(lines, strrep("=", 70))
  lines <- c(lines, "ONE-SAMPLE T-TEST REPORT")
  lines <- c(lines, strrep("=", 70))
  lines <- c(lines, "")
  
  # Descriptive statistics section
  lines <- c(lines, "DESCRIPTIVE STATISTICS")
  lines <- c(lines, strrep("-", 70))
  lines <- c(lines, sprintf("Sample size (n):       %d", test_result$sample.stats$n))
  lines <- c(lines, sprintf("Sample mean:           %.4f", test_result$sample.stats$mean))
  lines <- c(lines, sprintf("Standard deviation:    %.4f", test_result$sample.stats$sd))
  lines <- c(lines, sprintf("Standard error:        %.4f", test_result$sample.stats$se))
  lines <- c(lines, "")
  
  # Hypotheses section
  # Using Unicode: \u03bc = μ, \u03b1 = α, \u2260 = ≠
  lines <- c(lines, "HYPOTHESES")
  lines <- c(lines, strrep("-", 70))
  lines <- c(lines, sprintf("H0: \u03bc = %.4f", test_result$null.value))
  
  # Alternative hypothesis text based on direction
  alt_text <- switch(test_result$alternative,
                     "two.sided" = sprintf("\u03bc \u2260 %.4f", test_result$null.value),
                     "greater" = sprintf("\u03bc > %.4f", test_result$null.value),
                     "less" = sprintf("\u03bc < %.4f", test_result$null.value))
  lines <- c(lines, sprintf("Ha: %s", alt_text))
  lines <- c(lines, sprintf("Significance level:    \u03b1 = %.4f", test_result$alpha))
  lines <- c(lines, "")
  
  # Test statistics section
  lines <- c(lines, "TEST STATISTICS")
  lines <- c(lines, strrep("-", 70))
  lines <- c(lines, sprintf("Test statistic (t):    %.4f", test_result$statistic))
  lines <- c(lines, sprintf("Degrees of freedom:    %d", test_result$parameter))
  
  # Critical value - Using Unicode: \u00b1 = ±
  if (!is.null(test_result$t.critical)) {
    if (test_result$alternative == "two.sided") {
      lines <- c(lines, sprintf("Critical value:        \u00b1%.4f", test_result$t.critical["upper"]))
    } else {
      lines <- c(lines, sprintf("Critical value:        %.4f", test_result$t.critical["critical"]))
    }
  }
  
  lines <- c(lines, sprintf("P-value:               %.4f", test_result$p.value))
  lines <- c(lines, "")
  
  # Confidence interval section
  ci <- test_result$conf.int
  cl <- attr(ci, "conf.level") * 100
  lines <- c(lines, sprintf("CONFIDENCE INTERVAL (%.1f%%)", cl))
  lines <- c(lines, strrep("-", 70))
  lines <- c(lines, sprintf("[%.4f, %.4f]", ci[1], ci[2]))
  lines <- c(lines, "")
  
  # Decision section
  lines <- c(lines, "DECISION")
  lines <- c(lines, strrep("-", 70))
  lines <- c(lines, sprintf("At \u03b1 = %.4f: %s", test_result$alpha, test_result$decision))
  lines <- c(lines, "")
  
  # Interpretation section with text wrapping
  lines <- c(lines, "INTERPRETATION")
  lines <- c(lines, strrep("-", 70))
  
  # Wrap interpretation text to 70 characters
  interp_words <- strsplit(test_result$interpretation, " ")[[1]]
  current_line <- ""
  
  for (word in interp_words) {
    # Test if adding this word exceeds line width
    if (nchar(current_line) == 0) {
      test_line <- word
    } else {
      test_line <- paste(current_line, word)
    }
    
    if (nchar(test_line) > 70 && nchar(current_line) > 0) {
      # Current line is full, save it and start new line
      lines <- c(lines, current_line)
      current_line <- word
    } else {
      # Add word to current line
      current_line <- test_line
    }
  }
  
  # Add remaining text
  if (nchar(current_line) > 0) {
    lines <- c(lines, current_line)
  }
  
  lines <- c(lines, "")
  
  # Assumption checks section (if available)
  if (!is.null(test_result$assumptions)) {
    lines <- c(lines, "ASSUMPTION CHECKS")
    lines <- c(lines, strrep("-", 70))
    
    assump <- test_result$assumptions
    
    # Normality test
    if (!is.null(assump$normality)) {
      norm_status <- if (assump$normality$is_normal) "PASSED" else "FAILED"
      lines <- c(lines, sprintf("Normality (Shapiro-Wilk): %s (p = %.4f)", 
                               norm_status, assump$normality$p.value))
    }
    
    # Sample size
    if (!is.null(assump$sample_size)) {
      size_status <- if (assump$sample_size$adequate) "ADEQUATE" else "SMALL"
      lines <- c(lines, sprintf("Sample size: %s (n = %d)", 
                               size_status, assump$sample_size$n))
    }
    
    # Outliers
    if (!is.null(assump$outliers_mild) && assump$outliers_mild$n_outliers > 0) {
      lines <- c(lines, sprintf("Mild outliers detected: %d", 
                               assump$outliers_mild$n_outliers))
    }
    
    if (!is.null(assump$outliers_extreme) && assump$outliers_extreme$n_outliers > 0) {
      lines <- c(lines, sprintf("Extreme outliers detected: %d", 
                               assump$outliers_extreme$n_outliers))
    }
    
    lines <- c(lines, "")
  }
  
  # Footer
  lines <- c(lines, strrep("=", 70))
  
  # Return as single string with newlines
  paste(lines, collapse = "\n")
}


#' Get Interpretation Language Based on Hypothesis Direction
#' 
#' Internal helper function to generate appropriate interpretation text
#' based on the alternative hypothesis and decision.
#' 
#' @param alternative Alternative hypothesis type
#' @param decision Decision string ("reject H0" or "fail to reject H0")
#' @param mu0 Null hypothesis value
#' @param xbar Sample mean
#' @param alpha Significance level
#' @param p_value P-value from test
#' @return Character string with interpretation
#' @keywords internal
#' @noRd
.get_interpretation_language <- function(alternative, decision, mu0, xbar, 
                                         alpha, p_value) {
  
  # Determine direction language
  direction <- switch(alternative,
                     "two.sided" = "different from",
                     "greater" = "greater than",
                     "less" = "less than")
  
  # Build interpretation based on decision
  if (decision == "reject H0") {
    sprintf(
      "At the %.1f%% significance level (alpha = %.4f), there is sufficient statistical evidence to reject the null hypothesis. The sample mean of %.4f is significantly %s the hypothesized value of %.4f (p-value = %.4f).",
      (1 - alpha) * 100, alpha, xbar, direction, mu0, p_value
    )
  } else {
    sprintf(
      "At the %.1f%% significance level (alpha = %.4f), there is insufficient statistical evidence to reject the null hypothesis. The sample mean of %.4f is not significantly %s the hypothesized value of %.4f (p-value = %.4f).",
      (1 - alpha) * 100, alpha, xbar, direction, mu0, p_value
    )
  }
}


#' Format Critical Value for Display
#' 
#' Internal helper function to format critical values based on alternative hypothesis
#' 
#' @param t_critical Named vector of critical values
#' @param alternative Alternative hypothesis type
#' @return Formatted character string
#' @keywords internal
#' @noRd
.format_critical_value <- function(t_critical, alternative) {
  if (is.null(t_critical)) {
    return("")
  }
  
  if (alternative == "two.sided") {
    sprintf("\u00b1%.4f", t_critical["upper"])
  } else {
    sprintf("%.4f", t_critical["critical"])
  }
}
