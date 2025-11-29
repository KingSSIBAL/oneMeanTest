#' @importFrom utils capture.output
NULL


#' Report Methods for oneMeanTest Objects
#' 
#' Generate formatted reports of t-test results in different output formats.
#' This provides an alternative to format_ttest_report() with support for
#' multiple output formats.
#' 
#' @param x Object of class oneMeanTest
#' @param format Output format: "console", "markdown", or "latex"
#' @param ... Additional arguments (currently unused)
#' @return Formatted report as character string
#' @export
#' @examples
#' \dontrun{
#' set.seed(123)
#' x <- rnorm(30, mean = 5, sd = 2)
#' result <- one_mean_test(x, mu0 = 5)
#' 
#' # Console format (default)
#' cat(report(result, format = "console"))
#' 
#' # Markdown format
#' cat(report(result, format = "markdown"))
#' 
#' # LaTeX format
#' cat(report(result, format = "latex"))
#' }
report <- function(x, format = "console", ...) {
  UseMethod("report")
}

#' @rdname report
#' @export
report.oneMeanTest <- function(x, format = "console", ...) {
  # Validate format argument
  format <- match.arg(format, c("console", "markdown", "latex"))
  
  # Dispatch to appropriate helper function
  switch(format,
    console = .report_console(x, ...),
    markdown = .report_markdown(x, ...),
    latex = .report_latex(x, ...)
  )
}


#' Console Format Report
#' 
#' Internal function to generate console-friendly report.
#' This is essentially a wrapper around format_ttest_report().
#' 
#' @param x oneMeanTest object
#' @param ... Additional arguments
#' @return Character string with console report
#' @keywords internal
#' @noRd
.report_console <- function(x, ...) {
  # Use the existing format_ttest_report function
  if (exists("format_ttest_report", mode = "function")) {
    format_ttest_report(x)
  } else {
    # Fallback to simple print if format_ttest_report not available
    paste(capture.output(print(x)), collapse = "\n")
  }
}


#' Markdown Format Report
#' 
#' Internal function to generate Markdown-formatted report.
#' Suitable for R Markdown documents and GitHub.
#' 
#' @param x oneMeanTest object
#' @param ... Additional arguments
#' @return Character string with Markdown report
#' @keywords internal
#' @noRd
.report_markdown <- function(x, ...) {
  lines <- character()
  
  # Title
  lines <- c(lines, "# One-Sample T-Test Report")
  lines <- c(lines, "")
  
  # Summary info
  lines <- c(lines, sprintf("**Data:** `%s`", x$data.name))
  lines <- c(lines, sprintf("**Method:** %s", x$method))
  lines <- c(lines, "")
  
  # Hypotheses
  lines <- c(lines, "## Hypotheses")
  lines <- c(lines, "")
  lines <- c(lines, sprintf("- **H\u2080:** \u03bc = %.4f", x$null.value))
  
  alt_symbol <- switch(x$alternative,
                      "two.sided" = "\u2260",
                      "greater" = ">",
                      "less" = "<")
  lines <- c(lines, sprintf("- **H\u2090:** \u03bc %s %.4f", alt_symbol, x$null.value))
  lines <- c(lines, sprintf("- **Significance level:** \u03b1 = %.4f", x$alpha))
  lines <- c(lines, "")
  
  # Descriptive Statistics
  lines <- c(lines, "## Descriptive Statistics")
  lines <- c(lines, "")
  lines <- c(lines, "| Statistic | Value |")
  lines <- c(lines, "|-----------|-------|")
  lines <- c(lines, sprintf("| Sample size (n) | %d |", x$sample.stats$n))
  lines <- c(lines, sprintf("| Sample mean | %.4f |", x$sample.stats$mean))
  lines <- c(lines, sprintf("| Std. deviation | %.4f |", x$sample.stats$sd))
  lines <- c(lines, sprintf("| Std. error | %.4f |", x$sample.stats$se))
  lines <- c(lines, "")
  
  # Test Results
  lines <- c(lines, "## Test Results")
  lines <- c(lines, "")
  lines <- c(lines, "| Statistic | Value |")
  lines <- c(lines, "|-----------|-------|")
  lines <- c(lines, sprintf("| t-statistic | %.4f |", x$statistic))
  lines <- c(lines, sprintf("| df | %d |", x$parameter))
  
  if (!is.null(x$t.critical)) {
    if (x$alternative == "two.sided") {
      lines <- c(lines, sprintf("| Critical value | \u00b1%.4f |", x$t.critical["upper"]))
    } else {
      lines <- c(lines, sprintf("| Critical value | %.4f |", x$t.critical["critical"]))
    }
  }
  
  lines <- c(lines, sprintf("| p-value | %.4f |", x$p.value))
  lines <- c(lines, "")
  
  # Confidence Interval
  ci <- x$conf.int
  cl <- attr(ci, "conf.level") * 100
  lines <- c(lines, sprintf("## %.1f%% Confidence Interval", cl))
  lines <- c(lines, "")
  lines <- c(lines, sprintf("[%.4f, %.4f]", ci[1], ci[2]))
  lines <- c(lines, "")
  
  # Decision and Interpretation
  lines <- c(lines, "## Decision")
  lines <- c(lines, "")
  lines <- c(lines, sprintf("**%s** at \u03b1 = %.4f", x$decision, x$alpha))
  lines <- c(lines, "")
  lines <- c(lines, "## Interpretation")
  lines <- c(lines, "")
  lines <- c(lines, x$interpretation)
  lines <- c(lines, "")
  
  # Return combined text
  paste(lines, collapse = "\n")
}


#' LaTeX Format Report
#' 
#' Internal function to generate LaTeX-formatted report.
#' Suitable for academic papers and LaTeX documents.
#' 
#' @param x oneMeanTest object
#' @param ... Additional arguments
#' @return Character string with LaTeX report
#' @keywords internal
#' @noRd
.report_latex <- function(x, ...) {
  lines <- character()
  
  # Document structure (without full preamble)
  lines <- c(lines, "% One-Sample T-Test Results")
  lines <- c(lines, "")
  lines <- c(lines, "\\subsection{One-Sample T-Test Report}")
  lines <- c(lines, "")
  
  # Hypotheses
  lines <- c(lines, "\\textbf{Hypotheses:}")
  lines <- c(lines, "\\begin{itemize}")
  lines <- c(lines, sprintf("  \\item $H_0: \\mu = %.4f$", x$null.value))
  
  alt_symbol <- switch(x$alternative,
                      "two.sided" = "\\neq",
                      "greater" = ">",
                      "less" = "<")
  lines <- c(lines, sprintf("  \\item $H_a: \\mu %s %.4f$", alt_symbol, x$null.value))
  lines <- c(lines, sprintf("  \\item Significance level: $\\alpha = %.4f$", x$alpha))
  lines <- c(lines, "\\end{itemize}")
  lines <- c(lines, "")
  
  # Descriptive Statistics Table
  lines <- c(lines, "\\textbf{Descriptive Statistics:}")
  lines <- c(lines, "")
  lines <- c(lines, "\\begin{table}[h]")
  lines <- c(lines, "\\centering")
  lines <- c(lines, "\\begin{tabular}{lr}")
  lines <- c(lines, "\\hline")
  lines <- c(lines, "Statistic & Value \\\\")
  lines <- c(lines, "\\hline")
  lines <- c(lines, sprintf("Sample size ($n$) & %d \\\\", x$sample.stats$n))
  lines <- c(lines, sprintf("Sample mean ($\\bar{x}$) & %.4f \\\\", x$sample.stats$mean))
  lines <- c(lines, sprintf("Standard deviation ($s$) & %.4f \\\\", x$sample.stats$sd))
  lines <- c(lines, sprintf("Standard error & %.4f \\\\", x$sample.stats$se))
  lines <- c(lines, "\\hline")
  lines <- c(lines, "\\end{tabular}")
  lines <- c(lines, "\\end{table}")
  lines <- c(lines, "")
  
  # Test Results Table
  lines <- c(lines, "\\textbf{Test Results:}")
  lines <- c(lines, "")
  lines <- c(lines, "\\begin{table}[h]")
  lines <- c(lines, "\\centering")
  lines <- c(lines, "\\begin{tabular}{lr}")
  lines <- c(lines, "\\hline")
  lines <- c(lines, "Statistic & Value \\\\")
  lines <- c(lines, "\\hline")
  lines <- c(lines, sprintf("$t$-statistic & %.4f \\\\", x$statistic))
  lines <- c(lines, sprintf("Degrees of freedom & %d \\\\", x$parameter))
  
  if (!is.null(x$t.critical)) {
    if (x$alternative == "two.sided") {
      lines <- c(lines, sprintf("Critical value & $\\pm%.4f$ \\\\", x$t.critical["upper"]))
    } else {
      lines <- c(lines, sprintf("Critical value & %.4f \\\\", x$t.critical["critical"]))
    }
  }
  
  lines <- c(lines, sprintf("$p$-value & %.4f \\\\", x$p.value))
  lines <- c(lines, "\\hline")
  lines <- c(lines, "\\end{tabular}")
  lines <- c(lines, "\\end{table}")
  lines <- c(lines, "")
  
  # Confidence Interval
  ci <- x$conf.int
  cl <- attr(ci, "conf.level") * 100
  lines <- c(lines, sprintf("\\textbf{%.0f\\%% Confidence Interval:} $[%.4f, %.4f]$", 
                           cl, ci[1], ci[2]))
  lines <- c(lines, "")
  
  # Decision and Interpretation
  lines <- c(lines, sprintf("\\textbf{Decision:} %s at $\\alpha = %.4f$", 
                           x$decision, x$alpha))
  lines <- c(lines, "")
  lines <- c(lines, "\\textbf{Interpretation:}")
  lines <- c(lines, "")
  lines <- c(lines, x$interpretation)
  lines <- c(lines, "")
  
  # Return combined text
  paste(lines, collapse = "\n")
}
