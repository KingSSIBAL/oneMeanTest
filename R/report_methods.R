#' Report Methods for One-Sample T-Test
#'
#' Functions to generate formatted reports of test results
#' in various formats (text, markdown, LaTeX)

#' Generate report from test results
#'
#' Creates a formatted report of one-sample t-test results
#'
#' @param x Object of class oneMeanTest
#' @param format Output format: "text", "markdown", or "latex"
#' @param file Optional file path to save report
#' @param ... Additional arguments
#' @return Character string with formatted report (invisible if file specified)
#' @export
report <- function(x, format = c("text", "markdown", "latex"), file = NULL, ...) {
  UseMethod("report")
}

#' @export
report.oneMeanTest <- function(x, format = c("text", "markdown", "latex"), 
                                file = NULL, ...) {
  format <- match.arg(format)
  
  # Generate report based on format
  report_text <- switch(format,
                        "text" = format_ttest_report(x),
                        "markdown" = .format_markdown_report(x),
                        "latex" = .format_latex_report(x))
  
  # Save to file if specified
  if (!is.null(file)) {
    writeLines(report_text, file)
    message(sprintf("Report saved to: %s", file))
    return(invisible(report_text))
  }
  
  report_text
}

#' Format report as Markdown
#'
#' @param x oneMeanTest object
#' @return Markdown-formatted report
#' @keywords internal
#' @noRd
.format_markdown_report <- function(x) {
  lines <- character()
  
  # Header
  lines <- c(lines, "# One-Sample T-Test Report")
  lines <- c(lines, "")
  
  # Descriptive Statistics
  lines <- c(lines, "## Descriptive Statistics")
  lines <- c(lines, "")
  lines <- c(lines, sprintf("- **Sample size (n):** %d", x$sample.stats$n))
  lines <- c(lines, sprintf("- **Sample mean:** %.4f", x$sample.stats$mean))
  lines <- c(lines, sprintf("- **Standard deviation:** %.4f", x$sample.stats$sd))
  lines <- c(lines, sprintf("- **Standard error:** %.4f", x$sample.stats$se))
  lines <- c(lines, "")
  
  # Hypotheses
  lines <- c(lines, "## Hypotheses")
  lines <- c(lines, "")
  lines <- c(lines, sprintf("- **H0:** mu = %.4f", x$null.value))
  
  alt_text <- switch(x$alternative,
                     "two.sided" = sprintf("mu != %.4f", x$null.value),
                     "greater" = sprintf("mu > %.4f", x$null.value),
                     "less" = sprintf("mu < %.4f", x$null.value))
  lines <- c(lines, sprintf("- **Ha:** %s", alt_text))
  lines <- c(lines, sprintf("- **Significance level:** alpha = %.4f", x$alpha))
  lines <- c(lines, "")
  
  # Test Statistics
  lines <- c(lines, "## Test Statistics")
  lines <- c(lines, "")
  lines <- c(lines, sprintf("- **Test statistic (t):** %.4f", x$statistic))
  lines <- c(lines, sprintf("- **Degrees of freedom:** %d", x$parameter))
  lines <- c(lines, sprintf("- **P-value:** %.4f", x$p.value))
  lines <- c(lines, "")
  
  # Confidence Interval
  ci <- x$conf.int
  cl <- attr(ci, "conf.level") * 100
  lines <- c(lines, sprintf("## %.0f%% Confidence Interval", cl))
  lines <- c(lines, "")
  lines <- c(lines, sprintf("[%.4f, %.4f]", ci[1], ci[2]))
  lines <- c(lines, "")
  
  # Decision
  lines <- c(lines, "## Decision")
  lines <- c(lines, "")
  lines <- c(lines, sprintf("**%s** at alpha = %.4f", x$decision, x$alpha))
  lines <- c(lines, "")
  
  # Interpretation
  lines <- c(lines, "## Interpretation")
  lines <- c(lines, "")
  lines <- c(lines, x$interpretation)
  lines <- c(lines, "")
  
  paste(lines, collapse = "\n")
}

#' Format report as LaTeX
#'
#' @param x oneMeanTest object
#' @return LaTeX-formatted report
#' @keywords internal
#' @noRd
.format_latex_report <- function(x) {
  lines <- character()
  
  lines <- c(lines, "\\section{One-Sample T-Test Report}")
  lines <- c(lines, "")
  lines <- c(lines, "\\subsection{Descriptive Statistics}")
  lines <- c(lines, "\\begin{itemize}")
  lines <- c(lines, sprintf("  \\item Sample size ($n$): %d", x$sample.stats$n))
  lines <- c(lines, sprintf("  \\item Sample mean ($\\bar{x}$): %.4f", x$sample.stats$mean))
  lines <- c(lines, sprintf("  \\item Standard deviation ($s$): %.4f", x$sample.stats$sd))
  lines <- c(lines, sprintf("  \\item Standard error: %.4f", x$sample.stats$se))
  lines <- c(lines, "\\end{itemize}")
  lines <- c(lines, "")
  
  lines <- c(lines, "\\subsection{Hypotheses}")
  lines <- c(lines, "\\begin{align*}")
  lines <- c(lines, sprintf("  H_0:& \\mu = %.4f \\\\", x$null.value))
  
  alt_symbol <- switch(x$alternative,
                       "two.sided" = "\\neq",
                       "greater" = ">",
                       "less" = "<")
  lines <- c(lines, sprintf("  H_a:& \\mu %s %.4f", alt_symbol, x$null.value))
  lines <- c(lines, "\\end{align*}")
  lines <- c(lines, "")
  
  lines <- c(lines, "\\subsection{Test Statistics}")
  lines <- c(lines, "\\begin{itemize}")
  lines <- c(lines, sprintf("  \\item Test statistic: $t = %.4f$", x$statistic))
  lines <- c(lines, sprintf("  \\item Degrees of freedom: $df = %d$", x$parameter))
  lines <- c(lines, sprintf("  \\item $p$-value: %.4f", x$p.value))
  lines <- c(lines, "\\end{itemize}")
  lines <- c(lines, "")
  
  ci <- x$conf.int
  cl <- attr(ci, "conf.level") * 100
  lines <- c(lines, sprintf("\\subsection{%.0f\\%% Confidence Interval}", cl))
  lines <- c(lines, sprintf("$[%.4f, %.4f]$", ci[1], ci[2]))
  lines <- c(lines, "")
  
  lines <- c(lines, "\\subsection{Decision}")
  lines <- c(lines, sprintf("\\textbf{%s} at $\\alpha = %.4f$", x$decision, x$alpha))
  lines <- c(lines, "")
  
  paste(lines, collapse = "\n")
}

#' @export
summary.oneMeanTest <- function(object, ...) {
  cat("\nOne-Sample T-Test Summary\n")
  cat(strrep("=", 50), "\n\n")
  
  cat(sprintf("Sample: %s\n", object$data.name))
  cat(sprintf("n = %d, mean = %.4f, sd = %.4f\n\n",
              object$sample.stats$n,
              object$sample.stats$mean,
              object$sample.stats$sd))
  
  cat(sprintf("H0: mu = %.4f\n", object$null.value))
  cat(sprintf("Ha: mu %s %.4f\n\n",
              switch(object$alternative,
                     "two.sided" = "!=",
                     "greater" = ">",
                     "less" = "<"),
              object$null.value))
  
  cat(sprintf("t = %.4f, df = %d, p-value = %.4f\n",
              object$statistic,
              object$parameter,
              object$p.value))
  
  ci <- object$conf.int
  cl <- attr(ci, "conf.level")
  cat(sprintf("%.0f%% CI: [%.4f, %.4f]\n\n", cl * 100, ci[1], ci[2]))
  
  cat(sprintf("Decision: %s (alpha = %.4f)\n", object$decision, object$alpha))
  
  invisible(object)
}
