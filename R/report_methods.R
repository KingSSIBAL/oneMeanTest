#' Report Method for oneMeanTest
#' 
#' @param x Object of class oneMeanTest
#' @param format Output format: "console", "markdown", or "latex"
#' @export
report <- function(x, format = "console", ...) {
  UseMethod("report")
}

#' @export
report.oneMeanTest <- function(x, format = "console", ...) {
  switch(format,
    console = .report_console(x, ...),
    markdown = .report_markdown(x, ...),
    latex = .report_latex(x, ...)
  )
}
