#' Plotting Functions for One-Sample T-Test
#'
#' Functions to create visualizations of test results and diagnostic plots
#'
#' @name plotting
#' @keywords internal
NULL

#' Plot method for oneMeanTest objects
#'
#' Creates visualizations of one-sample t-test results
#'
#' @param x Object of class oneMeanTest
#' @param type Type of plot: "distribution", "ci", "diagnostic", or "all"
#' @param ... Additional arguments passed to plotting functions
#' @return Invisible NULL (plots are created as side effect)
#' @export
plot.oneMeanTest <- function(x, type = "distribution", ...) {
  if (!inherits(x, "oneMeanTest")) {
    stop("x must be an oneMeanTest object.", call. = FALSE)
  }
  
  type <- match.arg(type, c("distribution", "ci", "diagnostic", "all"))
  
  # Extract data from the object
  data_name <- x$data.name
  data_values <- eval(parse(text = data_name), envir = parent.frame(2))
  if (is.null(data_values) || length(data_values) == 0) {
    # Fallback: try to get from parent environments
    data_values <- tryCatch(
      get(data_name, envir = parent.frame(3)),
      error = function(e) {
        stop("Cannot retrieve original data. Please provide data directly to plotting functions.", 
             call. = FALSE)
      }
    )
  }
  
  if (type == "all") {
    oldpar <- graphics::par(mfrow = c(2, 2))
    on.exit(graphics::par(oldpar))
    
    plot_distribution(data_values, test_obj = x, ...)
    plot_ci(x, ...)
    plot_qq(data_values, ...)
    plot_boxplot(data_values, ...)
    
  } else if (type == "distribution") {
    plot_distribution(data_values, test_obj = x, ...)
  } else if (type == "ci") {
    plot_ci(x, ...)
  } else if (type == "diagnostic") {
    oldpar <- graphics::par(mfrow = c(1, 2))
    on.exit(graphics::par(oldpar))
    plot_qq(data_values, ...)
    plot_boxplot(data_values, ...)
  }
  
  invisible(NULL)
}


#' Plot data distribution with test information
#'
#' @param x oneMeanTest object or numeric vector
#' @param test_obj oneMeanTest object (optional, when x is numeric data)
#' @param ... Additional graphical parameters
#' @export
plot_distribution <- function(x, test_obj = NULL, ...) {
  # Handle both: plot_distribution(data) and plot_distribution(data, test_obj)
  # For backward compatibility with plot_distribution(oneMeanTest_obj)
  if (inherits(x, "oneMeanTest") && is.null(test_obj)) {
    test_obj <- x
    # Extract data from the object
    data_name <- test_obj$data.name
    data_values <- tryCatch(
      eval(parse(text = data_name), envir = parent.frame(2)),
      error = function(e) {
        tryCatch(
          get(data_name, envir = parent.frame(3)),
          error = function(e2) {
            stop("Cannot retrieve original data.", call. = FALSE)
          }
        )
      }
    )
  } else if (is.numeric(x)) {
    data_values <- x[!is.na(x)]
  } else {
    stop("x must be numeric or oneMeanTest object.", call. = FALSE)
  }
  
  # Create histogram
  graphics::hist(data_values, 
                 main = sprintf("Distribution of Sample Data (n = %d)", length(data_values)),
                 xlab = "Value",
                 col = "lightblue",
                 border = "white",
                 prob = TRUE,
                 ...)
  
  # Add density curve
  graphics::lines(stats::density(data_values, na.rm = TRUE), col = "darkblue", lwd = 2)
  
  # Add normal curve overlay
  xfit <- seq(min(data_values, na.rm = TRUE), 
              max(data_values, na.rm = TRUE), 
              length = 100)
  yfit <- stats::dnorm(xfit, mean = mean(data_values, na.rm = TRUE), 
                       sd = stats::sd(data_values, na.rm = TRUE))
  graphics::lines(xfit, yfit, col = "red", lwd = 2, lty = 2)
  
  # Add vertical lines and legend if test object provided
  if (!is.null(test_obj)) {
    graphics::abline(v = test_obj$estimate, col = "darkgreen", lwd = 2, lty = 1)
    graphics::abline(v = test_obj$null.value, col = "red", lwd = 2, lty = 3)
    
    # Add legend
    graphics::legend("topright", 
                     legend = c("Data density", "Normal curve", "Sample mean", "Null value"),
                     col = c("darkblue", "red", "darkgreen", "red"),
                     lty = c(1, 2, 1, 3),
                     lwd = 2,
                     cex = 0.8)
  } else {
    # Simple legend without test lines
    graphics::legend("topright", 
                     legend = c("Data density", "Normal curve"),
                     col = c("darkblue", "red"),
                     lty = c(1, 2),
                     lwd = 2,
                     cex = 0.8)
  }
  
  invisible(NULL)
}

#' Plot confidence interval
#'
#' @param x oneMeanTest object
#' @param ... Additional graphical parameters
#' @export
plot_ci <- function(x, ...) {
  if (!inherits(x, "oneMeanTest")) {
    stop("x must be an oneMeanTest object.", call. = FALSE)
  }
  
  ci <- x$conf.int
  estimate <- x$estimate
  mu0 <- x$null.value
  conf_level <- attr(ci, "conf.level")
  
  # Set up plot
  graphics::plot(1, estimate, 
                 xlim = c(0.5, 1.5),
                 ylim = range(c(ci, estimate, mu0)),
                 xlab = "",
                 ylab = "Value",
                 main = sprintf("%.0f%% Confidence Interval", conf_level * 100),
                 xaxt = "n",
                 pch = 19,
                 col = "darkblue",
                 cex = 1.5,
                 ...)
  
  # Draw CI
  graphics::arrows(1, ci[1], 1, ci[2], 
                   angle = 90, code = 3, length = 0.1, lwd = 2, col = "darkblue")
  
  # Add null value line
  graphics::abline(h = mu0, col = "red", lty = 2, lwd = 2)
  
  # Add labels
  graphics::text(1, ci[1], sprintf("%.4f", ci[1]), pos = 2, cex = 0.8)
  graphics::text(1, ci[2], sprintf("%.4f", ci[2]), pos = 2, cex = 0.8)
  graphics::text(1, estimate, sprintf("Mean: %.4f", estimate), pos = 4, cex = 0.8)
  graphics::text(1, mu0, sprintf("H0: %.4f", mu0), pos = 4, cex = 0.8, col = "red")
  
  # Decision
  decision_text <- if (x$decision == "reject H0") {
    "Reject H0"
  } else {
    "Fail to reject H0"
  }
  graphics::mtext(decision_text, side = 3, line = 0.5, cex = 0.9)
  
  invisible(NULL)
}

#' Q-Q plot for normality assessment
#'
#' @param x Numeric vector
#' @param ... Additional graphical parameters
#' @export
plot_qq <- function(x, ...) {
  x <- x[!is.na(x)]
  
  stats::qqnorm(x, main = "Normal Q-Q Plot", 
                pch = 19, col = "darkblue", ...)
  stats::qqline(x, col = "red", lwd = 2, lty = 2)
  
  invisible(NULL)
}

#' Histogram with normal curve
#'
#' @param x Numeric vector
#' @param ... Additional graphical parameters
#' @export
plot_histogram <- function(x, ...) {
  x <- x[!is.na(x)]
  
  graphics::hist(x, 
                 main = sprintf("Histogram with Normal Curve (n = %d)", length(x)),
                 xlab = "Value",
                 col = "lightblue",
                 border = "white",
                 prob = TRUE,
                 ...)
  
  # Add normal curve
  xfit <- seq(min(x), max(x), length = 100)
  yfit <- stats::dnorm(xfit, mean = mean(x), sd = stats::sd(x))
  graphics::lines(xfit, yfit, col = "red", lwd = 2)
  
  invisible(NULL)
}

#' Boxplot with outlier identification
#'
#' @param x Numeric vector
#' @param ... Additional graphical parameters
#' @export
plot_boxplot <- function(x, ...) {
  x <- x[!is.na(x)]
  
  graphics::boxplot(x, 
                    main = sprintf("Boxplot (n = %d)", length(x)),
                    ylab = "Value",
                    col = "lightblue",
                    border = "darkblue",
                    ...)
  
  # Add points for outliers
  bp <- graphics::boxplot(x, plot = FALSE)
  if (length(bp$out) > 0) {
    graphics::points(rep(1, length(bp$out)), bp$out, col = "red", pch = 19, cex = 1.2)
  }
  
  invisible(NULL)
}

#' Plot bootstrap distribution
#'
#' @param x oneMeanTest_bootstrap object
#' @param ... Additional graphical parameters
#' @export
plot.oneMeanTest_bootstrap <- function(x, ...) {
  if (!inherits(x, "oneMeanTest_bootstrap")) {
    stop("x must be an oneMeanTest_bootstrap object.", call. = FALSE)
  }
  
  oldpar <- graphics::par(mfrow = c(1, 2))
  on.exit(graphics::par(oldpar))
  
  # Plot bootstrap t-distribution
  graphics::hist(x$t.boot,
                 main = sprintf("Bootstrap t-distribution (B = %d)", length(x$t.boot)),
                 xlab = "t-statistic",
                 col = "lightblue",
                 border = "white",
                 prob = TRUE)
  graphics::abline(v = x$t.obs, col = "red", lwd = 2, lty = 2)
  graphics::legend("topright", "Observed t", col = "red", lty = 2, lwd = 2)
  
  # Plot bootstrap mean distribution
  graphics::hist(x$mean.boot,
                 main = sprintf("Bootstrap Mean Distribution (B = %d)", length(x$mean.boot)),
                 xlab = "Sample mean",
                 col = "lightgreen",
                 border = "white",
                 prob = TRUE)
  graphics::abline(v = x$mean.obs, col = "red", lwd = 2, lty = 2)
  graphics::abline(v = x$conf.int, col = "blue", lwd = 2, lty = 3)
  graphics::legend("topright", 
                   c("Observed mean", "95% CI"),
                   col = c("red", "blue"),
                   lty = c(2, 3),
                   lwd = 2)
  
  invisible(NULL)
}
