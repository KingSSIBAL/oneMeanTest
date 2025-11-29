# Plot methods for oneMeanTest objects

#' Plot methods for one-sample mean test objects
#'
#' Plot methods for objects of class \code{"oneMeanTest"} produced by
#' \code{\link{one_mean_test}}. Different values of \code{which} produce
#' different visualizations:
#'
#' \itemize{
#'   \item \code{"t"}: t-distribution with the observed test statistic and
#'     critical values for the chosen \code{alpha}.
#'   \item \code{"hist"}: histogram of the data with vertical lines for the
#'     sample mean and hypothesized mean \code{mu0}.
#'   \item \code{"qq"}: normal Q-Q plot of the data with a reference line.
#'   \item \code{"box"}: boxplot of the data with a vertical line at \code{mu0}.
#'   \item \code{"ci"}: confidence interval plot for the mean.
#' }
#'
#' For \code{"hist"}, \code{"qq"}, and \code{"box"}, the raw data must be
#' stored in the object via \code{attr(x, "data") <- your_data}.
#'
#' @param x An object of class \code{"oneMeanTest"}.
#' @param which Character string specifying which plot to draw. One of
#'   \code{"t"}, \code{"hist"}, \code{"qq"}, \code{"box"}, or \code{"ci"}.
#' @param ... Additional arguments passed on to the underlying graphics
#'   functions (currently unused).
#'
#' @return Invisibly returns \code{x} after producing a plot.
#'
#' @examples
#' set.seed(123)
#' x <- rnorm(30, mean = 5, sd = 2)
#' res <- one_mean_test(x, mu0 = 5, check_assumptions = FALSE)
#' attr(res, "data") <- x
#'
#' plot(res, which = "t")
#' plot(res, which = "ci")
#' plot(res, which = "hist")
#' plot(res, which = "qq")
#' plot(res, which = "box")
#'
#' @export
plot.oneMeanTest <- function(x, which = c("t", "hist", "qq", "box", "ci"), ...) {
  # Match plot type argument
  which <- match.arg(which)
  
  # Extract key statistics from the test result object
  ss <- x$sample.stats   # Sample statistics (n, mean, sd, se)
  n <- ss$n              # Sample size
  mean_x <- ss$mean      # Sample mean
  sd_x <- ss$sd          # Sample standard deviation
  df <- x$parameter      # Degrees of freedom
  t_stat <- x$statistic  # Observed t-statistic
  mu0 <- x$null.value    # Hypothesized mean
  alpha <- x$alpha       # Significance level

  # ============================================================
  # CHECK FOR RAW DATA (needed for some plots)
  # ============================================================
  
  # Some plots require the original data, not just summary statistics
  # Check if data is attached as an attribute
  if (which %in% c("hist", "qq", "box")) {
    if (is.null(attr(x, "data"))) {
      stop("Raw data not stored in object; cannot draw this plot. ",
           "Use attr(result, 'data') <- x to attach the data.", call. = FALSE)
    }
  }

  # ============================================================
  # PLOT: T-DISTRIBUTION WITH TEST STATISTIC
  # ============================================================
  
  if (which == "t") {
    # Create sequence of t-values for plotting the density curve
    # Range from -4 to 4 covers most of the distribution
    curve_x <- seq(-4, 4, length.out = 400)
    
    # Plot the t-distribution density curve
    # This shows the theoretical distribution under H0
    graphics::plot(
      curve_x,
      stats::dt(curve_x, df = df),  # t-density with df degrees of freedom
      type = "l",                     # Line plot
      xlab = "t",
      ylab = "Density",
      main = "t-distribution with test statistic"
    )

    # Add vertical lines for critical values (rejection region boundaries)
    # For two-sided test: both positive and negative critical values
    t_crit <- stats::qt(1 - alpha / 2, df = df)
    graphics::abline(v = c(-t_crit, t_crit), col = "red", lty = 2)
    
    # Add vertical line for observed t-statistic
    # This shows where our sample falls relative to the null distribution
    graphics::abline(v = t_stat, col = "blue", lwd = 2)
    
    # Add legend to identify the lines
    graphics::legend(
      "topright",
      legend = c("t density", "Critical values", "Observed t"),
      col = c("black", "red", "blue"),
      lty = c(1, 2, 1),
      lwd = c(1, 1, 2),
      bty = "n"
    )

  # ============================================================
  # PLOT: HISTOGRAM OF DATA
  # ============================================================
    
  } else if (which == "hist") {
    # Get raw data from attribute
    y <- attr(x, "data")
    
    # Create histogram with automatic binning (Freedman-Diaconis rule)
    graphics::hist(
      y,
      breaks = "FD",        # Freedman-Diaconis binning rule
      col = "grey90",       # Light grey bars
      border = "white",     # White borders between bars
      xlab = "x",
      main = "Histogram of data"
    )
    
    # Add vertical line for sample mean (blue)
    graphics::abline(v = mean(y), col = "blue", lwd = 2)
    
    # Add vertical line for hypothesized mean (red, dashed)
    # This shows how the data center compares to H0
    graphics::abline(v = mu0, col = "red", lty = 2, lwd = 2)
    
    # Add legend
    graphics::legend(
      "topright",
      legend = c("Sample mean", "Hypothesized mean"),
      col = c("blue", "red"),
      lty = c(1, 2),
      lwd = 2,
      bty = "n"
    )

  # ============================================================
  # PLOT: Q-Q PLOT (NORMALITY CHECK)
  # ============================================================
    
  } else if (which == "qq") {
    # Q-Q plot compares sample quantiles to theoretical normal quantiles
    # Points should fall on reference line if data are normally distributed
    y <- attr(x, "data")
    
    # Create Q-Q plot
    stats::qqnorm(y, main = "Normal Q-Q plot")
    
    # Add reference line (expected pattern under normality)
    # Line passes through Q1 and Q3 of both distributions
    stats::qqline(y, col = "red", lwd = 2)

  # ============================================================
  # PLOT: BOXPLOT
  # ============================================================
    
  } else if (which == "box") {
    # Boxplot shows distribution shape and identifies outliers
    y <- attr(x, "data")
    
    # Create horizontal boxplot
    graphics::boxplot(y, horizontal = TRUE, main = "Boxplot of data")
    
    # Add vertical line for hypothesized mean
    # Shows where H0 falls relative to data distribution
    graphics::abline(v = mu0, col = "red", lty = 2, lwd = 2)

  # ============================================================
  # PLOT: CONFIDENCE INTERVAL
  # ============================================================
    
  } else if (which == "ci") {
    # Visual representation of confidence interval
    ci <- x$conf.int
    
    # Create plot with single point at sample mean
    # Y-axis limits set to span the confidence interval
    graphics::plot(
      1, mean_x,
      xlim = c(0.5, 1.5),
      ylim = range(ci),
      xaxt = "n",              # Suppress x-axis (not meaningful)
      xlab = "",
      ylab = "Mean",
      main = "Confidence interval for the mean"
    )
    
    # Draw error bar representing the confidence interval
    # Two-headed arrow from lower to upper bound
    graphics::arrows(
      x0 = 1, y0 = ci[1L],   # Start at lower bound
      x1 = 1, y1 = ci[2L],   # End at upper bound
      angle = 90,            # 90-degree arrow heads
      code = 3,              # Arrow heads on both ends
      length = 0.1           # Arrow head size
    )
    
    # Plot sample mean as a point
    graphics::points(1, mean_x, pch = 19, col = "blue")
    
    # Add horizontal line for hypothesized mean
    # Shows if mu0 falls within the confidence interval
    graphics::abline(h = mu0, col = "red", lty = 2)
    
    # Add simple x-axis label
    graphics::axis(1, at = 1, labels = "mean")
  }

  # Return the input object invisibly (allows chaining)
  invisible(x)
}
