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
  which <- match.arg(which)
  ss <- x$sample.stats
  n <- ss$n
  mean_x <- ss$mean
  sd_x <- ss$sd
  df <- x$parameter
  t_stat <- x$statistic
  mu0 <- x$null.value
  alpha <- x$alpha

  # For data-based plots, we expect raw data stored in an attribute
  if (which %in% c("hist", "qq", "box")) {
    if (is.null(attr(x, "data"))) {
      stop("Raw data not stored in object; cannot draw this plot. ",
           "Use attr(result, 'data') <- x to attach the data.", call. = FALSE)
    }
  }

  if (which == "t") {
    # t-distribution with rejection region
    curve_x <- seq(-4, 4, length.out = 400)
    graphics::plot(
      curve_x,
      stats::dt(curve_x, df = df),
      type = "l",
      xlab = "t",
      ylab = "Density",
      main = "t-distribution with test statistic"
    )

    # critical values (two-sided)
    t_crit <- stats::qt(1 - alpha / 2, df = df)
    graphics::abline(v = c(-t_crit, t_crit), col = "red", lty = 2)
    graphics::abline(v = t_stat, col = "blue", lwd = 2)
    graphics::legend(
      "topright",
      legend = c("t density", "Critical values", "Observed t"),
      col = c("black", "red", "blue"),
      lty = c(1, 2, 1),
      lwd = c(1, 1, 2),
      bty = "n"
    )

  } else if (which == "hist") {
    y <- attr(x, "data")
    graphics::hist(
      y,
      breaks = "FD",
      col = "grey90",
      border = "white",
      xlab = "x",
      main = "Histogram of data"
    )
    graphics::abline(v = mean(y), col = "blue", lwd = 2)
    graphics::abline(v = mu0, col = "red", lty = 2, lwd = 2)
    graphics::legend(
      "topright",
      legend = c("Sample mean", "Hypothesized mean"),
      col = c("blue", "red"),
      lty = c(1, 2),
      lwd = 2,
      bty = "n"
    )

  } else if (which == "qq") {
    y <- attr(x, "data")
    stats::qqnorm(y, main = "Normal Q-Q plot")
    stats::qqline(y, col = "red", lwd = 2)

  } else if (which == "box") {
    y <- attr(x, "data")
    graphics::boxplot(y, horizontal = TRUE, main = "Boxplot of data")
    graphics::abline(v = mu0, col = "red", lty = 2, lwd = 2)

  } else if (which == "ci") {
    ci <- x$conf.int
    graphics::plot(
      1, mean_x,
      xlim = c(0.5, 1.5),
      ylim = range(ci),
      xaxt = "n",
      xlab = "",
      ylab = "Mean",
      main = "Confidence interval for the mean"
    )
    graphics::arrows(
      x0 = 1, y0 = ci[1L],
      x1 = 1, y1 = ci[2L],
      angle = 90, code = 3, length = 0.1
    )
    graphics::points(1, mean_x, pch = 19, col = "blue")
    graphics::abline(h = mu0, col = "red", lty = 2)
    graphics::axis(1, at = 1, labels = "mean")
  }

  invisible(x)
}
