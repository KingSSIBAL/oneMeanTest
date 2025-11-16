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

  # Retrieve original data if available in assumptions (not ideal but simple)
  # If you want more control, you can store x$data directly in the result object.
  data_for_plots <- NULL
  if (!is.null(x$assumptions) && !is.null(x$assumptions$n)) {
    # user must supply raw data separately for plotting hist/qq/box if needed
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
    if (is.null(attr(x, "data"))) {
      stop("Raw data not stored in object; cannot draw histogram.", call. = FALSE)
    }
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
    if (is.null(attr(x, "data"))) {
      stop("Raw data not stored in object; cannot draw Q-Q plot.", call. = FALSE)
    }
    y <- attr(x, "data")
    stats::qqnorm(y, main = "Normal Q-Q plot")
    stats::qqline(y, col = "red", lwd = 2)

  } else if (which == "box") {
    if (is.null(attr(x, "data"))) {
      stop("Raw data not stored in object; cannot draw boxplot.", call. = FALSE)
    }
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
}
