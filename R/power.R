#' Power Analysis Functions for One-Sample T-Test
#'
#' Functions to calculate statistical power, required sample size,
#' and detectable effect sizes for one-sample t-tests.
#'
#' @keywords power

#' Calculate power for one-sample t-test
#'
#' Computes the statistical power for a one-sample t-test given
#' sample size, effect size, and significance level.
#'
#' @param n Sample size
#' @param delta True difference from null value (effect in original units)
#' @param sd Standard deviation
#' @param alpha Significance level (default 0.05)
#' @param alternative Alternative hypothesis: "two.sided", "less", or "greater"
#'
#' @return Numeric power value (0 to 1)
#' @export
#'
#' @examples
#' # Power to detect effect of 0.5 SD with n = 30
#' power_t_test(n = 30, delta = 0.5, sd = 1, alpha = 0.05)
power_t_test <- function(
  n,
  delta,
  sd,
  alpha = 0.05,
  alternative = c("two.sided", "less", "greater")
) {
  alternative <- match.arg(alternative)

  if (!is.numeric(n) || length(n) != 1L || is.na(n)) {
    stop("`n` must be a single numeric value.", call. = FALSE)
  }
  if (n < 2) {
    stop("Sample size must be at least 2.", call. = FALSE)
  }
  if (!is.numeric(sd) || length(sd) != 1L || is.na(sd) || sd <= 0) {
    stop("Standard deviation must be a positive number.", call. = FALSE)
  }
  if (!is.numeric(alpha) || length(alpha) != 1L || is.na(alpha) ||
      alpha <= 0 || alpha >= 1) {
    stop("Alpha must be between 0 and 1.", call. = FALSE)
  }

  # Non-centrality parameter: delta / (sd / sqrt(n))
  ncp <- delta / (sd / sqrt(n))
  df  <- n - 1

  if (alternative == "two.sided") {
    t_crit <- stats::qt(1 - alpha / 2, df)
    power  <- stats::pt(-t_crit, df, ncp = ncp) +
      stats::pt(t_crit, df, ncp = ncp, lower.tail = FALSE)
  } else if (alternative == "greater") {
    t_crit <- stats::qt(1 - alpha, df)
    power  <- stats::pt(t_crit, df, ncp = ncp, lower.tail = FALSE)
  } else { # "less"
    t_crit <- stats::qt(alpha, df)
    power  <- stats::pt(t_crit, df, ncp = ncp, lower.tail = TRUE)
  }

  power
}

#' Calculate required sample size for desired power
#'
#' Determines the sample size needed to achieve specified power.
#'
#' @param power Desired power (default 0.80)
#' @param delta True difference from null value
#' @param sd Standard deviation
#' @param alpha Significance level (default 0.05)
#' @param alternative Alternative hypothesis
#'
#' @return Required sample size (integer)
#' @export
#'
#' @examples
#' # Sample size needed for 80% power to detect effect of 0.5 SD
#' sample_size_t_test(power = 0.80, delta = 0.5, sd = 1)
sample_size_t_test <- function(
  power = 0.80,
  delta,
  sd,
  alpha = 0.05,
  alternative = c("two.sided", "less", "greater")
) {
  alternative <- match.arg(alternative)

  if (!is.numeric(power) || length(power) != 1L || is.na(power) ||
      power <= 0 || power >= 1) {
    stop("Power must be between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(sd) || length(sd) != 1L || is.na(sd) || sd <= 0) {
    stop("Standard deviation must be a positive number.", call. = FALSE)
  }
  if (!is.numeric(delta) || length(delta) != 1L || is.na(delta) || delta == 0) {
    stop("Effect size (delta) must be a non-zero numeric value.", call. = FALSE)
  }

  n_lower   <- 2L
  n_upper   <- 10000L
  tolerance <- 0.01

  while (n_upper - n_lower > 1L) {
    n_mid    <- ceiling((n_lower + n_upper) / 2)
    power_mid <- power_t_test(n_mid, delta, sd, alpha, alternative)

    if (abs(power_mid - power) < tolerance) {
      return(as.integer(n_mid))
    }

    if (power_mid < power) {
      n_lower <- n_mid
    } else {
      n_upper <- n_mid
    }
  }

  as.integer(n_upper)
}

#' Calculate detectable effect size for given power
#'
#' Determines the effect size that can be detected with specified power.
#'
#' @param n Sample size
#' @param power Desired power (default 0.80)
#' @param sd Standard deviation
#' @param alpha Significance level (default 0.05)
#' @param alternative Alternative hypothesis
#'
#' @return Detectable effect size (in original units)
#' @export
#'
#' @examples
#' # Effect size detectable with 80% power and n = 30
#' effect_size_t_test(n = 30, power = 0.80, sd = 1)
effect_size_t_test <- function(
  n,
  power = 0.80,
  sd,
  alpha = 0.05,
  alternative = c("two.sided", "less", "greater")
) {
  alternative <- match.arg(alternative)

  if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 2) {
    stop("Sample size must be at least 2.", call. = FALSE)
  }
  if (!is.numeric(power) || length(power) != 1L || is.na(power) ||
      power <= 0 || power >= 1) {
    stop("Power must be between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(sd) || length(sd) != 1L || is.na(sd) || sd <= 0) {
    stop("Standard deviation must be a positive number.", call. = FALSE)
  }

  delta_lower <- 0.01 * sd
  delta_upper <- 10 * sd
  tolerance   <- 0.001

  while (delta_upper - delta_lower > tolerance) {
    delta_mid  <- (delta_lower + delta_upper) / 2
    power_mid  <- power_t_test(n, delta_mid, sd, alpha, alternative)

    if (abs(power_mid - power) < 0.01) {
      return(delta_mid)
    }

    if (power_mid < power) {
      delta_lower <- delta_mid
    } else {
      delta_upper <- delta_mid
    }
  }

  (delta_lower + delta_upper) / 2
}

#' Plot power curve
#'
#' Creates a plot showing how power changes with sample size.
#'
#' @param delta Effect size
#' @param sd Standard deviation
#' @param alpha Significance level (default 0.05)
#' @param alternative Alternative hypothesis
#' @param n_range Range of sample sizes to plot (default 10 to 100)
#'
#' @return Invisible data frame with n and power values
#' @export
plot_power_curve <- function(
  delta,
  sd,
  alpha = 0.05,
  alternative = c("two.sided", "less", "greater"),
  n_range = 10:100
) {
  alternative <- match.arg(alternative)

  if (!is.numeric(n_range) || anyNA(n_range) || length(n_range) < 1L) {
    stop("`n_range` must be a numeric vector of sample sizes.", call. = FALSE)
  }
  n_range <- as.integer(n_range[n_range >= 2L])
  if (length(n_range) == 0L) {
    stop("`n_range` must contain values of at least 2.", call. = FALSE)
  }

  power_values <- vapply(
    n_range,
    function(n) power_t_test(n, delta, sd, alpha, alternative),
    numeric(1L)
  )

  graphics::plot(
    n_range, power_values,
    type = "l",
    lwd  = 2,
    col  = "darkblue",
    xlab = "Sample Size (n)",
    ylab = "Statistical Power",
    main = sprintf(
      "Power Curve (delta = %.2f, sd = %.2f, alpha = %.3f)",
      delta, sd, alpha
    ),
    ylim = c(0, 1)
  )

  graphics::abline(h = 0.80, col = "red",    lty = 2, lwd = 1)
  graphics::abline(h = 0.90, col = "orange", lty = 2, lwd = 1)
  graphics::grid()
  graphics::legend(
    "bottomright",
    legend = c("Power", "80% Power", "90% Power"),
    col    = c("darkblue", "red", "orange"),
    lty    = c(1, 2, 2),
    lwd    = c(2, 1, 1)
  )

  invisible(data.frame(n = n_range, power = power_values))
}
