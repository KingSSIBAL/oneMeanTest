#' ggplot2 Visualization Functions for One-Sample T-Test
#'
#' Enhanced plotting functions using ggplot2 for publication-quality graphics
#'
#' @name ggplot_visualizations
NULL

# Declare global variables for R CMD check
utils::globalVariables(c(
  ".data", "x", "density", "t_stat", "mean", "group", "lower", "upper",
  "Measure", "Estimate", "CI_Lower", "CI_Upper", "Test", "P_adjusted",
  "Significant", "n", "power", "sample"
))


#' Check if ggplot2 is available
#' @keywords internal
#' @noRd
.check_ggplot2 <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for this function.\n",
         "Install it with: install.packages('ggplot2')", 
         call. = FALSE)
  }
}

#' ggplot2 method for oneMeanTest objects
#'
#' Creates ggplot2 visualizations of test results
#'
#' @param data Object of class oneMeanTest
#' @param type Type of plot: "distribution", "ci", "qq", "diagnostic"
#' @param theme ggplot2 theme to use (default theme_minimal)
#' @param ... Additional arguments
#' @return ggplot object
#' @export
#' @examples
#' \dontrun{
#' library(ggplot2)
#' x <- rnorm(30, mean = 5, sd = 2)
#' result <- one_mean_test(x, mu0 = 5)
#' autoplot(result, type = "distribution")
#' }
autoplot.oneMeanTest <- function(data, 
                                  type = c("distribution", "ci", "qq", "diagnostic"),
                                  theme = ggplot2::theme_minimal(),
                                  ...) {
  .check_ggplot2()
  type <- match.arg(type)
  
  # Extract data
  data_name <- data$data.name
  x_data <- eval(parse(text = data_name), envir = parent.frame(2))
  
  if (type == "distribution") {
    ggplot_distribution(x_data, data, theme)
  } else if (type == "ci") {
    ggplot_ci(data, theme)
  } else if (type == "qq") {
    ggplot_qq(x_data, theme)
  } else if (type == "diagnostic") {
    ggplot_diagnostic(x_data, data, theme)
  }
}

#' Distribution plot with ggplot2
#'
#' @param x Numeric vector
#' @param test_result oneMeanTest object (optional)
#' @param theme ggplot2 theme
#' @return ggplot object
#' @export
ggplot_distribution <- function(x, test_result = NULL, 
                                theme = ggplot2::theme_minimal()) {
  .check_ggplot2()
  
  df <- data.frame(x = x[!is.na(x)])
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$x)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                            bins = 30, 
                            fill = "lightblue", 
                            color = "white", 
                            alpha = 0.7) +
    ggplot2::geom_density(color = "darkblue", linewidth = 1.2) +
    ggplot2::labs(
      title = "Distribution of Data",
      subtitle = if (!is.null(test_result)) {
        sprintf("Sample: n=%d, mean=%.2f, sd=%.2f", 
                test_result$sample.stats$n,
                test_result$sample.stats$mean,
                test_result$sample.stats$sd)
      } else NULL,
      x = "Value",
      y = "Density"
    ) +
    theme +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(size = 10)
    )
  
  # Add normal curve overlay
  mean_x <- mean(df$x, na.rm = TRUE)
  sd_x <- stats::sd(df$x, na.rm = TRUE)
  
  p <- p + ggplot2::stat_function(
    fun = stats::dnorm,
    args = list(mean = mean_x, sd = sd_x),
    color = "red",
    linetype = "dashed",
    linewidth = 1
  )
  
  # Add vertical lines
  if (!is.null(test_result)) {
    p <- p +
      ggplot2::geom_vline(xintercept = test_result$estimate, 
                          color = "darkgreen", linewidth = 1, linetype = "solid") +
      ggplot2::geom_vline(xintercept = test_result$null.value, 
                          color = "red", linewidth = 1, linetype = "dotted") +
      ggplot2::annotate("text", 
                        x = test_result$estimate, 
                        y = Inf, 
                        label = sprintf("Mean: %.2f", test_result$estimate),
                        vjust = 2, hjust = 1.1, color = "darkgreen", size = 3.5) +
      ggplot2::annotate("text", 
                        x = test_result$null.value, 
                        y = Inf, 
                        label = sprintf("H0: %.2f", test_result$null.value),
                        vjust = 2, hjust = -0.1, color = "red", size = 3.5)
  }
  
  p
}

#' Confidence interval plot with ggplot2
#'
#' @param test_result oneMeanTest object
#' @param theme ggplot2 theme
#' @return ggplot object
#' @export
ggplot_ci <- function(test_result, theme = ggplot2::theme_minimal()) {
  .check_ggplot2()
  
  ci <- test_result$conf.int
  estimate <- test_result$estimate
  mu0 <- test_result$null.value
  conf_level <- attr(ci, "conf.level")
  
  df <- data.frame(
    x = 1,
    estimate = estimate,
    lower = ci[1],
    upper = ci[2]
  )
  
  # Determine if CI contains mu0
  contains_mu0 <- (mu0 >= ci[1] && mu0 <= ci[2])
  ci_color <- if (contains_mu0) "blue" else "red"
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$x, y = .data$estimate)) +
    ggplot2::geom_hline(yintercept = mu0, 
                        color = "red", 
                        linetype = "dashed", 
                        linewidth = 1) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
                           width = 0.2,
                           linewidth = 1.5,
                           color = ci_color) +
    ggplot2::geom_point(size = 4, color = ci_color) +
    ggplot2::labs(
      title = sprintf("%.0f%% Confidence Interval", conf_level * 100),
      subtitle = sprintf("Decision: %s", test_result$decision),
      x = "",
      y = "Value"
    ) +
    ggplot2::xlim(0.5, 1.5) +
    ggplot2::ylim(min(ci[1], mu0) - abs(ci[1] - ci[2]) * 0.1,
                  max(ci[2], mu0) + abs(ci[1] - ci[2]) * 0.1) +
    theme +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", size = 14)
    ) +
    ggplot2::annotate("text", x = 1.15, y = mu0, 
                      label = sprintf("H0: mu = %.2f", mu0),
                      hjust = 0, color = "red", size = 4) +
    ggplot2::annotate("text", x = 1.15, y = estimate,
                      label = sprintf("x-bar = %.2f", estimate),
                      hjust = 0, color = ci_color, size = 4) +
    ggplot2::annotate("text", x = 1.15, y = ci[1],
                      label = sprintf("%.2f", ci[1]),
                      hjust = 0, size = 3) +
    ggplot2::annotate("text", x = 1.15, y = ci[2],
                      label = sprintf("%.2f", ci[2]),
                      hjust = 0, size = 3)
  
  p
}

#' Q-Q plot with ggplot2
#'
#' @param x Numeric vector
#' @param theme ggplot2 theme
#' @return ggplot object
#' @export
ggplot_qq <- function(x, theme = ggplot2::theme_minimal()) {
  .check_ggplot2()
  
  x_clean <- x[!is.na(x)]
  df <- data.frame(sample = x_clean)
  
  p <- ggplot2::ggplot(df, ggplot2::aes(sample = .data$sample)) +
    ggplot2::geom_qq(color = "darkblue", size = 2, alpha = 0.6) +
    ggplot2::geom_qq_line(color = "red", linewidth = 1, linetype = "dashed") +
    ggplot2::labs(
      title = "Normal Q-Q Plot",
      subtitle = "Assessing normality assumption",
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    ) +
    theme +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14)
    )
  
  p
}

#' Boxplot with ggplot2
#'
#' @param x Numeric vector
#' @param mu0 Null hypothesis value (optional)
#' @param theme ggplot2 theme
#' @return ggplot object
#' @export
ggplot_boxplot <- function(x, mu0 = NULL, theme = ggplot2::theme_minimal()) {
  .check_ggplot2()
  
  x_clean <- x[!is.na(x)]
  df <- data.frame(x = x_clean, group = "Data")
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$group, y = .data$x)) +
    ggplot2::geom_boxplot(fill = "lightblue", 
                          color = "darkblue", 
                          alpha = 0.7,
                          outlier.color = "red",
                          outlier.size = 3) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Boxplot",
      subtitle = "Outliers shown in red",
      x = "",
      y = "Value"
    ) +
    theme +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", size = 14)
    )
  
  if (!is.null(mu0)) {
    p <- p + ggplot2::geom_hline(yintercept = mu0, 
                                  color = "red", 
                                  linetype = "dashed",
                                  linewidth = 1) +
      ggplot2::annotate("text", x = 1, y = mu0,
                        label = sprintf("mu0 = %.2f", mu0),
                        hjust = -0.1, vjust = -0.5, color = "red")
  }
  
  p
}

#' Diagnostic plot panel with ggplot2
#'
#' @param x Numeric vector
#' @param test_result oneMeanTest object (optional)
#' @param theme ggplot2 theme
#' @return ggplot object (patchwork combined if available)
#' @export
ggplot_diagnostic <- function(x, test_result = NULL, 
                               theme = ggplot2::theme_minimal()) {
  .check_ggplot2()
  
  # Create individual plots
  p1 <- ggplot_distribution(x, test_result, theme)
  p2 <- ggplot_qq(x, theme)
  p3 <- ggplot_boxplot(x, 
                       if (!is.null(test_result)) test_result$null.value else NULL,
                       theme)
  
  # Try to use patchwork if available
  if (requireNamespace("patchwork", quietly = TRUE)) {
    combined <- (p1 | p2) / p3 +
      patchwork::plot_annotation(
        title = "Diagnostic Plots",
        theme = ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 16))
      )
    return(combined)
  } else {
    # Return list if patchwork not available
    message("Install 'patchwork' package for combined diagnostic plot layout")
    return(list(distribution = p1, qq = p2, boxplot = p3))
  }
}

#' Power curve plot with ggplot2
#'
#' @param delta Effect size
#' @param sd Standard deviation
#' @param alpha Significance level
#' @param n_range Range of sample sizes
#' @param theme ggplot2 theme
#' @return ggplot object
#' @export
ggplot_power_curve <- function(delta, sd, alpha = 0.05, 
                                n_range = 10:100,
                                theme = ggplot2::theme_minimal()) {
  .check_ggplot2()
  
  # Calculate power for each n
  power_values <- sapply(n_range, function(n) {
    power_t_test(n, delta, sd, alpha)
  })
  
  df <- data.frame(
    n = n_range,
    power = power_values
  )
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$n, y = .data$power)) +
    ggplot2::geom_line(color = "darkblue", linewidth = 1.5) +
    ggplot2::geom_hline(yintercept = 0.80, 
                        color = "red", 
                        linetype = "dashed",
                        linewidth = 1) +
    ggplot2::geom_hline(yintercept = 0.90, 
                        color = "orange", 
                        linetype = "dashed",
                        linewidth = 1) +
    ggplot2::labs(
      title = "Power Curve",
      subtitle = sprintf("Effect size: delta=%.2f, SD=%.2f, alpha=%.3f", delta, sd, alpha),
      x = "Sample Size (n)",
      y = "Statistical Power"
    ) +
    ggplot2::ylim(0, 1) +
    theme +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14)
    ) +
    ggplot2::annotate("text", x = max(n_range), y = 0.80,
                      label = "80% Power", hjust = 1.1, vjust = -0.5, color = "red") +
    ggplot2::annotate("text", x = max(n_range), y = 0.90,
                      label = "90% Power", hjust = 1.1, vjust = -0.5, color = "orange")
  
  p
}

#' Effect size comparison plot
#'
#' @param effect_size_summary Effect size summary object
#' @param theme ggplot2 theme
#' @return ggplot object
#' @export
ggplot_effect_sizes <- function(effect_size_summary, 
                                 theme = ggplot2::theme_minimal()) {
  .check_ggplot2()
  
  df <- effect_size_summary
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$Measure, y = .data$Estimate)) +
    ggplot2::geom_point(size = 4, color = "darkblue") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$CI_Lower, ymax = .data$CI_Upper),
                           width = 0.2,
                           linewidth = 1,
                           color = "darkblue") +
    ggplot2::geom_hline(yintercept = 0, 
                        linetype = "dashed", 
                        color = "gray50") +
    ggplot2::labs(
      title = "Effect Size Estimates",
      subtitle = sprintf("%.0f%% Confidence Intervals", 
                        attr(effect_size_summary, "conf.level") * 100),
      x = "",
      y = "Effect Size"
    ) +
    theme +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14)
    ) +
    ggplot2::coord_flip()
  
  p
}

#' Bootstrap distribution plot
#'
#' @param bootstrap_result oneMeanTest_bootstrap object
#' @param theme ggplot2 theme
#' @return ggplot object
#' @export
ggplot_bootstrap <- function(bootstrap_result, theme = ggplot2::theme_minimal()) {
  .check_ggplot2()
  
  df_t <- data.frame(t_stat = bootstrap_result$t.boot)
  df_mean <- data.frame(mean = bootstrap_result$mean.boot)
  
  p1 <- ggplot2::ggplot(df_t, ggplot2::aes(x = .data$t_stat)) +
    ggplot2::geom_histogram(bins = 50, 
                            fill = "lightblue", 
                            color = "white",
                            alpha = 0.7) +
    ggplot2::geom_vline(xintercept = bootstrap_result$t.obs,
                        color = "red",
                        linewidth = 1.2,
                        linetype = "dashed") +
    ggplot2::labs(
      title = "Bootstrap t-distribution",
      x = "t-statistic",
      y = "Frequency"
    ) +
    theme
  
  p2 <- ggplot2::ggplot(df_mean, ggplot2::aes(x = .data$mean)) +
    ggplot2::geom_histogram(bins = 50,
                            fill = "lightgreen",
                            color = "white",
                            alpha = 0.7) +
    ggplot2::geom_vline(xintercept = bootstrap_result$mean.obs,
                        color = "red",
                        linewidth = 1.2,
                        linetype = "dashed") +
    ggplot2::geom_vline(xintercept = bootstrap_result$conf.int,
                        color = "blue",
                        linewidth = 1,
                        linetype = "dotted") +
    ggplot2::labs(
      title = "Bootstrap mean distribution",
      x = "Sample mean",
      y = "Frequency"
    ) +
    theme
  
  if (requireNamespace("patchwork", quietly = TRUE)) {
    p1 + p2
  } else {
    list(t_distribution = p1, mean_distribution = p2)
  }
}

#' Multiple tests comparison plot
#'
#' @param multiple_tests_result multiple_t_tests object
#' @param theme ggplot2 theme
#' @return ggplot object
#' @export
ggplot_multiple_tests <- function(multiple_tests_result, 
                                   theme = ggplot2::theme_minimal()) {
  .check_ggplot2()
  
  df <- multiple_tests_result$summary
  df$Test <- factor(df$Test, levels = rev(df$Test))
  df$Significant <- ifelse(df$Reject, "Yes", "No")
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$Test, y = .data$P_adjusted, 
                                         color = .data$Significant)) +
    ggplot2::geom_point(size = 4) +
    ggplot2::geom_hline(yintercept = multiple_tests_result$alpha,
                        linetype = "dashed",
                        color = "red",
                        linewidth = 1) +
    ggplot2::scale_color_manual(values = c("Yes" = "red", "No" = "blue")) +
    ggplot2::labs(
      title = "Multiple Testing Results",
      subtitle = sprintf("Correction: %s, alpha = %.3f", 
                        multiple_tests_result$method,
                        multiple_tests_result$alpha),
      x = "",
      y = "Adjusted P-value",
      color = "Significant"
    ) +
    theme +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14)
    ) +
    ggplot2::coord_flip()
  
  p
}
