# Data preparation and example generators

#' Generate example test score data
#'
#' Convenience function to generate a numeric vector of test scores for
#' illustrating the one-sample t-test.
#'
#' @param n Sample size.
#' @param mean True mean used for simulation.
#' @param sd True standard deviation used for simulation.
#' @param seed Optional integer seed for reproducibility.
#'
#' @return A numeric vector of length \code{n}.
#' @export
generate_example_scores <- function(n = 30, mean = 80, sd = 10, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  if (!is.numeric(n) || length(n) != 1L || n < 2) {
    stop("n must be a single integer >= 2.", call. = FALSE)
  }
  stats::rnorm(n, mean = mean, sd = sd)
}

#' Generate example product weight data
#'
#' Convenience function to generate product weights (e.g., grams) for
#' illustrating the one-sample t-test.
#'
#' @param n Sample size.
#' @param mean True mean used for simulation.
#' @param sd True standard deviation used for simulation.
#' @param seed Optional integer seed for reproducibility.
#'
#' @return A numeric vector of length \code{n}.
#' @export
generate_example_weights <- function(n = 30, mean = 500, sd = 5, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  if (!is.numeric(n) || length(n) != 1L || n < 2) {
    stop("n must be a single integer >= 2.", call. = FALSE)
  }
  stats::rnorm(n, mean = mean, sd = sd)
}

#' Example analysis pipeline
#'
#' This helper shows a simple workflow: generate data, run the one-sample
#' t-test, and return the test object. This can be used in demonstrations
#' and vignettes.
#'
#' @param n Sample size.
#' @param true_mean True mean for data generation.
#' @param sd True standard deviation for data generation.
#' @param mu0 Hypothesized mean.
#' @param seed Optional integer seed.
#'
#' @return An object of class \code{"oneMeanTest"}.
#' @export
example_one_mean_pipeline <- function(
    n = 30,
    true_mean = 80,
    sd = 10,
    mu0 = 75,
    seed = NULL
) {
  x <- generate_example_scores(n = n, mean = true_mean, sd = sd, seed = seed)
  res <- one_mean_test(x, mu0 = mu0)
  attr(res, "data") <- x
  res
}
