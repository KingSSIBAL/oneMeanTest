# Internal utility helpers (not exported)

.check_numeric_vector <- function(x, arg_name = "x") {
  if (missing(x)) {
    stop(sprintf("Argument '%s' is missing.", arg_name), call. = FALSE)
  }
  if (!is.numeric(x)) {
    stop(sprintf("Argument '%s' must be numeric.", arg_name), call. = FALSE)
  }
  if (!is.vector(x)) {
    stop(sprintf("Argument '%s' must be a numeric vector.", arg_name), call. = FALSE)
  }
  if (length(x) < 2L) {
    stop(sprintf("Argument '%s' must have length at least 2.", arg_name), call. = FALSE)
  }
  invisible(TRUE)
}

.remove_na_with_warning <- function(x, arg_name = "x") {
  n_before <- length(x)
  x <- x[!is.na(x)]
  n_after <- length(x)
  if (n_after == 0L) {
    stop(sprintf("Argument '%s' contains only NA values.", arg_name), call. = FALSE)
  }
  if (n_after < n_before) {
    warning(
      sprintf(
        "Removed %d NA value(s) from '%s' (n = %d -> n = %d).",
        n_before - n_after, arg_name, n_before, n_after
      ),
      call. = FALSE
    )
  }
  x
}

.match_alternative <- function(alternative) {
  alt <- match.arg(alternative, c("two.sided", "less", "greater"))
  alt
}

.check_alpha <- function(alpha) {
  if (!is.numeric(alpha) || length(alpha) != 1L || is.na(alpha)) {
    stop("alpha must be a single numeric value.", call. = FALSE)
  }
  if (alpha <= 0 || alpha >= 1) {
    stop("alpha must be strictly between 0 and 1.", call. = FALSE)
  }
  invisible(TRUE)
}

.check_conf_level <- function(conf.level) {
  if (!is.numeric(conf.level) || length(conf.level) != 1L || is.na(conf.level)) {
    stop("conf.level must be a single numeric value.", call. = FALSE)
  }
  if (conf.level <= 0 || conf.level >= 1) {
    stop("conf.level must be strictly between 0 and 1.", call. = FALSE)
  }
  invisible(TRUE)
}
