# Internal utility helpers (not exported)

.check_numeric_vector <- function(x, name = "x") {
  if (!is.numeric(x)) {
    stop(sprintf("`%s` must be a numeric vector.", name), call. = FALSE)
  }
  if (length(x) == 0L) {
    stop(sprintf("`%s` must have length >= 1.", name), call. = FALSE)
  }
}

.remove_na_with_warning <- function(x, name = "x") {
  n_before <- length(x)
  x <- x[!is.na(x)]
  n_after <- length(x)

  if (n_after == 0L) {
    stop(sprintf("All values in `%s` are NA.", name), call. = FALSE)
  }

  if (n_after < n_before) {
    warning(sprintf(
      "Removed %d NA value(s) from `%s` (n = %d -> n = %d).",
      n_before - n_after, name, n_before, n_after
    ), call. = FALSE)
  }

  if (n_after < 2L) {
    stop(sprintf("`%s` must have at least 2 non-missing values.", name),
         call. = FALSE)
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
