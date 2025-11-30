# Internal utility helpers (not exported)
#
# These functions are used internally throughout the package for input
# validation and argument checking. They are not exported and are intended
# for use only inside other package functions.

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

# ============================================================
# CUSTOM IMPLEMENTATIONS (replacing built-in functions)
# ============================================================

#' Custom implementation of sample() with replacement
#' 
#' Randomly samples elements from a vector with replacement
#' 
#' @param x Vector to sample from
#' @param size Number of items to sample
#' @param replace Logical; sample with replacement?
#' @return Sampled vector
#' @keywords internal
#' @noRd
.custom_sample <- function(x, size, replace = FALSE) {
  n <- length(x)
  
  if (!replace && size > n) {
    stop("Cannot sample more items than available without replacement.", call. = FALSE)
  }
  
  # Generate random uniform values and convert to indices
  # Using runif() which is still built-in, but this is the random number generator
  # Replacing this would require implementing a full RNG which is beyond scope
  random_indices <- ceiling(runif(size) * n)
  
  # Return sampled elements
  x[random_indices]
}

#' Custom implementation of quantile()
#' 
#' Computes sample quantiles using Type 7 algorithm (R's default)
#' Type 7: Linear interpolation, p(k) = (k-1)/(n-1)
#' 
#' @param x Numeric vector
#' @param probs Probabilities for quantiles (0 to 1)
#' @param names Logical; include names in output?
#' @return Named or unnamed vector of quantiles
#' @keywords internal
#' @noRd
.custom_quantile <- function(x, probs = c(0, 0.25, 0.5, 0.75, 1), names = TRUE) {
  # Remove NAs
  x <- x[!is.na(x)]
  n <- length(x)
  
  if (n == 0L) {
    stop("Cannot compute quantiles of empty vector.", call. = FALSE)
  }
  
  # Sort the data
  x_sorted <- sort(x)
  
  # Initialize result vector
  result <- numeric(length(probs))
  
  # Compute each quantile
  for (i in seq_along(probs)) {
    p <- probs[i]
    
    # Type 7 algorithm (R's default)
    # h = (n-1) * p + 1
    h <- (n - 1) * p + 1
    
    # h_floor is the lower index
    h_floor <- floor(h)
    
    # Handle edge cases
    if (h_floor < 1) {
      result[i] <- x_sorted[1]
    } else if (h_floor >= n) {
      result[i] <- x_sorted[n]
    } else {
      # Linear interpolation between x[h_floor] and x[h_floor + 1]
      h_frac <- h - h_floor
      result[i] <- x_sorted[h_floor] + h_frac * (x_sorted[h_floor + 1] - x_sorted[h_floor])
    }
  }
  
  # Add names if requested
  if (names && !is.null(probs)) {
    names(result) <- paste0(format(100 * probs), "%")
  }
  
  result
}

#' Custom implementation of pt() - t-distribution CDF
#' 
#' Computes cumulative probability for Student's t-distribution
#' Uses relationship with incomplete beta function
#' 
#' @param q Quantile value
#' @param df Degrees of freedom
#' @param lower.tail Logical; if TRUE, returns P(X <= q), else P(X > q)
#' @return Probability
#' @keywords internal
#' @noRd
.custom_pt <- function(q, df, lower.tail = TRUE) {
  # For very large df, t-distribution approaches standard normal
  if (df > 1000) {
    # Use normal approximation
    p <- .custom_pnorm(q, lower.tail = lower.tail)
    return(p)
  }
  
  # Use relationship between t-distribution and beta distribution
  # If T ~ t(df), then P(T <= t) can be expressed using incomplete beta function
  # P(T <= t) = 0.5 + 0.5 * sign(t) * I_x(df/2, df/2)
  # where x = df/(df + t^2) and I is the regularized incomplete beta function
  
  x <- df / (df + q^2)
  
  # Compute incomplete beta using numerical integration
  p_val <- 0.5 + 0.5 * sign(q) * (1 - .incomplete_beta(x, df/2, 0.5))
  
  if (!lower.tail) {
    p_val <- 1 - p_val
  }
  
  p_val
}

#' Custom implementation of qt() - t-distribution quantile function
#' 
#' Computes quantile (inverse CDF) for Student's t-distribution
#' Uses bisection method to find the value
#' 
#' @param p Probability (0 to 1)
#' @param df Degrees of freedom
#' @return Quantile value
#' @keywords internal
#' @noRd
.custom_qt <- function(p, df) {
  if (p <= 0 || p >= 1) {
    stop("Probability p must be between 0 and 1.", call. = FALSE)
  }
  
  # For very large df, use normal approximation
  if (df > 1000) {
    return(.custom_qnorm(p))
  }
  
  # Use bisection method to find quantile
  # We need to find t such that P(T <= t) = p
  
  # Set initial bounds (very wide)
  lower <- -100
  upper <- 100
  tolerance <- 1e-6
  max_iter <- 100
  
  for (iter in 1:max_iter) {
    mid <- (lower + upper) / 2
    p_mid <- .custom_pt(mid, df, lower.tail = TRUE)
    
    if (abs(p_mid - p) < tolerance) {
      return(mid)
    }
    
    if (p_mid < p) {
      lower <- mid
    } else {
      upper <- mid
    }
  }
  
  # Return best estimate
  (lower + upper) / 2
}

#' Helper: Incomplete beta function using numerical integration
#' 
#' Approximates the regularized incomplete beta function I_x(a, b)
#' 
#' @param x Upper limit of integration
#' @param a First shape parameter
#' @param b Second shape parameter
#' @return Approximate value of incomplete beta
#' @keywords internal
#' @noRd
.incomplete_beta <- function(x, a, b) {
  # For our purposes (t-distribution), we use a simple approximation
  # More sophisticated implementations would use continued fractions
  
  if (x <= 0) return(0)
  if (x >= 1) return(1)
  
  # Simple numerical integration using trapezoidal rule
  n_steps <- 1000
  dx <- x / n_steps
  
  # Integrand: t^(a-1) * (1-t)^(b-1)
  integral <- 0
  for (i in 0:n_steps) {
    t <- i * dx
    if (t == 0 || t == 1) {
      weight <- 0.5
    } else {
      weight <- 1
    }
    if (i == 0 || i == n_steps) weight <- 0.5
    
    # Add small epsilon to avoid log(0)
    if (t > 0 && t < 1) {
      # Use log-space for numerical stability
      log_integrand <- (a - 1) * log(t) + (b - 1) * log(1 - t)
      integral <- integral + weight * exp(log_integrand) * dx
    }
  }
  
  # Normalize by beta function B(a,b)
  log_beta <- lgamma(a) + lgamma(b) - lgamma(a + b)
  
  integral / exp(log_beta)
}

#' Helper: Standard normal CDF (for normal approximation)
#' 
#' @param q Quantile
#' @param lower.tail Logical
#' @return Probability
#' @keywords internal
#' @noRd
.custom_pnorm <- function(q, lower.tail = TRUE) {
  # Use error function approximation
  # P(Z <= z) = 0.5 * (1 + erf(z/sqrt(2)))
  
  p <- 0.5 * (1 + .erf(q / sqrt(2)))
  
  if (!lower.tail) {
    p <- 1 - p
  }
  
  p
}

#' Helper: Standard normal quantile (for normal approximation)
#' 
#' @param p Probability
#' @return Quantile
#' @keywords internal
#' @noRd
.custom_qnorm <- function(p) {
  # Use inverse error function approximation
  # z = sqrt(2) * erf_inv(2*p - 1)
  
  .inv_erf(2 * p - 1) * sqrt(2)
}

#' Helper: Error function
#' 
#' @param x Value
#' @return erf(x)
#' @keywords internal
#' @noRd
.erf <- function(x) {
  # Abramowitz and Stegun approximation
  a1 <-  0.254829592
  a2 <- -0.284496736
  a3 <-  1.421413741
  a4 <- -1.453152027
  a5 <-  1.061405429
  p  <-  0.3275911
  
  sign_x <- sign(x)
  x <- abs(x)
  
  t <- 1 / (1 + p * x)
  y <- 1 - (((((a5 * t + a4) * t) + a3) * t + a2) * t + a1) * t * exp(-x * x)
  
  sign_x * y
}

#' Helper: Inverse error function
#' 
#' @param y Value between -1 and 1
#' @return erf^(-1)(y)
#' @keywords internal
#' @noRd
.inv_erf <- function(y) {
  # Newton's method
  if (abs(y) >= 1) {
    return(sign(y) * Inf)
  }
  
  # Initial guess
  x <- y * 0.7  # Simple initial estimate
  
  for (iter in 1:10) {
    f <- .erf(x) - y
    fprime <- 2 / sqrt(pi) * exp(-x^2)
    x <- x - f / fprime
  }
  
  x
}
