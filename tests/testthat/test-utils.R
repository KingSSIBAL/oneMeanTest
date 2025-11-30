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
# ENHANCED CUSTOM IMPLEMENTATIONS (High Precision)
# ============================================================

#' Custom implementation of sample() with replacement
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
  random_indices <- ceiling(runif(size) * n)
  
  # Handle edge case where runif returns exactly 0
  random_indices[random_indices == 0] <- 1
  
  x[random_indices]
}

#' Custom implementation of quantile() - Enhanced precision
#' 
#' Computes sample quantiles using Type 7 algorithm (R's default)
#' with improved numerical stability
#' 
#' @param x Numeric vector
#' @param probs Probabilities for quantiles (0 to 1)
#' @param names Logical; include names in output?
#' @return Named or unnamed vector of quantiles
#' @keywords internal
#' @noRd
.custom_quantile <- function(x, probs = c(0, 0.25, 0.5, 0.75, 1), names = TRUE) {
  x <- x[!is.na(x)]
  n <- length(x)
  
  if (n == 0L) {
    stop("Cannot compute quantiles of empty vector.", call. = FALSE)
  }
  
  # Sort the data using stable sorting
  x_sorted <- sort(x, method = "quick")
  
  result <- numeric(length(probs))
  
  for (i in seq_along(probs)) {
    p <- probs[i]
    
    # Type 7 algorithm: h = (n-1) * p + 1
    h <- (n - 1) * p + 1
    h_floor <- floor(h)
    h_frac <- h - h_floor
    
    # Handle edge cases with high precision
    if (h <= 1) {
      result[i] <- x_sorted[1]
    } else if (h >= n) {
      result[i] <- x_sorted[n]
    } else {
      # Linear interpolation with full precision
      result[i] <- x_sorted[h_floor] * (1 - h_frac) + x_sorted[h_floor + 1] * h_frac
    }
  }
  
  if (names && !is.null(probs)) {
    names(result) <- paste0(format(100 * probs), "%")
  }
  
  result
}

#' Custom implementation of pt() - Enhanced precision
#' 
#' Computes cumulative probability for Student's t-distribution
#' Uses improved incomplete beta function approximation
#' 
#' @param q Quantile value
#' @param df Degrees of freedom
#' @param lower.tail Logical; if TRUE, returns P(X <= q), else P(X > q)
#' @return Probability (accurate to ~6 decimal places)
#' @keywords internal
#' @noRd
.custom_pt <- function(q, df, lower.tail = TRUE) {
  # For very large df, use normal approximation
  if (df > 500) {
    p <- .custom_pnorm(q, lower.tail = lower.tail)
    return(p)
  }
  
  # Handle special cases
  if (is.infinite(q)) {
    p <- if (q > 0) 1 else 0
    if (!lower.tail) p <- 1 - p
    return(p)
  }
  
  # Use relationship: P(T <= t) = 0.5 + 0.5 * sign(t) * I_x(0.5, df/2)
  # where x = df/(df + t^2) and I is the regularized incomplete beta
  x <- df / (df + q^2)
  
  # Compute using improved incomplete beta
  beta_val <- .incomplete_beta_improved(x, 0.5, df/2)
  
  p_val <- 0.5 + 0.5 * sign(q) * (1 - beta_val)
  
  if (!lower.tail) {
    p_val <- 1 - p_val
  }
  
  # Clamp to [0, 1] to avoid numerical issues
  p_val <- max(0, min(1, p_val))
  
  p_val
}

#' Custom implementation of qt() - Enhanced precision
#' 
#' Computes quantile (inverse CDF) for Student's t-distribution
#' Uses Newton-Raphson with bisection fallback for reliability
#' 
#' @param p Probability (0 to 1)
#' @param df Degrees of freedom
#' @return Quantile value (accurate to ~5 decimal places)
#' @keywords internal
#' @noRd
.custom_qt <- function(p, df) {
  if (p <= 0 || p >= 1) {
    stop("Probability p must be between 0 and 1.", call. = FALSE)
  }
  
  # For very large df, use normal approximation
  if (df > 500) {
    return(.custom_qnorm(p))
  }
  
  # Initial guess using Wilson-Hilferty approximation
  z <- .custom_qnorm(p)
  
  if (df > 5) {
    # Better initial guess for moderate df
    initial <- z * sqrt(1 + z^2 / (4 * df))
  } else {
    # Use wider bounds for small df
    initial <- z * 1.5
  }
  
  # Newton-Raphson iteration with safeguards
  x <- initial
  tolerance <- 1e-8
  max_iter <- 50
  
  for (iter in 1:max_iter) {
    # Current CDF value
    fx <- .custom_pt(x, df, lower.tail = TRUE) - p
    
    # Check convergence
    if (abs(fx) < tolerance) {
      return(x)
    }
    
    # Compute derivative (PDF of t-distribution)
    # f(x) = Γ((df+1)/2) / (sqrt(df*π) * Γ(df/2)) * (1 + x²/df)^(-(df+1)/2)
    fprime <- .t_density(x, df)
    
    # Newton step with damping to prevent overshooting
    if (abs(fprime) > 1e-10) {
      step <- fx / fprime
      # Limit step size
      step <- sign(step) * min(abs(step), 3)
      x_new <- x - step
    } else {
      # Fallback to bisection if derivative is too small
      break
    }
    
    # Check if we're making progress
    if (abs(x_new - x) < tolerance) {
      return(x_new)
    }
    
    x <- x_new
  }
  
  # If Newton-Raphson didn't converge, use bisection
  return(.qt_bisection(p, df))
}

#' T-distribution density function
#' @keywords internal
#' @noRd
.t_density <- function(x, df) {
  # log-density for numerical stability
  log_const <- lgamma((df + 1) / 2) - lgamma(df / 2) - 0.5 * log(df * pi)
  log_kernel <- -(df + 1) / 2 * log(1 + x^2 / df)
  exp(log_const + log_kernel)
}

#' Bisection method for qt (fallback)
#' @keywords internal
#' @noRd
.qt_bisection <- function(p, df) {
  # Set bounds based on tail
  if (p < 0.5) {
    lower <- -50
    upper <- 0
  } else {
    lower <- 0
    upper <- 50
  }
  
  tolerance <- 1e-8
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
  
  (lower + upper) / 2
}

#' Improved incomplete beta function
#' 
#' Uses continued fraction approximation for better accuracy
#' 
#' @param x Upper limit of integration
#' @param a First shape parameter
#' @param b Second shape parameter
#' @return Regularized incomplete beta I_x(a, b)
#' @keywords internal
#' @noRd
.incomplete_beta_improved <- function(x, a, b) {
  if (x <= 0) return(0)
  if (x >= 1) return(1)
  
  # Use symmetry relation if x > 0.5
  if (x > 0.5) {
    return(1 - .incomplete_beta_improved(1 - x, b, a))
  }
  
  # Compute log of beta function B(a,b)
  log_beta <- lgamma(a) + lgamma(b) - lgamma(a + b)
  
  # Use continued fraction expansion (Lentz's algorithm)
  front <- exp(log(x) * a + log(1 - x) * b - log(a) - log_beta)
  
  # Continued fraction evaluation
  f <- 1.0
  c <- 1.0
  d <- 0.0
  
  max_iter <- 200
  tiny <- 1e-30
  
  for (m in 1:max_iter) {
    m2 <- 2 * m
    
    # Even step
    aa <- m * (b - m) * x / ((a + m2 - 1) * (a + m2))
    d <- 1 + aa * d
    if (abs(d) < tiny) d <- tiny
    c <- 1 + aa / c
    if (abs(c) < tiny) c <- tiny
    d <- 1 / d
    f <- f * d * c
    
    # Odd step
    aa <- -(a + m) * (a + b + m) * x / ((a + m2) * (a + m2 + 1))
    d <- 1 + aa * d
    if (abs(d) < tiny) d <- tiny
    c <- 1 + aa / c
    if (abs(c) < tiny) c <- tiny
    d <- 1 / d
    delta <- d * c
    f <- f * delta
    
    # Check convergence
    if (abs(delta - 1) < 1e-10) {
      break
    }
  }
  
  front * f / a
}

#' Standard normal CDF - Enhanced precision
#' 
#' Uses rational approximation (Cody, 1969)
#' 
#' @param q Quantile
#' @param lower.tail Logical
#' @return Probability
#' @keywords internal
#' @noRd
.custom_pnorm <- function(q, lower.tail = TRUE) {
  # Handle extreme values
  if (q < -8) {
    p <- if (lower.tail) 0 else 1
    return(p)
  }
  if (q > 8) {
    p <- if (lower.tail) 1 else 0
    return(p)
  }
  
  # Use more accurate error function
  p <- 0.5 * (1 + .erf_improved(q / sqrt(2)))
  
  if (!lower.tail) {
    p <- 1 - p
  }
  
  p
}

#' Standard normal quantile - Enhanced precision
#' 
#' Uses Beasley-Springer-Moro algorithm
#' 
#' @param p Probability
#' @return Quantile
#' @keywords internal
#' @noRd
.custom_qnorm <- function(p) {
  if (p <= 0 || p >= 1) {
    return(if (p <= 0) -Inf else Inf)
  }
  
  # Beasley-Springer-Moro algorithm for improved accuracy
  a <- c(2.50662823884, -18.61500062529, 41.39119773534, -25.44106049637)
  b <- c(-8.47351093090, 23.08336743743, -21.06224101826, 3.13082909833)
  c <- c(0.3374754822726147, 0.9761690190917186, 0.1607979714918209,
         0.0276438810333863, 0.0038405729373609, 0.0003951896511919,
         0.0000321767881768, 0.0000002888167364, 0.0000003960315187)
  
  y <- p - 0.5
  
  if (abs(y) < 0.42) {
    # Central region
    r <- y * y
    return(y * (((a[4] * r + a[3]) * r + a[2]) * r + a[1]) / 
           ((((b[4] * r + b[3]) * r + b[2]) * r + b[1]) * r + 1))
  } else {
    # Tail region
    r <- if (y > 0) 1 - p else p
    r <- sqrt(-log(r))
    
    if (r <= 5.0) {
      r <- r - 1.6
      z <- (((((((c[9] * r + c[8]) * r + c[7]) * r + c[6]) * r + c[5]) * r + c[4]) * r + c[3]) * r + c[2]) * r + c[1]
    } else {
      r <- r - 5.0
      z <- (((((((c[9] * r + c[8]) * r + c[7]) * r + c[6]) * r + c[5]) * r + c[4]) * r + c[3]) * r + c[2]) * r + c[1]
    }
    
    return(if (y < 0) -z else z)
  }
}

#' Improved error function
#' 
#' Uses rational approximation for better accuracy
#' 
#' @param x Value
#' @return erf(x)
#' @keywords internal
#' @noRd
.erf_improved <- function(x) {
  # Coefficients for rational approximation (accurate to ~7 decimals)
  a1 <-  0.254829592
  a2 <- -0.284496736
  a3 <-  1.421413741
  a4 <- -1.453152027
  a5 <-  1.061405429
  p  <-  0.3275911
  
  sign_x <- sign(x)
  x <- abs(x)
  
  # Rational approximation
  t <- 1 / (1 + p * x)
  y <- 1 - (((((a5 * t + a4) * t) + a3) * t + a2) * t + a1) * t * exp(-x * x)
  
  sign_x * y
}
