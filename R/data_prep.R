#' Data Preparation Functions for One-Sample T-Test
#'
#' Internal functions to prepare and validate data for statistical testing.
#' These functions handle various input formats (vectors, data frames, formulas)
#' and ensure data is in the correct format for analysis.
#'
#' @keywords internal

#' Prepare data from various input formats
#'
#' Extracts numeric data from data frames, formulas, or vectors
#'
#' @param data Data frame, numeric vector, or other data structure
#' @param variable Character string naming the variable (for data frames)
#' @return Numeric vector ready for analysis
#' @keywords internal
#' @noRd
prepare_data <- function(data, variable = NULL) {
  # If data is already a numeric vector, return it
  if (is.numeric(data) && is.null(variable)) {
    return(data)
  }
  
  # If data is a data frame and variable is specified
  if (is.data.frame(data) && !is.null(variable)) {
    if (!variable %in% names(data)) {
      stop(sprintf("Variable '%s' not found in data frame.", variable), 
           call. = FALSE)
    }
    return(data[[variable]])
  }
  
  # If data is a list, try to extract first numeric element
  if (is.list(data) && !is.data.frame(data)) {
    numeric_elements <- sapply(data, is.numeric)
    if (any(numeric_elements)) {
      return(data[[which(numeric_elements)[1]]])
    }
  }
  
  # Try to coerce to numeric
  result <- tryCatch(
    as.numeric(data),
    warning = function(w) NULL,
    error = function(e) NULL
  )
  
  if (is.null(result)) {
    stop("Cannot extract numeric data from provided input.", call. = FALSE)
  }
  
  result
}

#' Extract numeric vector from various formats
#'
#' Converts various R objects to numeric vectors
#'
#' @param x Input object
#' @return Numeric vector
#' @keywords internal
#' @noRd
extract_numeric <- function(x) {
  # Already numeric
  if (is.numeric(x)) {
    return(as.numeric(x))
  }
  
  # Factor - convert via character to preserve meaning
  if (is.factor(x)) {
    warning("Converting factor to numeric. Check if this is intended.", 
            call. = FALSE)
    return(as.numeric(as.character(x)))
  }
  
  # Character - try to parse as numeric
  if (is.character(x)) {
    result <- suppressWarnings(as.numeric(x))
    if (all(is.na(result)) && !all(is.na(x))) {
      stop("Cannot convert character data to numeric.", call. = FALSE)
    }
    return(result)
  }
  
  # Logical - convert to 0/1
  if (is.logical(x)) {
    return(as.numeric(x))
  }
  
  # Try generic coercion
  tryCatch(
    as.numeric(x),
    error = function(e) {
      stop(sprintf("Cannot convert object of class '%s' to numeric.", 
                   class(x)[1]), call. = FALSE)
    }
  )
}

#' Validate data input for statistical tests
#'
#' Checks that data meets requirements for one-sample t-test
#'
#' @param x Numeric vector to validate
#' @param min_n Minimum required sample size (default 2)
#' @return Invisible TRUE if valid, stops with error otherwise
#' @keywords internal
#' @noRd
validate_data_input <- function(x, min_n = 2) {
  # Check if numeric
  if (!is.numeric(x)) {
    stop("Data must be numeric.", call. = FALSE)
  }
  
  # Check length
  if (length(x) < 1) {
    stop("Data must have at least one observation.", call. = FALSE)
  }
  
  # Check for all NA
  if (all(is.na(x))) {
    stop("All values are NA.", call. = FALSE)
  }
  
  # Check minimum sample size after removing NA
  x_clean <- x[!is.na(x)]
  if (length(x_clean) < min_n) {
    stop(sprintf("Need at least %d non-missing observations, but only have %d.", 
                 min_n, length(x_clean)), call. = FALSE)
  }
  
  # Check for infinite values
  if (any(is.infinite(x_clean))) {
    warning("Data contains infinite values.", call. = FALSE)
  }
  
  # Check for zero variance (all values identical)
  if (length(unique(x_clean)) == 1) {
    warning("All non-missing values are identical (zero variance).", 
            call. = FALSE)
  }
  
  invisible(TRUE)
}

#' Extract variable name from formula or expression
#'
#' Helper to get variable name from formula interface
#'
#' @param formula Formula object
#' @return Character string with variable name
#' @keywords internal
#' @noRd
extract_variable_name <- function(formula) {
  if (!inherits(formula, "formula")) {
    stop("Input must be a formula.", call. = FALSE)
  }
  
  # Get the response variable (left side)
  response <- all.vars(formula[[2]])
  
  if (length(response) == 0) {
    stop("Formula must have a response variable.", call. = FALSE)
  }
  
  response[1]
}

#' Prepare data from formula interface
#'
#' Extracts data using formula notation (e.g., ~ variable)
#'
#' @param formula Formula specifying the variable
#' @param data Data frame containing the variable
#' @return Numeric vector
#' @keywords internal
#' @noRd
prepare_data_formula <- function(formula, data) {
  if (!inherits(formula, "formula")) {
    stop("First argument must be a formula.", call. = FALSE)
  }
  
  if (!is.data.frame(data)) {
    stop("Data must be a data frame when using formula interface.", 
         call. = FALSE)
  }
  
  # Extract variable name
  var_name <- extract_variable_name(formula)
  
  # Extract data
  if (!var_name %in% names(data)) {
    stop(sprintf("Variable '%s' not found in data.", var_name), call. = FALSE)
  }
  
  data[[var_name]]
}
