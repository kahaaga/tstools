#' Check if input contains any NA values.
#' @param x A vector to check.
contains_na <- function(x) {
  if (any(is.na(x))) return(TRUE)
  return(FALSE)
}

#' Checks if an input vector is valid.
#' @param x A vector to check.
is_valid_input <- function(x) {
  if (is.null(x)) return(FALSE)
  if (contains_na(x)) return(FALSE)
  return(TRUE)
}
