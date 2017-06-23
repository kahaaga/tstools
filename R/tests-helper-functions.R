#' Check if input contains any NA values.
contains_na <- function(x) {
  if (any(is.na(x))) return(TRUE)
  return(FALSE)
}

#' Check if input contains any NULL values.
is_null <- function(x) {
  if (any(is.null(x))) return(TRUE)
  return(FALSE)
}
