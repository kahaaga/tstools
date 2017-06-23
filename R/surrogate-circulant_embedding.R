#' Generate a surrogate series using Davies and Harte's circulant embedding.
#' The values of the surrogate series are not constrained to the values of
#' the original time series.
#'
#' @param series The series for which to generate a surrogate.
#' @export ce_surrogate
ce_surrogate <- function(series) {
  if (contains_na(series)) {
    warning("series contains na! surrogate will not be valid")
  }

  if (is_null(series)) {
    warning("series is NULL! can't generate surrogate.")
  }

  as.vector(fractal::surrogate(x = series, method = "ce"))
}

