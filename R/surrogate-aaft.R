#' Generate an amplitude adjusted Fourier transform surrogate according
#' to Theiler. The surrogate series preserves the amplitude distribution
#' (histogram) of the original time series, and are constrained to the
#' values of the original time series.
#'
#' @param series The series for which to generate a surrogate.
#' @export aaft_surrogate
aaft_surrogate <- function(series) {
  if (contains_na(series)) {
    warning("series contains na! surrogate will not be valid")
  }

  if (is_null(series)) {
    warning("series is NULL! can't generate surrogate.")
  }

  as.vector(fractal::surrogate(x = series, method = "aaft"))
}

