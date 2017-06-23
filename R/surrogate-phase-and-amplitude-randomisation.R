#' Generate a Davison and Hinkley's surrogate series. Randomises both
#' the phases and amplitudes in the frequency domain before inverting back
#' to the time domain. The resulting surrogates are not constrained to the
#' values of the original time series.
#'
#' @param series The series for which to generate a surrogate.
#' @export dh_surrogate
dh_surrogate <- function(series) {
  if (contains_na(series)) {
    warning("series contains na! surrogate will not be valid.")
  }

  if (is_null(series)) {
    warning("series is NULL! can't generate surrogate.")
  }

  as.vector(fractal::surrogate(x = series, method = "dh"))
}

