#' Generate a Davison and Hinkley's surrogate series. Randomises both
#' the phases and amplitudes in the frequency domain before inverting back
#' to the time domain. The resulting surrogates are not constrained to the
#' values of the original time series.
#'
#' @param series The series for which to generate a surrogate.
#' @export dh_surrogate
dh_surrogate <- function(series) {
  if (!is_valid_input(series)) {
    rlang::abort("Surrogate generation failed. Input series is not valid!")
  }

  as.vector(fractal::surrogate(x = series, method = "dh"))
}

