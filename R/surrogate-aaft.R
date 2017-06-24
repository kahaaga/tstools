#' Generate an amplitude adjusted Fourier transform surrogate according
#' to Theiler. The surrogate series preserves the amplitude distribution
#' (histogram) of the original time series, and are constrained to the
#' values of the original time series.
#'
#' @param series The series for which to generate a surrogate.
#' @export aaft_surrogate
aaft_surrogate <- function(series) {
  if (!is_valid_input(series)) {
    rlang::abort("Surrogate generation failed. Input series is not valid!")
  }

  as.vector(fractal::surrogate(x = series, method = "aaft"))
}

