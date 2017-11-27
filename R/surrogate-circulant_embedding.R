#' Generate a surrogate series using Davies and Harte's circulant embedding.
#' The values of the surrogate series are not constrained to the values of
#' the original time series.
#'
#' @param series The series for which to generate a surrogate.
#' @export ce_surrogate
ce_surrogate <- function(series) {
  if (!is_valid_input(series)) {
    rlang::abort("Surrogate generation failed. Input series is not valid!")
  }

  as.vector(fractal::surrogate(x = series, method = "ce"))
}
