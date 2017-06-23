#' Generate a phase randomised surrogate according to Theiler.
#'
#' @param series The series for which to generate a surrogate.
#' @export phase_randomised_surrogate
phase_randomised_surrogate <- function(series) {
  if (contains_na(series)) {
    warning("series contains na! surrogate will not be valid")
  }

  if (is_null(series)) {
    warning("series is NULL! can't generate surrogate.")
  }

  as.vector(fractal::surrogate(x = series, method = "phase"))
}

