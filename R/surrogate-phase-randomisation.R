#' Generate a phase randomised surrogate according to Theiler.
#'
#' @param series The series for which to generate a surrogate.
#' @export phase_randomised_surrogate
phase_randomised_surrogate <- function(series) {
  if (!is_valid_input(series)) {
    rlang::abort("Surrogate generation failed. Input series is not valid!")
  }

  as.vector(fractal::surrogate(x = series, method = "phase"))
}

