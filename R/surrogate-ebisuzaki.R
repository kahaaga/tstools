#' Generate a phase-randomised surrogate according to Ebisuzaki.
#'
#' @param series The series for which to generate a surrogate.
#' @export ebisuzaki_surrogate
ebisuzaki_surrogate <- function(series) {
  if (!is_valid_input(series)) {
    rlang::abort("Surrogate generation failed. Input series is not valid!")
  }

  surrogate <- rEDM::make_surrogate_data(ts = series,
                            method = "ebisuzaki",
                            num_surr = 1)
  return(as.numeric(surrogate))
}
