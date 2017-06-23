#' Generate a phase-randomised surrogate according to Ebisuzaki.
#'
#' @param series The series for which to generate a surrogate.
#' @export ebisuzaki_surrogate
ebisuzaki_surrogate <- function(series) {
  if (contains_na(series)) {
    warning("series contains na! surrogate will not be valid")
  }

  if (is_null(series)) {
    warning("series is NULL! can't generate surrogate.")
  }

  rEDM::make_surrogate_data(ts = series,
                            method = "ebisuzaki",
                            num_surr = 1)
}

