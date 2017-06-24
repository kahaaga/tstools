#' Generate a surrogate series by randomly shuffling the values
#' of the original time series.
#'
#' @param series The series for which to generate a surrogate.
#' @export random_surrogate
random_surrogate <- function(series) {
  if (!is_valid_input(series)) {
    rlang::abort("Surrogate generation failed. Input series is not valid!")
  }

  rEDM::make_surrogate_data(ts = series,
                            method = "random",
                            num_surr = 1)
}

