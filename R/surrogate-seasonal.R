#' Generate a seasonal surrogate according to Sugihara et al.
#'
#' @param series The series for which to generate a surrogate.
#' @export seasonal_surrogate
seasonal_surrogate <- function(series, t.period = 1) {
  if (!is_valid_input(series)) {
    rlang::abort("Surrogate generation failed. Input series is not valid!")
  }

  rEDM::make_surrogate_data(ts = series,
                            method = "seasonal",
                            num_surr = 1,
                            T_period = t.period)
}

