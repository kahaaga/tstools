#' Generate a seasonal surrogate according to Sugihara et al.
#'
#' @param series The series for which to generate a surrogate.
#' @export seasonal_surrogate
seasonal_surrogate <- function(series, t.period = 1) {
  if (contains_na(series)) {
    warning("series contains na! surrogate will not be valid")
  }

  if (is_null(series)) {
    warning("series is NULL! can't generate surrogate.")
  }

  rEDM::make_surrogate_data(ts = series,
                            method = "seasonal",
                            num_surr = 1,
                            T_period = t.period)
}

