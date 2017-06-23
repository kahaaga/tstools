#' Generate a surrogate series by randomly shuffling the values
#' of the original time series.
#'
#' @param series The series for which to generate a surrogate.
#' @export random_surrogate
random_surrogate <- function(series) {
  if (contains_na(series)) {
    warning("series contains na! surrogate will not be valid")
  }

  if (is_null(series)) {
    warning("series is NULL! can't generate surrogate.")
  }

  rEDM::make_surrogate_data(ts = series,
                            method = "random",
                            num_surr = 1)
}

