#' Finds the index of the first local minima in a series.
#' If value == F and no local minima is found, returns NA.
#' If value == T and no local minima is found, returns min(v)
#'
#' @param v A numeric vector containing the series.
#' @param value Return the value at the local minima instead of the index?
#' @return The index or value of the first local minima.
#' @export first_local_minima
first_local_minima <- function(v, value = F) {
    tmp = zoo::rollapply(v, 3, function(x) which.min(x) == 2)
    index = which(tmp == TRUE)[1] + 1

    if (is.na(index)) {
        if (value == T) return(min(v))
        else return(NA)
    } else {
      if (value == T) return(v[index])
      else return(index)
    }
}
