#' Finds the index of the first local minima in a series.
#' If no local minima is found, the function returns the
#' minimum value of the series.
#'
#' @param v A numeric vector containing the series.
#' @param value Return the value at the local minima instead of the index?
#' @return The index or value of the first local minima.
#' @export first_local_minima
first_local_minima <- function(v, value = F) {
    tmp = zoo::rollapply(v, 3, function(x) which.min(x) == 2)
    index = which(tmp == TRUE)[1] + 1

    if (is.na(index)) {
        cat("\nNo local minima was found.\n")
        return(match(min(v), v) + 1)
    } else {
        return(index)
    }
}
