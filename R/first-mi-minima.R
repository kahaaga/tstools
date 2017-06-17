#' Finds the first minima or zero of the mutual information (mi) function
#' of the series 'v'.
#'
#' @param v A numeric vector containing the series.
#' @param lag.max The maximum lag to consider in the mi function.
#' @param plot.mi Should the mi function be plotted?
#' @return The first lag that minimises the mi function.
#' @export first_mi_minima
#'
first_mi_minima <- function(v, lag.max, partitions = 16, plot.mi.func = F) {
    mutual.information.function = tseriesChaos::mutual(series = v,
                                          lag.max = lag.max,
                                          partitions = partitions,
                                          plot = plot.mi.func)
    mutual.information.function = as.vector(mutual.information.function)


    # If the mi function crosses zero
    if (length(which(mutual.information.function < 0)) > 0) {
        lag = which(mutual.information.function < 0)[1] - 1 # subtract 1 because of indexing in tseriesChaos::mutual
    } else {
        lag = tstools::first_local_minima(mutual.information.function) - 1 # subtract 1 because of indexing in tseriesChaos::mutual
    }
    return(lag)
}