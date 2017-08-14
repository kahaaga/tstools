#' Finds the first minima or zero of the autocorrelation function of the
#' series 'v'.
#'
#' @param v A numeric vector containing the series.
#' @param lag.max The maximum lag to consider for the autocorrelation
#'   function.
#' @param plot.acf Should the autocorrelation function be plotted?
#' @return The first lag that minimises the autocorrelation function.
#' @export first_acf_minima
#'
first_acf_minima <- function(v, lag.max, plot.acf = F) {
    autocorrelation.function = stats::acf(x = v,
                                   lag.max = lag.max,
                                   plot = ifelse(plot.acf, T, F))$acf

    # Check if the autocorrelation function crosses the line where it is
    # indistinguishable from zero. The uncertainty bounds are calculated
    # as in the forecast::autoplot.acf function source code, but with
    # 99% confidence instead of 95%.
    upper.uncertainty.bound = stats::qnorm((1 + 0.99)/2)/sqrt(201)
    if (length(which(autocorrelation.function < upper.uncertainty.bound)) > 0) {
        lag = which(autocorrelation.function < upper.uncertainty.bound)[1] - 1 # Subtract 1 to accomodate for indexing in stats::acf
    } else {
        lag = first_local_minima(autocorrelation.function) - 1 # Subtract 1 to accomodate for indexing in stats::acf
    }
    return(lag)
}
