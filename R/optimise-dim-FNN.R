#' Finds the minimal embedding dimension of a series 'v' according to
#' the method of false nearest neighbours (FNN).
#'
#' From:
#' M. B. Kennel, R. Brown, and H. D. I. Abarbanel,
#' Determining embedding dimension for phase-space reconstruction
#' using a geometrical construction, Phys. Rev. A 45, 3403 (1992).
#'
#' Implemented in the TISEAN package by Reiner Hegger, Holger Kantz
#' and Thomas Schreiber, and ported to R in the tseriesChaos package
#' by Antonio Fabio Di Narzo
#'
#' @param v A numeric vector containing the series.
#' @param max.embedding.dim The maximum embedding dimension to consider
#' @param embedding.lag The embedding lag.
#' @param orbital.lag The Theiler window. An orbital lag to avoid
#'     temporal correlation. Defaults to NULL, in which case the orbital
#'     lag is chosen as the first local minima of the autocorrelation
#'     function (lag.method = "acf" or lag.method = "autocorrelationfunction")
#'     or the lagged mutual information function (lag.method = "mi" or
#'     lag.method = "mutual information").
#' @param lag.max The maximum number of lags for the autocorrelation
#'     function (lag.method = "acf" or lag.method = "autocorrelation function")
#'     or the mutual information function (lag.method = "mi" or
#'     lag.method = "mutual information function").
#' @param plot.temporal.correlation.function Should the function used
#'     to determine the orbital lag (temporal separation window) be
#'     plotted?
#' @param lag.method The method to compute the orbital lag (Theiler window),
#'   which excludes temporal neighbours to reduce correlation bias. Either
#'   use the autocorrelation function (lag.method = "acf") or the lagged
#'   mutual information function (lag.method = "mi")
#' @return The minimum embedding dimension yielding zero false neighbours
#'   or a false neighbours proportion below some threshold.
#' @export optimise_dim_FNN
optimise_dim_FNN <- function(v,
                    max.embedding.dim= 10,
                    embedding.lag = 1,
                    orbital.lag = NULL,
                    lag.max = ceiling(length(v)*0.2),
                    lag.method = "correlation",
                    plot.lag.method = F,
                    threshold = 0.95) {

    lag.method = tolower(lag.method)

    if (is.null(orbital.lag)) {
        if (lag.method == "correlation" ||
            lag.method == "afc" ||
            lag.method == "autocorrelation" ||
            lag.method == "autocorrelationfunction") {
            if (plot.lag.method) acf(x = v, lag.max = lag.max, plot = TRUE)$acf
            orbital.lag = first_acf_minima(v, lag.max = lag.max)
        } else if (lag.method == "mutual information" ||
                   lag.method == "mi") {
            orbital.lag = first_mi_minima(v, lag.max = lag.max)
        }
    }
    # Estimate the fraction of false nearest neighbours in embedding
    # dimension ranging from 1:max.embedding dimension
    fnn = tseriesChaos::false.nearest(series = v,
                                      m = max.embedding.dim,
                                      d = embedding.lag,
                                      t = orbital.lag)

    # Find the first dimension where FNN rate is below threshold
    # and return it if an acceptable FNN has been accomplished
    # within the range of dimensions. If not, return
    # 'max.embedding.dimension'.
    minimum.FNN.dimension = which(fnn[1, ] < (1 - threshold))[1]

    if (!is.na(minimum.FNN.dimension)) {
        return(as.integer(minimum.FNN.dimension))
    } else {
        return(max.embedding.dim)
    }
}
