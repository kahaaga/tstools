#' Finds the minimal embedding dimension of a series 'v' according to
#' the method of simplex projection.
#' @param v A numeric vector containing the series.
#' @param min.embedding.dim The minimum embedding dimension.
#' @param max.embedding.dim The minimum embedding dimension.
#' @param embedding.lag The embedding lag.
#' @param plot.simplex.projection Should the simplex projection results be plotted?
#'
#' @return The embedding dimension that optimises simplex projection of the series.
#'
#' @export optimise_dim_simplex
#'
optimise_dim_simplex <- function(v,
                                 min.embedding.dim = 2,
                                 max.embedding.dim = 10,
                                 embedding.lag = 1,
                                 plot.simplex.projection = FALSE) {
    # rEDM produces warning messages if libraries overlap,
    # as they do by default. Just ignore this, so turn off
    # warnings temporarily.
    options(warn = -1)
    simplex.output = rEDM::simplex(time_series = v,
                  E = min.embedding.dim:max.embedding.dim,
                  tau = embedding.lag)
    options(warn = 0) # Reactivate warnings.


    optimal.embedding.dim <- simplex.output$E[which.max(simplex.output$rho)][1]

    #if (plot.simplex.projection) {
    #    plot(simplex.output$E, simplex.output$rho)
    #}

    return(as.numeric(optimal.embedding.dim))
}

