#' Finds the optimal embedding dimensions and embedding lags for a series 'v'
#'
#' The function works by first finding the minimal embedding dimension E by
#' either the false nearest neighbours (FNN) method, or by Whitney's embedding
#' theorem (the minimum embedding dimension must be at least the first integer
#' dimension larger than the box counting dimension of the attractor). The
#' minimal embedding dimension found is then used as input for the simplex
#' projection algorithm, which estimates a suitable E by optimizing
#' self-prediciton.
#'
#' Both pre-estimation methods may be used simultaneously. In this case, the
#' minimal embedding dimension is the maximum of the FNN and the boxcount
#' estimates. The final embedding dimension is then determined by simplex
#' projection over dimensions E:max.E.
#'
#' If no optimization is done prior to simplex projection, optimization is
#' performed over dimensions min.E:max.E. min.E defaults to 2.
#'
#' The embedding lag is estimated from the simplex projection routine.
#'
#' @param v Numeric vector containing the series.
#' @param exclude.first.col Should first column in data frame be dropped?
#' @param min.E The minimum embedding dimension to consider.
#' @param max.E The maximum embedding dimension to consider.
#' @param min.tau The minimum embedding lag to consider.
#' @param max.tau The maximum embedding lag to consider
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
#' @param lag.method The method to compute the orbital lag (Theiler window),
#'   which excludes temporal neighbours to reduce correlation bias. Either
#'   use the autocorrelation function (lag.method = "acf" or "autocorrelation")
#'   or the lagged mutual information function (lag.method = "mi" or
#'   "mutual information").
#' @param optimise.FNNdim Should false nearest neighbour criteria be applied?
#' @param optimise.boxcountdim Should box counting dimension criteria be applied?
#' @param plot.simplex.projection Plot the results of the simplex projection?
#' @return A data frame containing the optimal embeddings.
#' @export optimise_embedding_dim
#'
optimise_embedding_dim <- function(v,
                                   optimise.simplex = T,
                                   optimise.FNNdim = T,
                                   optimise.boxcountdim = T,
                                   min.embedding.dim = 2,
                                   max.embedding.dim = 10,
                                   orbital.lag = NULL,
                                   lag.max = ceiling(length(v)*0.2),
                                   lag.method = "mi",
                                   embedding.lag = 1,
                                   plot.simplex.projection = F,
                                   return.all = T) {
    optimal.embedding.dims = c("simplex.projection.optimisation" = NA,
                               "FNN.criterion" = NA,
                               "boxcount.criterion" = NA)

    if (!optimise.simplex &&
        !optimise.FNNdim &&
        !optimise.boxcountdim) {
            warning("No optimization activated. Defaulting to simplex projection only",
                    immediate. = T)
            dim = optimise_dim_simplex(v,
                                       min.embedding.dim = min.embedding.dim,
                                       max.embedding.dim = max.embedding.dim,
                                       embedding.lag = embedding.lag,
                                       plot.simplex.projection = plot.simplex.projection
                                       )
            optimal.embedding.dims[["simplex.projection"]] = dim
    }

    if (optimise.simplex) {
        dim = optimise_dim_simplex(v = v,
                                   min.embedding.dim = min.embedding.dim,
                                   max.embedding.dim = max.embedding.dim,
                                   embedding.lag = embedding.lag,
                                   plot.simplex.projection = plot.simplex.projection
        )
        optimal.embedding.dims["simplex.projection.optimisation"] = dim
    }

    if (optimise.FNNdim) {
        dim = optimise_dim_FNN(v = v,
                               max.embedding.dim = max.embedding.dim,
                               embedding.lag = embedding.lag,
                               orbital.lag = orbital.lag,
                               lag.max = )
        optimal.embedding.dims["FNN.criterion"] = dim
    }
    if (optimise.boxcountdim) {
        dim = optimise_dim_boxcount(v = v)
        optimal.embedding.dims["boxcount.criterion"] = dim
    }
    if (return.all) return(optimal.embedding.dims)
    return(max(optimal.embedding.dims))
}