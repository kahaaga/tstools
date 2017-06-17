#' Finds the minimal embedding dimension of a series 'v' according to
#' the boxcounting dimension criterion.
#'
#' @param v A numeric vector containing the series.
#' @return The embedding dimension according to the
#'   boxcounting dimension criterion.
#' @export optimise_dim_boxcount
optimise_dim_boxcount <- function(v) {
    boxcount.dimension = fractaldim::fd.estim.boxcount(v)$fd
    return(ceiling(boxcount.dimension * 2))
}
