pacman::p_load(grDevices)

#' Removes outliers from a vector (values outside 1.5 * IQR).
#'
#' @param v A scalar valued vector.
#' @return The vector 'v' with the outliers removed.
#' @export remove.outliers
remove.outliers <- function(v) {
    v[!v %in% boxplot.stats(v)$out]
}
