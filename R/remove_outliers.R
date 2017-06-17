#' Removes outliers from a vector (values outside 1.5 * IQR).
#'
#' @param v A scalar valued vector.
#' @return The vector 'v' with the outliers removed.
#' @export remove_outliers
remove_outliers <- function(v) {
    v[!v %in% grDevices::boxplot.stats(v)$out]
}
