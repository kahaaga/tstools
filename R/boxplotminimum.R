#' Find the smallest v greater than or equal to
#' lower hinge of boxplot, which is
#' Q1 (25th quantile) - 1.5 * IQR
#'
#' @param v A scalar valued vector.
#' @return The minimum value for a boxplot based on 'v'.
#' @export boxplot.minimum
boxplot.minimum <- function(v) {
    n = length(v)
    Q1 = quantile(v, 0.25, na.rm = T)
    interquartile.range = IQR(v, na.rm = T)
    filtered = v[v >= Q1 - (1.5 * interquartile.range)]
    return(min(filtered))
}
