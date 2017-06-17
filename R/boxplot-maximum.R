#' Find the largest v less than or equal to
#' upper hinge of boxplot, which is
#' Q1 (25th quantile) + 1.5 * IQR
#'
#' @param v A scalar valued vector.
#' @return The maximum value for a boxplot based on 'v'.
#' @export boxplot_maximum
boxplot_maximum <- function(v) {
    Q3 = stats::quantile(v, 0.75, na.rm = T)
    interquartile.range = stats::IQR(v, na.rm = T)
    filtered = v[v <= Q3 + (1.5 * interquartile.range)]
    return(max(filtered))
}
