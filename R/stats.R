#' Computes a set of basic statistics for a scalar valued vector
#'
#' @param v A scalar valued vector.
#' @return A named vector of statistical parameters of the vector 'v'
#' @export
summary_stats <- function(v,
                          percentiles = c(0.01, 0.05, 0.1, 0.3, 0.7, 0.9, 0.95, 0.99)) {
    if (class(v) == "data.frame") v = as.vector(v)

    v.samplesize.without.nans = length(v[complete.cases(v)])
    v.samplesize = length(v)
    v.var = stats::var(v, na.rm = T)
    v.stdev = stats::sd(v, na.rm = T)
    v.min = min(v, na.rm = T)
    v.max = max(v, na.rm = T)
    v.mean = mean(v, na.rm = T)
    v.median = stats::median(v, na.rm = T)
    v.skewness = moments::skewness(v, na.rm = T)
    v.kurtosis = moments::kurtosis(v, na.rm = T)
    v.mad = stats::mad(v, na.rm = T)
    v.IQR = stats::IQR(v, na.rm = T)
    Q1 = quantile(v, 0.25, na.rm = T)[[1]]
    Q3 = quantile(v, 0.75, na.rm = T)[[1]]
    boxplotmin = Q1 - (1.5 * v.IQR)
    boxplotmax = Q3 + (1.5 * v.IQR)
    boxplotnotchlower = v.median - (1.58 * v.IQR / sqrt(v.samplesize.without.nans))
    boxplotnotchupper = v.median + (1.58 * v.IQR / sqrt(v.samplesize.without.nans))

    # Compute quantiles
    percentiles = sapply(X = percentiles,  FUN = function(p) {
                            qi = stats::quantile(v, p, na.rm = T)
                        })

    v.outliers.fraction = length(grDevices::boxplot.stats(v))/v.samplesize

    summary.stats = c("min" = v.min,
                      "max" = v.max,
                      "mean" = v.mean,
                      "median" = v.median,
                      "var" = v.var,
                      "stdev" = v.stdev,
                      "MAD" = v.mad,
                      "skewness" = v.skewness,
                      "kurtosis" = v.kurtosis,
                      "IQR" = v.IQR,
                      "Q1"  = Q1,
                      "Q3"  = Q3,
                      "boxplotmin" = boxplotmin,
                      "boxplotmax" = boxplotmax,
                      "boxplotnotchlower" = boxplotnotchlower,
                      "boxplotnotchupper" = boxplotnotchupper,
                      "samplesize" = v.samplesize,
                      "samplesize.without.nans" = v.samplesize.without.nans,
                      "outlier.fraction" = v.outliers.fraction)

    return(c(summary.stats, percentiles))
}
