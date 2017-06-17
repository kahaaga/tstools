' Computes a set of basic statistics for a scalar valued vector
#'
#' @param v A scalar valued vector.
#' @return A named vector of statistical parameters of the vector 'v'
#' @export basicstats
stats <- function(v) {
    if (class(v) == "data.frame") v = as.ve
    v.min = min(v, na.rm = T)
    v.max = max(v, na.rm = T)
    v.mean = mean(v, na.rm = T)
    v.median = stats::median(v, na.rm = T)
    v.mad = stats::mad(v, na.rm = T)
    v.IQR = stats::IQR(v, na.rm = T)
    v.p01 = stats::quantile(v, 0.01, na.rm = T)[[1]]
    v.p05 = stats::quantile(v, 0.05, na.rm = T)[[1]]
    v.p95 = stats::quantile(v, 0.95, na.rm = T)[[1]]
    v.p99 = stats::quantile(v, 0.99, na.rm = T)[[1]]
    v.samplesize.with.nans = length(v[complete.cases(v)])
    v.samplesize = length(v)

    v.outliers.fraction = length(grDevices::boxplot.stats(v))/v.samplesize


    return(c("min" = v.min,
             "max" = v.max,
             "mean" = v.mean,
             "median" = v.median,
             "MAD" = v.mad,
             "IQR" = v.IQR,
             "p01" = v.p01,
             "p05" = v.p05,
             "p95" = v.p95,
             "p99" = v.p99,
             "samplesize" = v.samplesize,
             "samplesize.withnans" = v.samplesize.with.nans,
             "outlier.fraction" = v.outliers.fraction))
}
