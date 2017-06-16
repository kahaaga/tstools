' Computes a set of basic statistics for a scalar valued vector
#'
#' @param v A scalar valued vector.
#' @return A named vector of statistical parameters of the vector 'v'
#' @export basicstats
basicstats <- function(v) {

    v.min = base::min(v)
    v.max = base::max(v)
    v.mean = base::mean(v)
    v.median = stats::median(v)
    v.mad = stats::mad(v)
    v.IQR = stats::IQR(v)
    v.p01 = stats::quantile(v, 0.01)
    v.p05 = stats::quantile(v, 0.05)
    v.p95 = stats::quantile(v, 0.95)
    v.p99 = stats::quantile(v, 0.99)
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
             "outlier.fraction" = v.outliers.fraction))
}
