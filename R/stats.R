#' Computes a set of basic statistics for a scalar valued vector
#'
#' @param v A scalar valued vector.
#' @return A named vector of statistical parameters of the vector 'v'
#' @export stats
stats <- function(v) {
    if (class(v) == "data.frame") v = as.vector(v)

    v.samplesize.without.nans = length(v[complete.cases(v)])
    v.samplesize = length(v)
    v.stdev = stats::sd(v, na.rm = T)
    v.min = min(v, na.rm = T)
    v.max = max(v, na.rm = T)
    v.mean = mean(v, na.rm = T)
    v.median = stats::median(v, na.rm = T)
    v.mad = stats::mad(v, na.rm = T)
    v.IQR = stats::IQR(v, na.rm = T)
    Q1 = quantile(v, 0.25, na.rm = T)
    Q3 = quantile(v, 0.75, na.rm = T)
    notchlower = v.median - (1.58 * v.IQR/sqrt(v.samplesize.without.nans))
    notchupper = v.median + (1.58 * v.IQR/sqrt(v.samplesize.without.nans))
    v.p01 = stats::quantile(v, 0.01, na.rm = T)[[1]]
    v.p05 = stats::quantile(v, 0.05, na.rm = T)[[1]]
    v.p95 = stats::quantile(v, 0.95, na.rm = T)[[1]]
    v.p99 = stats::quantile(v, 0.99, na.rm = T)[[1]]

    v.outliers.fraction = length(grDevices::boxplot.stats(v))/v.samplesize


    return(c("min" = v.min,
             "max" = v.max,
             "mean" = v.mean,
             "median" = v.median,
             "stdev" = v.stdev,
             "MAD" = v.mad,
             "IQR" = v.IQR,
             "Q1"  = Q1,
             "Q3"  = Q3,
             "notchlower" = notchlower,
             "notchupper" = notchupper,
             "1st.percentile" = v.p01,
             "5th.percentile" = v.p05,
             "95th.percentile" = v.p95,
             "99th.percentile" = v.p99,
             "samplesize" = v.samplesize,
             "samplesize.without.nans" = v.samplesize.without.nans,
             "outlier.fraction" = v.outliers.fraction))
}
