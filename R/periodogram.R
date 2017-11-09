
#' Computes a periodogram for a time series. Wrapper around multitaper::spec.
#' @param ts A time series of equally spaced data for which to compute the periodogram.
#' @param nw nw a positive double precision number, the time-bandwidth parameter.
#' @param k k a positive integer, the number of tapers, often 2*nw.
#' @export
periodogram <- function(ts,
                        frequency = 1, dtUnits = "year", deltat = 1000,
                        nw = 4.0, k = 8) {
  timeseries = stats::ts(ts, deltat = 1, frequency = 1)
  multitaper.spec = multitaper::spec.mtm(timeseries, dtUnits = dtUnits, deltat = deltat, nw = nw, k = k)
  spec = multitaper.spec$spec
  freq = multitaper.spec$freq

  spectral.estimate = data.frame(freq, spec)

  return(spectral.estimate)
}
