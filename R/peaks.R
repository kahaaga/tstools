#' Finds peaks in a time series by using a sliding window.
#'
#' @param x The vector of numbers for which to identify peaks
#' @param npoints The number of points to either side of the local maxima.
#'
#' Author: user 'stas g' on stackexchange at
#'https://stats.stackexchange.com/questions/22974/how-to-find-local-peaks-valleys-in-a-series-of-data
find_local_maxima <- function (x, npoints = 3){
  shape <- diff(sign(diff(x, na.pad = FALSE)))

  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - npoints + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + npoints + 1
    w <- ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1]))
      return(i + 1)
    else
      return(numeric(0))
  })

  pks <- unlist(pks)
  pks
}

#' Finds peaks in a time series by using a sliding window.
#' Wrapper around find_local_maxima().
#'
#' @param x The vector of numbers for which to identify peaks
#' @param npoints The number of points to either side of the local maxima.
#'
#' Author: user 'stas g' on stackexchange at
#'https://stats.stackexchange.com/questions/22974/how-to-find-local-peaks-valleys-in-a-series-of-data
find_peaks <- function(x, npoints = 3) {
  find_local_maxima(x, npoints)
}

#' Finds troughs in a time series by using a sliding window.
#'
#' @param x The vector of numbers for which to identify peaks
#' @param npoints The number of points to either side of the local minima.
#'
#' Author: user 'stas g' on stackexchange at
#'https://stats.stackexchange.com/questions/22974/how-to-find-local-peaks-valleys-in-a-series-of-data
find_local_minima <- function (x, npoints = 3){
  # Negate the input to find local minima with the local maxima function.
  find_local_maxima(-x)
}

#' Finds peaks in a time series by using a sliding window.
#' Wrapper around find_local_maxima().
#'
#' @param x The vector of numbers for which to identify peaks
#' @param npoints The number of points to either side of the local maxima.
#'
#' Author: user 'stas g' on stackexchange at
#'https://stats.stackexchange.com/questions/22974/how-to-find-local-peaks-valleys-in-a-series-of-data
find_troughs <- function(x, npoints = 3) {
  find_local_minima(x, npoints)
}
