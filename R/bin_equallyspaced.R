#' Create equally spaced bins for a column of a data frame.
#'
#' @param df The data frame.
#' @param by The column of the data frame.
#' @param bin.size The size of the bins.
bin_equallyspaced_df <- function(df, by = NULL, bin.size, include.lowest = T) {
  # Minimum bin time is rounded down to nearest bin
  bin.min = plyr::round_any(min(df[, by]), bin.size, f = floor)

  # Maximum bin time is rounded up to nearest bin
  bin.max = plyr::round_any(max(df[, by]), bin.size, f = ceiling)

  # Find bin breaks
  bin.breaks = seq(from = bin.min, to = bin.max, by = bin.size)

  # Create bins
  bins = cut(df[, by], breaks = bin.breaks, include.lowest = T)

  return(bins)
}

#' Create equally spaced bins for a vector.
#'
#' @param v The vector.
#' @param bin.size The size of the bins.
bin_equallyspaced_vector <- function(v, bin.size, include.lowest = T) {

  if (!is.vector(v)) stop("v is not a vector")

  # Minimum bin time is rounded down to nearest bin
  bin.min = plyr::round_any(min(v), bin.size, f = floor)

  # Maximum bin time is rounded up to nearest bin
  bin.max = plyr::round_any(max(v), bin.size, f = ceiling)

  # Find bin breaks
  bin.breaks = seq(from = bin.min, to = bin.max, by = bin.size)

  # Create bins
  bins = cut(v, breaks = bin.breaks, include.lowest = T)

  return(bins)
}
