#'  Bin a dataset with multiple columns by one of the columns.
#'
#' @param dt A data frame containing the data to be binned.
#' @param bin.size The size of the bins
#' @param bin.average.function The function to use for averaging bins. Default
#'   is the arithmetic mean.
#' @param by The name of the column to bin by.
#' @param start The minimum value of the 'by' column for which to subset data.
#'     Defaults to the first lower bin limit < min(dt[, by]), obeying bin.size.
#' @param end The maximum value of the 'by' column for which to subset data.
#'     Defaults to the first upper bin limit > max(dt[, by]), obeying bin.size.
#' @param add.binning.info Should info on the binning be added to the resulting
#'   dataframe?
#' @param interpolate Should empty bins be interpolated linearly?
#' @param remove.na Should NAs remaining after interpolation (usually at
#'   endpoints after interpolatin) be removed? BEWARE: be careful about removing
#'    nans before interpolating.
#' @param time.sampled.at How time indices are constructed. Either 'start',
#'   'mid' or 'end' of bins. Defaults to 'mid'.
#' @export bin
bin <- function(dt,
                bin.size,
                by,
                bin.average.function = mean.narm,
                interpolate = T,
                remove.na = T,
                time.sampled.at = "mid",
                start = NULL,
                end = NULL,
                add.binning.info = F) {

  bin.func <- dplyr::enquo(bin.average.function)

  dt$bin <- create_bins_df(df = dt, by = by,
                          start = start, end = end,
                          bin.size = bin.size)

  # Summarise observations bin-wise using the supplied function.
  dt <- dplyr::group_by(dt, bin)
  dt <- dplyr::summarise_all(dt, dplyr::funs(!!bin.func))
  dt <- dplyr::ungroup(dt)

  # Sample the "by" column at either start point,
  # mid-point or end-point of bin intervals
  dt[, by] <- Colwiseinterval_mean(col = as.vector(dt$bin),
                                  time.sampled.at = time.sampled.at)
  dt <- dplyr::ungroup(dt)
  dt <- as.data.frame(dt)

  # Remove "bin" column before interpolating

  dt <- dt[, 2:(ncol(dt))]

  if (interpolate & remove.na) {
    dt[, !(names(dt) %in% c(by))] <- zoo::na.approx(dt[, 2:ncol(dt)])
    dt <- dt[stats::complete.cases(dt), ]
  }
  if (interpolate & !remove.na) {
    dt[, !(names(dt) %in% c(by))] <- zoo::na.approx(dt[, 2:ncol(dt)])
  }
  if (!interpolate & remove.na) {
    warning(paste("Careful! Removing rows containing NA without interpolating.",
                   "Data are not on on an eqully spaced grid anymore.",
                  "Data will be missing for some values of the binning (by)",
                  "variable!"))
    dt <- dt[stats::complete.cases(dt), ]
  }

  dt <- dt[order(dt[, by], decreasing = T), ]

  if (add.binning.info) {
    dt$bin.size <- rep(bin.size)

    dt$time.sampled.at <- rep(time.sampled.at)
  } else {
    dt <- dt[, !(names(dt) %in% c("bin"))]
  }


  return(dt)
}

create_bins_df <- function(df, by, bin.size,
                           start = NULL,
                           end = NULL,
                           include.lowest = T) {

  # If not provided, minimum bin time is rounded down to nearest bin edge, given
  # the minimum of df[, by]. Otherwise, use the provided value.
  bin.min <- ifelse(test = !is.null(start),
                   yes = start,
                   no = plyr::round_any(min(df[, by]), bin.size, f = floor))

  # If not provided, maximum bin time is rounded up to nearest bin edge, given
  # the maximum of df[, by]. Otherwise, use the provided value.
  bin.max <- ifelse(test = !is.null(end),
                   yes = end,
                   no = plyr::round_any(max(df[, by]), bin.size, f = ceiling))

  # Find bin breaks
  bin.breaks <- seq(from = bin.min, to = bin.max, by = bin.size)

  # Create bins
  bins <- cut(df[, by], breaks = bin.breaks, include.lowest = T)

  return(bins)
}

Colwiseinterval_mean <- function(col, time.sampled.at = "mid") {
  return(sapply(col, FUN = function(row) {
    interval_mean(row, time.sampled.at)
    })
  )
}

interval_mean <- function(interval, time.sampled.at = "start") {
  int.start <- strsplit(x = interval, split = ",")[[1]][1]
  int.stop <- strsplit(x = interval, split = ",")[[1]][2]

  start <- substring(int.start, 2, nchar(int.start))
  stop <- substring(int.stop, 1, nchar(int.stop) - 1)

  start <- as.numeric(start)
  stop <- as.numeric(stop)

  if (time.sampled.at == "start") return(start)
  if (time.sampled.at == "mid") return(mean(c(start, stop)))
  if (time.sampled.at == "end") return(stop)
}

mean.narm <- function(v) {
  return(mean(v, na.rm = T))
}
