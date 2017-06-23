#' Bin a dataset by a common column.
#'
#' @param dt A data frame containing the data to be binned.
#' @param bin.size The size of the bins
#' @param bin.average.function The function to use for averaging bins. Default is the arithmetic mean.
#' @param bin.column The column to use for binning.
#' @param interpolate Should empty bins be interpolated linearly?
#' @param remove.na Should NAs remaining after interpolation (usually at endpoints after interpolatin) be removed? BEWARE: be careful about removing nans before interpolating.
#' @param time.sampled.at How time indices are constructed. Either 'start', 'mid' or 'end' of bins. Defaults to 'mid'.
#' @export bin
bin <- function(dt,
                bin.size,
                bin.column,
                bin.average.function = mean.narm,
                interpolate = T,
                remove.na = T,
                time.sampled.at = "mid") {
    bin.min = plyr::round_any(min(dt[,bin.column]), bin.size, f=floor)   # Round down to nearest bin
    bin.max = plyr::round_any(max(dt[,bin.column]), bin.size, f=ceiling) # Round up to nearest bin

    dt$bins = cut(dt[,bin.column], breaks = seq(from = bin.min, to = bin.max, by = bin.size), include.lowest=T)
    dt = dplyr::group_by(dt, .data$bins)
    dt = dplyr::summarise_all(dt, funs(bin.average.function))
    dt = dplyr::ungroup(dt)
    dt[, bin.column] = Colwiseinterval_mean(col = dt[, bin.column], time.sampled.at = time.sampled.at)
    dt = dplyr::ungroup(dt)
    dt = as.data.frame(dt)
    dt = dt[, 2:ncol(dt)-1]
    print(head(dt))

    #return(dt)
    if (interpolate & remove.na) {
        dt[, 2:ncol(dt)] = zoo::na.approx(dt[, 2:ncol(dt)])
        dt = dt[stats::complete.cases(dt),]
    }
    if (interpolate & !remove.na) {
        dt[, 2:ncol(dt)] = zoo::na.approx(dt[, 2:ncol(dt)])
    }
    if (!interpolate & remove.na) {
        warning("Careful! Removing NA bins without interpolating. Data are not on on an equidistant grid anymore!!")
        dt = dt[stats::complete.cases(dt),]
    }
    print(dt)
    dt = dt[order(dt[, bin.column], decreasing = T), ]
    dt$bin.size = rep(bin.size)
    dt$time.sampled.at = rep(time.sampled.at)

    return(dt)
}

Colwiseinterval_mean <- function(col, time.sampled.at = "mid") {
    return(sapply(col, FUN = function(row) {interval_mean(row, time.sampled.at)}))
}

interval_mean <- function(interval, time.sampled.at = "start") {
    int.start = strsplit(x = interval, split = ",")[[1]][1]
    int.stop = strsplit(x = interval, split = ",")[[1]][2]
    start = substring(int.start, 2, nchar(int.start))
    stop = substring(int.stop, 1, nchar(int.stop) - 1)
    start = as.numeric(start)
    stop = as.numeric(stop)
    if (time.sampled.at == "start") return(start)
    if (time.sampled.at == "mid") return(mean(c(start, stop)))
    if (time.sampled.at == "end") return(stop)
}

mean.narm = function(v) {
    return(mean(v, na.rm = T))
}


interpolate_binned_data <- function(ds) {
    ds[2:ncol(ds)] = zoo::na.approx(ds[,2:ncol(ds)], rule = 2)
    return(ds)
}
