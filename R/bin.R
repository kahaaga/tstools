pacman::p_load(tidyr, dplyr)

#' Bin a dataset by a common column "Age".
#'
#' @param dt A data frame containing the data to be binned.
#' @param bin.size The size of the bins
#' @param bin.average.function The function to use for averaging bins. Defaults to mean function.
#' @param interpolate Should empty bins be interpolated linearly?
#' @param remove.na Should NAs remaining after interpolation (usually at endpoints after interpolatin) be removed? BEWARE: be careful about removing nans before interpolating.
#' @param agepoint How should time indices be constructed? Start, mid or endpoint of bins?
#' @export bin
#'
bin <- function(dt,
                bin.size,
                common.column,
                bin.average.function = mean.narm,
                interpolate = T,
                remove.na = T,
                agepoint = "mid") {
    bin.min = plyr::round_any(min(dt[, common.column]), bin.size, f=floor)   # Round down to nearest bin
    bin.max = plyr::round_any(max(dt[, common.column]), bin.size, f=ceiling) # Round up to nearest bin

    dt$bin = cut(dt[, common.column], breaks = seq(from = bin.min, to = bin.max, by = bin.size), include.lowest=T)
    dt = dplyr::group_by(.data = dt, .dots = bin)
    dt = dplyr::summarise_each(dt, funs(bin.average.function))
    dt = dplyr::ungroup(dt) %>% as.data.frame
    dt[, common.column] = ColwiseBinMean(col = dt$.dots, agepoint = agepoint)
    dt = dplyr::ungroup(dt)
    dt = as.data.frame(dt)
    dt = dt[, 2:(ncol(dt)-1)]
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
    dt = dt[order(dt[, common.column], decreasing = T), ]
    dt$bin.size = rep(bin.size)
    dt$agepoint = rep(agepoint)

    return(dt)
}

ColwiseBinMean <- function(col, agepoint = "mid") {
    return(sapply(col, FUN = function(row) {BinMean(row, agepoint)}))
}

BinMean <- function(bin, agepoint = "mid") {
    start = as.numeric(sub("[\\(\\[]", "", sub(",.*", "", bin)))
    stop = as.numeric(sub("]", "", sub(".*,", "", bin)))

    if (agepoint == "start") return(start)
    if (agepoint == "mid") return(start + (stop - start)/2)
    if (agepoint == "end") return(stop)
}

mean.narm = function(v) {
    return(mean(v, na.rm = T))
}


interpolate_binned_data <- function(ds) {
    ds[2:ncol(ds)] = zoo::na.approx(ds[,2:ncol(ds)], rule = 2)
    return(ds)
}
