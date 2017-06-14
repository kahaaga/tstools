require(tidyr)
require(dplyr)

#' Bin a dataset by a common column "Age".
#'
#' @param dt A data frame containing the data to be binned.
#' @param bin.size The size of the bins
#' @param bin.average.function The function to use for averaging bins. Defaults to mean function.
#' @param interpolate Should empty bins be interpolated linearly?
#' @param remove.na Should NAs remaining after interpolation (usually at endpoints after interpolatin) be removed? BEWARE: be careful about removing nans before interpolating.
#'
#' @export bin
#'
bin <- function(dt,
                bin.size,
                bin.average.function = mean.narm,
                interpolate = T,
                remove.na = T) {
    bin.min = plyr::round_any(min(dt$Age), bin.size, f=floor)   # Round down to nearest bin
    bin.max = plyr::round_any(max(dt$Age), bin.size, f=ceiling) # Round up to nearest bin

    binned = dt %>%
        dplyr::mutate_(bin = cut(dt$Age, breaks = seq(from = bin.min, to = bin.max, by = bin.size), include.lowest=T)) %>%
        dplyr::group_by_(bin) %>%  #summarise_each(bin.average.function, by="bin")
        dplyr::summarise_each_q(bin.average.function) %>%
        tidyr::complete_(bin) %>%
        dplyr::mutate_(Age = ColwiseBinMean(bin))
    binned = dplyr::arrange_(dplyr::desc(binned$Age)) %>% as.data.frame %>%
        dplyr::select_(-bin) # We don't need the bin column anymore

    if (interpolate & remove.na) {
        binned = zoo::na.approx(binned)
        binned = binned[stats::complete.cases(binned),]
        binned = as.data.frame(binned)
    }
    if (!interpolate & remove.na) {
        warning("Careful! Removing NA bins without interpolating. Data are not on on an equidistant grid anymore!!")
    }

    return(binned)
}

interpolate_binned_data <- function(ds) {
    ds[2:ncol(ds)] = zoo::na.approx(ds[,2:ncol(ds)], rule = 2)
    return(ds)
}

ColwiseBinMean <- function(col) {
    return(sapply(col, FUN=function(row) BinMean(row)))
}

BinMean <- function(bin) {
    start = as.numeric(sub("[\\(\\[]", "", sub(",.*", "", bin)))
    stop = as.numeric(sub("]", "", sub(".*,", "", bin)))

    return(start + (stop - start)/2)
}

mean.narm = function(v) {
    return(mean(v, na.rm = T))
}
