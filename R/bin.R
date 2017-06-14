require(tidyr)
require(dplyr)

#' Bin a dataset by a common column "Age".
#'
#' @param dt A data frame containing the data to be binned.
#' @param dt A data frame containing the data to be binned.
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
        dplyr::mutate(bin = cut(Age, breaks = seq(from = bin.min, to = bin.max, by = bin.size), include.lowest=T)) %>%
        dplyr::group_by(bin) %>%  #summarise_each(bin.average.function, by="bin")
        dplyr::summarise_all(bin.average.function) %>%
        tidyr::complete(bin) %>%
        dplyr::mutate(Age = ColwiseBinMean(bin)) %>%
        dplyr::arrange(desc(Age)) %>%
        as.data.frame %>%
        dplyr::select(-bin) # We don't need the bin column anymore

    if (interpolate & remove.na) {
        binned = zoo::na.approx(binned)
        binned = binned[complete.cases(binned),]
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