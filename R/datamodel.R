#' A wrapper around datamodel()
#'
#' @param data A vector of data.
#' @param sigmas A vector of 1 sigma uncertainties associated with the data.
#' @param n.sigma How many sigmas should be allowed?
#' @param lower_bounds A vector of lower bounds for the data.
#' @param upper_bounds A vector of upper bounds for the data.
#' @export datamodels A matrix of data models
datamodels <- function(data,
                       sigmas = NA,
                       n.sigma = 2,
                       lower_bounds = NA,
                       upper_bounds = NA,
                       n.models = 1) {

    models = replicate(n = n.models,
                       expr = datamodel(data = data,
                                        sigmas = sigmas,
                                        n.sigma = n.sigma,
                                        lower_bounds = lower_bounds,
                                        upper_bounds = upper_bounds),
                       simplify = TRUE)
    return(models)
}

#' Draws a random data model given a vector 'data' and
#' its associated uncertainties, either in the form of
#' 'sigmas' or a set of 'lower_bounds' and 'upper_bounds'.
#' If 'sigmas' is provided, draw from a Gaussian centered
#' on the data. If bounds are provided, draw from a
#' Gaussian truncated at the lower and upper bounds.
#'
#' @param data A vector of data.
#' @param sigmas A vector of 1 sigma uncertainties associated with the data.
#' @param n.sigma How many sigmas should be allowed?
#' @param lower_bounds A vector of lower bounds for the data.
#' @param upper_bounds A vector of upper bounds for the data.
#'
datamodel <- function(data,
                      sigmas = NA,
                      n.sigma = 2,
                      lower_bounds = NA,
                      upper_bounds = NA) {
    if (any(is.na(sigmas)) == FALSE) {
        return(datamodel.sigmas(data = data,
                                sigmas = sigmas,
                                n.sigma = n.sigma))
    } else if (is.vector(lower_bounds) && is.vector(upper_bounds)) {
        return(datamodel.sigmas(data = data,
                                sigmas = sigmas,
                                n.sigma = n.sigma))
    } else {
        warning("Input to datamodel() is not valid.")
    }
}

datamodel.sigmas <- function(data, sigmas, n.sigma) {
    datamodel = matrix(nrow = length(data), ncol = 1)
    for (i in 1:length(data)) {
        datamodel[i] = msm::rtnorm(n = 1, mean = data[i], sd = n.sigma * sigmas[i],
                                   lower = data[i] - n.sigma * sigmas[i],
                                   upper = data[i] + n.sigma * sigmas[i])
    }
    return(datamodel)
}

datamodel.truncated <- function(data, lower_bounds, upper_bounds, n.sigma) {
    datamodel = matrix(nrow = length(data), ncol=1)
    for (i in 1:length(data)) {
        datamodel[i] = msm::rtnorm(n=1, mean = data[i], sd = n.sigma,
                                   lower = lower_bounds[i],
                                   upper = upper_bounds[i])
    }
    return(datamodel)
}
