#' Draws random datagiven a vector 'data' and
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
#' @return  A matrix of randomly drawn data.
#'
#' @export draw_random_data
draw_random_data <- function(data,
                       sigmas = NA,
                       n.sigma = 2,
                       n.replicates = 1,
                       lower_bounds = NA,
                       upper_bounds = NA) {

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
#' @param lower.bounds A vector of lower bounds for the data.
#' @param upper.bounds A vector of upper bounds for the data.
#' @return A vector containing the
datamodel <- function(data,
                      sigmas = NA,
                      n.sigma = 2,
                      lower.bounds = NA,
                      upper.bounds = NA) {
    if (any(is.na(sigmas)) == FALSE) {
        return(datamodel_sigmas(data = data,
                                sigmas = sigmas,
                                n.sigma = n.sigma))
    } else if (is.vector(lower.bounds) && is.vector(upper.bounds)) {
        return(datamodel_truncated(data = data,
                                sigmas = sigmas,
                                n.sigma = n.sigma))
    } else {
        warning("Input to datamodel() is not valid.")
    }
}

datamodel_sigmas <- function(data, sigmas, n.sigma) {
    datamodel = matrix(nrow = length(data), ncol = 1)
    for (i in 1:length(data)) {
        datamodel[i] = msm::rtnorm(n = 1, mean = data[i], sd = n.sigma * sigmas[i],
                                   lower = data[i] - n.sigma * sigmas[i],
                                   upper = data[i] + n.sigma * sigmas[i])
    }
    return(datamodel)
}

datamodel_truncated <- function(data, lower.bounds, upper.bounds, n.sigma) {
    datamodel = matrix(nrow = length(data), ncol = 1)
    for (i in 1:length(data)) {
        datamodel[i] = msm::rtnorm(n = 1,
                                   mean = data[i],
                                   sd = n.sigma,
                                   lower = lower.bounds[i],
                                   upper = upper.bounds[i])
    }
    return(datamodel)
}