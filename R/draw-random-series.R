#' Draws random datagiven a vector 'data' and
#' its associated uncertainties, either in the form of
#' 'sigmas' or a set of 'lower.bounds' and 'upper.bounds'.
#' If 'sigmas' is provided, draw from a Gaussian centered
#' on the data. If bounds are provided, draw from a
#' Gaussian truncated at the lower and upper bounds.
#'
#' @param data A vector of data.
#' @param sigmas A vector of 1 sigma uncertainties associated with the data.
#' @param n.sigma How many sigmas should be allowed?
#' @param lower.bounds A vector of lower bounds for the data.
#' @param upper.bounds A vector of upper bounds for the data.
#' @return  A matrix of randomly drawn data.
#'
#' @export draw_random_data
draw_random_data <- function(data,
                       sigmas = NULL,
                       n.sigma = 2,
                       n.replicates = 1,
                       lower.bounds = NULL,
                       upper.bounds = NULL) {

    models = replicate(n = n.replicates,
                       expr = datamodel(data = data,
                                        sigmas = sigmas,
                                        n.sigma = n.sigma,
                                        lower.bounds = lower.bounds,
                                        upper.bounds = upper.bounds),
                       simplify = TRUE)
    return(models)
}

#' Draws a random data model given a vector 'data' and
#' its associated uncertainties, either in the form of
#' 'sigmas' or a set of 'lower.bounds' and 'upper.bounds'.
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
                      sigmas = NULL,
                      n.sigma = 2,
                      lower.bounds = NULL,
                      upper.bounds = NULL) {
    if (any(is.na(sigmas)) == FALSE) {
      return(datamodel_sigmas(data = data,
                              sigmas = sigmas,
                              n.sigma = n.sigma))

    } else if (is.null(sigmas) ||
              (is.vector(lower.bounds) && is.vector(upper.bounds))) {
      return(datamodel_truncated(data = data,
                                 lower.bounds = lower.bounds,
                                 upper.bounds = upper.bounds,
                                 n.sigma = n.sigma))
    } else {
        warning("Input to datamodel() is not valid.")
    }
}

#' Draws random data according to a Gaussian distribution centered
#' around the original data with standard deviations given by
#' n.sigma * sigmas.
#' @param data A vector of data.
#' @param sigmas A vector of standard deviations.
#' @param n.sigma The number of standard deviations for the Gaussian.
datamodel_sigmas <- function(data, sigmas, n.sigma) {
    datamodel = matrix(nrow = length(data), ncol = 1)
    for (i in 1:length(data)) {
        datamodel[i] = msm::rtnorm(n = 1, mean = data[i], sd = n.sigma * sigmas[i],
                                   lower = data[i] - n.sigma * sigmas[i],
                                   upper = data[i] + n.sigma * sigmas[i])
    }
    return(datamodel)
}

#' Draws random data according to a Gaussian with standard deviation
#' n.sigma centered at the original data and truncated at the
#' lower.bounds and upper.bounds.
#' @param data A vector of data.
#' @param lower.bounds A vector of lower bounds.
#' @param upper.bounds A vector of upper bounds.
#' @param n.sigma The number of standard deviations for the Gaussian.
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
