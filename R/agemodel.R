pacman::p_load(msm)

#' A wrapper around agemodel() that returns a matrix of multiple age models.
#' Each column is an age model.
#'
#' @param ages A vector containing the age data.
#' @param sigmas A vector containing the 1 sigma uncertainties associated with the age data.
#' @param firstdiffagreementratio How large can the slope between time steps be? Defaults to 1, or 100%.
#' @param tolerance A tolerance level to speed up computations when age points are very close.
#' @export agemodels
#'
agemodels <- function(ages,
                      sigmas,
                      n.models = 2,
                      n.sigma = 2,
                      firstdiffagreementratio = 1,
                      tolerance = 10^-(10)
                      ) {

    agemodels = replicate(n = n.models,
                          expr = agemodel(ages = ages,
                                        sigmas = sigmas,
                                        n.sigma = n.sigma,
                                        firstdiffagreementratio = firstdiffagreementratio,
                                        tolerance = tolerance))
    return(agemodels)
}

#' Draw a random, strictly increasing age model given a vector 'ages' and
#' their associated 1 sigma uncertainties ('sigmas') according to a
#' Gaussian. Requires that the difference between time steps should be
#' within a ratio firstdiffagreementratio' (default to 1, or 100%).
#'
#' @param ages A vector containing the age data.
#' @param sigmas A vector containing the 1 sigma uncertainties associated with the age data.
#' @param firstdiffagreementratio How large can the slope between time steps be? Defaults to 1, or 100%.
#' @param tolerance A tolerance level to speed up computations when age points are very close.
#' @export agemodel
#'
agemodel <- function(ages, sigmas,
                     n.sigma = 2,
                     firstdiffagreementratio=1,
                     tolerance = 10^-(12)) {

    if (all(ages == cummax(ages))) {
    #    cat("Ages are strictly increasing. Using data as is. \n")
    } else if (all(ages == cummin(ages))) {
    #    cat("Ages are strictly decreasing. Reversing data\n")
        ages = rev(ages)
        sigmas = rev(sigmas)
    } else {
        warning("Ages are neither strictly decreasing nor increasing. Check input!\n")
    }

    #cat("Drawing agemodel ... \n")
    l = length(ages)
    agemodel = numeric(l)

    # Set upper and lower bounds for first draw
    lower_bound = ages[1] - n.sigma * sigmas[1]
    upper_bound = min(ages[1] - n.sigma * sigmas[1],
                      ages[2] - n.sigma * sigmas[2])

    # First age
    agemodel[1] = msm::rtnorm(n=1, mean = ages[1], sd = n.sigma * sigmas[1],
                              lower = lower_bound,
                              upper = upper_bound)

    # Iteratively draw ages from 2:n-1
    for (i in 2:l-1) {
        #flush.console()
        #if (i %% 10 == 0) cat("\ri = ", i)
        #flush.console()
        lower_bound = max(ages[i] - n.sigma * sigmas[i], agemodel[i-1]+tolerance)
        upper_bound = min(ages[i] + n.sigma * sigmas[i],
                          ages[i+1] + n.sigma * sigmas[i+1],
                          agemodel[i-1] * (1 + firstdiffagreementratio))
        upper_bound = max(lower_bound + tolerance,
                          upper_bound)
        if (lower_bound > upper_bound) {
            redraw_n = 0
            #cat("got stuck. restarting function\n")

            break;
            while (lower_bound > upper_bound) {
                redraw_n = redraw_n + 1
                if (redraw_n > 5000) {
                    break
                }
                lower_bound = max(mean_age - n.sigma * sigma,
                                  agemodel[i-2] + tolerance)
                upper_bound = min(mean_age + n.sigma * sigma,
                                  ages[i] + n.sigma * sigma,
                                  agemodel[i-2] * (1 + firstdiffagreementratio))
            }
        } else {
            agemodel[i] =  msm::rtnorm(n=1, mean = ages[i], sd = n.sigma * sigmas[i],
                                       lower = lower_bound,
                                       upper = upper_bound)
        }
    }
    # Final age
    lower_bound = max(ages[l] - n.sigma * sigmas[l], agemodel[l-1] + tolerance)
    upper_bound = min(ages[l] + n.sigma * sigmas[l], agemodel[l-1] * ( 1+ firstdiffagreementratio))
    agemodel[l] = msm::rtnorm(n=1, mean = ages[l], sd = n.sigma * sigmas[l],
                              lower = lower_bound,
                              upper = upper_bound)

    # Make sure final agemodel is strictly increasing,
    # if not, redraw
    monotonically.strictly.increasing = F

    while (!(monotonically.strictly.increasing)) {
        if (all(diff(agemodel) > 0)) {
            break
        } else {
            cat("Agemodel not strictly increasing. Re-drawing.")
            agemodel = agemodel(ages = ages,
                                sigmas = sigmas,
                                n.sigma = n.sigma,
                                firstdiffagreementratio = firstdiffagreementratio,
                                tolerance = tolerance)
        }
    }
    if (all(ages == cummax(ages))) return(rev(agemodel))
    else if (all(ages == cummin(ages))) return(agemodel)
}
