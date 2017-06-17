#' Generates surrogate series for 'ts', which is a numeric vector.
#'
#'  Implemented methods are:
#'    "random"    (Randomly shuffle data)
#'    "phase"     (Theiler's phase randomization - constrained)
#'    "ebisuzaki" (Another form (identical?) of phase randomisation)
#'    "aaft"      (Theiler's AAFT - constrained)
#'    "ce"        (Davies and Harte's Circulant Embedding - nonconstrained)
#'    "dh"        (Davison and Hinkleys phase and amplitude randomization - nonconstrained)
#'
#'    The null hypotheses for these methods are (information about the
#'    methods of the 'fractal' package is mostly paraphrased or directly
#'     copied from its R-documentation):
#'    "random":    The data represents white noise.
#'    "phase":     The original data can be reproduced by a linear Gaussian process.
#'                 The phase at each frequency of the Fourier transform of the original
#'                 time series is randomised to be uniformly distributed on [0, 2*PI].
#'                 Periodograms of the surrogate and the original time series are the same.
#'    "ebisuzaki":
#'    "aaft:       Create normally distributed white noise, rank-ordered according to the
#'                 original time series. The white noise is then phase randomised, and its
#'                 rank is calculated. The surrogate is then produced by rank ordering the
#'                 original time series according to the new rank. Surrogate preserves the
#'                 amplitude distribution (histogram) of the original time series.
#'    "ce":        Circulant embedding. Generate surrogate whose estimated SDF (periodogram)
#'                 is not constrained to be the same as for the original time series.
#'                 Unconstrained.
#'    "dh":        Davison-Hinkley surrogates. Randomises both the phases and the amplitudes
#'                 in the frequency domain. The surrogate is then produced by inverting back
#'                 to the time domain. Unconstrained.
#'
#' @param ts Time series for which to generate surrogate time series. A vector.
#' @param n.surrogates The number of surrogates to generate.
#' @param method String indicating the surrogate time series generation method.
#'
#' @export create_surrogates
create_surrogates <- function(ts,
                              surrogate.method = "random",
                              n.surrogates = 1,
                              print.to.console = F) {

    # Check if method is valid
    if (!(surrogate.method %in% c("aaft", "iaaft", "ebisuzaki", "random", "phase", "ce", "dh", "seasonal"))) {
        stop(paste("@OneWayCCM: Surrogate type", paste("'", surrogate.method, "'", sep = ""),"not valid"))
    }

    if (tolower(surrogate.method) == "ebisuzaki") {
        surr.data = rEDM::make_surrogate_data(ts = ts, method="ebisuzaki", num_surr = n.surrogates)

        # Random shuffle
    } else if (tolower(surrogate.method) == "random") {
        surr.data = rEDM::make_surrogate_data(ts = ts, method="random", num_surr = n.surrogates)

        # Seasonal surrogates.
    } else if (tolower(surrogate.method) == "seasonal") {
        surr.data = rEDM::make_surrogate_data(ts = ts, method = "seasonal", num_surr = n.surrogates, T_period = 1)
    }

    else if (tolower(surrogate.method) == "iaaft" |
             tolower(surrogate.method) == "i-aaft" ){
        surr.data = replicate(n.surrogates, .iAAFT(Xcor = ts))
    }

    # Amplitude adjusted Fourier transform (Theiler, 1992).
    if (tolower(surrogate.method) == "aaft") {
        surr.data = replicate(n.surrogates, fractal_package_surrogate(ts, method = "aaft"))

        # Phase randomisation (Theiler, 1992).
    } else if (tolower(surrogate.method) == "phase") {
        surr.data = replicate(n.surrogates, fractal_package_surrogate(ts, method = "phase"))

        # Circulant embedding (non-constrained realizations)
    } else if (tolower(surrogate.method) == "ce" |
               tolower(surrogate.method) == "circulant embedding") {
        surr.data = replicate(n.surrogates, fractal_package_surrogate(ts, method = "ce"))

        # Phase and amplitude randomisation (non-constrained realizations)
    } else if (tolower(surrogate.method) == "dh" |
               tolower(surrogate.method) == "davison-hinkley" |
               tolower(surrogate.method) == "phase and amplitude randomisation") {
        surr.data = replicate(n.surrogates, fractal_package_surrogate(ts, method = "dh"))

    }

    return(surr.data)
}


#' Simple wrapper for the fractal::surrogate function, to enable creating
#' multiple surrogate time series in one call.
fractal_package_surrogate <- function(ts, method = "aaft") {
    if (!method %in% c("aaft", "ce", "dh", "phase")) warning("ERROR: surrogate method is not valid")
    fractal::surrogate(x = ts, method = method)
}
