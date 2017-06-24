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
#' @export surrogate_ensemble
surrogate_ensemble <- function(ts,
                                surrogate.method = "random",
                                n.surrogates = 1,
                                print.to.console = F) {

    # Check if method is valid
    if (!(surrogate.method %in% c("aaft", "iaaft",
                                  "ebisuzaki", "random",
                                  "phase", "ce",
                                  "dh", "seasonal"))) {
        stop(paste("Surrogate type", paste("'", surrogate.method, "'", sep = ""),"not valid"))
    }

    if (tolower(surrogate.method) == "ebisuzaki") {
      surr.data = replicate(n.surrogates, ebisuzaki_surrogate(series = ts))

    # Random shuffle
    } else if (tolower(surrogate.method) == "random") {
      surr.data = replicate(n.surrogates, random_surrogate(series = ts))

    else if (tolower(surrogate.method) == "iaaft" ||
             tolower(surrogate.method) == "i-aaft" ){
        surr.data = replicate(n.surrogates, iaaft_surrogate(series = ts))
    }

    # Amplitude adjusted Fourier transform (Theiler, 1992).
    if (tolower(surrogate.method) == "aaft") {
        surr.data = replicate(n.surrogates, aaft_surrogate(series = ts))

    # Phase randomisation (Theiler, 1992).
    } else if (tolower(surrogate.method) == "phase") {
        surr.data = replicate(n.surrogates,
                              phase_randomised_surrogate(series = ts))

    # Circulant embedding (non-constrained realizations)
    } else if (tolower(surrogate.method) == "ce" ||
               tolower(surrogate.method) == "circulant embedding") {
        surr.data = replicate(n.surrogates, ce_surrogate(series = ts))

    # Phase and amplitude randomisation (non-constrained realizations)
    } else if (tolower(surrogate.method) == "dh" ||
               tolower(surrogate.method) == "davison-hinkley" ||
               tolower(surrogate.method) == "phase and amplitude randomisation" ||
               tolower(surrogate.method) == "phase and amplitude") {
        surr.data = replicate(n.surrogates, dh_surrogate(series = ts))
    }

    return(surr.data)
}


#' Checks if a given surrogate method is among the currently
#' implemented surrogate methods.
#'
#' @param surrogate.method The surrogate method to check.
#' @export validate_surrogate_method
validate_surrogate_method <- function(surrogate.method) {
  # Validate surrogate method.
  if (!(tolower(surrogate.method) %in% c("aaft", "iaaft",
                                         "random", "phase",
                                         "ce", "dh", "seasonal"))
      ) {

    stop(paste("Surrogate type",
               paste("'", surrogate.method, "'", sep=""),
               "not valid"))
  }
}
