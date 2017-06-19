#' Generate an iterated amplitude-adjusted Fourier transform surrogate.
#' @param series The series for which to generate a surrogate.
#' @param n.max.iter The maximum number of refinement.
#' @return An iaaft representation of the original series, sticking to
#'   the original values of the series (but otherwise shuffled) and
#'   preserving the autocorrelation function of the original series.
#' @export iaaft
iaaft <- function(series, n.max.iter = 100) {
    n = length(series)

    # Fourier transform of the original series
    original_fft = stats::fft(series)

    # Amplitudes of the original Fourier transform
    original_fft_amplitudes = Mod(original_fft)

    # Sample a Gaussian vector and sort it
    gaussian = sort(rnorm(n, 0, 2), index.return = T)$ix

    # Sort the original series according to the sorted Gaussian. After a number
    # of iterations, this will be our final surrogate series.
    series.randsorted = series[gaussian]

    # Iterate until autocorrelation functions match sufficiently.
    iteration = 0
    convergence.achieved = FALSE

    tolerance = 0.00001

    # Compute root mean square difference between original and randomly sorted
    # series
    acf.diff.old = rms_diff(acf(series, n - 1, plot = F)$acf,
                            acf(series.randsorted, n - 1, plot = F)$acf)

    while (!convergence.achieved && iteration <= n.max.iter) {
        series.randsorted.fft = fft(series.randsorted)

        # Extract the phases from the Fourier transform of the randomly
        # sorted series.
        randomised.phases = Arg(series.randsorted.fft)

        # New spectrum preserving original amplitudes but using the randomised
        # phases.
        new_fft = original_fft_amplitudes * exp(randomised.phases * 1i)

        # fft with inverse = T returns unnormalised values, so normalise by n
        surrogate = Re(fft(new_fft, inverse = T))/n # Sample the real part

        surrogate[sort(surrogate, index.return=T)$ix] = sort(series)

        series.randsorted = surrogate

        # Check for convergence
        acf.diff.new = rms_diff(acf(series, n - 1, plot = F)$acf,
                                acf(series.randsorted, n - 1, plot = F)$acf)

        #if (rms_diff(acf.diff.old, acf.diff.new) < tolerance) {
        #    convergence.achieved = T
        #    cat("\nConvergence achieved after ", iteration, " iterations.\n")
        #} else {
            acf.diff.old = acf.diff.new
        #}
        iteration = iteration + 1
    }
    return(surrogate)
}

#' Absolute root mean square difference between two vectors
#' @param v1 The first vector.
#' @param v2 The second vector.
rms_diff <- function(v1, v2) {
    abs(sqrt((v1 - v2)^2))[1]
}
