#' Performs CCM over multiple library sizes. This function only exists to allow
#' parallelisation over library sizes at the lowermost level.
#'
#' @importFrom magrittr "%>%"
#' @param data A data frame containing two columns - one for the presumed driver
#'   and one for the response.
#' @param library.sizes Either a single maximum library size (cross mapping
#'   is performed for a range of value from the smallest possible library
#'   size to the provided library size) or a user-specified range
#'   of library sizes. If user-provided, make sure to provide at least
#'   20 different library sizes to ensure robust convergence assessment.
#' @param low.libsize If one library size is specified, cross map
#'   for library sizes ranging from 'low.libsize' to 'high.libsize'.
#' @param high.libsize If one library size is specified, cross map
#'   for library sizes ranging from 'low.libsize' to 'high.libsize'.
#' #' @param data A data frame containing two columns - one for the presumed driver
#'   and one for the response.
#' @param lag The lag (called prediction horizon in rEDM::ccm) for which to
#' compute CCM.
#' @param E The embedding dimension. Defaults to NULL, which triggers automated
#' optimisation of the embedding dimension up to the dimension specified by
#' 'max.E'.
#' @param tau The embedding lag. Defaults to NULL, which triggers automated
#' optimisation of the embedding lag up to the dimension specified by 'max.tau'.
#'  For sparsely sampled time series (for example geological time series), it is
#'   wise to set this value to 1. For densely sampled time series, this should
#'   be set to the first minima of the autocorrelation function of the presumed
#'   driver.
#' @param lib Indices of the original library time series to use as the library
#' (training) set.
#' @param pred Indices of the original target time series to use as prediction
#' set. If this overlaps with the training set, make sure to use leave-K-out
#' cross validation setting the 'exclusion.radius' parameters to a minimum of
#' E + 1.
#' @param samples.original The number of random libraries to draw when
#' calculating the cross map skill.
#' @param samples.surrogates The number of surrogate time series in the null
#' ensemble.
#' @param num.neighbours The number of nearest neighbours to use in predictions.
#'  Defaults to E + 1.
#' @param random.libs Whether or not to sample random library (training) sets.
#'   Defaults to TRUE.
#' @param with.replacement Should samples be drawn with replacement? Defaults
#'   to TRUE.
#' @param exclusion.radius The number of temporal neighbours to exclude for the
#'   leave-K-out cross validation. Defaults to E + 1.
#' @param epsilon Exlude neighbours if the are within a distance of 'epsilon'
#'   from the predictee.
#' @param silent Suppress warnings?
#' @param RNGseed A random number seed. For reproducibility.
#' @param surrogate.method Which method should be used to generate surrogate
#'   time series? Defaults to "AAFT". For more options, see the description of
#'   the 'surrogate_ensemble' function in this package.
#' @param n.surrogates Should a surrogate test also be performed? If so, 'n.surrogates' sets
#'   the number of surrogate time series to use. By default, no surrogate test is performed
#'  (n.surrogates = 0).
#' @param parallel Activate parallellisation? Defaults to true. Currently,
#'   this only works decently on Mac and Linux systems.
#' @param library.column Integer indicating which column to use as the library
#'   column (presumed response).
#' @param target.column Integer indicating which column to use as the target
#'   column (presumed driver). Defaults to the opposite of 'library.column'.
#' @param surrogate.column Which column to use to generate surrogates. Defaults
#'   to the value of 'target.column' (the presumed driver).
#' @param n.libsizes.to.check Minimum number of library sizes for the
#' convergence test.
#' @param time.series.length.threshold Display a warning if the time series length drops
#'   below this threshold.
#' @param time.unit The time unit of the raw time series.
#' @param time.bin.size The temporal resolution of the raw time series (given in the units
#'   indicated by 'time.unit').
#' @param print.to.console Display progress?
#' @param time.run Time the run?
#' @export
ccm_over_library_sizes <- function(lag,
                                   data,
                                   E = 2, # Attractor reconstruction dimensions
                                   tau = 1, # Attractor reconstruction lags
                                   library.sizes = 100,
                                   low.libsize = min(E * tau + max(2, abs(lag)),
                                                     10, 20, na.rm = T),
                                   n.libsizes.to.check = 30,
                                   high.libsize = max(library.sizes), #min(floor(max(library.sizes) * 1.5), max(library.sizes) - E*tau - abs(lag)),
                                   lib = c(1, dim(data)[1]),
                                   pred = lib, # Training and prediction libraries overlap (uses leave-n-out cross validation instead of separate libraries)
                                   samples.original = 100,
                                   samples.surrogates = 0,
                                   n.surrogates = 0,
                                   surrogate.method = "AAFT",
                                   time.unit = "bins",
                                   time.bin.size = 1,
                                   num.neighbours = E + 1,
                                   random.libs = TRUE, #
                                   with.replacement = TRUE, # Indicates whether to sample vectors with replacement
                                   exclusion.radius = E, # Exclude vectors from nearest neighbor search space  whose time index are within the exclusion.radius
                                   epsilon = NULL, ## Exclude vectors from nearest neighbor search space that are within a distance epsilon from the predictee #
                                   RNGseed = 1111,
                                   parallel = F,
                                   time.run = F,
                                   print.to.console = F,
                                   time.series.length.threshold = 100,
                                   library.column = 1,
                                   target.column = 1,
                                   surrogate.column = target.column,
                                   silent = T) {


  # Either generate a custom range of library sizes, or use the one
  # provided by the user.
  if (length(library.sizes) < 20) {
    warning("The number of library sizes provided is not sufficient to perform robust convergence testing. Generating a valid selection of library sizes and using these instead.")

    l1 <- as.integer(seq(from = low.libsize,
                        to = ceiling(high.libsize / 4),
                        length.out = ceiling(2 * n.libsizes.to.check / 3))) # More points at lower library sizes
    l2 <- ceiling((high.libsize - low.libsize)/2)
    l3 <- as.integer(seq(from = ceiling(high.libsize / 1.5),
                        to = high.libsize,
                        length.out = ceiling(n.libsizes.to.check / 3)) - 1) # More points at higher library sizes
    library.sizes <- unique(c(l1, l3, library.sizes))
  }


  if (parallel) {
    ccm <- parallel::mclapply(library.sizes,
                    FUN = ccm_on_single_libsize,
                    data = data,
                    E = E,
                    tau = tau,
                    samples.original = samples.original,
                    with.replacement = with.replacement,
                    RNGseed = RNGseed,
                    exclusion.radius = exclusion.radius,
                    epsilon = epsilon,
                    silent = silent,
                    lag = lag,
                    lib = lib,
                    pred = pred,
                    num.neighbours = num.neighbours,
                    random.libs = random.libs,
                    library.column = library.column,
                    target.column = target.column,
                    mc.cores = parallel::detectCores() - 1
    )

  } else {
    results <- suppressWarnings(rEDM::ccm(block = data,
                  E = E,
                  tau = tau,
                  lib_sizes = library.sizes,
                  num_samples = samples.original,
                  replace = with.replacement,
                  RNGseed = RNGseed,
                  exclusion_radius = exclusion.radius,
                  epsilon = epsilon,
                  silent = silent,
                  tp = lag,
                  lib = lib,
                  pred = pred,
                  num_neighbors = num.neighbours,
                  random_libs = random.libs,
                  lib_column = library.column,
                  target_column = target.column,
                  first_column_time = FALSE))

  }

  # Indicate that the analysis type is original (not surrogate)
  results$analysis.type <- rep("original")
  results$surrogate.index <- rep(0)

  return(results)
}
