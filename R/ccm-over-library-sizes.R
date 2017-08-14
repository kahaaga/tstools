

#' importFrom magrittr "%>%"
#' @param library.sizes Either a single maximum library size (cross mapping
#'   is performed for a range of value from the smallest possible library
#'   size to the provided library size) or a user-specified range
#'   of library sizes. If user-provided, make sure to provide at least
#'   20 different library sizes to ensure robust convergence assessment.
#' @param low.libsize If one library size is specified, cross map
#'   for library sizes ranging from 'low.libsize' to 'high.libsize'.
#' @param high.libize If one library size is specified, cross map
#'   for library sizes ranging from 'low.libsize' to 'high.libsize'.
#' @export ccm_over_library_sizes
ccm_over_library_sizes <- function(lag,
                                   data,
                                   E = 2, # Attractor reconstruction dimensions
                                   tau = 1, # Attractor reconstruction lags
                                   library.sizes = 100,
                                   low.libsize = min(E * tau + max(2, abs(lag)),
                                                     10, 20, na.rm = T),
                                   n.libsizes.to.check = 30,
                                   high.libsize = min(ceiling(max(library.sizes) * 1.5),
                                                      max(library.sizes) - E*tau - abs(lag)),
                                   lib = c(1, dim(data)[1]),
                                   pred = lib, # Training and prediction libraries overlap (uses leave-n-out cross validation instead of separate libraries)
                                   samples.original = 300,
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
  if (n.libsizes.to.check < 20) {
    if (!silent) warning("@CheckConvergence()\tOnly ", n.libsizes.to.check, "library sizes are being checked for convergence. Spurious non-convergence or convergence might be the result. Consider increasing the value of 'n.libsizes.convergence.check'")
  }

  # Either generate a custom range of library sizes, or use the one
  # provided by the user.
  if (length(library.sizes) < 20) {
    if (!silent) warning("The number of library sizes provided is not sufficient to perform robust convergence testing.\nGenerating a valid selection of library sizes and using these instead.")

    l1 = as.integer(seq(from = low.libsize,
                        to = ceiling(high.libsize / 4),
                        length.out = ceiling(2 * n.libsizes.to.check / 3))) # More points at lower library sizes
    l2 = ceiling((high.libsize-low.libsize)/2)
    l3 = as.integer(seq(from = ceiling(high.libsize / 1.5),
                        to = high.libsize,
                        length.out = ceiling(n.libsizes.to.check / 3)) - 1) # More points at higher library sizes
    library.sizes = unique(c(l1, l3, library.sizes))
  }


  if (parallel) {

    ccm =  mclapply(library.sizes,
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
    results = rEDM::ccm(block = data,
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
                  first_column_time = FALSE)

  }

  # Indicate that the analysis type is original (not surrogate)
  results$analysis.type = rep("original")
  results$surrogate.index = rep(0)

  return(results)
}
