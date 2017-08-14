#' Performs CCM for a single set of parameters and a specified library size.
#' Convenience function to allow low-level parallellisation.
#'
#' @param data A data frame containing two columns - one for the presumed driver
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
#' @param parallell Activate parallellisation? Defaults to true. Currently,
#'   this only works decently on Mac and Linux systems.
#' @param library.column Integer indicating which column to use as the library
#'   column (presumed response).
#' @param target.column Integer indicating which column to use as the target
#'   column (presumed driver). Defaults to the opposite of 'library.column'.
#' @param surrogate.column Which column to use to generate surrogates. Defaults
#'   to the value of 'target.column' (the presumed driver).


#' @export
ccm_on_single_libsize <- function(library.size,
                data,
                lag = 0,
                E = 2,
                tau = 1,
                lib = c(1, dim(data)[1]),
                pred = lib,
                samples.original = 100,
                samples.surrogates = 50,
                num.neighbours = E + 1,
                random.libs = TRUE,
                with.replacement = TRUE,
                exclusion.radius = E + 1,
                epsilon = NULL,
                silent = TRUE,
                RNGseed = 1111,
                surrogate.method = "AAFT",
                n.surrogates = 0,
                parallel = FALSE,
                library.column = 1,
                target.column = 2,
                surrogate.column = 2,
                print.to.console = T,
                print.surrogate.to.console = T,
                n.libsizes.to.check = 20) {


  # Refer to library and target columns by name, not index.
  cols = column_names_as_string(column.names = colnames(data),
                                library.column = library.column,
                                target.column = target.column,
                                surrogate.column = surrogate.column)

  original.ccm = rEDM::ccm(block = data,
                           E = E,
                           tau = tau,
                           lib_sizes = library.size,
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
                             lib_column = cols["library.column"],
                             target_column = cols["target.column"],
                             first_column_time = FALSE)


  # Indicate that the analysis type is original (not surrogate)
  original.ccm$analysis.type = rep("original")
  original.ccm$surrogate.index = rep(0)


  return(original.ccm)
}
