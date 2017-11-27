#' Performs CCM for a given lag.
#'
#' @param data A data frame containing two columns - one for the presumed driver
#'   and one for the response.
#' @param library.sizes Either a single maximum library size (cross mapping
#'   is performed for a range of value from the smallest possible library
#'   size to the provided library size) or a user-specified range
#'   of library sizes. If user-provided, make sure to provide at least
#'   20 different library sizes to ensure robust convergence assessment.
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
#' @param print.to.console Display progress?
#' @param convergence.test Run convergence test?
#' @param n.libsizes.to.check Minimum number of library sizes in convergence
#'   test.
#' @export
ccm <- function(data,
                library.sizes = 100,
                convergence.test = T,
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
                n.libsizes.to.check = 20) {


  # Refer to library and target columns by name, not index.
  cols <- column_names_as_string(column.names = colnames(data),
                                library.column = library.column,
                                target.column = target.column,
                                surrogate.column = surrogate.column)


  # Cross map using the provided data
  if (convergence.test == TRUE) {
    ccm.result <- ccm_over_library_sizes(
      data = data,
      library.sizes = library.sizes,
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
             library.column = cols["library.column"],
             target.column = cols["target.column"])
  } else {
    if (length(library.sizes) == 1) {
      ccm.result <- ccm_on_single_libsize(data = data,
                                           library.size = library.sizes,
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
                                           library.column = cols["library.column"],
                                           target.column = cols["target.column"])
    }
  }

  #######################
  # Convergence check
  #######################
  if (convergence.test == TRUE) {
    params <- suppressWarnings(get_convergence_parameters(ccm.result))
  } else {
    params <- suppressWarnings(get_convergence_parameters(ccm.result = NULL))
  }


  # Surrogate cross map at the largest provided library size.
  if (n.surrogates > 0) {
    validate_surrogate_method(surrogate.method)

    surrogate.results <- surrogate_ccm(
      original.data = data,
      E = 2,
      tau = 1,
      library.size = library.sizes,
      with.replacement = with.replacement,
      RNGseed = RNGseed,
      exclusion.radius = exclusion.radius,
      num.neighbours = num.neighbours,
      epsilon = epsilon,
      silent = silent,
      lag = lag,
      lib = lib,
      pred = pred,
      random.libs = random.libs,
      library.column = cols["library.column"],
      target.column = cols["target.column"],
      surrogate.column = cols["surrogate.column"],
      samples.surrogates = samples.surrogates,
      n.surrogates = n.surrogates,
      parallel = parallel,
      surrogate.method = surrogate.method)

    ccm.results <- data.table::rbindlist(l = list(ccm.result,
                                                  surrogate.results),
                                        use.names = TRUE)

    ccm.results$confidence.level <- params["confidence.level"]
    ccm.results$p.value <- params["p.value"]
    ccm.results$alpha <- params["alpha"]
    ccm.results$convergent <- params["convergent"]
    ccm.results$k <- params["k"]
    ccm.results$a <- params["a"]
    ccm.results$b <- params["b"]
  }

  # Combine everything
  if (n.surrogates == 0) {
    ccm.results <- ccm.result
    ccm.results$confidence.level <- params["confidence.level"]
    ccm.results$p.value <- params["p.value"]
    ccm.results$alpha <- params["alpha"]
    ccm.results$convergent <- params["convergent"]
    ccm.results$k <- params["k"]
    ccm.results$a <- params["a"]
    ccm.results$b <- params["b"]
  }

  # Some analysis info
  ccm.results$time.of.analysis <- Sys.time()
  ccm.results$analyst <- Sys.info()["effective_user"]
  ccm.results$sysname <- Sys.info()["sysname"]
  ccm.results$Rversion <- version$version.string
  ccm.results$id <- stringi::stri_rand_strings(n = 1, length = 20)

  return(ccm.results)
}
