#' Cross-mapping for a surrogate ensemble.
#'
#' @param original.data A data frame containing two columns - one for the
#' presumed driver and one for the response.
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
#' @param library.size The size of each random library (training set).
#' @param lib Indices of the original library time series to use as the library
#' (training) set.
#' @param pred Indices of the original target time series to use as prediction
#' set. If this overlaps with the training set, make sure to use leave-K-out
#' cross validation setting the 'exclusion.radius' parameters to a minimum of
#' E + 1.
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
#' @export
#'
surrogate_ccm <- function(original.data,
                         E = 2,
                         tau = 1,
                         library.size = ceiling(nrow(original.data) * 0.9),
                         with.replacement = T,
                         RNGseed = 1111,
                         exclusion.radius = E + 1,
                         num.neighbours = E + 1,
                         epsilon=NULL,
                         silent=T,
                         lag = 0,
                         lib = c(1, nrow(original.data)[1]),
                         pred = lib, # Training and prediction libraries overlap (uses leave-n-out cross validation instead of separate libraries)
                         random.libs = TRUE,
                         library.column = 1,
                         target.column  = 2,
                         surrogate.column = target.column,
                         samples.surrogates = 100,
                         n.surrogates = 100,
                         parallel = F,
                         surrogate.method = "aaft") {


  #' Create a list of data frames where the desired column has been replaced
  #' by surrogate data sets.
  surrogate.datasets = create_surrogate_dataframes(original.data = original.data,
                                                 surrogate.column = surrogate.column,
                                                 surrogate.method = surrogate.method,
                                                 n.surrogates = n.surrogates)


  # Run CCM for each data frame containing a data frame where the desired column
  # has been replaced by a surrogate realization.
  if (parallel) {
    n.available.cores = parallel::detectCores() - 1
    surrogate.ccms = parallel::mclapply(X = surrogate.datasets,
                              FUN = rEDM::ccm,
                                E = E,
                                tau = tau,
                                num_samples = samples.surrogates,
                                lib_sizes = library.size,
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
                                mc.cores = n.available.cores
                              )
  } else {
    surrogate.ccms = lapply(X = surrogate.datasets,
                            FUN = rEDM::ccm,
                              E = E,
                              tau = tau,
                              num_samples = samples.surrogates,
                              lib_sizes = library.size,
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
                              target_column = target.column
                            )
  }
  # Add column indicating type of analysis to each of the results
  surrogate.ccms = mapply(`[<-`, surrogate.ccms,
                   'analysis.type', value = "surrogate",
                   SIMPLIFY = FALSE)

  # Combine all surrogate analyses into data table
  surrogate.ccms = data.table::rbindlist(surrogate.ccms, idcol = "surrogate.index")

  return(surrogate.ccms)
}

#' Create a dataframe of multiple surrogate series.
#'
#' @param original.data A two-column data frame.
#' @param surrogate.method Method used to generate surrogates. Defaults to
#' "aaft".
#' @param n.surrogates Number of surrogate series to generate.
#' @param surrogate.column The index or name of of the column for which
#'        to generate surrogate data. When doing causal analyses,
#'        this should correspond to the target column - the putative driver.
#' @param print.to.console Display progress?
create_surrogate_dataframes <- function(original.data,
                                      surrogate.column,
                                      surrogate.method = "aaft",
                                      n.surrogates = 100,
                                      print.to.console = F) {
  # ------------------------------------------------------------------------------------------------
  # Create surrogate data sets. This ('surrogate.datasets') a list of datasets identical to the input data,
  # except the values in 'surrogate.column' is replaced by a surrogate realization of that time series.
  # The other time series is left untouched.

  # We then cross map from the surrogate library to the target (checking what causal
  # signal arise from time series generated by the null hypothesis represented by
  # the surrogate data).
  surrogate.data = surrogate_ensemble(ts = original.data[, surrogate.column],
                                      surrogate.method = surrogate.method,
                                      n.surrogates = n.surrogates)

  #' Replaces a column of a data frame with surrogate
  ReplaceColumn <- function(original.data, surrogate.data, replace.column = surrogate.column) {
    original.data[, replace.column] = surrogate.data
    return(original.data)
  }

  # Generate a list of surrogate data sets (each is data frame with two columns,
  # where the desired time series has been replaced with a surrogate realization
  # of that time series).
  surrogate.dataframes = apply(X = surrogate.data,
                               MARGIN = 2,
                               FUN = ReplaceColumn,
                                replace.column = surrogate.column,
                                original.data = original.data
                               )

  return(surrogate.dataframes)
}



validate_surrogate_method <- function(surrogate.method) {
  # Validate surrogate method.
  if (!(tolower(surrogate.method) %in% c("aaft",
                                         "iaaft",
                                         "random",
                                         "phase",
                                         "ce",
                                         "dh",
                                         "seasonal"))) {

    stop(paste("Surrogate type",
               paste("'", surrogate.method, "'", sep = ""),
               "not valid")
    )
  }
}

