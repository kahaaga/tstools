#' Cross-mapping for a surrogate ensemble.
#'
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
                         num_neighbors = E + 1,
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
    surrogate.ccms = mclapply(X = surrogate.datasets,
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

#' @param Original data (a two-column data frame)
#' @param Surrogate data (a n.observations-by-n.surrogates matrix)
#' @param surrogate_column The index or name of of the column for which
#'        to generate surrogate data. When doing causal analyses,
#'        this should correspond to the target column - the putative driver.
create_surrogate_dataframes <- function(original.data,
                                      surrogate.column,
                                      surrogate.method = "aaft",
                                      n.surrogates=100,
                                      print.to.console=F) {
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
               paste("'", surrogate.method, "'", sep=""),
               "not valid")
    )
  }
}

