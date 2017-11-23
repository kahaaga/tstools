

#' Cross-mapping from one variable to another variable over a range of lags.
#'
#' @param data A data frame containing two columns - one for the presumed driver and one for the response.
#' @param library.column Integer indicating which column to use as the library column (presumed response)
#'    (1 for the first column and 2 for the second column).
#' @param target.column Integer indicating which column to use as the target column (presumed driver).
#'   Defaults to the opposite of 'library.column'.
#' @param lags A vector of lags to compute CCM for.
#' @param E The embedding dimension. Defaults to NULL, which triggers automated optimisation of the embedding
#'   dimension up to the dimension specified by 'max.E'.
#' @param tau The embedding lag. Defaults to NULL, which triggers automated optimisation of the embedding lag
#'   up to the dimension specified by 'max.tau'. For sparsely sampled time series (for example geological
#'   time series), it is wise to set this value to 1. For densely sampled time series, this should be set to
#'   the first minima of the autocorrelation function of the presumed driver.
#' @param max.E The maximum embedding dimension for which to optimise 'E'.
#' @param max.tau The maximum embedding lag for which to optimise 'tau'.
#' @param samples The number of random libraries to draw when calculating cross map skill.
#' @param library.sizes The size of the random libraries drawn when calculating cross map skill.
#' @param lib Indices of the original library time series to use as the library (training) set.
#' @param pred Indices of the original target time series to use as prediction set. If this overlaps
#'   with the training set, make sure to use leave-K-out cross validation setting the
#'   'exclusion.radius' parameters to a minimum of E + 1.
#' @param exclusion.radius The number of temporal neighbours to exclude for the leave-K-out cross
#'   validation.
#' @param random.libs Whether or not to sample random library (training) sets. Defaults to TRUE.
#' @param with.replacement Should samples be drawn with replacement? Defaults to TRUE.
#' @param epsilon Exlude neighbours if the are within a distance of 'epsilon'
#'   from the predictee.
#' @param silent Suppress warnings?
#' @param convergence.test Should a convergence test be performed? Analyses where CCM does not
#'   convergence are nonsensical, so this option defaults to TRUE.
#' @param n.libsizes.to.check How many library sizes to use for the convergence check?
#'   Defaults to 30.
#' @param regression.convergence.plots Display regression plots for the convergence test?
#'   Defaults to FALSE.
#' @param n.surrogates Should a surrogate test also be performed? If so, 'n.surrogates' sets
#'   the number of surrogate time series to use. By default, no surrogate test is performed
#'  (n.surrogates = 0).
#' @param always.run.surrogates Should surrogate analyses be performed even if the convergence test
#'   fails? Defaults to FALSE (there is no reason to perform significance testing if the analysis is not
#'   causal to begin with).
#' @param surrogate.column Which column to use to generate surrogates. Defaults to the value of
#'   'target.column' (the presumed driver).
#' @param samples.surrogates The number of surrogate time series in the null
#' ensemble.
#' @param surrogate.method The type of surrogate time series to generate. Will vary depending
#'   on what null hypothesis is being tested. Defaults to AAFT surrogates.
#' @param num.neighbours The number of nearest neighbours to use in predictions. Defaults to E + 1.
#' @param RNGseed For reproducivility. Seed to use for the random number generator.
#' @param parallel Activate parallellisation? Defaults to true. Currently, this only works
#'   decently on Mac and Linux systems.
#' @param parallelize.on.each.lag Should parallellisation be done on the outer lag loop?
#'   Defaults to TRUE. Otherwise, parallellisation is done over the surrogate analyses.
#' @param num.cores The number of CPU cores to use for parallelisation. Defaults to one
#'   core less than what is available.
#' @param time.series.length.threshold Display a warning if the time series length drops
#'   below this threshold.
#' @param time.unit The time unit of the raw time series.
#' @param time.bin.size The temporal resolution of the raw time series (given in the units
#'   indicated by 'time.unit').
#' @param print.to.console Display progress?
#' @param time.run Time the run?

#' @importFrom foreach "%dopar%"
ccm_lagged_oneway <- function(data,
                            lags,
                            E = NULL,
                            tau = NULL,
                            library.sizes = as.integer(nrow(data)/2),
                            lib = c(1, dim(data)[1]),
                            pred = lib,
                            samples.original = 100,
                            samples.surrogates = 50,
                            n.surrogates = 0,
                            surrogate.method = "AAFT",
                            always.run.surrogates = F,
                            time.unit = NULL,
                            time.bin.size = NULL,
                            num.neighbours = E + 1,
                            random.libs = TRUE, #
                            with.replacement = TRUE,
                            exclusion.radius = ifelse(is.null(E), yes = 10, no = E + 1),
                            epsilon = NULL,
                            RNGseed = 1111,
                            silent = TRUE,
                            time.run = F,
                            print.to.console = T,
                            time.series.length.threshold = 100,
                            library.column = 1,
                            target.column = 2,
                            surrogate.column = target.column,
                            convergence.test = TRUE,
                            parallel = TRUE,
                            # TRUE by default (spawning one process for each lag; disables analysis status for each lag).
                            # Otherwise, spawn separate processes for surrogate CCM within the analysis for each lag (analysis output is visible).
                            parallelize.on.each.lag = F,
                            num.cores = parallel::detectCores() - 1,
                            n.libsizes.to.check = 30,
                            regression.convergence.plots = F,
                            max.E = 10,
                            max.tau = 1) {

  # Windows
  if (parallel & parallelize.on.each.lag & !Sys.info()["sysname"] == "Windows") {
    cat(" Parallelizing over lags (mclapply). No output will be printed.")
    lagged.ccm.result = parallel::mclapply(lags,
                                           FUN = ccm,
                                           parallel = parallel,
                                           data = data,
                                           E = E,
                                           tau = tau,
                                           library.column = library.column,
                                           target.column = target.column,
                                           surrogate.column = surrogate.column,
                                           library.sizes = library.sizes,
                                           lib = lib,
                                           pred = pred,
                                           samples.original = samples.original,
                                           samples.surrogates = samples.surrogates,
                                           surrogate.method = surrogate.method,
                                           n.surrogates = n.surrogates,
                                           num.neighbours = num.neighbours,
                                           random.libs = random.libs,
                                           with.replacement = with.replacement,
                                           exclusion.radius = exclusion.radius,
                                           epsilon = epsilon,
                                           RNGseed = RNGseed,
                                           silent = silent,
                                           n.libsizes.to.check = n.libsizes.to.check,
                                           convergence.test = convergence.test,
                                           print.to.console = print.to.console)

    # Mac and linux
  } else if (parallel & parallelize.on.each.lag & Sys.info()["sysname"] == "Windows") {
    cat("  Windows is the operating system. Parallelising using foreach. No output.")
    num.cores <- min(length(lags), parallel::detectCores() - 1) # Number of jobs to run simultaneously
    cl <- parallel::makeCluster(num.cores) # Make clusters
    doSNOW::registerDoSNOW(cl) # use the above cluster

    lagged.ccm.result = foreach::foreach(lag = min(lags):max(lags),
                                .export = c("ccm",
                                            "ccm_over_library_sizes",
                                            "ccm_on_single_libsize",
                                            "surrogate_ccm",
                                            "create_surrogate_dataframes",
                                            "validate_surrogate_method"),
                                combine = "rbind", .inorder = FALSE,
                                .packages = c("dplyr", "parallel")) %dopar%  {
                                  lapply(lags, FUN = ccm,
                                         data = data,
                                         E = E,
                                         tau = tau,
                                         library.column = library.column,
                                         target.column = target.column,
                                         surrogate.column = surrogate.column,
                                         library.sizes = library.sizes,
                                         lib = lib, pred = pred,
                                         samples.original = samples.original,
                                         samples.surrogates = samples.surrogates,
                                         surrogate.method = surrogate.method,
                                         n.surrogates = n.surrogates,
                                         num.neighbours = num.neighbours,
                                         random.libs = random.libs,
                                         with.replacement = with.replacement,
                                         exclusion.radius = exclusion.radius,
                                         epsilon = epsilon,
                                         RNGseed = RNGseed,
                                         parallel = F,
                                         silent = silent,
                                         print.to.console = print.to.console,
                                         n.libsizes.to.check = n.libsizes.to.check,
                                         convergence.test = convergence.test)
                                }
    parallel::stopCluster(cl) # close cluster
    lagged.ccm.result = unlist(lagged.ccm.result, recursive = FALSE) # Unlist nested list created by foreach

  } else if (parallel & !parallelize.on.each.lag |
             !parallel) {
    if (Sys.info()["sysname"] == "Windows") {
      cat("Windows is the operating system. Cannot parallelise inner surrogate loops.\n")
      parallel = F
    }
    lagged.ccm.result = lapply(lags,
                               FUN = ccm,
                               data = data,
                               E = E,
                               tau = tau,
                               library.column = library.column,
                               target.column = target.column,
                               surrogate.column = surrogate.column,
                               library.sizes = library.sizes,
                               lib = lib,
                               pred = pred,
                               samples.original = samples.original,
                               samples.surrogates = samples.surrogates,
                               surrogate.method = surrogate.method,
                               n.surrogates = n.surrogates,
                               num.neighbours = num.neighbours,
                               random.libs = random.libs,
                               with.replacement = with.replacement,
                               exclusion.radius = exclusion.radius,
                               epsilon = epsilon,
                               RNGseed = RNGseed,
                               parallel = parallel,
                               silent = silent,
                               print.to.console = print.to.console,
                               n.libsizes.to.check = n.libsizes.to.check,
                               convergence.test = convergence.test)
  }

  # Combine results from all lags
  lagccm = data.table::rbindlist(lagged.ccm.result)

  # Get columns as strings, not integers
  columns = column_names_as_string(column.names = colnames(data),
                         library.column = library.column,
                         target.column = target.column,
                         surrogate.column = surrogate.column)

  library.column = columns["library.column"]
  target.column = columns["target.column"]
  surrogate.column = columns["surrogate.column"]

  cat(library.column, target.column, surrogate.column)

  # Add analysis parameters
  lagccm$causal.direction = rep(paste(target.column, "->", library.column))
  lagccm$crossmap.direction = rep(paste(library.column, "xmap.", target.column))

  # Change column names to conform with R naming conventions
  cols = colnames(lagccm)
  cols[cols == "tp"] = "lag"
  cols[cols == "num_neighbors"] = "num.neighbours"
  cols[cols == "lib_column"] = "library.column"
  cols[cols == "target_column"] = "target.column"
  cols[cols == "lib_size"] = "library.size"
  cols[cols == "num_pred"] = "num.pred"
  colnames(lagccm) = cols

  # Add information about the analysis.
  lagccm$time.bin.size = rep(time.bin.size)
  lagccm$time.unit = rep(time.unit)
  lagccm$ts.length = rep(length(data[,1]))
  lagccm$samples.original = rep(samples.original)
  lagccm$samples.surrogates =  rep(samples.surrogates)
  lagccm$surrogate.column =  rep(surrogate.column)
  lagccm$surrogate.method =  rep(surrogate.method)
  lagccm$exclusion.radius =  rep(exclusion.radius)
  lagccm$n.surrogates =  rep(n.surrogates)
  lagccm$RNGseed =  rep(RNGseed)
  lagccm$lib.start =  rep(min(lib))
  lagccm$lib.end =  rep(max(lib))
  lagccm$pred.start =  rep(min(pred))
  lagccm$pred.end =  rep(max(pred))
  lagccm$num.neighbours =  rep(num.neighbours)
  lagccm$random.libs =  rep(random.libs)
  lagccm$with.replacement =  rep(with.replacement)
  lagccm$max.E =  rep(max.E)
  lagccm$max.tau = rep(max.tau)


  return(lagccm)
}
