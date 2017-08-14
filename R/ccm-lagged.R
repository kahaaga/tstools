
#' Cross-mapping from one variable to another variable over a range of lags.
#' Wrapper around OnewayLaggedCCM allowing multiple values for each input argument.
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
#' @param max.E The maximum embedding dimension for which to optimise 'E'. Only relevant if either 'E' or tau is set to NULL.
#' @param max.tau The maximum embedding lag for which to optimise 'tau'. Only relevant if either 'E' or tau is set to NULL.
#' @param min.E The minimum embedding dimension for which to optimise 'E'. Only relevant if either 'E' or tau is set to NULL.
#' @param min.tau The minimum embedding lag for which to optimise 'tau'. Only relevant if either 'E' or tau is set to NULL.
#' @param optimise.FNNdim Optimise false nearest neighbours? Defaults to FALSE, meaning that simplex projection only is used
#' for optimisation. If TRUE, simplex projection is performed with the minimum embedding dimension outputted from
#' the FNN algorithm (and the box counting dimension, if acitivated). Only relevant if either 'E' or tau is set to NULL.
#' @param optimise.boxcountdim Optimise box counting dimension? Defaults to FALSE, meaning that simplex projection only is used
#'   for optimisation. If TRUE, simplex projection is performed with the minimum embedding dimension as the first integer
#'   larger than twice the box counting algorithm of the attractor (also considering the minimum dimension indicated by the
#'   FNN algorithm, if activated). Only relevant if either 'E' or tau is set to NULL.
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
#' @param convergence.test Should a convergence test be performed? Analyses where CCM does not
#'   convergence are nonsensical, so this option defaults to TRUE.
#' @param n.libsizes.to.check How many library sizes to use for the convergence check?
#'   Defaults to 30.
#' @param regression.convergence.plots Display regression plots for the convergence test?
#'   Defaults to FALSE.
#' @param n.surrogates Should a surrogate test also be performed? If so, 'n.surrogates' sets
#'   the number of surrogate time series to use. By default, no surrogate test is performed
#'  (n.surrogates = 0).
#' @param samples.surrogates The number of surrogate series in each null ensemble.
#' @param always.run.surrogates Should surrogate analyses be performed even if the convergence test
#'   fails? Defaults to FALSE (there is no reason to perform significance testing if the analysis is not
#'   causal to begin with).
#' @param surrogate.column Which column to use to generate surrogates. Defaults to the value of
#'   'target.column' (the presumed driver).
#' @param surrogate.method The type of surrogate time series to generate. Will vary depending
#'   on what null hypothesis is being tested. Defaults to AAFT surrogates.
#' @param num.neighbours The number of nearest neighbours to use in predictions. Defaults to E + 1.
#' @param RNGseed For reproducivility. Seed to use for the random number generator.
#' @param parallell Activate parallellisation? Defaults to true. Currently, this only works
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
#' @export
ccm_lagged <- function(data,
                      lags,
                      Es = NULL,
                      taus = NULL,
                      library.sizes = c(as.integer(nrow(data)/2)),
                      lib = c(1, dim(data)[1]),
                      pred = lib,
                      samples=100,
                      samples.surrogates = 50,
                      n.surrogates = 0,
                      surrogate.methods = c("aaft"),
                      time.unit = NULL,
                      time.bin.size = NULL,
                      num.neighbours = E + 1,
                      random.libs = TRUE,
                      with.replacement = TRUE,
                      exclusion.radius = E + 1,
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
                      regression.convergence.plots = F,
                      always.run.surrogates = F,
                      n.libsizes.to.check = 20,
                      optimise.FNNdim = F,
                      optimise.boxcountdim = F,
                      min.E = 2,
                      max.E = 10,
                      min.tau = 1,
                      max.tau = 1,
                      plot.simplex.projection = F,
                      ...) {

  # If no embedding parameters are provided, find the best embedding
  # for the putatitive driver and use that embedding.
  if (is.null(Es) | is.null(taus)) {
    #cat("\nOptimizing embedding dimension and lags ..\n")

    optimal.embedding = find_optimal_embeddings(data,
                                                min.E = min.E,
                                                max.E = max.E,
                                                min.tau = min.tau,
                                                max.tau = max.tau,
                                                optimise.FNNdim = optimise.FNNdim,
                                                optimise.boxcountdim = optimise.boxcountdim,
                                                plot.simplex.projection = plot.simplex.projection)
    if (is.null(Es)) Es = optimal.embedding[1]
    if (is.null(taus)) taus = optimal.embedding[2]
  }

  # Generate all possible combinations of the input parameters.
  params = expand.grid(Es, taus, surrogate.methods)
  colnames(params) = c("E", "tau", "surrogate.method")


  # Perform lagged CCM for each combination of the parameters.
  results = list()

  for (i in 1:nrow(params)) {
    E = params[i, "E"]
    tau = params[i, "tau"]
    surrogate.method = as.character(params[i, "surrogate.method"])

    ccm = ccm_lagged_oneway(lags = lags,
                          data = data,
                          E = E,
                          tau = tau,
                          library.sizes = library.sizes,
                          lib = lib,
                          pred = lib,
                          samples = samples,
                          samples.surrogates = samples.surrogates,
                          n.surrogates = n.surrogates,
                          surrogate.method = surrogate.method,
                          time.unit = time.unit,
                          time.bin.size = time.bin.size,
                          num.neighbours = num.neighbours,
                          random.libs = random.libs,
                          with.replacement = with.replacement,
                          exclusion.radius = exclusion.radius,
                          epsilon = epsilon,
                          RNGseed = RNGseed,
                          silent = silent,
                          parallel = parallel,
                          parallelize.on.each.lag = parallelize.on.each.lag,
                          time.run = time.run,
                          print.to.console = print.to.console,
                          time.series.length.threshold = time.series.length.threshold,
                          library.column = library.column,
                          target.column = target.column,
                          surrogate.column = surrogate.column,
                          convergence.test = convergence.test,
                          n.libsizes.to.check = n.libsizes.to.check,
                          regression.convergence.plots = regression.convergence.plots,
                          always.run.surrogates = always.run.surrogates,
                          max.E = max.E,
                          max.tau = max.tau)

    # Add information about the parameters that are not recorded in deeper CCM functions.
    ccm$max.E = rep(max.E)
    ccm$max.tau = rep(max.tau)
    ccm$n.libsizes.to.check = rep(n.libsizes.to.check)
    ccm$optimise.FNNdim = rep(optimise.FNNdim)
    ccm$optimise.boxcountdim = rep(optimise.boxcountdim)

    # Store the result.
    results[[i]] = ccm
  }

  return(data.table::rbindlist(results))
}

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
#' @param surrogate_method The type of surrogate time series to generate. Will vary depending
#'   on what null hypothesis is being tested. Defaults to AAFT surrogates.
#' @param num.neighbours The number of nearest neighbours to use in predictions. Defaults to E + 1.
#' @param RNGseed For reproducivility. Seed to use for the random number generator.
#' @param parallell Activate parallellisation? Defaults to true. Currently, this only works
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
#'
#' @importFrom foreach "%dopar%"
ccm_lagged_oneway <- function(data,
                            lags,
                            E = NULL,
                            tau = NULL,
                            library.sizes = as.integer(nrow(data)/2),
                            lib = c(1, dim(data)[1]),
                            pred = lib,
                            samples = 100,
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
  # Make sure to garbage collect
  gc()
  cat("\nANALYSIS SETUP\n")
  cat("========================================================================================\n")
  cat("Time series:\t\t", dim(data)[1], "points")
  if (!is.null(time.bin.size) & !is.null(time.unit)) cat(" at", time.bin.size, time.unit, "resolution.\n")
  cat("\nE =\t\t\t By optimisation\n")
  cat("\ttau =\t\t\t By optimisation\n")
  cat("\nsamples =\t\t", samples, "\texclusion.radius =\t", exclusion.radius)
  cat("\nlibrary.sizes =\t\t", library.sizes)
  cat("\nlibrary indices =\t", lib, "\nprediction indices =\t", pred)
  if (n.surrogates > 0) cat("\nSurrogates =\t\t", n.surrogates,
                            surrogate.method, "surrogates", "sampled",
                            samples.surrogates, "times each\n")
  cat("\nCAUSALITY TEST")
  cat("\n======================================================================================\n")
  if (is.numeric(library.column) &
      is.numeric(target.column) &
      is.numeric(surrogate.column)) {
    library.column = colnames(data)[library.column]
    target.column = colnames(data)[target.column]
    surrogate.column = colnames(data)[surrogate.column]

    cat("Causality test:\t\t Does", paste("'", target.column, "'", sep=""),
        "dynamically drive", paste("'", library.column, "'", sep=""), "?")
  }
  else  cat("\nCausality test:\t\t Does", target.column,
            "dynamically drive", library.column, "?")

  cat("\nConvergence test ...... ", ifelse(convergence.test, "Yes.", "No."),
      "Exponential regression over", n.libsizes.to.check, "library sizes.")

  if (n.surrogates > 0) {
    validate_surrogate_method(surrogate.method)
    cat("\nNull hypothesis .......  Causal effect of",
        paste("'", target.column, "'", sep=""), "on",
        paste("'", library.column,"'", sep=""), "is indistinguishable\n",
        "\t\t\t from the causal effect of a", paste("'", surrogate.method, "'", sep=""),
        "null-model on", paste("'", library.column,"'", sep=""))

    cat("\nTest .................. ","One-sided. \n\t\t\t Disregard null-hypothesis",
        "if predictive skill exceeds the\n\t\t\t 68.27th, 95.45th or 99.73th percentiles\n\t\t\t of",
        "the predictive skill obtained from replacing the driver\n\t\t\t with a", paste(n.surrogates, "-member", sep=""), "ensemble of",
        paste("'", surrogate.method, "'", sep=""), "processes generated\n\t\t\t",
        "from the", ifelse(surrogate.column == target.column,
                           yes="driver", no="library"),
        paste("('", surrogate.column, "')", sep=""), "time series.")
  } else {
    cat("\nNull hypothesis .......  None.\n")
  }

  cat("\nCross mapping ........ \t")

  # Windows
  if (parallel & parallelize.on.each.lag & !Sys.info()["sysname"] == "Windows") {
    cat(" Parallelizing over lags (mclapply). No output will be printed.")
    lagged.ccm.result = parallel::mclapply(lags, FUN = ccm, parallel = parallel, data = data, E = E, tau = tau, library.column = library.column, target.column = target.column, surrogate.column = surrogate.column, library.sizes = library.sizes, lib = lib, pred = pred, samples = samples, samples.surrogates = samples.surrogates, surrogate.method = surrogate.method, n.surrogates = n.surrogates, num.neighbours = num.neighbours, random.libs = random.libs, with.replacement = with.replacement, exclusion.radius = exclusion.radius, epsilon = epsilon,RNGseed = RNGseed, silent = silent, n.libsizes.to.check = n.libsizes.to.check, convergence.test = convergence.test, print.to.console = print.to.console)

    # Mac and linux
  } else if (parallel & parallelize.on.each.lag & Sys.info()["sysname"] == "Windows") {
    cat("  Windows is the operating system. Parallelising using foreach. No output.")
    num.cores <- min(length(lags), parallel::detectCores() - 1) # Number of jobs to run simultaneously
    cl <- parallel::makeCluster(num.cores) # Make clusters
    doSNOW::registerDoSNOW(cl) # use the above cluster

    lagged.ccm.result = foreach::foreach(lag=min(lags):max(lags),
                                .export = c("ccm",
                                            "ccm_over_library_sizes",
                                            "ccm_on_single_libsize",
                                            "surrogate_ccm",
                                            "create_surrogate_dataframes",
                                            "validate_surrogate_method"),
                                combine = "rbind", .inorder = FALSE,
                                .packages = c("dplyr", "parallel")) %dopar%  {
                                  lapply(lags, FUN = ccm, data = data, E = E, tau = tau, library.column = library.column, target.column = target.column, surrogate.column = surrogate.column, library.sizes = library.sizes, lib = lib, pred = pred, samples = samples, samples.surrogates = samples.surrogates, surrogate.method = surrogate.method, n.surrogates = n.surrogates, num.neighbours = num.neighbours, random.libs = random.libs, with.replacement = with.replacement, exclusion.radius = exclusion.radius, epsilon = epsilon, RNGseed = RNGseed, parallel = F, silent = silent, print.to.console = print.to.console, n.libsizes.to.check = n.libsizes.to.check, convergence.test = convergence.test)
                                }
    parallel::stopCluster(cl) # close cluster
    lagged.ccm.result = unlist(lagged.ccm.result, recursive = FALSE) # Unlist nested list created by foreach

  } else if (parallel & !parallelize.on.each.lag |
             !parallel) {
    if (Sys.info()["sysname"] == "Windows") {
      cat("Windows is the operating system. Cannot parallelise inner surrogate loops.\n")
      parallel = F
    }
    lagged.ccm.result = lapply(lags, FUN = ccm, data = data, E = E, tau = tau, library.column = library.column, target.column = target.column, surrogate.column = surrogate.column, library.sizes = library.sizes, lib = lib, pred = pred, samples = samples, samples.surrogates = samples.surrogates, surrogate.method = surrogate.method, n.surrogates = n.surrogates, num.neighbours = num.neighbours, random.libs = random.libs, with.replacement = with.replacement, exclusion.radius = exclusion.radius, epsilon = epsilon,RNGseed = RNGseed, parallel = parallel, silent = silent, print.to.console = print.to.console, n.libsizes.to.check = n.libsizes.to.check, convergence.test = convergence.test)
  }

  # Combine results from all lags
  lagged.ccm.result = data.table::rbindlist(lagged.ccm.result)

  # Add analysis parameters
  if (is.numeric(library.column) &
      is.numeric(target.column)) {
    lagged.ccm.result$causal.direction = rep(paste(colnames(data)[target.column],
                                                   "->",
                                                   colnames(data)[library.column]))

    lagged.ccm.result$crossmap.direction = rep(paste(colnames(data)[library.column],
                                                     "xmap.",
                                                     colnames(data)[target.column]))

  } else {
    lagged.ccm.result$crossmap.direction = rep(paste(library.column,
                                                     "xmap.",
                                                     target.column)
                                               )
    lagged.ccm.result$causal.direction = rep(paste(target.column,
                                                   "->",
                                                   library.column)
                                             )
  }

  # Add information about the analysis.
  lagged.ccm.result$time.bin.size = rep(time.bin.size)
  lagged.ccm.result$time.unit = rep(time.unit)
  lagged.ccm.result$ts.length = rep(length(data[,1]))

  return(lagged.ccm.result)
}









