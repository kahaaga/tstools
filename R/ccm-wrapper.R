#' Cross-mapping from one variable to another variable over a range of lags.
#' Wrapper around OnewayLaggedCCM allowing multiple values for each input
#' argument. Useful when performing sensitivity tests. NOTE: automated
#' optimisation of embedding parameters is only available in this function;
#' lower-level functions do not implement them.
#'
#' @param data A data frame containing two columns - one for the presumed driver
#'  and one for the response.
#' @param library.column Integer indicating which column to use as the library
#'   column (presumed response) (1 for the first column and 2 for the second
#'  column).
#' @param target.column Integer indicating which column to use as the target
#'   column (presumed driver). Defaults to the opposite of 'library.column'.
#' @param lags A vector of lags to compute CCM for.
#' @param Es Vector of embedding dimensions. Defaults to NULL, which triggers
#'   automated optimisation of the embedding dimension up to the dimension
#'   specified by 'max.E'.
#' @param taus Embedding lags. Can either be several types:
#'   1. A vector of integer lags, in which case every lag provided is considered
#'   2. NULL, which sets the embedding lag to a default value of 1.
#'   3. "mi", which triggers automated optimisation of the embedding lag up to
#'   the dimension specified by 'max.tau' using the first minima of the lagged
#'   mutual information function.
#'   4. "acf", which triggers automated optimisation of the embedding lag up to
#'   the dimension specified by 'max.tau' using the first minima of the lagged
#'   autocorrelation function.
#'   For densely sampled time series, the embedding lag should be set to the
#'   first minima of the autocorrelation ("acf"), or mutual information ("mi")
#'   functions.  For sparsely sampled time series (for example geological time
#'   series), set the embedding lag(s) to low values (defaults to 1 if NULL).
#'
#' @param min.E The minimum embedding dimension for which to optimise 'E'. Only
#'   relevant if 'Es' is set to NULL.
#' @param max.E The maximum embedding dimension for which to optimise 'E'.
#'   Only relevant if 'Es' is set to NULL.
#' @param max.tau The maximum embedding lag for which to optimise 'tau'. Only
#'   relevant if 'taus' is set to "mi" or "acf".
#' @param optimise.FNNdim Optimise false nearest neighbours? Defaults to FALSE,
#'   meaning that simplex projection only is used
#' for optimisation. If TRUE, simplex projection is performed with the minimum
#'   embedding dimension outputted from
#' the FNN algorithm (and the box counting dimension, if acitivated). Only
#'   relevant if either 'E' or tau is set to NULL.
#' @param optimise.boxcountdim Optimise box counting dimension? Defaults to
#'   FALSE, meaning that simplex projection only is used for optimisation. If
#'   TRUE, simplex projection is performed with the minimum embedding dimension
#'   as the first integer larger than twice the box counting algorithm of the
#'   attractor (also considering the minimum dimension indicated by the FNN
#'   algorithm, if activated). Only relevant if either 'E' or tau is set to NULL.
#' @param which.optimdim.test Use the embedding dimension estimated by which
#'   optimisation procedure? Defaults to "all", which uses the maximum of the
#'   three methods. Other options are "FNN", "boxcount" or "simplex".
#' @param samples.original The number of random libraries to draw when
#'   calculating cross map skill.
#' @param library.sizes The size of the random libraries drawn when calculating
#'   cross map skill.
#' @param lib Indices of the original library time series to use as the library
#'   (training) set.
#' @param pred Indices of the original target time series to use as prediction
#'   set. If this overlaps with the training set, make sure to use leave-K-out
#'   cross validation setting the 'exclusion.radius' parameters to a minimum of
#'   E + 1.
#' @param exclusion.radius The number of temporal neighbours to exclude for the
#'   leave-K-out cross validation. Must be at least E + 1. Can be one of the
#'   following:
#'   1. An integer, in which case that exclusion radius is used.
#'   2. NULL, which sets the exclusion radius of E + 1.
#'   3. "mi", which triggers automated optimisation of the exclusion radius up
#'   to using the first minima of the lagged mutual information function ran
#'   over a maximum lag approximately equal to 5% of the time series length.
#'   3. "acf", which triggers automated optimisation of the exclusion radius up
#'   to using the first minima of the lagged autocorrelation information
#'   function ran over a maximum lag approximately equal to 5% of the time
#'   series length.
#'
#'   If the embedding lag is high, the exclusion radius can be set to E + 1.
#'   If the embedding lag is low, the exclusion radius should be higher, or,
#'   even better, estimated using either "mi" or "acf".
#' @param random.libs Whether or not to sample random library (training) sets.
#'   Defaults to TRUE.
#' @param with.replacement Should samples be drawn with replacement? Defaults
#'   to TRUE.
#' @param convergence.test Should a convergence test be performed? Analyses
#'   where CCM does not convergence are nonsensical, so this option defaults to
#'   TRUE.
#' @param n.libsizes.to.check How many library sizes to use for the convergence
#'   check? Defaults to 30.
#' @param regression.convergence.plots Display regression plots for the
#'   convergence test? Defaults to FALSE.
#' @param n.surrogates Should a surrogate test also be performed? If so,
#'   'n.surrogates' sets the number of surrogate time series to use. By default,
#'   no surrogate test is performed (n.surrogates = 0).
#' @param samples.surrogates The number of surrogate series in each null
#'   ensemble.
#' @param always.run.surrogates Should surrogate analyses be performed even if
#'   the convergence test fails? Defaults to FALSE (there is, usually, no reason
#'   to perform significance testing if the analysis is not causal to begin with).
#' @param surrogate.column Which column to use to generate surrogates. Defaults
#'   to the value of 'target.column' (the presumed driver).
#' @param surrogate.methods Vector of types of surrogate time series to
#'   generate. Will vary depending on what null hypothesis is being tested.
#'   Defaults to AAFT surrogates.
#' @param num.neighbours The number of nearest neighbours to use in predictions.
#'   Defaults to NULL, in which case it is set to E + 1 for each analysis (this
#'   number will vary depending on the particular set of embedding parameters
#'   used for that analysis).
#' @param RNGseed For reproducivility. Seed to use for the random number
#'   generator.
#' @param parallel Activate parallellisation? Defaults to true. Currently, this
#'   only works decently on Mac and Linux systems.
#' @param epsilon Exlude neighbours if the are within a distance of 'epsilon'
#'   from the predictee.
#' @param silent Suppress warnings?
#' @param parallelize.on.each.lag Should parallellisation be done on the outer
#'   lag loop? Defaults to TRUE. Otherwise, parallellisation is done over the
#'   surrogate analyses.
#' @param num.cores The number of CPU cores to use for parallelisation. Defaults
#'   to one core less than what is available.
#' @param time.series.length.threshold Display a warning if the time series
#'   length drops below this threshold.
#' @param time.unit The time unit of the raw time series.
#' @param time.bin.size The temporal resolution of the raw time series (given in
#'   the units indicated by 'time.unit').
#' @param print.to.console Display progress?
#' @param time.run Time the run?
#' @param plot.simplex.projection Plot simplex projection output?
#'
#' @export
ccm_lagged <- function(data,
                       lags = 0,
                       Es = NULL,
                       taus = NULL,
                       library.sizes = c(as.integer(nrow(data) / 2)),
                       lib = c(1, dim(data)[1]),
                       pred = lib,
                       samples.original = 100,
                       samples.surrogates = 50,
                       n.surrogates = 0,
                       surrogate.methods = c("aaft"),
                       time.unit = NULL,
                       time.bin.size = NULL,
                       num.neighbours = NULL,
                       random.libs = TRUE,
                       with.replacement = TRUE,
                       exclusion.radius = NULL,
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
                       parallelize.on.each.lag = F,
                       num.cores = parallel::detectCores() - 1,
                       regression.convergence.plots = F,
                       always.run.surrogates = F,
                       n.libsizes.to.check = 20,
                       optimise.FNNdim = T,
                       optimise.boxcountdim = T,
                       which.optimdim.test = "all",
                       min.E = 2,
                       max.E = 10,
                       max.tau = 10,
                       plot.simplex.projection = F,
                       ...) {

  # Save optimisation status.
  lagoptimisation = ifelse(taus == "acf" | taus == "mi", taus, "none")
  dimoptimisation = ifelse(is.null(Es), T, F)
  exclusionradiusoptim = ifelse(exclusion.radius == "acf" | exclusion.radius == "mi", exclusion.radius, "none")
  #optimisationtest = ifelse(dimoptimisation, "none", which.optimdim.test)

  # Warn if the number of library.sizes is too low. ----
  if (n.libsizes.to.check < 20) {
    # We need a reasonable amount if point to do a proper exponential
    # function fit, so default to 20 if the provided number is less.
    warning("Minimum number of libsizes to check is too low. Default to 20.")
    n.libsizes.to.check = 20
  }

  # If no embedding parameters are provided, find the best embedding parameters.

  # Estimating the embedding lag ----

  # The idea behind embedding lag optimisation is that points in the time
  # series might be temporally correlated. Using small embedding lags may in
  # such cases yield embeddings that are very stretched out in some directions
  # compared to others. To get embeddings that are less streched, use a higher
  # embedding lag.
  #
  # The disadvantage of using a high embedding lag is that many data points are
  # left out (i.e. information is lost). For applications where the number of
  # data points is low, it makes sense not to make the embedding lag too big
  # (loosing precious data points). Therefore, if no lag is provided, the
  # embedding lag defaults to 1.

  # We use the putative driver time series to estimate best embedding lag.
  if (is.numeric(taus)) {
    taus = taus
  } else if (is.null(taus)) {
    taus = 1
  } else if (taus == "mi") {
    # Estimate the first minima of the lagged mutual information function for
    # the putative driver time series.
    v = data[, target.column]

    taus = first_mi_minima(
      v = v,
      lag.max = min(max.tau, ceiling(length(v) * 0.05))
    )
  } else if (taus == "acf") {
    # Estimate the first minima of the lagged autocorrelation function
    # for the putative driver time series.
    v = data[, target.column]

    taus = first_mi_minima(
      v = v,
      lag.max = min(max.tau, ceiling(length(v) * 0.05))
    )
  }

  # Estimate embedding dimension ----
  if (is.null(Es)) {
    Es = vector(length = length(taus))

    for (i in 1:length(taus)) {
      optimal.embed.dim = optimise_embedding_dim(
        v = data[, target.column],
        min.embedding.dim = min.E,
        max.embedding.dim = max.E,
        embedding.lag = taus[i],
        optimise.FNNdim = optimise.FNNdim,
        optimise.boxcountdim = optimise.boxcountdim,
        plot.simplex.projection = plot.simplex.projection,
        return.all = T
      )

      # Select the embedding dimension.
      if (which.optimdim.test == "FNN") {
        Es[i] = optimal.embed.dim["FNN.criterion"]
      } else if (which.optimdim.test == "boxcount") {
        Es[i] = optimal.embed.dim["boxcount.criterion"]
      } else if (which.optimdim.test == "simplex") {
        Es[i] = optimal.embed.dim["simplex.projection.optimisation"]
      } else if (which.optimdim.test == "all") {
        Es[i] = max(optimal.embed.dim, na.rm = T)
      }
    }
  }

  # Perform lagged CCM for each combination of the parameters ----

  # Generate all possible combinations of the input parameters
  params = expand.grid(Es, taus, surrogate.methods)
  colnames(params) = c("E", "tau", "surrogate.method")

  results = list()
  for (i in 1:nrow(params)) {
    tau = params[i, "tau"]
    E = params[i, "E"]
    surrogate.method = as.character(params[i, "surrogate.method"])

    # If exclusion radius or number of neighbours is explicitly provided,
    # use the provided values. Otherwise, use the first minimum of the
    # mutual information or autocorrelation function with a maximum lag
    # of 5% of the number of available data points. If this number is lower
    # than E + 1, use E + 1.
    if (is.numeric(exclusion.radius)) {
      exclusion.radius = exclusion.radius
    } else if (is.null(exclusion.radius)) {
      exclusion.radius = E + 1
    } else if (exclusion.radius == "mi") {
      # Estimate the first minima of the lagged mutual information function for
      # the putative driver time series.
      v = data[, target.column]
      exclusion.radius = first_mi_minima(v = v,
                                         lag.max = ceiling(length(v) * 0.05))
    } else if (exclusion.radius == "acf") {
      # Estimate the first minima of the lagged autocorrelation function
      # for the putative driver time series.
      v = data[, target.column]
      exclusion.radius = first_mi_minima(v = v,
                                  lag.max = ceiling(length(v) * 0.05))
    }

    # Use E + 1 nearest neighbours unless otherwise specified. If the provided
    # number of neighbours is too low, also use E + 1.
    if (is.null(num.neighbours)) {
      num.neighbours = E + 1
    } else if (num.neighbours < E + 1) {
      warning(paste("num.neighbours (", num.neighbours, ") is too low.",
                    "Defaulting to E + 1 neighbours."))
      num.neighbours = E + 1
    }

    # Perform CCM for the given set of parameters
    ccm = ccm_lagged_oneway(
      lags = lags,
      data = data,
      E = E,
      tau = tau,
      library.sizes = library.sizes,
      lib = lib,
      pred = lib,
      samples.original = samples.original,
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
      max.tau = max.tau
    )

    # Add information about the parameters that are not recorded in deeper
    # CCM functions.
    ccm$max.E = rep(max.E)
    ccm$max.tau = rep(max.tau)
    ccm$n.libsizes.to.check = rep(n.libsizes.to.check)

    # Information about optimisation
    ccm$which.exclusionradius.test = rep(exclusionradiusoptim)

    ccm$which.optimlag.test =  rep(lagoptimisation)

    ccm$which.optimdim.test = rep(ifelse(dimoptimisation, which.optimdim.test, "none"))
    ccm$optimise.simplex = rep(ifelse(dimoptimisation, T, F))
    ccm$optimise.FNNdim = rep(ifelse(dimoptimisation, optimise.FNNdim, F))
    ccm$optimise.boxcountdim = rep(ifelse(dimoptimisation, optimise.boxcountdim, F))

    # Store the result.
    results[[i]] = ccm
  }

  # Bind all results together.
  results = data.table::rbindlist(results)

  return(results)
}
