#' Performs CCM for a given lag.
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
                print.surrogate.to.console = T,
                n.libsizes.to.check = 20) {


  # Refer to library and target columns by name, not index.
  cols = column_names_as_string(column.names = colnames(data),
                                library.column = library.column,
                                target.column = target.column,
                                surrogate.column = surrogate.column)


  # Cross map using the provided data
  if (convergence.test == TRUE) {
    ccm.result = ccm_over_library_sizes(
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
      ccm.result = ccm_on_single_libsize(data = data,
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
    params = get_convergence_parameters(ccm.result)
  } else {
    params = get_convergence_parameters(ccm.result = NULL)
  }


  # Surrogate cross map at the largest provided library size.
  if (n.surrogates > 0) {
    validate_surrogate_method(surrogate.method)

    surrogate.results = surrogate_ccm(original.data = data,
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
                                      num_neighbors = num_neighbors,
                                      random.libs = random.libs,
                                      library.column = cols["library.column"],
                                      target.column = cols["target.column"],
                                      surrogate.column = cols["surrogate.column"],
                                      samples.surrogates = samples.surrogates,
                                      n.surrogates = n.surrogates,
                                      parallel = parallel,
                                      surrogate.method = surrogate.method)

    ccm.results = data.table::rbindlist(l = list(ccm.result, surrogate.results),
                                        use.names = TRUE)

    ccm.results$confidence.level = params["confidence.level"]
    ccm.results$p.value = params["p.value"]
    ccm.results$alpha = params["alpha"]
    ccm.results$convergent = params["convergent"]
    ccm.results$k = params["k"]
    ccm.results$a = params["a"]
    ccm.results$b = params["b"]
  }

  # Combine everything
  if (n.surrogates == 0) {
    ccm.results = ccm.result
    ccm.results$confidence.level = params["confidence.level"]
    ccm.results$p.value = params["p.value"]
    ccm.results$alpha = params["alpha"]
    ccm.results$convergent = params["convergent"]
    ccm.results$k = params["k"]
    ccm.results$a = params["a"]
    ccm.results$b = params["b"]
  }

  # Some analysis info
  ccm.results$time.of.analysis = Sys.time()
  ccm.results$analyst = Sys.info()["effective_user"]
  ccm.results$sysname = Sys.info()["sysname"]
  ccm.results$Rversion = version$version.string
  ccm.results$id = stringi::stri_rand_strings(n = 1, length = 20)

  return(ccm.results)
}
