

#' Performs CCM for a single direction
#' @export
ccm <- function(library.size,
                data,
                lag = 0,
                E = 2,
                tau =1,
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
                print.surrogate.to.console = T) {


  # Refer to library and target columns by name, not index.
  cols = column_names_as_string(column.names = colnames(data),
                                library.column = library.column,
                                target.column = target.column,
                                surrogate.column = surrogate.column)

  if (n.surrogates > 0) {
    ValidateSurrogateMethod(surrogate.method)

    results = surrogate_ccm(original.data = data,
                           E = 2,
                           tau = 1,
                           library.size = library.size,
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

    # Add column indicating type of analysis to each of the results
    results = mapply(`[<-`, results,
                      'analysis.type', value = "surrogate",
                     SIMPLIFY = FALSE)
  }

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

  if (n.surrogates > 0) {
    results[["original"]] = original.ccm
    combined = data.table::rbindlist(results)
  } else {
    combined = original.ccm
  }
  return(combined)
}

