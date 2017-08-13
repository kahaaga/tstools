#' importFrom magrittr "%>%"
#' @export check_ccm_convergence
check_ccm_convergence <- function(lag,
                             data,
                             E = 2, # Attractor reconstruction dimensions
                             tau = 1, # Attractor reconstruction lags
                             low.libsize = min(E * tau + max(2, abs(lag)),
                                               10, 20, na.rm = T),
                             library.size = 100,
                             n.libsizes.to.check = 30,
                             high.libsize = min(ceiling(library.size * 1.5),
                                                library.size - E*tau - abs(lag)),
                             lib = c(1, dim(data)[1]),
                             pred = lib, # Training and prediction libraries overlap (uses leave-n-out cross validation instead of separate libraries)
                             samples.original = 1000,
                             samples.surrogates = 100,
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
                             silent = TRUE,
                             parallel = F,
                             time.run = F,
                             print.to.console = F,
                             time.series.length.threshold = 100,
                             library.column = 1,
                             target.column = 1,
                             surrogate.column = target.column,
                             plot.regressions = T) {
  if (n.libsizes.to.check < 20) {
    warning("@CheckConvergence()\tOnly ", n.libsizes.to.check, "library sizes are being checked for convergence. Spurious non-convergence or convergence might be the result. Consider increasing the value of 'n.libsizes.convergence.check'")
  }
  l1 = as.integer(seq(from = low.libsize,
                      to = ceiling(high.libsize / 4),
                      length.out = ceiling(2 * n.libsizes.to.check / 3))) # More points at lower library sizes
  l2 = ceiling((high.libsize-low.libsize)/2)
  l3 = as.integer(seq(from = ceiling(high.libsize/1.5),
                      to = high.libsize,
                      length.out = ceiling(n.libsizes.to.check / 3)) - 1) # More points at higher library sizes
  library.sizes = c(l1, l3)

  if (parallel) {

    ccm =  mclapply(library.sizes,
                    FUN = ccm,
                    lag = lag,
                    data = data,
                    E = E,
                    tau = tau,
                    lib = lib,
                    pred = pred,
                    samples.original = samples.original,
                    n.surrogates = 0,
                    num.neighbours = num.neighbours,
                    random.libs = random.libs,
                    with.replacement = with.replacement,
                    exclusion.radius = exclusion.radius,
                    epsilon = epsilon,
                    RNGseed = RNGseed,
                    parallel = parallel,
                    silent = silent,
                    print.to.console = print.to.console,
                    library.column = library.column,
                    target.column = target.column,

                    mc.cores = parallel::detectCores() - 1
                    )

    ccm = as.data.frame(do.call("rbind", ccm))
  } else {
    ccm =  lapply(library.sizes,
                  FUN = ccm,
                  lag = lag,
                  data = data,
                  E = E,
                  tau = tau,
                  lib = lib,
                  pred = pred,
                  samples.original = samples.original,
                  n.surrogates = 0,
                  num.neighbours = num.neighbours,
                  random.libs = random.libs,
                  with.replacement = with.replacement,
                  exclusion.radius = exclusion.radius,
                  epsilon = epsilon,
                  RNGseed = RNGseed,
                  parallel = parallel,
                  silent = silent,
                  print.to.console = print.to.console,
                  library.column = library.column,
                  target.column = target.column)

    ccm = as.data.frame(do.call("rbind", ccm))

  }
  # Prepare for regression
  df = ccm[, c("lib_size", "rho")]

  regression.parameter = rlang::sym("rho")
  grouping.variable = rlang::sym("lib_size")

  g = dplyr::group_by(df, !!grouping.variable) %>%
    dplyr::summarise(mean.rho = mean(regression.parameter)) %>%
    dplyr::ungroup()
  print(g)

  return(df)
  colnames(df) = c("L", "rho")
  df$rho = df$rho
  start.index = which(df$rho > 0)[1]
  coeffs = c(NA, NA, NA)
  names(coeffs) = c("k", "a", "b")

  if (is.na(start.index)) {
    slope = NA
  } else {
    df = df[start.index:nrow(df), ]
    df$rho0 = df[1, "rho"]
    df$rho.max = rep(max(df$rho))
    df$L0 = df[1, "L"]
    # Remove the smallest libsizes if they are negative

    # Create exponential regression model
    # Logaritmic dataframe representation of the exponential expression
    dt = do.call("cbind", list(log(df$rho0),
                               log(df$rho.max - df$rho),
                               (df$L - df$L0)
    )
    )
    dt = as.data.frame(dt)

    dt$L = df$L
    dt = as.data.frame(dt <- dt[!is.infinite(rowSums(dt)), ] )
    dt$logrhos = dt[, 1] - dt[, 2]
    dt$L = dt[, 3]

    if (all(!is.finite(dt$logrhos))) {
      slope = 0
    } else {
      slope = lm(dt$logrhos ~ dt$L)$coefficients[2]
      f2 <- (rho ~ rho.max - rho0*exp(-(k) * (L - L0)))

      exp.model2 = try(nls(data = df,
                           formula = f2,
                           start = list(k = .1)),
                       silent=T)

      if (!class(exp.model2) == "try-error") {
        L = seq(min(library.sizes), max(library.sizes), 1)

        predicted.rho.model2 = predict(exp.model2, list(L = L))
        pred2 = as.data.frame(cbind(L, predicted.rho.model2))

        if (plot.regressions) {
          p <- ggplot2::ggplot() +
            ggplot2::geom_point(data = df, mapping = ggplot2::aes(x = L, y = rho), col = "red") +
            ggplot2::geom_line(data = pred2, mapping = ggplot2::aes(x = L, y = predicted.rho.model2)) +
            ggplot2::scale_y_continuous(limits = c(0, 1)) +
            ggplot2::theme_bw()
          print(p)
        }
      }
    }

    coeffs[1] = slope

    # Nonlinear regressions
    # #####################
    # Fit a simple slowly converging model.
    f1 <- (rho ~ a*L/(b+L))

    exp.model1 = try(nls(data = df, formula = f1, start = list(a = 0.1, b = 0.1)), silent = T)

    if (!class(exp.model1) == "try-error") {
      coeffs["a"] = coef(exp.model1)["a"]
      coeffs["b"] = coef(exp.model1)["b"]

      # Define training points (library sizes L) to predict rho for.
      L = seq(min(library.sizes), max(library.sizes), 1)
      predicted.rho.model1 = predict(exp.model1, list(L = L))
      pred1 = cbind(L, predicted.rho.model1)
      pred1 = as.data.frame(pred1)

      if (plot.regressions) {
        p <- ggplot2::ggplot() +
          ggplot2::geom_point(data = df, mapping = ggplot2::aes(x = L, y = rho), col="blue") +
          ggplot2::geom_line(data = pred1, mapping = ggplot2::aes(x = L, y = predicted.rho.model1)) +
          ggplot2::scale_y_continuous(limits = c(0, 1)) +
          ggplot2::theme_bw()
        print(p)
      }
    } else if (class(exp.model1) == "try-error") {
      #warning("Exponential model could not be fitted")
    }
  }

  if (slope > 0 & is.finite(slope)) cat("Convergent\n")
  else cat("Not convergent")
  return(coeffs)
}

#' Performs an exponential regression on the form p = p_max - p0*exp(-c*(L-L0))
#' The same as in van Nes et al. (2016). In this paper, we use it with data
#' where where p is the CCM predictive skill and L is the library size.
ExponentalRegression2 <- function(data) {
  exp.model = nls(data = df,
                  formula = rho ~ a * L/(b + L),
                  start = list(a = 0.1, b = 0.1))
  return(exp.model)
}


