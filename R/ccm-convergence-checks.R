#' Performs an exponential regression on the form p = p_max - p0*exp(-c*(L-L0))
#' The same as in van Nes et al. (2016). In this paper, we use it with data
#' where where p is the CCM predictive skill and L is the library size.
ExponentalRegression2 <- function(data) {
  exp.model = nls(data = df,
                  formula = rho ~ a * L/(b + L),
                  start = list(a = 0.1, b = 0.1))
  return(exp.model)
}


#' Computes various parameters that can be used to determine if
#' cross-map skill increases with increasing library size.
#'
#' @param ccm.result A data frame
get_convergence_parameters <- function(ccm.result,
                                       confidence.level = 0.99,
                                       plot = F) {


  ####################
  # Set up for testing.
  ####################

  # Select the relevant data columns
  df = ccm.result[, c("lib_size", "rho")]

  # Extract the library sizes to use for regressions
  library.sizes = unique(df$lib_size)

  # Create a vector to store the various test results
  coeffs = c(NA, NA, NA, NA, NA, NA, NA, NA)
  names(coeffs) = c("k", "a", "b", "highlow.difference", "p.value", "alpha", "confidence.level", "convergent")

  if (is.null(ccm.result)) return(coeffs)

  # Test 1:
  # Check whether cross-map skill is higher at larger
  # library sizes than at lower library sizes. Runs
  # a smoother over the data before comparing the
  # very largest and smallest library sizes.
  grouping.variable = rlang::sym("lib_size")

  medians = dplyr::group_by(df, !!grouping.variable) %>%
    dplyr::summarise(median.rho = median(rho, na.rm = T)) %>%
    as.data.frame()

  # Smooth the data using a running mean
  medians$median.rho = zoo::rollmean(x = medians$median.rho,
                                     k = 3, #floor(length(library.sizes)) / 5,
                                     na.pad = T,
                                     align = "right")
  medians = medians[complete.cases(medians), ]

  # Compute difference between high and low library sizes
  indices.low = 1:5
  indices.high = (nrow(medians) - 5):nrow(medians)

  # Nonparametric hypothesis test to check whether ccm skill at largest
  # library sizes is significantly higher than at the smallest library
  # sizes. The difference in ccm skill must be larger than one standard
  # deviation of the ccm skill at the largest library sizes (at lower
  # library sizes, standard deviations are usually very high and thus
  # not giving useful information). We therefore prefer to use the spread
  # statistic for the highest library sizes.
  wilcox = stats::wilcox.test(x = medians[indices.high, "median.rho"],
                              y = medians[indices.low, "median.rho"],
                              alternative = "greater",
                              mu = sd(medians[indices.high, "median.rho"]),
                              conf.level = confidence.level,
                              exact = FALSE)

  coeffs["highlow.difference"] = mean(medians[indices.high, "median.rho"]) -
                                 mean(medians[indices.low, "median.rho"])
  coeffs["p.value"] = wilcox$p.value
  coeffs["alpha"] = 1 - confidence.level

  coeffs["confidence.level"] = confidence.level
  coeffs["convergent"] = ifelse(test = wilcox$p.value < 1 - confidence.level,
                                yes = TRUE,
                                no = FALSE)

  # Test 2:
  # Fit an exponential regression model and determine convergence
  # by the estimated model parameter.
  colnames(df) = c("L", "rho")
  df$rho = df$rho
  start.index = which(df$rho > 0)[1]


  if (is.na(start.index)) {
    slope = NA
  } else {
    df = df[start.index:nrow(df), ]
    df$rho0 = df[1, "rho"]
    df$rho.max = rep(quantile(df$rho, probs = 0.95, na.rm = T))
    df$L0 = df[1, "L"]

    # Logaritmic dataframe representation of the exponential expression,
    # in case some values are negative.
    dt = do.call("cbind", list(log(df$rho0),
                               log(df$rho.max - df$rho),
                               (df$L - df$L0))
                )
    dt = as.data.frame(dt)

    dt$L = df$L
    dt = as.data.frame(dt <- dt[!is.infinite(rowSums(dt)), ])
    dt$logrhos = dt[, 1] - dt[, 2]
    dt$L = dt[, 3]

    if (all(!is.finite(dt$logrhos))) {
      slope = 0
    } else {
      slope = lm(dt$logrhos ~ dt$L)$coefficients[2]
      f2 <- suppressWarnings(rho ~ rho.max - rho0*exp(-(k) * (L - L0)))

      exp.model2 = try(nls(data = df,
                           formula = f2,
                           start = list(k = .1)),
                       silent=T)

      if (!class(exp.model2) == "try-error") {
        L = seq(min(library.sizes), max(library.sizes), 1)

        predicted.rho.model2 = predict(exp.model2, list(L = L))
        pred2 = as.data.frame(cbind(L, predicted.rho.model2))
      }
    }

    coeffs[1] = slope

    # Test 3:
    # Fit a simple slowly converging model and determine convergence
    # by the estimated model parameters.
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

    } else if (class(exp.model1) == "try-error") {
      #warning("Exponential model could not be fitted")
    }
  }
  if (plot) {
    p <- ggplot2::ggplot() +
      ggplot2::geom_boxplot(data = reshape2::melt(df, id.vars = "L", measure.vars = "rho"),
                            mapping = ggplot2::aes(x = L, y = value, group = L),
                            alpha = 0.8,
                            fill = "blue",
                            col = "black",
                            outlier.alpha = 0.05) +
      ggplot2::geom_line(data = pred1, mapping = ggplot2::aes(x = L, y = predicted.rho.model1, col = "Slowly converging model")) +
      ggplot2::geom_line(data = pred2, mapping = ggplot2::aes(x = L, y = predicted.rho.model2, col = "Exponential model"), size = 1) +
      ggplot2::geom_point(data = medians, mapping = ggplot2::aes(x = lib_size, y = median.rho, col = "Smoothed medians"), size = 1) +
      ggplot2::scale_y_continuous(limits = c(0, 0.7)) +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid = element_blank(),
                     legend.title = element_blank()) +
      xlab("Library size (L)") +
      ylab("CCM skill (rho)")
    print(p)
  }

  return(coeffs)
}

