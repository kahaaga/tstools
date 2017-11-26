context("Convergent cross mapping (CCM)")


# Default for exclusion radius, embedding lag and embedding dimension
suppressWarnings(
  ccm_lagged(data = chaoticmaps::dejong_map(n = 80), lags = 0,
             convergence.test = T,
             library.sizes = seq(20, 41, 1),
             samples.original = 30,
             exclusion.radius = NULL,
             taus = NULL,  Es = NULL)
)


# Default for exclusion radius, embedding lag and embedding dimension
suppressWarnings(
  ccm_lagged(data = chaoticmaps::dejong_map(n = 80), lags = 0,
             library.sizes = 40, convergence.test = F,
             samples.original = 30,
             exclusion.radius = "mi",
             taus = NULL,  Es = NULL)
)


# Default for exclusion radius, embedding lag and embedding dimension
ex1b <- suppressWarnings(
  ccm_lagged(data = chaoticmaps::dejong_map(n = 80), lags = 0,
             library.sizes = 40, convergence.test = F,
             samples.original = 30,
             exclusion.radius = "acf",
             taus = NULL,  Es = NULL)
)

# Explicitly set embedding lag and embedding dimensions, default for exclusion radius.
ex2 <- suppressWarnings(
  ccm_lagged(data = chaoticmaps::dejong_map(n = 80), lags = 0,
             library.sizes = 40, convergence.test = F,
             samples.original = 30,
             exclusion.radius = NULL,
             taus = c(1, 2), Es = c(2, 3))
)

# Default for exclusion radius, embedding lag and embedding dimension
ex3 <- suppressWarnings(
  ccm_lagged(data = chaoticmaps::dejong_map(n = 80), lags = 0,
             library.sizes = 40, convergence.test = F,
             samples.original = 30,
             exclusion.radius = NULL,
             taus = NULL,  Es = NULL)
)

# Optimise tau using mutual information criterion
suppressWarnings(
  ccm_lagged(data = chaoticmaps::dejong_map(n = 80), lags = 0,
             library.sizes = 40, convergence.test = F,
             samples.original = 30,
             exclusion.radius = NULL,
             taus = "mi",  Es = NULL)
)

# Optimise tau  using autocorrelation criterion
suppressWarnings(
  ccm_lagged(data = chaoticmaps::dejong_map(n = 80), lags = 0,
             library.sizes = 40, convergence.test = F,
             samples.original = 30,
             exclusion.radius = NULL,
             taus = "acf",  Es = NULL)
)


# Different methods for optimising the embedding dimension
suppressWarnings(
  ccm_lagged(data = chaoticmaps::dejong_map(n = 80), lags = 0,
             library.sizes = 40, convergence.test = F,
             samples.original = 30,
             exclusion.radius = NULL,
             taus = "acf",  Es = NULL,
             which.optimdim.test = "FNN")
)

suppressWarnings(
  ccm_lagged(data = chaoticmaps::dejong_map(n = 80), lags = 0,
             library.sizes = 40, convergence.test = F,
             samples.original = 30,
             exclusion.radius = NULL,
             taus = "acf",  Es = NULL,
             which.optimdim.test = "boxcount")
)

suppressWarnings(
  ccm_lagged(data = chaoticmaps::dejong_map(n = 80), lags = 0,
             library.sizes = 40, convergence.test = T,
             n.libsizes.to.check = 15,
             samples.original = 30,
             exclusion.radius = NULL,
             taus = "acf",  Es = NULL,
             which.optimdim.test = "simplex")
)

expect_error(
  ccm_lagged(data = chaoticmaps::dejong_map(n = 80), lags = -1:1,
             library.sizes = 40, convergence.test = T,
             n.libsizes.to.check = 15,
             samples.original = 30,
             exclusion.radius = NULL,
             taus = "acff",  Es = NULL,
             which.optimdim.test = "simplex")
)
