context("Convergent cross mapping (CCM)")

dt = rbind(CO2[4:5], CO2[4:5])[1:100, ]


# Default for exclusion radius, embedding lag and embedding dimension
ex1a = suppressWarnings(
  ccm_lagged(data = chaoticmaps::dejong_map(n = 80), lags = 0,
             samples.original = 30,
             exclusion.radius = NULL,
             taus = NULL,  Es = NULL)
)


# Default for exclusion radius, embedding lag and embedding dimension
ex1b = suppressWarnings(
  ccm_lagged(data = chaoticmaps::dejong_map(n = 80), lags = 0,
             samples.original = 30,
             exclusion.radius = "mi",
             taus = NULL,  Es = NULL)
)


# Default for exclusion radius, embedding lag and embedding dimension
ex1b = suppressWarnings(
  ccm_lagged(data = chaoticmaps::dejong_map(n = 80), lags = 0,
             samples.original = 30,
             exclusion.radius = "acf",
             taus = NULL,  Es = NULL)
)

# Explicitly set embedding lag and embedding dimensions, default for exclusion radius.
ex2 = suppressWarnings(
  ccm_lagged(data = chaoticmaps::dejong_map(n = 80), lags = 0,
             samples.original = 30,
            exclusion.radius = NULL,
            taus = c(1, 2), Es = c(2, 3))
  )

# Default for exclusion radius, embedding lag and embedding dimension
ex3 = suppressWarnings(
  ccm_lagged(data = chaoticmaps::dejong_map(n = 80), lags = 0,
             samples.original = 30,
             exclusion.radius = NULL,
             taus = NULL,  Es = NULL)
)

# Optimise tau
ex3 = suppressWarnings(
  ccm_lagged(data = chaoticmaps::dejong_map(n = 80), lags = 0,
             samples.original = 30,
             exclusion.radius = NULL,
             taus = "mi",  Es = NULL)
)

# Optimise tau
ex3 = suppressWarnings(
  ccm_lagged(data = chaoticmaps::dejong_map(n = 80), lags = 0,
             samples.original = 30,
             exclusion.radius = NULL,
             taus = "acf",  Es = NULL)
)
