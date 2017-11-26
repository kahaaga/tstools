context("Convergent cross mapping (CCM) with surrogates")


# Default for exclusion radius, embedding lag and embedding dimension
suppressWarnings(
  ccm_lagged(data = chaoticmaps::dejong_map(n = 80),
             lags = 0,
             convergence.test = T,
             samples.original = 25,
             samples.surrogates = 25,
             n.surrogates = 25,
             taus = 1,  Es = 2)
)
