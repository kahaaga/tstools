context("Convergent cross mapping (CCM) with surrogates")

suppressWarnings(
  ccm_lagged(data = chaoticmaps::dejong_map(n = 80),
             lags = 0,
             samples.original = 25,
             samples.surrogates = 25,
             surrogate.methods = c("aaft", "random", "iaaft"),
             n.surrogates = 25,
             taus = 1,  Es = 2,
             parallel = F)
)

suppressWarnings(
  ccm_lagged(data = chaoticmaps::dejong_map(n = 80),
             lags = 0,
             samples.original = 25,
             samples.surrogates = 25,
             surrogate.methods = c("aaft", "random", "iaaft"),
             n.surrogates = 25,
             taus = 1,  Es = 2,
             parallel = T)
)
