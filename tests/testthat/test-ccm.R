context("Convergent cross mapping (CCM)")

dt = rbind(CO2[4:5], CO2[4:5])[1:100, ]

# Explicitly set embedding dimension
ccm = suppressWarnings(
  ccm_lagged(data = dt, lags = 0, E = 2, tau = 1,
             samples.original = 30,
             n.surrogates = 0)
)
# Optimise embedding dimension
# ccm = suppressWarnings(
#   ccm_lagged(data = dt, lags = -1:1,
#            samples.original = 30,
#            samples.surrogates = 30,
#            n.surrogates = 20)
#   )
#

#summary = directionalcausaltest(res = ccm)
