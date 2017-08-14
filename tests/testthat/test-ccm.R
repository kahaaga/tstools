context("Convergent cross mapping (CCM)")

dt = rbind(CO2[4:5], CO2[4:5])
test_that("Lagged cross mapping works with convergence test", {
  suppressWarnings(ccm_lagged(data = dt,
                              lags = -1:1,
                              n.surrogates = 1,
                              library.sizes = 100,
                              print.to.console = F))
})

test_that("Lagged cross mapping works without convergence test", {
  suppressWarnings(ccm_lagged(data = dt,
                              lags = -1:1,
                              n.surrogates = 1,
                              library.sizes = 100,
                              print.to.console = F,
                              convergence.test = F))
})
