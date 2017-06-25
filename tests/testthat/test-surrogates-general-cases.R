testthat::context("Surrogates general cases test")


# Some time series.
ts = as.numeric(precip)

test_that("Surrogate ensembles are created successfully", {
  expect_that(surrogate_ensemble(ts, "aaft", 2), is_a("matrix"))
  expect_that(surrogate_ensemble(ts, "iaaft", 2), is_a("matrix"))
  expect_that(surrogate_ensemble(ts, "ce", 2), is_a("matrix"))
  expect_that(surrogate_ensemble(ts, "dh", 2), is_a("matrix"))
  expect_that(surrogate_ensemble(ts, "phase", 2), is_a("matrix"))
  expect_that(surrogate_ensemble(ts, "ebisuzaki", 2), is_a("matrix"))
  expect_that(surrogate_ensemble(ts, "random", 2), is_a("matrix"))
})

test_that("Bad surrogate method name gives error", {
  expect_error(surrogate_ensemble(ts, surrogate.method = "wrong method", 2))
})

ts[2] = NA

test_that("Creating surrogate for series with NA fails", {
  expect_error(aaft_surrogate(ts))
  expect_error(iaaft_surrogate(ts))
  expect_error(dh_surrogate(ts))
  expect_error(ce_surrogate(ts))
  expect_error(random_surrogate(ts))
  expect_error(phase_surrogate(ts))
  expect_error(phase_randomised_surrogate(ts))
  expect_error(ebisuzaki_surrogate(ts))
})

tsnull = NULL

test_that("Creating surrogate for NULL fails", {
  expect_error(aaft_surrogate(tsnull))
  expect_error(iaaft_surrogate(tsnull))
  expect_error(dh_surrogate(tsnull))
  expect_error(ce_surrogate(tsnull))
  expect_error(random_surrogate(ts))
  expect_error(phase_surrogate(ts))
  expect_error(phase_randomised_surrogate(ts))

  expect_error(ebisuzaki_surrogate(ts))
})
