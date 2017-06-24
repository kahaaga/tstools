testthat::context("Surrogates general cases test")


# Some time series.
ts = as.numeric(precip)
ts[2] = NA

tsnull = NULL

test_that("Creating surrogate for series with NA fails", {
  expect_error(aaft_surrogate(ts))
  expect_error(iaaft_surrogate(ts))
  expect_error(dh_surrogate(ts))
  expect_error(ce_surrogate(ts))
})

test_that("Creating surrogate for series with NULL fails", {
  expect_error(aaft_surrogate(tsnull))
  expect_error(iaaft_surrogate(tsnull))
  expect_error(dh_surrogate(tsnull))
  expect_error(ce_surrogate(tsnull))
})
