context("IAAFT surrogates")

# Some time series.
ts1 = as.numeric(precip)
ts2 = as.numeric(datasets::BJsales.lead)
ts3 = as.numeric(datasets::treering)

# Create surrogates for the time series.
iaaft.surr1 = iaaft_surrogate(ts1)
iaaft.surr2 = iaaft_surrogate(ts2)
iaaft.surr3 = iaaft_surrogate(ts3)

# Original data and surrogates should contain the exact same values
test_that("iaaft surrogates are constrained to original data", {
  expect_equal(sort(iaaft.surr1), sort(ts1))
  expect_equal(sort(iaaft.surr2), sort(ts2))
  expect_equal(sort(iaaft.surr3), sort(ts3))
})

# But the original data and the surrogate should not be
# be the same series (respecting the order)
test_that("iaaft surrogates and original series are not identical", {
  expect_failure(expect_equal(iaaft.surr1, ts1))
  expect_failure(expect_equal(iaaft.surr2, ts2))
  expect_failure(expect_equal(iaaft.surr3, ts3))
})


