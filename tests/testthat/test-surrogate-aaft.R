context("AAFT surrogates")

# Some time series.
ts1 = as.numeric(precip)
ts2 = as.numeric(datasets::BJsales.lead)
ts3 = as.numeric(datasets::treering)

# Create surrogates for the time series.
aaft.surr1 = aaft_surrogate(ts1)
aaft.surr2 = aaft_surrogate(ts2)
aaft.surr3 = aaft_surrogate(ts3)

# Original data and surrogates should contain the exact same values
test_that("aaft surrogates are constrained to original data", {
  expect_equal(sort(aaft.surr1), sort(ts1))
  expect_equal(sort(aaft.surr2), sort(ts2))
  expect_equal(sort(aaft.surr3), sort(ts3))
})

# But the original data and the surrogate should not be
# be the same series (respecting the order)
test_that("aaft surrogates and original series are not identical", {
  expect_failure(expect_equal(aaft.surr1, ts1))
  expect_failure(expect_equal(aaft.surr2, ts2))
  expect_failure(expect_equal(aaft.surr3, ts3))
})


