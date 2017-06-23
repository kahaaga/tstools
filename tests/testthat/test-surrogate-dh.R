context("Davison-Hinkley surrogates test")
# Some time series.
ts1 = as.numeric(precip)
ts2 = as.numeric(datasets::BJsales.lead)
ts3 = as.numeric(datasets::treering)

# Create surrogates for the time series.
dh.surr1 = dh_surrogate(ts1)
dh.surr2 = dh_surrogate(ts2)
dh.surr3 = dh_surrogate(ts3)

test_that("dh surrogate doesn't equal original data", {
  expect_failure(expect_equal(dh.surr1, ts1))
  expect_failure(expect_equal(dh.surr2, ts2))
})

# The original data and the Davison-Hinckley surrogate should,
# in general, not contain the same values (because the values of
# these surrogates are not constrained to the original data).
test_that("dh surrogates are not constrained to original data", {
  expect_failure(expect_equal(sort(dh.surr1), sort(ts1)))
  expect_failure(expect_equal(sort(dh.surr2), sort(ts2)))
})
