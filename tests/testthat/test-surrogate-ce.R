context("Circulant embedding surrogates")

# Some time series.
ts1 = as.numeric(precip)
ts2 = as.numeric(datasets::BJsales.lead)
ts3 = as.numeric(datasets::treering)

# Create surrogates for the time series.
ce.surr1 = ce_surrogate(ts1)
ce.surr2 = ce_surrogate(ts2)
ce.surr3 = ce_surrogate(ts3)


test_that("ce surrogate doesn't equal original data", {
  expect_failure(expect_equal(ce.surr1, ts1))
  expect_failure(expect_equal(ce.surr2, ts2))
  expect_failure(expect_equal(ce.surr3, ts3))
})

# The original data and the Davison-Hinckley surrogate should,
# in general, not contain the same values (because the values of
# these surrogates are not constrained to the original data).
test_that("ce surrogates are not constrained to original data", {
  expect_failure(expect_equal(sort(ce.surr1), sort(ts1)))
  expect_failure(expect_equal(sort(ce.surr2), sort(ts2)))
  expect_failure(expect_equal(sort(ce.surr3), sort(ts3)))
})
