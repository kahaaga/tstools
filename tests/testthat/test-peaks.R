context("Local minima and maxima tests")

test_that(desc = "local_minimum_found",
  expect_true(length(find_local_minima(chaoticmaps::dejong_map(n = 80)$x)) > 0)
)

test_that(desc = "local_maximum_found",
  expect_true(length(find_local_maxima(chaoticmaps::dejong_map(n = 80)$x)) > 0)
)

test_that(desc = "peaks found",
  expect_true(length(find_peaks(chaoticmaps::dejong_map(n = 80)$x)) > 0)
)

test_that(desc = "troughs found",
  expect_true(length(find_troughs(chaoticmaps::dejong_map(n = 80)$x)) > 0)
)

test_that(desc = "acf visits strictly positive case",
  acf(c(10, 9, 8, 9, 10), lag.max = 1)
)
