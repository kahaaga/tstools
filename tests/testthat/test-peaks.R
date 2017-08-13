#
# context("Local minima and maxima tests")
#
# test_that("Local maxima are identified", {
#   # Use treering data
#
#   x = c(1, 2, 3, 2, 4, 6, 7, 5, 4, 3, 2, 1)
#
#   # One point to each side
#   peaks = find_peaks(x, n = 1)
#   expect_equal(peaks, c(3, 7))
#
#   # Two points to each side
#   peaks = find_peaks(x, n = 2)
#   expect_equal(peaks, c(7))
#
#   # Three points to each side
#   peaks = find_peaks(x, n = 3)
#   expect_equal(peaks, c(7))
#
# })
#
# test_that("Local mimima are identified", {
#   # Use treering data
#
#   x = c(6,5,4,5,6,7,6,5,6,7,8,6,5,4,5,6,5)
#
#
#   # One point to each side
#   peaks = find_troughs(x, n = 1)
#   expect_equal(peaks, c(3, 8, 14))
#
#   # Two points to each side
#   peaks = find_troughs(x, n = 2)
#   expect_equal(peaks, c(3, 8, 14))
#
# })
#
# test_that("Returns null when no maxima or minima are found", {
#   expect_null(find_troughs(c(1,1,1,1), 1))
#   expect_null(find_peaks(c(1,1,1,1), 1))
#   expect_null(find_troughs(c(3,2,3), 2))
# })
