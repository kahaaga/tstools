v = c(10, 8, 7, 6, 4.5, 7, 7, 8, 5, 4, 3, 1)
w = c(10, 10, 10, 10, 10, 10, 10, 10, 10)

dt = as.numeric(precip)

test_that("first local minima is identified", {
  expect_that(first_local_minima(v, value = F), equals(5))
  expect_that(first_local_minima(v, value = T), equals(4.5))

  expect_false(is.null(first_local_minima(v)))

  expect_that(first_local_minima(w), equals(NA))
  expect_that(first_local_minima(w, value = T), equals(10))
})


test_that("mutual information minima works", {
  expect_that(first_mi_minima(dt, lag.max = 10, plot.mi.func = T), equals(1))
  expect_that(first_mi_minima(dt, lag.max = 10, plot.mi.func = F), equals(1))
  expect_that(length(first_mi_minima(dt, lag.max = 10)), equals(1))

})


test_that("autocorrelation function minima works", {
  expect_that(first_acf_minima(dt, lag.max = 10, plot.acf = T), equals(1))
  expect_that(first_acf_minima(dt, lag.max = 10, plot.acf = F), equals(1))
  expect_that(length(first_acf_minima(dt, lag.max = 10)), equals(1))
})
