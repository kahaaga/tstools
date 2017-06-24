v = c(10, 8, 7, 6, 4.5, 7, 7, 8, 5, 4, 3, 1)
w = c(10, 10, 10, 10, 10, 10, 10, 10, 10)

test_that("first local minima is identified", {
  expect_that(first_local_minima(v, value = F), equals(5))
  expect_that(first_local_minima(v, value = T), equals(4.5))

  expect_false(is.null(first_local_minima(v)))

  expect_that(first_local_minima(w), equals(NA))
  expect_that(first_local_minima(w, value = T), equals(10))
})

