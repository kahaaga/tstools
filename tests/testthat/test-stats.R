context("Stats functions tests")

dt = rnorm(n = 100, mean = 0, sd = 1)
dt[80] = sd(dt)*10
dt[50] = -sd(dt)*10

test_that("Outliers are removed", {
  expect_true(length(remove_outliers(dt)) < length(dt))
})
