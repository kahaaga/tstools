context("Drawing random data from uncertainty range test")

dt = as.numeric(precip)
n = length(dt)
sigmas = rnorm(n = n, sd = sd(dt) * 0.1)
lower.bounds = -abs(rnorm(n = n, sd = sd(dt) * 0.1))
upper.bounds = abs(rnorm(n = n, sd = sd(dt) * 0.1))

test_that("Single data model based on sigmas works", {
  expect_that(length(datamodel(data = dt, sigmas = sigmas)),
                     equals(n))
})

test_that("Single data model based on upper and lower bounds works", {
  expect_that(length(datamodel(data = dt,
                               upper.bounds = upper.bounds,
                               lower.bounds = lower.bounds)),
              equals(n))
})
