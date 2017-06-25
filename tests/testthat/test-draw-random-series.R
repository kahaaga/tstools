context("Drawing random data from uncertainty range test")

dt = as.numeric(precip)
n = length(dt)
sigmas = rnorm(n = n, sd = sd(dt) * 0.1)
lower.bounds = -abs(rnorm(n = n, sd = sd(dt) * 0.1))
upper.bounds = abs(rnorm(n = n, sd = sd(dt) * 0.1))


test_that("Single data model based on sigmas works", {
  expect_that(length(datamodel_sigmas(data = dt, sigmas = sigmas, n.sigma = 1)),
                     equals(n))
})

test_that("Single data model based on upper and lower bounds works", {
  expect_that(length(datamodel_truncated(data = dt,
                               upper.bounds = upper.bounds,
                               lower.bounds = lower.bounds,
                               n.sigma = 1)),
              equals(n))
})

test_that("General data model succeeds irrespective of type of standard deviations", {

  expect_that(length(datamodel(data = dt, sigmas = sigmas, n.sigma = 2)), equals(n))
  expect_that(length(datamodel(data = dt,
                               upper.bounds = upper.bounds,
                               lower.bounds = lower.bounds,
                               n.sigma = 2)),
              equals(n))

  expect_that(ncol(draw_random_data(dt, sigmas = sigmas, n.replicates = 5)), equals(5))
  expect_that(ncol(draw_random_data(dt,
                                    lower.bounds = lower.bounds,
                                    upper.bounds = upper.bounds,
                                    n.replicates = 5)), equals(5))

})

