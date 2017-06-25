context("Draw random ages test")
set.seed(1)

time = as.numeric(start(treering)[1]:end(treering)[1])
time = time[1:300]
data = as.numeric(unclass(treering))[1:300]
sigma = abs(rnorm(n = length(data), mean = 0, sd = 0.03))

test_that("Agemodel fails when ages are not strictly increasing.", {
  ages = time
  ages[2] = ages[1]
  sigma = rnorm(n = length(dt), mean = 0, sd = 0.03)
  expect_error(draw_agemodel(ages = time, sigmas = sigma))
})

test_that("Negative uncertainties for agemodel gives error.", {
  sigma = rnorm(n = length(dt), mean = 0, sd = 0.03)
  sigma[1] = - 1
  expect_error(draw_agemodel(ages = time, sigmas = sigma))
})

test_that("Only positive uncertainties works for agemodel", {
  sigma = abs(rnorm(n = length(dt), mean = 0, sd = 0.02))
  expect_error(draw_agemodel(ages = time, sigmas = sigma))
})

test_that("Drawing age model ensemble works", {
  expect_true(ncol(draw_random_ages(ages = time, sigmas = sigma)) == 1)
  expect_true(ncol(draw_random_ages(ages = time, sigmas = sigma, n.replicates = 5)) == 5)
  expect_error(draw_random_ages(ages = time, sigmas = sigma, n.replicates = NULL))
  expect_error(draw_random_ages(ages = NULL, sigmas = sigma, n.replicates = NULL))
  expect_error(draw_random_ages(ages = time, sigmas = NULL, n.replicates = 3))
})
