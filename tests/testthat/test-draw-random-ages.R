context("Draw random ages test")
set.seed(1)

time = as.numeric(start(treering)[1]:end(treering)[1])
data = as.numeric(unclass(treering))[1:300]

sigma = rnorm(n = length(dt), mean = 0, sd = 0.03)

#test_that("Drawing random ages works", {
#  am = draw_agemodel(ages = time,
#                     sigmas = sigma,
#                     firstdiffagreementratio = 0.5)
#  expect_true(length(data) == length(am))
#})
