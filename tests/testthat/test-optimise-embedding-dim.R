context("Optimise embedding dimension tests")

dt = as.numeric(unclass(treering))[1:300]

# Suppress warnings here, but explicitly test for it afterwards.

test_that("Optimisation using simplex projection works", {
  # Only estimate dimension and proceed with other tests.
  optimal.dim.simplex = suppressWarnings(optimise_dim_simplex(dt))


  # Tests.
  expect_warning(optimise_dim_simplex(dt))
  expect_warning(optimise_dim_simplex(dt, plot.simplex.projection = T))

  expect_that(optimal.dim.simplex, is_a("numeric"))
  expect_that(length(optimal.dim.simplex), equals(1))
})

test_that("Optimisation using false nearest neighbours (FNN) works", {
  optimal.dim.FNN = suppressWarnings(optimise_dim_FNN(dt))

  # Tests
  expect_that(optimal.dim.FNN, is_a("numeric"))
  expect_that(length(optimal.dim.FNN), equals(1))
})

test_that("Optimisation using boxcounting dimension works", {
  # Only estimate dimension and proceed with other tests.
  optimal.dim.boxcount = suppressWarnings(optimise_dim_boxcount(dt))

  # Tests
  expect_that(optimal.dim.boxcount, is_a("numeric"))
  expect_that(length(optimal.dim.boxcount), equals(1))
})

test_that("No optimisation method yields warning", {
  expect_warning(optimise_embedding_dim(v = dt,
                                        optimise.simplex = F,
                                        optimise.FNNdim = F,
                                        optimise.boxcountdim = F))
})

test_that("Optimisation with default settings works", {
  optimal.dims = suppressWarnings(optimise_embedding_dim(dt))

  expect_warning(optimise_embedding_dim(dt))

  expect_that(length(optimal.dims), equals(3))
  expect_that(names(optimal.dims), equals(c("simplex.projection.optimisation",
                                          "FNN.criterion",
                                          "boxcount.criterion")))
})

test_that("Optimisation with defaults and return.all = FALSE", {
  expect_that(length(suppressWarnings(optimise_embedding_dim(dt, return.all = FALSE))),
              equals(1))
  expect_that(suppressWarnings(optimise_embedding_dim(dt, return.all = FALSE)),
              is_a("numeric"))
})
