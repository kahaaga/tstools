context("Boxplot statistics test.")

# Get a series to work with
s <- as.numeric(datasets::BJsales.lead)

test_that("boxplot_minimum behaves correctly", {
  expect_true(boxplot_minimum(s) < mean(s))
  expect_true(boxplot_minimum(s) < median(s))
})

test_that("boxplot_maximum behaves correctly", {
  expect_true(boxplot_maximum(s) > mean(s))
  expect_true(boxplot_maximum(s) > median(s))
})
