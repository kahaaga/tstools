context("Boxplot statistics test.")

# Get a series to work with
s = as.numeric(datasets::BJsales.lead)

# Generate some data.
test_that("boxplot min is larger than minimum of series", {
  expect_true(min(s) < boxplot_minimum(s))
})
