context("Binning vectors")
v1 = 1:300
v2 = -30:300

test_that("Number of bins is correct", {
  # With only positive bins
  expect_equal(nlevels(bin_equallyspaced_vector(v1, 1)), length(v1) - 1)
  expect_equal(nlevels(bin_equallyspaced_vector(v1, length(v1))), 1)

  # Including negative bins
  expect_equal(nlevels(bin_equallyspaced_vector(v2, 1)), length(v2) - 1)
  expect_equal(nlevels(bin_equallyspaced_vector(v2, length(v2))), 2)
})

test_that("fails when input is not a vector", {

})

context("Binning data frame on column")

testdata = as.data.frame(as.numeric(treering[1:300]))
testdata$time = 1:nrow(testdata)
colnames(testdata)[1] = "treering"

test_that("Binning data frame on column works", {
  bins = bin_equallyspaced_df(df = testdata, by = "time", bin.size = 1)
  test_that(length(bin_equallyspaced_vector(1:300, 1)), equals(300))
})

