context("Helper functions tests")

test_that("contains_na() behaves as expected", {
  expect_false(contains_na(c(1, 2, 4, 5)))
  expect_false(contains_na(c(1, "a", 4, 5)))
  expect_false(contains_na(c(NULL, 2, "c")))
  expect_warning(expect_false(contains_na(NULL)))
})
