context("Bin")

# Some data with an age column we want to bin
#
dt <- chaoticmaps::henon_map(add.timestep = T)
dt$t = as.numeric(dt$t)

# Add some NA values
dt[54, 2] = NA
dt[43, 3] = NA
dt[80:86, 3] = NA

test_that("bin -> interpolate -> remove remaining na rows", {
  bin(dt = dt, bin.size = 2, by = "t", interpolate = T, remove.na = T,
      add.binning.info = T)
})

test_that("time sampled at end points", {
  bin(dt = dt, bin.size = 2, by = "t", interpolate = T, remove.na = T,
      add.binning.info = T, time.sampled.at = "end")
})

test_that(paste("bin -> interpolate ->",
                "don't remove NA rows remaining after interpolation"), {
  bin(dt = dt, bin.size = 2, by = "t", interpolate = T, remove.na = F)
})

test_that("remove.na without interpolation gives warning",
  expect_warning(
    bin(dt = dt, bin.size = 2, by = "t", interpolate = F, remove.na = T)
  )
)
