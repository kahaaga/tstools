context("Convergent cross mapping (CCM) over library sizes")

dt <- chaoticmaps::henon_map(add.timestep = T)
dt$t = as.numeric(dt$t)
#
# test_that("ccm_over_library_sizes without parallelisation",
#   suppressWarnings(ccm_over_library_sizes(lag = 0,
#                                  data = dt[1:100, 2:3],
#                                  samples.original = 30,
#                                  library.sizes = 100,
#                                  parallel = F))
# )
#
# test_that("ccm over multiple library sizes", {
#   suppressWarnings(
#     ccm_over_library_sizes(lag = 0,
#                            data = dt[1:100, 2:3],
#                            samples.original = 30,
#                            library.sizes = 30,
#                            parallel = F))
#   }
# )
#
# test_that("convergence check works", {
#   ccm <- suppressWarnings(
#     ccm_over_library_sizes(lag = 0,
#                                 data = dt[1:100, 2:3],
#                                 samples.original = 30,
#                                 library.sizes = 70,
#                                 parallel = T))
#   get_convergence_parameters(ccm.result = ccm)
# }
# )
