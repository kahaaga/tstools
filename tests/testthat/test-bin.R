test_that("binning dataframes works", {
    # Use treering data

    time = as.numeric(start(treering)[1]:end(treering)[1])
    treering.data = as.numeric(unclass(treering))
    dt = data.frame(time, treering.data)
    colnames(dt) = c("time", "treering")

})
