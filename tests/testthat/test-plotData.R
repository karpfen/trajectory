test_that ("plotData", {
               fName <- "../sampleDBspatial.sqlite"
               tbl <- "sampletable"
               pts <- readSQLite (fName, tbl)
               trj <- makeTrajectories (pts, "speed", "dist", 1)
               testthat::expect_error (plotLeaflet (pts, pts),
                   "traj must contain geometries of type LINESTRING.")
               testthat::expect_error (plotLeaflet (trj, trj),
                   "pts must contain geometries of type POINT.")
})

test_that ("popup", {
               dat <- list ("a")
               dat ["a"] <- "a"
               txt <- popup ("a", "a", dat)
               testthat::expect_is (txt, "character")
})
