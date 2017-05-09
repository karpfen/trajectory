test_that ("plotData", {
               fName <- "../sampleDBspatial.sqlite"
               tbl <- "sampletable"
               pts <- readSQLite (fName, tbl)
               trj <- makeTrajectories (pts, "speed", "dist", 1)
               testthat::expect_error (plotMap (pts, pts),
                   "traj must contain geometries of type LINESTRING.")
               testthat::expect_error (plotMap (trj, trj),
                   "pts must contain geometries of type POINT.")
})
