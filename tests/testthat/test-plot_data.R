test_that ("plot_data", {
               fName <- "../sample_db_spatial.sqlite"
               tbl <- "sampletable"
               pts <- read_sqlite (fName, tbl)
               trj <- make_trajectories (pts, "speed", "dist", 1)
               testthat::expect_error (plot_map (pts, pts),
                   "traj must contain geometries of type LINESTRING.")
               testthat::expect_error (plot_map (trj, trj),
                   "pts must contain geometries of type POINT.")
})

test_that ("popup", {
               dat <- list ("a")
               dat ["a"] <- "a"
               txt <- popup ("a", "a", dat)
               testthat::expect_is (txt, "character")
})
