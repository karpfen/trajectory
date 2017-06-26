test_that ("make_trajectories", {
               fName <- "../sample_db_spatial.sqlite"
               tbl <- "sampletable"
               dat <- read_sqlite (fName, tbl)
               trj <- make_trajectories (dat, "speed", "dist", 1)
               testthat::expect_type (trj, "list")
})
