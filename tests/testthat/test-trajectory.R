test_that ("makeTrajectories", {
               fName <- "../sampleDBspatial.sqlite"
               tbl <- "sampletable"
               dat <- readSQLite (fName, tbl)
               trj <- makeTrajectories (dat, "speed", "dist", 1)
               testthat::expect_type (trj, "list")
})
