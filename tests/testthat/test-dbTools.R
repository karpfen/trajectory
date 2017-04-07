test_that ("readSQLite", {
               fName <- "../sampleDB.sqlite"
               tbl <- "sampletable"
               dat <- readSQLite (fName, tbl)
               testthat::expect_type (dat, "list")
               fName <- "../sampleDBspatial.sqlite"
               dat <- readSQLite (fName, tbl, TRUE)
               testthat::expect_type (dat, "list")
               dat <- readSQLite (fName, tbl, FALSE)
               testthat::expect_type (dat, "list")
               testthat::expect_error (readSQLite (fName, "nonexistent", FALSE))
})
