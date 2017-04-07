test_that ("readSQLite", {
               fName <- "../sampleDB.sqlite"
               tbl <- "sampletable"
               dat <- readSQLite (fName, tbl)
               testthat::expect_type (dat, "list")
})
