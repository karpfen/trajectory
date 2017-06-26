test_that ("read_sqlite", {
               f_name <- "../sample_db.sqlite"
               tbl <- "sampletable"
               dat <- read_sqlite (f_name, tbl)
               testthat::expect_type (dat, "list")
               f_name <- "../sample_db_spatial.sqlite"
               dat <- read_sqlite (f_name, tbl, TRUE)
               testthat::expect_type (dat, "list")
               dat <- read_sqlite (f_name, tbl, FALSE)
               testthat::expect_type (dat, "list")
               testthat::expect_error (read_sqlite (f_name, "nonexistent",
                                                    FALSE))
               testthat::expect_error (read_sqlite ("nonexistent", "", FALSE))
})

test_that ("postgres2sqlite", {
               cred <- "../test_creds.csv"
               tbl <- "error"
               f_name <- "test.sqlite"
               testthat::expect_error (postgres2sqlite (cred, tbl, f_name))
})

test_that ("postgres2gpkg", {
               cred <- "../test_creds.csv"
               tbl <- "error"
               f_name <- "test.sqlite"
               testthat::expect_error (postgres2gpkg (cred, tbl, f_name))
})
