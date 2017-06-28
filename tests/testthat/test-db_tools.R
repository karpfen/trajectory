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
