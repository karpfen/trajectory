test_that ("make_trajectories", {
               f_name <- "../sample_db_spatial.sqlite"
               tbl <- "sampletable"
               dat <- read_sqlite (f_name, tbl)
               err_msg_ord <- paste ("Specified feature to order by is not",
                                   "present in the point data.")
               err_msg_foi <- paste ("Specified feature of interest is not",
                                     "present in the point data.")

               testthat::expect_error (make_trajectories (dat, "speed",
                                                          "error"), err_msg_ord)
               testthat::expect_error (make_trajectories (dat, "error",
                                                          "dist"), err_msg_foi)
               trj <- make_trajectories (dat, "speed", "dist", 2)
               testthat::expect_type (trj, "list")
})

test_that ("overall_ldm", {
               f_name <- "../sample_db_spatial.sqlite"
               tbl <- "sampletable"
               dat <- read_sqlite (f_name, tbl)
               trj <- make_trajectories (dat, "speed", "dist", 2)
               ldm <- get_overall_ldm (trj)
               testthat::expect_equal (ldm, 270)
})
