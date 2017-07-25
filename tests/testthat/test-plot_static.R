test_that ("plot_ldm", {
               f_name <- "../sample_db_spatial.sqlite"
               tbl <- "sampletable"
               pts <- read_sqlite (f_name, tbl)
               traj <- make_trajectories (pts, "speed", "dist", 2)
               traj$ldm <- runif (dim (traj) [1])
               pl <- plot_ldm (traj, "ldm", "ldm")
               testthat::expect_type (pl, "list")
               e_msg <- "Attribute 'err' is not present in the trajectory data."
               testthat::expect_error (plot_ldm (traj, "err", "ldm"),
                                       e_msg)
               testthat::expect_error (plot_ldm (traj, "ldm", "err"),
                                       e_msg)
               traj$ldm <- NULL
               e_msg <- "Attribute 'ldm' is not present in the trajectory data."
               testthat::expect_error (plot_ldm (traj, "dist", "dist"),
                                       e_msg)

})

test_that ("test_shifty90", {
    x <- 100
    x_shift <- shifty90 (x)
    testthat::expect_equal (x_shift, 10)
    x <- -100
    x_shift <- shifty90 (x)
    testthat::expect_equal (x_shift, -10)
})

test_that ("test_shifty180", {
    x <- 200
    x_shift <- shiftx180 (x)
    testthat::expect_equal (x_shift, 20)
    x <- -200
    x_shift <- shiftx180 (x)
    testthat::expect_equal (x_shift, -20)
})
