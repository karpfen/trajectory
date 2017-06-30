test_that ("plot_ldm", {
               f_name <- "../sample_db_spatial.sqlite"
               tbl <- "sampletable"
               pts <- read_sqlite (f_name, tbl)
               traj <- make_trajectories (pts, "speed", "dist", 2)
               traj$ldm <- runif (dim (traj) [1])
               pl <- plot_ldm (traj, "ldm", "ldm", "ldm")
               testthat::expect_type (pl, "list")
               e_msg <- "Attribute 'err' is not present in the trajectory data."
               testthat::expect_error (plot_ldm (traj, "err", "ldm", "ldm"),
                                       e_msg)
               testthat::expect_error (plot_ldm (traj, "ldm", "err", "ldm"),
                                       e_msg)
               testthat::expect_error (plot_ldm (traj, "ldm", "ldm", "err"),
                                       e_msg)

})
