test_that ("plot_ldm", {
               f_name <- "../sample_db_spatial.sqlite"
               tbl <- "sampletable"
               pts <- read_sqlite (f_name, tbl)
               traj <- make_trajectories (pts, "speed", "dist", 2)
               traj$ldm <- runif (dim (traj) [1])
               pl <- plot_ldm (traj)
               testthat::expect_type (pl, "list")
})
