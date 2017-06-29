test_that ("read_credentials", {
               cred <- read_credentials ("../test_creds.csv")
               testthat::expect_type (cred, "list")
})

test_that ("filter_points", {
               f_name <- "../sample_db_spatial.sqlite"
               tbl <- "sampletable"
               pts <- read_sqlite (f_name, tbl)
               traj <- make_trajectories (pts, "speed", "dist", 2)
               filtered <- filter_points (traj, pts, "speed")
               testthat::expect_type (filtered, "list")
})

test_that ("exclude_points", {
               f_name <- "../sample_db_spatial.sqlite"
               ex_list <- "../excludelist.csv"
               tbl <- "sampletable"
               pts <- read_sqlite (f_name, tbl)
               ex <- exclude_points (pts, ex_list)
               testthat::expect_type (ex, "list")
})
