#' set_map_data_trajectories
#'
#' Set trajectory data to be used by plotting functions.
#'
#' @param data_traj \code{sf} object containing trajectory data.
#'
#' @noRd
set_map_data_trajectories <- function (data_traj)
{
    op <- options ()
    op.map_data <- list (map_data.trajectories = data_traj)
    options (op.map_data)
}

#' set_map_data_points
#'
#' Set point data to be used by plotting functions.
#'
#' @param data_points \code{sf} object containing trajectory data.
#'
#' @noRd
set_map_data_points <- function (data_points)
{
    op <- options ()
    op.map_data <- list (map_data.points = data_points)
    options (op.map_data)
}

#' get_map_data_trajectories
#'
#' Get trajectory data to be used by plotting functions.
#'
#' @noRd
get_map_data_trajectories <- function ()
{
}

#' get_map_data_points
#'
#' Get point data to be used by plotting functions.
#'
#' @noRd
get_map_data_points <- function ()
{
}
