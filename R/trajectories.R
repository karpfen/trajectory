#' Creates a set of trajectories from a sfc of points
#'
#' @param pts \code{sf} object containing points.
#'
#' @param foi Feature of interest by which the different trajectories are
#' differentiated.
#'
#' @param order_by Field by which points are ordered.
#'
#' @param n minimum number of points per trajectory.
#'
#' @return A \code{sf} object containing the trajectories.
#'
#' @export
make_trajectories <- function (pts, foi, order_by, n)
{
    col_names <- names (pts)
    if (!foi %in% col_names)
        stop ("Specified feature of interest is not present in the point data.")
    if (!order_by %in% col_names)
        stop ("Specified feature to order by is not present in the point data.")

    pts <- pts [pts [[foi]] %in% names (which (table (pts [[foi]]) > n)), ]
    feats <- unique (pts [[foi]])
    sfc <- list ("LINESTRING", length (feats))
    coord_list <- vector (mode = "list", length = length (feats))
    for (i in seq_along (feats))
    {
        feature <- feats [i]
        traj_pts <- pts [pts [[foi]] == feature, ]
        traj_pts <- traj_pts [order (traj_pts [[order_by]]), ]
        coords <- sf::st_coordinates (traj_pts)
        coord_list [[i]] <- coords
        sfc [[i]] <- sf::st_linestring (coords)
    }
    ldm <- rcpp_ldm (coord_list)
    sfc <- sf::st_sfc (sfc, crs = 4326)
    feats <- data.frame (feats)
    names (feats) <- foi
    traj <- sf::st_sf (sfc, feats)
    traj <- make_movement_indices (traj)
    traj <- cbind (traj, ldm)
    traj [traj$trajectory_length > 0, ]
}

#' Calculates a number of movement indices for each trajectory
#'
#' Calculates average distance travelled per point and great circle distance
#' covered between start and end point for each trajectory in metres.
#'
#' @param traj \code{sf} object containing the trajectories.
#'
#' @return A \code{sf} object containing the trajectories with additional
#' fields.
make_movement_indices <- function (traj)
{
    len <- dim (traj) [1]
    trajectory_length <- vector (length = len, mode = "numeric")
    length_start_end <- vector (length = len, mode = "numeric")
    distance_per_point <- vector (length = len, mode = "numeric")
    number_of_points <- vector (length = len, mode = "numeric")
    for (i in seq_len (len))
    {
        trj <- traj [i, ]
        geom <- sf::st_coordinates (trj)
        st <- geom %>% head (1) %>% magrittr::extract (c (2, 1))
        en <- geom %>% tail (1) %>% magrittr::extract (c (2, 1))
        length_start_end [i] <- geosphere::distVincentyEllipsoid (st, en)
        trajectory_length [i] <- sf::st_length (trj)
        num_pts <- dim (geom) [1]
        number_of_points [i] <- num_pts
        distances <- vector (length = num_pts - 1, mode = "numeric")
        for (j in seq_along (geom [-1, 1]))
        {
            st <- geom [j, ] %>% magrittr::extract (c (2, 1))
            en <- geom [j + 1, ] %>% magrittr::extract (c (2, 1))
            distances [j] <- geosphere::distVincentyEllipsoid (st, en)
        }
        distance_per_point [i] <- mean (distances)
    }
    cbind (traj, trajectory_length, length_start_end, distance_per_point,
           number_of_points)
}

#' Calculates the linear directional mean of all trajectories combined
#'
#' @param traj \code{sf} object containing trajectories.
#'
#' @return the linear directional mean in degrees over all given trajectories.
#'
#' @export
get_overall_ldm <- function (traj)
{
    coords <- sf::st_coordinates (traj) [, 1:2] %>% matrix (ncol = 2) %>% list
    rcpp_ldm (coords)
}
