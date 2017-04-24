#' Creates a set of trajectories from a sfc of points
#'
#' @param pts \code{sf} object containing points
#' @param foi Feature of interest by which the different trajectories are
#' differentiated
#' @param orderBy Field by which points are ordered
#' @param n minimum number of points per trajectory
#'
#' @return A \code{sf} object containing the trajectories
#'
#' @export
makeTrajectories <- function (pts, foi, orderBy, n)
{
    pts <- pts [pts [[foi]] %in% names (which (table (pts [[foi]]) > n)), ]
    feats <- unique (pts [[foi]])
    sfc <- list ("LINESTRING", length (feats))
    for (i in seq_along (feats))
    {
        feature <- feats [i]
        traj_pts <- pts [pts [[foi]] == feature, ]
        traj_pts <- traj_pts [order (traj_pts [[orderBy]]), ]
        coords <- sf::st_coordinates (traj_pts)
        sfc [[i]] <- sf::st_linestring (coords)
    }
    sfc <- sf::st_sfc (sfc, crs = 4326)
    feats <- data.frame (feats)
    names (feats) <- foi
    traj <- sf::st_sf (sfc, feats)
    traj <- makeMovementIndices (traj)
    traj
}

#' Calculates a number of movement indices for each trajectory
#'
#' Calculates average distance travelled per point and great circle distance
#' covered between start and end point for each trajectory in metres
#'
#' @param traj \code{sf} object containing the trajectories
#'
#' @return A \code{sf} object containing the trajectories with additional fields
makeMovementIndices <- function (traj)
{
    len <- dim (traj) [1]
    trajectory_length <- vector (length = len, mode = "numeric")
    length_start_end <- vector (length = len, mode = "numeric")
    distance_per_point <- vector (length = len, mode = "numeric")
    for (i in seq_along (traj$sfc))
    {
        trj <- traj [i,]
        geom <- sf::st_coordinates (trj)
        st <- geom %>% head (1) %>% magrittr::extract (c (2, 1))
        en <- geom %>% tail (1) %>% magrittr::extract (c (2, 1))
        length_start_end [i] <- geosphere::distVincentyEllipsoid (st, en)
        trajectory_length [i] <- sf::st_length (trj)
        distances <- vector (length = dim (geom) [1] - 1, mode = "numeric")
        for (j in seq_along (geom [-1,1]))
        {
            st <- geom [j,] %>% magrittr::extract (c (2, 1))
            en <- geom [j + 1,] %>% magrittr::extract (c (2, 1))
            distances [j] <- geosphere::distVincentyEllipsoid (st, en)
        }
        distance_per_point [i] <- mean (distances)
    }
    dat <- data.frame (trajectory_length, length_start_end, distance_per_point)
    traj <- sf::st_bind_cols (traj, dat)
    traj
}
