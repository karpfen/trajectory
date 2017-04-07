#' Creates a set of trajectories from a sfc of points
#'
#' @param \code{sf} object containing points
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
    traj <- list ("LINESTRING", length (feats))
    for (i in 1:length (feats))
    {
        feature <- feats [i]
        traj_pts <- pts [pts [[foi]] == feature, ]
        traj_pts <- traj_pts [order (traj_pts [[orderBy]]), ]
        coords <- sf::st_coordinates (traj_pts)
        traj [[i]] <- sf::st_linestring (coords)
    }
    traj
}
