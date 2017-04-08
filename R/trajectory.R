#' Creates a set of trajectories from a sfc of points
#'
#' @param pts \code{sf} object containing points
#' @param foi Feature of interest by which the different trajectories are
#' differentiated
#' @param orderBy Field by which points are ordered
#' @param n minimum number of points per trajectory
#' @param join if \code{TRUE}, the resulting \code{sf} object contains both
#' trajectories and their related points, if \code{FALSE}, only the trajectories
#'
#' @return A \code{sf} object containing the trajectories
#'
#' @export
makeTrajectories <- function (pts, foi, orderBy, n, join=TRUE)
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
    if (join)
    {
        missingCols <- names (pts)
        missingCols <- missingCols [! missingCols %in% names (traj)]
        invisible (lapply (missingCols, FUN = function (x) traj[[x]] <<- NA))
        traj <- rbind (pts, traj)
    }
    traj
}
