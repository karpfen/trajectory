#' Create checkpoint geometries
#'
#' @param pts \code{sf} object containing points.
#' include.
#' @param x \code{vector} of x coordinates of the checkpoint center.
#' @param y \code{vector} of y coordinates of the checkpoint center.
#' @param size Radius of the checkpoints.
#'
#' @return \code{sf} object containing the input points along with their closest
#' checkpoint number.
#'
#' @export
get_checkpoints <- function (pts, x, y, size)
{
    cp_coords <- list (size = length (x))
    for (i in seq_along (x))
        cp_coords [[i]] <- sf::st_point (c (x [i], y [i]), dim = "XY")
    cp_coords <- sf::st_sfc (cp_coords, crs = 4326)

    ln <- dim (pts) [1]
    nearest_checkpoint <- vector (mode = "numeric", length = ln)
    for (i in seq_len (ln))
    {
        pnt <- pts [i, ]
        pt_dist <- sf::st_distance (pnt, cp_coords)
        pt_dist_min <- min (pt_dist)
        nn <- which (pt_dist == pt_dist_min)
        nearest_checkpoint [i] <- NA
        if (pt_dist_min %>% as.numeric < size)
            nearest_checkpoint [[i]] <- nn
    }
    cbind (pts, nearest_checkpoint)
}
