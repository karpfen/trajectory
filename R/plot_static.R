#' Generates a polar plot showing the linear directional mean of all
#' trajectories.
#'
#' @param traj \code{sfc} containing trajectories and a ldm attribute.
#'
#' @param y The parameter displayed on the y axis.
#'
#' @param size The attribute defining the point size in the plot.
#'
#' @export
plot_ldm <- function (traj, y, size)
{
    if (!"ldm" %in% names (traj))
        stop_err ("ldm")
    if (!y %in% names (traj))
        stop_err (y)
    if (!size %in% names (traj))
        stop_err (size)

    traj <- traj [traj$ldm != 0, ]
    traj$classes <-  with (traj,
                              cut (traj$ldm, breaks =
                                   quantile (traj$ldm,
                                             probs = seq (0, 1, by = 0.25)),
                                   include.lowest = TRUE))

    ggplot2::ggplot (data = traj, ggplot2::aes (x = traj$ldm, y = traj [[y]],
                                                colour = traj$classes)) +
        ggplot2::geom_point (ggplot2::aes (size = traj [[size]])) +
        ggplot2::coord_polar (start = -pi / 2, direction = -1) +
        ggplot2::scale_x_continuous (limits = c (0, 360), breaks = 0:7 * 45,
                                     name = "ldm") +
        ggplot2::scale_y_continuous (trans = "log", name = y) +
        ggplot2::theme_bw () +
        ggplot2::theme (legend.position = "none")
}

#' Stops the program and generates an error message for the given attribute.
#'
#' @noRd
stop_err <- function (attribute)
{
    stop (paste0 ("Attribute '", attribute,
                  "' is not present in the trajectory data."))
}

shiftx180 <- function (x)
{
    while (x > 180)
        x <- x - 180
    while (x < -180)
        x <- x + 180

    return (x)
}

shifty90 <- function (y)
{
    while (y > 90)
        y <- y - 90
    while (y < -90)
        y <- y + 90

    return (y)
}

#' Generates a static map of the given set of trajectories.
#'
#' @param traj \code{sfc} containing trajectories and a ldm attribute.
#'
#' @param buffer Buffer of area to include around the trajectories.
#'
#' @export
plot_static <- function (traj, buffer = 0)
{
    bx <- sf::st_bbox (traj)
    bbx <- vector (mode = "numeric", length = 4)
    bbx [1] <- bx ["xmin"]
    bbx [2] <- bx ["xmax"]
    bbx [3] <- bx ["ymin"]
    bbx [4] <- bx ["ymax"]

    buffer <- max (buffer, 0)
    buffer <- buffer / 100
    dx <- abs (bbx [1] - bbx [2]) * buffer
    dy <- abs (bbx [3] - bbx [4]) * buffer

    bbx [1] <- shiftx180 (min (bbx [1], bbx [2]) - dx)
    bbx [2] <- shiftx180 (max (bbx [1], bbx [2]) + dx)
    bbx [3] <- shifty90 (min (bbx [3], bbx [4]) - dy)
    bbx [4] <- shifty90 (max (bbx [3], bbx [4]) + dy)

    tmap::tm_shape (World, bbox = bbx, projection = 4326, is.master = TRUE) +
    tmap::tm_fill () +
    tmap::tm_borders("grey20") +
    tmap::tm_grid () +
    tmap::tm_text ("name") +
    tmap::tm_shape (metro) +
    tmap::tm_symbols () +
    tmap::tm_text ("name") +
    tmap::tm_shape (traj) +
    tmap::tm_lines ()
}
