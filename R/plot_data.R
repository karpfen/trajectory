#' Generates a polar plot showing the linear directional mean and trajectory
#' length.
#'
#' @param traj \code{sfc} containing trajectories.
#'
#' @param x The parameter displayed on the x axis.
#'
#' @param y The parameter displayed on the y axis.
#'
#' @param size The attribute represented by dot size or a fixed value.
#'
#' @export
plot_ldm <- function (traj, x, y, size)
{
    if (!x %in% names (traj))
        stop_err (x)

    if (!y %in% names (traj))
        stop_err (y)

    if (!size %in% names (traj))
        stop_err (size)

    traj <- traj [traj$ldm != 0, ]
    traj$classes <-  with (traj,
                              cut (traj [[x]], breaks =
                                   quantile (traj [[x]],
                                             probs = seq (0, 1, by = 0.25)),
                                   include.lowest = TRUE))

    ggplot2::ggplot (data = traj, ggplot2::aes (x = traj [[x]], y = traj [[y]],
                                                colour = traj$classes)) +
        ggplot2::geom_point (ggplot2::aes (size = traj [[size]])) +
        ggplot2::coord_polar (start = -pi / 2, direction = -1) +
        ggplot2::scale_x_continuous (limits = c (0, 360), breaks = 0:7 * 45,
                                     name = x) +
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
