#' Generates a polar plot showing the linear directional mean and trajectory
#' length.
#'
#' @param traj \code{sfc} containing trajectories.
#'
#' @export
plot_ldm <- function (traj)
{
    traj <- traj [traj$ldm != 0, ]
    traj$classes <-  with (traj,
                              cut (traj$ldm, breaks =
                                   quantile (traj$ldm, probs = seq (0, 1,
                                                                    by = 0.25)),
                                   include.lowest = TRUE))

    ggplot2::ggplot (data = traj, ggplot2::aes (y = traj$trajectory_length,
                                                   x = traj$ldm,
                                                   colour = traj$classes)) +
        ggplot2::geom_point (ggplot2::aes (size = traj$number_of_points)) +
        ggplot2::coord_polar (start =  -pi / 2) +
        ggplot2::theme_bw () +
        ggplot2::scale_y_continuous (trans = "log") 
        ggplot2::scale_x_continuous (limits = c (-180, 180))
}
