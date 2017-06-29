#' Read Database Credentials
#'
#' Reads a csv containing database information and returns them as a data.frame.
#'
#' @param f_name Filename of the csv file containing the credentials
#'
#' @return A \code{data.frame} containing the credentials
read_credentials <- function (f_name)
{
    dat <- utils::read.csv2 (f_name, header = FALSE)
    dat <- as.data.frame (t (dat))
    names (dat) <- as.matrix (utils::head (dat, 1))
    dat <- dat [-1, ]
    dat <- data.frame (lapply (dat, as.character), stringsAsFactors = FALSE)
    dat$port <- as.integer (dat$port)
    dat
}

#' Filter points that are parts of trajectories
#'
#' Returns only the points that share a specified attribute with trajectories.
#'
#' @param traj \code{sf} object containing the trajectories.
#'
#' @param pts \code{sf} object containing points.
#'
#' @param join_by shared attribute by which to filter.
#'
#' @return A \code{sf} object containing the filtered points.
filter_points <- function (traj, pts, join_by)
{
    join_el <- pts [[join_by]]
    keep <- vector (length = length (join_el), mode = "logical")
    for (i in seq_along (join_el))
        keep [i] <- join_el [i] %in% traj [[join_by]]
    pts [keep, ]
}

#' Filter points based on attribute values in a list
#'
#' Returns only the points that are not listed
#'
#' @param pts \code{sf} object containing points.
#'
#' @param excludelist text file containing attribute name and values
#'
#' @return A \code{sf} object containing the filtered points.
#'
#' @export
exclude_points <- function (pts, excludelist)
{
    rmv <- vector (length = dim (pts) [1], mode = "logical")
    excludedat <- utils::read.table (excludelist, header = TRUE)
    exclude_by <- names (excludedat)
    for (ex_by in exclude_by)
    {
        dat <- pts [[ex_by]]
        if (!is.null (dat))
        {
            for (i in seq_along (dat))
            {
                ex_vec <- as.vector (excludedat [[ex_by]])
                for (ex in seq_along (ex_vec))
                {
                    if (!rmv [i])
                        rmv [i] <- dat [i] == ex_vec [ex]
                }
            }
        }
    }
    pts [!rmv, ]
}
