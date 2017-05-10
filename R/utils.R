#' Read Database Credentials
#'
#' Reads a csv containing database information and returns them as a data.frame.
#'
#' @param fName Filename of the csv file containing the credentials
#'
#' @return A \code{data.frame} containing the credentials
readCredentials <- function (fName)
{
    dat <- utils::read.csv2 (fName, header=FALSE)
    dat <- as.data.frame (t (dat))
    names (dat) <- as.matrix (utils::head (dat, 1))
    dat <- dat [-1, ]
    dat <- data.frame (lapply (dat, as.character), stringsAsFactors=FALSE)
    dat$port <- as.integer (dat$port)
    dat
}

#' Filter points that are parts of trajectories
#'
#' Returns only the points that share a specified attribute with trajectories.
#'
#' @param traj \code{sf} object containing the trajectories.
#' @param pts \code{sf} object containing points.
#' @param joinBy shared attribute by which to filter.
#'
#' @return A \code{sf} object containing the filtered points.
filterPoints <- function (traj, pts, joinBy)
{
    joinEl <- pts [[joinBy]]
    keep <- vector (length = length (joinEl), mode = "logical")
    for (i in seq_along (joinEl))
        keep [i] <- joinEl [i] %in% traj [[joinBy]]
    pts [keep, ]
}

#' Filter points based on attribute values in a list
#'
#' Returns only the points that are not listed
#'
#' @param pts \code{sf} object containing points.
#' @param excludelist text file containing attribute name and values
#'
#' @return A \code{sf} object containing the filtered points.
excludePoints <- function (pts, excludelist)
{
    rmv <- vector (length = dim (pts) [1], mode = "logical")
    excludedat <- utils::read.table (excludelist, header = TRUE)
    excludeBy <- names (excludedat)
    for (exBy in excludeBy)
    {
        dat <- pts [[exBy]]
        if (!is.null (dat))
        {
            for (i in seq_along (dat))
            {
                exVec <- as.vector (excludedat [[exBy]])
                for (ex in seq_along (exVec))
                {
                    if (!rmv [i])
                        rmv [i] <- dat [i] == exVec [ex]
                }
            }
        }
    }
    pts [!rmv, ]
}
