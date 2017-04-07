#' Read Database Credentials
#'
#' Reads a csv containing database information and returns them as a data.frame.
#'
#' @param fName Filename of the csv file containing the credentials
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
