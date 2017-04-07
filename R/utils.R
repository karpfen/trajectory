# Reads a csv containing database information and returns them as a data.frame
readCredentials <- function (fName)
{
    dat <- read.csv2 (fName, header=FALSE)
    dat <- as.data.frame (t (dat))
    names (dat) <- as.matrix (head (dat, 1))
    dat <- dat [-1, ]
    dat <- data.frame (lapply (dat, as.character), stringsAsFactors=FALSE)
    dat$port <- as.integer (dat$port)
    dat
}
