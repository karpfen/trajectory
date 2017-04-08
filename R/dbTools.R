source ("R/utils.R")

#' Fetches data from a PostgreSQL table and writes it to a local SQLite file
#'
#' @param credentialFile A csv file containing the database credentials
#' @param tblName Name of the table to be downloaded
#' @param outFileName Name of the output file
#' @param ask if \code{TRUE}, ask user for confirmation after showing the table
#' size
#'
#' @export
postgres2sqlite <- function (credentialFile, tblName, outFileName, ask=TRUE)
{
    dat <- getPostgreSQLtbl (credentialFile, tblName, outFileName, ask)
    drvSQLite <- DBI::dbDriver ("SQLite")
    conSqlite <- RSQLite::dbConnect (drv = drvSQLite,
                                     dbname = outFileName)
    RSQLite::dbWriteTable (conSqlite, tblName, dat)
    RSQLite::dbDisconnect (conSqlite)
}

#' Fetches data from a PostGIS table and writes it to a local Geopackage file
#'
#' Rows without geometries will be omitted.
#'
#' @param credentialFile A csv file containing the database credentials
#' @param tblName Name of the table to be downloaded
#' @param outFileName Name of the output file
#' @param ask if \code{TRUE}, ask user for confirmation after showing the table
#' size
#'
#' @export
postgres2gpkg <- function (credentialFile, tblName, outFileName, ask=TRUE)
{
    if (!endsWith (tolower (outFileName), ".gpkg"))
        outFileName <- paste0 (outFileName, ".gpkg")
    dat <- getPostgreSQLtbl (credentialFile, tblName, outFileName, ask)
    dat <- dat [!is.na (dat$lat), ]
    pts <- list ("POINT", dim (dat) [1])
    for (i in 1:dim (dat) [1])
    {
        ln <- dat$lon [i]
        lt <- dat$lat [i]
        pts [[i]] <- sf::st_point (c (ln, lt), "XY")
    }
    sfc <- sf::st_sfc (pts, crs = 4326)
    dat$lat <- NULL
    dat$lon <- NULL
    pts_out <- sf::st_sf (sfc, dat)
    sf::st_write (pts_out, outFileName) 
}

#' Fetches data from a PostgreSQL table returns it as a data.frame
#'
#' @param credentialFile A csv file containing the database credentials
#' @param tblName Name of the table to be downloaded
#' @param outFileName Name of the output file
#' @param ask if \code{TRUE}, ask user for confirmation after showing the table
#' size
getPostgreSQLtbl <- function (credentialFile, tblName, outFileName, ask=TRUE)
{
    cred <- readCredentials (credentialFile)
    drv <- DBI::dbDriver ("PostgreSQL")
    con <- RPostgreSQL::dbConnect (drv, user = cred$user,
                                   password = cred$password, host = cred$host,
                                   port = cred$port, dbname = cred$dbname)

    if (RPostgreSQL::dbExistsTable (con, tblName))
    {
        print (paste0 ("Found table ", tblName, " on ", cred$host, "/",
                       cred$dbname, "."))
        sql <- paste ("SELECT count (*) FROM", tblName, ";")
        size <- RPostgreSQL::dbGetQuery (con, sql)
        if (ask)
        {
            txt <- paste0 ("Number of rows: ", size,
                           ". Proceed Downloading? (y) ")
            res <- readline (txt)
            proceed <- tolower (res) == "y" || res == ""
        } else
        {
            print (paste0 ("Number of rows: ", size, "."))
            proceed <- TRUE
        }
        if (proceed)
        {
            print ("Downloading data...")
            sql <- paste0 ("SELECT column_name, data_type FROM 
                           information_schema.columns WHERE 
                           table_name = '", tblName, "';")
                           rset <- RPostgreSQL::dbGetQuery (con, sql)
                           rset <- rset [rset$data_type != "USER-DEFINED", ]
                           cols <- paste (rset$column_name, collapse = ",")
                           sql <- paste ("SELECT", cols, ", st_y (geom_org) as lat,",
                                         "st_x (geom_org) as lon FROM", tblName, ";")
                           rset <- RPostgreSQL::dbGetQuery (con, sql)
        }
    }
    RPostgreSQL::dbDisconnect (con)
    rset
}

#' Read SQLite file and return a data.frame
#'
#' @param fName Name of the SQLite file
#' @param tblName Name of the table to be fetched
#' @param sf if \code{TRUE}, returns a \code{sf} object containing only data
#' with valid coordinates in fields lat and lon
#'
#' @return A \code{data.frame} containing the entire table
#'
#' @export
readSQLite <- function (fName, tblName, sf = TRUE)
{
    drvSQLite <- RSQLite::dbDriver ("SQLite")
    conSqlite <- RSQLite::dbConnect (drv = drvSQLite, dbname = fName)
    tbls <- RSQLite::dbListTables (conSqlite)
    if (tblName %in% tbls)
        dat <- RSQLite::dbReadTable (conSqlite, tblName)
    else
    {
        tbls <- paste (tbls, collapse = ", ")
        msg <- paste0 ("Database ", fName, " does not contain table '", tblName,
                       "'. Available tables are: ", tbls)
        stop (msg)
    }
    RSQLite::dbDisconnect (conSqlite)
    if (sf)
    {
        if (!all (c ("lat", "lon") %in% names (dat)))
        {
            print ("SQLite file does not contain fields lat and long. Returning
                   regular data.frame.")
                   return (dat)
        }
        dat <- dat [!is.na (dat$lat), ]
        pts <- list ("POINT", dim (dat) [1])
        for (i in 1:dim (dat) [1])
        {
            ln <- dat$lon [i]
            lt <- dat$lat [i]
            pts [[i]] <- sf::st_point (c (ln, lt), "XY")
        }
        sfc <- sf::st_sfc (pts, crs = 4326)
        dat$lat <- NULL
        dat$lon <- NULL
        pts_out <- sf::st_sf (sfc, dat)
        pts_out
    }
    else
        dat
}
