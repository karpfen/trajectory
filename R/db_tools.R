#' Fetches data from a PostgreSQL table and writes it to a local SQLite file
#'
#' @param credential_file A csv file containing the database credentials
#'
#' @param tbl_name Name of the table to be downloaded
#'
#' @param out_file_name Name of the output file
#'
#' @param bbx Optional bounding box. A numeric \code{vector} of length 4 with
#' \code{xmin}, \code{ymin}, \code{xmax} and \code{ymax}.
#'
#' @param ask if \code{TRUE}, ask user for confirmation after showing the table
#' size
#'
#' @export
postgres2sqlite <- function (credential_file, tbl_name, out_file_name,
                             bbx = NULL, ask=TRUE)
{
    dat <- get_postgresql_tbl (credential_file, tbl_name, out_file_name, bbx,
                               ask)
    drv_sqlite <- DBI::dbDriver ("SQLite")
    conn_sqlite <- RSQLite::dbConnect (drv = drv_sqlite,
                                     dbname = out_file_name)
    RSQLite::dbWriteTable (conn_sqlite, tbl_name, dat)
    RSQLite::dbDisconnect (conn_sqlite)
}

#' Fetches data from a PostGIS table and writes it to a local Geopackage file
#'
#' Rows without geometries will be omitted.
#'
#' @param credential_file A csv file containing the database credentials
#'
#' @param tbl_name Name of the table to be downloaded
#'
#' @param out_file_name Name of the output file
#'
#' @param bbx Optional bounding box. A numeric \code{vector} of length 4 with
#' \code{xmin}, \code{ymin}, \code{xmax} and \code{ymax}.
#'
#' @param ask if \code{TRUE}, ask user for confirmation after showing the table
#' size
#'
#' @export
postgres2gpkg <- function (credential_file, tbl_name, out_file_name, bbx = NULL,
                           ask = TRUE)
{
    if (!endsWith (tolower (out_file_name), ".gpkg"))
        out_file_name <- paste0 (out_file_name, ".gpkg")
    dat <- get_postgresql_tbl (credential_file, tbl_name, out_file_name, bbx,
                               ask)
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
    sf::st_write (pts_out, out_file_name)
}

#' Fetches data from a PostgreSQL table returns it as a data.frame
#'
#' @param credential_file A csv file containing the database credentials
#'
#' @param tbl_name Name of the table to be downloaded
#'
#' @param out_file_name Name of the output file
#'
#' @param bbx Optional bounding box. A numeric \code{vector} of length 4 with
#' \code{xmin}, \code{ymin}, \code{xmax} and \code{ymax}.
#'
#' @param ask if \code{TRUE}, ask user for confirmation after showing the table
#' size
#'
#' @noRd
get_postgresql_tbl <- function (credential_file, tbl_name, out_file_name,
                                bbx = NULL, ask=TRUE)
{
    cred <- read_credentials (credential_file)
    drv <- DBI::dbDriver ("PostgreSQL")
    con <- RPostgreSQL::dbConnect (drv, user = cred$user,
                                   password = cred$password, host = cred$host,
                                   port = cred$port, dbname = cred$dbname)

    if (RPostgreSQL::dbExistsTable (con, tbl_name))
    {
        print (paste0 ("Found table ", tbl_name, " on ", cred$host, "/",
                       cred$dbname, "."))
        sql <- paste ("SELECT count (*) FROM", tbl_name, ";")
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
            sql <- paste0 ("SELECT column_name, data_type FROM ",
                           "information_schema.columns WHERE ",
                           "table_name = '", tbl_name, "';")
                           rset <- RPostgreSQL::dbGetQuery (con, sql)
                           rset <- rset [rset$data_type != "USER-DEFINED", ]
                           cols <- paste (rset$column_name, collapse = ",")
                           if (is.null (bbx))
                           {
                               sql <- paste ("SELECT", cols,
                                             ", st_y (geom_org) as lat,",
                                             "st_x (geom_org) as lon FROM",
                                             tbl_name, ";")
                           } else
                           {
                               sql <- paste ("SELECT", cols,
                                             ", st_y (geom_org) as lat,",
                                             "st_x (geom_org) as lon FROM",
                                             tbl_name, " WHERE geom_org &&",
                                             "ST_MakeEnvelope (", bbx [1], ",",
                                             bbx [2], ",", bbx [3], ",",
                                             bbx [4], ");")
                           }
            rset <- RPostgreSQL::dbGetQuery (con, sql)
        } else
        {
            RPostgreSQL::dbDisconnect (con)
            stop ("No data have been downloaded.")
        }
    }
    RPostgreSQL::dbDisconnect (con)
    rset
}

#' Read SQLite file and return a data.frame
#'
#' @param f_name Name of the SQLite file
#'
#' @param tbl_name Name of the table to be fetched
#'
#' @param sf if \code{TRUE}, returns a \code{sf} object containing only data
#' with valid coordinates in fields lat and lon
#'
#' @param bbox a numeric \code{vector} of length 4 with \code{xmin},
#' \code{ymin}, \code{xmax} and \code{ymax}.
#'
#' @return A \code{data.frame} containing the specified table contents
#'
#' @export
read_sqlite <- function (f_name, tbl_name, sf = TRUE, bbox = NULL)
{
    if (!file.exists (f_name))
    {
        msg <- paste0 ("Database '", f_name, "' not found.")
        stop (msg)
    }
    drv_sqlite <- RSQLite::dbDriver ("SQLite")
    conn_sqlite <- RSQLite::dbConnect (drv = drv_sqlite, dbname = f_name)
    tbls <- RSQLite::dbListTables (conn_sqlite)
    if (tbl_name %in% tbls)
    {
        if (!is.null (bbox) & !is.numeric (bbox) & length (bbox) != 4)
        {
            bbox <- NULL
            msg <- "bbox is not a numeric vector of length 4. Ignoring bbox."
            warning (msg)
        }
        if (is.null (bbox))
            dat <- RSQLite::dbReadTable (conn_sqlite, tbl_name)
        else
        {
            sql <- paste0 ("SELECT * from ", tbl_name, " WHERE ",
                           "lon > ", bbox [1], " AND ",
                           "lon < ", bbox [3], " AND ",
                           "lat > ", bbox [2], " AND ",
                           "lat < ", bbox [4], ";")
            dat <- RSQLite::dbGetQuery (conn_sqlite, sql)
        }
        if (dim (dat) [1] == 0)
            stop ("Query yields no results.")
    }
    else
    {
        tbls <- paste (tbls, collapse = ", ")
        msg <- paste0 ("Database '", f_name, "' does not contain table '",
                       tbl_name, "'. Available tables are: ", tbls)
        stop (msg)
    }
    RSQLite::dbDisconnect (conn_sqlite)
    if (sf)
    {
        if (!all (c ("lat", "lon") %in% names (dat)))
        {
            warning ("SQLite file does not contain fields lat and long. Returning
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

#' Read GeoPackage file and return a spatial data.frame
#'
#' @param f_name Name of the GPKG file
#'
#' @param bbox a numeric \code{vector} of length 4 with \code{xmin},
#' \code{ymin}, \code{xmax} and \code{ymax}.
#'
#' @return A \code{data.frame} containing the specified table contents
#'
#' @export
read_gpkg <- function (f_name, bbox = NULL)
{
    if (!file.exists (f_name))
    {
        msg <- paste0 ("GPKG file '", f_name, "' not found.")
        stop (msg)
    }
    dat <- sf::st_read (f_name, quiet = TRUE)
    if (!is.null (bbox) & !is.numeric (bbox) & length (bbox) != 4)
    {
        bbox <- NULL
        msg <- "bbox is not a numeric vector of length 4. Ignoring bbox."
        warning (msg)
    }
    if (!is.null (bbox))
    {
        p1 <- paste0 (bbox [1], " ", bbox [2])
        p2 <- paste0 (bbox [1], " ", bbox [4])
        p3 <- paste0 (bbox [3], " ", bbox [4])
        p4 <- paste0 (bbox [3], " ", bbox [2])
        pol_wkt <- paste0 ("POLYGON((", p1, ", ", p2, ", ", p3, ", ", p4, ", ",
                           p1, "))")
        crs <- sf::st_crs (dat)
        bbox_pol <- sf::st_as_sfc (pol_wkt, crs = crs)
        in_bbx <- sf::st_within (dat, bbox_pol, sparse = FALSE)
        dat <- dat [in_bbx, ]
    }
    if (dim (dat) [1] == 0)
        stop ("Query yields no results.")
    dat
}
