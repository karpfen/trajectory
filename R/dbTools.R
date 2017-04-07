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
postgres2sqlite <- function (credentialFile, tblName, outFileName, ask = TRUE)
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
            drvSQLite <- DBI::dbDriver ("SQLite")
            conSqlite <- RSQLite::dbConnect (drv = drvSQLite,
                                             dbname = outFileName)
            RSQLite::dbWriteTable (conSqlite, "tweets", rset)
            RSQLite::dbDisconnect (conSqlite)
        }
    }
    RPostgreSQL::dbDisconnect (con)
}

#' Read SQLite file and return a data.frame
#'
#' @param fName Name of the SQLite file
#' @param tblName Name of the table to be fetched
#'
#' @return A \code{data.frame} containing the entire table
#'
#' @export
readSQLite <- function (fName, tblName)
{
    drvSQLite <- RSQLite::dbDriver ("SQLite")
    conSqlite <- RSQLite::dbConnect (drv = drvSQLite, dbname = fName)
    tbls <- RSQLite::dbListTables (conSqlite)
    if (tblName %in% tbls)
        dat <- RSQLite::dbReadTable (conSqlite, tblName)
    RSQLite::dbDisconnect (conSqlite)
    dat
}
