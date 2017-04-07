library (RPostgreSQL)
library (RSQLite)
source ("utils.R")

# TODO: Bounding Box support
postgres2sqlite <- function (credentialFile, tblName, outFileName, ask = TRUE)
{
    cred <- readCredentials (credentialFile)
    drv <- dbDriver ("PostgreSQL")
    con <- dbConnect (drv, user = cred$user, password = cred$password,
                      host = cred$host, port = cred$port, dbname = cred$dbname)

    if (dbExistsTable (con, tblName))
    {
        print (paste0 ("Found table ", tblName, " on ", cred$host, "/",
                      cred$dbname, "."))
        sql <- paste ("SELECT count (*) FROM", tblName, ";")
        size <- dbGetQuery (con, sql)
        if (ask)
        {
            txt <- paste0 ("Number of rows: ", size, ". Proceed Downloading? (y) ")
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
            rset <- dbGetQuery (con, sql)
            rset <- rset [rset$data_type != "USER-DEFINED", ]
            cols <- paste (rset$column_name, collapse = ",")
            sql <- paste ("SELECT", cols, ", st_y (geom_org) as lat,",
                          "st_x (geom_org) as lon FROM", tblName, ";")
            rset <- dbGetQuery (con, sql)
            drvSQLite <- dbDriver ("SQLite")
            conSqlite <- dbConnect (drv = drvSQLite, dbname = outFileName)
            dbWriteTable (conSqlite, "tweets", rset)
            dbDisconnect (conSqlite)
        }
    }
    dbDisconnect (con)
}

readSQLite <- function (fName, tblName)
{
    drvSQLite <- dbDriver ("SQLite")
    conSqlite <- dbConnect (drv = drvSQLite, dbname = fName)
    tbls <- dbListTables (conSqlite)
    if (tblName %in% tbls)
        dat <- dbReadTable (conSqlite, tblName)
    dbDisconnect (conSqlite)
    dat
}
