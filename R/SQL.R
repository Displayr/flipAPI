#' Gets data from SQL database
#'
#' @description Connects to SQL database and returns the result of a query
#' @param query SQL query to generate data returned
#' @param data_provider Type of database. One of "MySQL", "Postgres", "Redshift",
#'  "Oracle", "Snowflake", "SQL Server".
#' @param server Host name or IP where data base is hosted
#' @param database Name of database
#' @param user User name of data base account
#' @param password Password of data base account
#' @param max_records Maximum number of rows returned in `query`
#' @param port Port number to connect to data base
#' @param warehouse Data base warehouse (only for snowflake database)
#' @importFrom DBI dbConnect dbGetQuery dbDisconnect
#' @export
RunSQL <- function(query, data_provider, server, database, user, password = "",
                   max_records = 100, port = 1433, warehouse = "")
{
    cat("data_provider:" data_provider, "\n")
    if (data_provider %in% c("MySQL", "Postgres", "Redshift"))
    {
        drv <- switch(data_provider,
            MySQL = RMySQL::MySQL(),
            Postgres = RPostgres::Postgres(),
            Redshift = RPostgres::Redshift())
        con <- dbConnect(drv,
                    dbname = database,
                    host = server,
                    user = user,
                    password = password,
                    port = port)

    } else if (data_provider == "Oracle")
    {
        # Using the TNS service name seems to be the only way to connect to Oracle
        # https://github.com/r-dbi/odbc/issues/506
        tns <- paste0("(DESCRIPTION = (ADDRESS_LIST =
            (ADDRESS = (PROTOCOL = TCP)(HOST = ", server, ")(PORT = ", port, ")))
            (CONNECT_DATA = (SERVICE_NAME = ", database, ")))")
        con <- dbConnect(odbc::odbc(),
                    driver = "Oracle 21 ODBC driver",
                    dbq = tns,
                    uid = user,
                    pwd = password)
    } else
    {
        driver <- switch(data_provider,
            Snowflake = "SnowflakeDSIIDriver",
            'SQL Server' = "ODBC Driver 17 for SQL Server")
        con <- dbConnect(odbc::odbc(),
                    driver = driver,
                    server = server,
                    database = database,
                    uid = user,
                    pwd = password,
                    port = port,
                    warehouse = warehouse)
    }
    print(con)
    res <- dbGetQuery(con, query, n = max_records)
    dbDisconnect(con)
    res
}
