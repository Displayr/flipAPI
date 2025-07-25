#' GeocodeIP addresses
#'
#' Returns the country and continent of a vector of IPv4 or IPv6 strings.
#' @param ips Character  \code{\link{vector}} of IP addresses.
#' @return A \code{\link{data.frame}} with columns
#' \itemize{
#' \item \code{ips} - the original input ips
#' \item \code{continent_name} - the name of the continent where the IP is located
#' \item \code{country_name} - the name of the country where the IP is located
#' \item \code{country_code} - the 2 letter country code (ISO Alpha-2)
#' }
#' @details Uses the \href{https://lite.ip2location.com/ip2location-lite}{IP2Location LITE}
#'    or \href{https://www.ip2location.com/database/ip2location}{IP2Location} databases
#'    from IP2Location to resolve country locations from IP addresses.
#'    The database must be downloaded and the database path set as the environment variable
#'    \code{IP2LOCATION_DB_PATH} before use. The database can be downloaded from
#'    \href{https://lite.ip2location.com/ip2location-lite}{IP2Location LITE} or
#'    \href{https://www.ip2location.com/database/ip2location}{IP2Location}.
#'    To interact with the database a python environment is required that has the
#'    \href{https://pypi.org/project/IP2Location/}{IP2Location} python package installed.
#'    If an invalid IP address or the information is not available in the database,
#'    the \code{country_name} and \code{country_code} columns will be set to \code{NA}.
#'    The LITE database should achieve an accuracy at the country level of 98\%. If
#'    you need more accuracy, you can use the commercial license with the IP2Location
#'    database. More details on the differences between the databases can be found
#'    at \href{https://lite.ip2location.com/edition-comparison}{IP2Location comparisons}.
#' @examples
#' \dontrun{
#' GeocodeIPs(c("123.51.111.134", "216.27.61.137", "2001:780:53d2::1"))
#' }
#' @importFrom ip2location get_all
#' @importFrom flipU StopForUserError
#' @export
GeocodeIPs <- function(ips) {
    if (length(dim(ips)) == 2)
    {
        if (dim(ips)[2] != 1)
            warning("Only the first column will be geocoded.")
        ips <- ips[, 1]
    }
    if (!((is.character(ips) && is.null(dim(ips))) || is.factor(ips)))
        StopForUserError("Please provide a character vector of IP addresses.")

    loadDatabase()
    checkIP2LocationLibraryAvailable()
    lapply(ips, get_all) |> standardiseGeocodeIPsColumns(ips = ips)
}

getIP2LocationDatabasePath <- function() {
    db.path <- Sys.getenv("IP2LOCATION_DB_PATH", unset = NA)
    if (is.na(db.path)) {
        throwErrorWithSupportIfOnRServer(
            "The IP2Location database is not installed. ",
            "Please download it from https://www.ip2location.com and ",
            "set the IP2LOCATION_DB_PATH environment variable to the database path."
        )
    }
    if (!file.exists(db.path)) {
        throwErrorWithSupportIfOnRServer(
            "The IP2Location database was not found at the path: ", db.path, ". ",
            "Please check the path and try again."
        )
    }
    # Check it is readable
    if (file.access(db.path, mode = 4L) != 0L) {
        throwErrorWithSupportIfOnRServer(
            "The IP2Location database is not readable at the path: ", db.path, ". ",
            "Please check the file permissions and try again."
        )
    }
    db.path
}

requiredGeocodeIPsColumns <- c("ip", "country_short", "country_long")

#' @importFrom ip2location open
loadDatabase <- function() {
    db.path <- getIP2LocationDatabasePath()
    ip2location::open(db.path)
}

#' @importFrom reticulate py_module_available
checkIP2LocationLibraryAvailable <- function() {
    if (!py_module_available("IP2Location")) {
        throwErrorWithSupportIfOnRServer(
            "The IP2Location python package is not installed. ",
            "Install it before calling GeocodeIPs()"
        )
    }
}

#' @importFrom countrycode countrycode
standardiseGeocodeIPsColumns <- function(responses, ips) {
    required.names <- c("ip", "country_short", "country_long")
    invalid.ips <- vapply(responses, `[[`, character(1), "country_short") %in% c("-", "INVALID IP ADDRESS")
    if (any(invalid.ips)) {
        templates <- lapply(
            ips[invalid.ips],
            function(x) list(ip = x, country_short = NA_character_, country_long = NA_character_)
        )
        responses[invalid.ips] <- templates
    }
    output <- do.call(rbind.data.frame, args = responses)
    if (!all(required.names %in% colnames(output))) {
        throwErrorWithSupportIfOnRServer("The database names have changed.")
    }
    invalid.responses <- is.na(output[["country_short"]])
    if (!"continent_name" %in% names(output)) {
        output[["continent_name"]] <- NA
        if (requireNamespace("countrycode", quietly = TRUE)) {
            output[!invalid.responses, "continent_name"] <- countrycode(
                output[!invalid.responses, "country_short"],
                origin = "iso2c",
                destination = "continent"
            )
        }
    }
    database.names <- c("ip", "continent_name", "country_long", "country_short")
    output <- output[match(database.names, names(output), incomparables = 0L)]
    names(output) <- c("ips", "continent_name", "country_name", "country_code")
    output
}

#' @importFrom flipU IsRServer
throwErrorWithSupportIfOnRServer <- function(...) {
    message <- paste0(...)
    if (IsRServer()) {
        message <- paste0(
            "There was a problem with the IP address database. ",
            "Please contact support if this is causing hardship. ",
            "Error details: ",
            message
        )
    }
    stop(message)
}
