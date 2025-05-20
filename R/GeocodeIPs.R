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
#' @examples
#' GeocodeIPs(c("123.51.111.134", "216.27.61.137", "2001:780:53d2::1"))
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
    lapply(ips, get_all) |> standardiseGeocodeIPsColumns(ips = ips)
}

getIP2LocationDatabasePath <- function() {
    db.path <- Sys.getenv("IP2LOCATION_DB_PATH", unset = NA)
    if (is.na(db.path)) {
        StopForUserError(
            "The IP2Location database is not installed. ",
            "Please download it from https://www.ip2location.com and ",
            "set the IP2LOCATION_DB_PATH environment variable to the database path."
        )
    }
    if (!file.exists(db.path)) {
        StopForUserError(
            "The IP2Location database was not found at the path: ", db.path, ". ",
            "Please check the path and try again."
        )
    }
    # Check it is readable
    if (file.access(db.path, mode = 4L) != 0L) {
        StopForUserError(
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
        StopForUserError("The database names have changed.")
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
