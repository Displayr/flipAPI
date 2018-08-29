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
#' @details Uses the \href{https://www.maxmind.com/en/home}{MaxMind} database from
#'    the \code{\link[rgeolocate:maxmind]{rgeolocate}} package.
#'    Returns \code{\link{NA}} when no data is available.
#' @examples
#' GeocodeIPs(c("123.51.111.134", "216.27.61.137", "2001:780:53d2::1"))
#' @importFrom rgeolocate maxmind
#' @export

GeocodeIPs <- function(ips) {
    
    if (length(dim(ips)) == 2)
    {
        ips <- ips[, 1]
        warning("Only the first column will be geocoded.")
    }
    if (!((is.character(ips) && is.null(dim(ips))) || is.factor(ips)))
        stop("Please provide a character vector of IP addresses.")
    ips <- as.vector(ips)
    
    file <- system.file("extdata", "GeoLite2-Country.mmdb", package = "rgeolocate")
    
    locations <- maxmind(ips, file,
                         fields = c("continent_name", "country_name", "country_code"))
    
    return(cbind(ips = ips, locations))
}

