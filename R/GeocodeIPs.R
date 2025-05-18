#' GeocodeIP addresses
#'
#' Returns the country and continent of a vector of IPv4 or IPv6 strings.
#' @param ips Character  \code{\link{vector}} of IP addresses.
#' @return A \code{\link{data.frame}} with columns
#' \itemize{
#' \item \code{ips} - the original input ips
#' \item \code{status} - the result of the geocoding query
#' \item \code{country_name} - the name of the country where the IP is located
#' \item \code{city} - the name of the city where the IP is located
#' }
#' @examples
#' GeocodeIPs(c("123.51.111.134", "216.27.61.137", "2001:780:53d2::1"))
#' @importFrom locateip locate_ip
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

    responses <- lapply(ips, locate_ip, fields = c("status,country,city"))
    valid.responses <- lengths(responses) == 3L
    if (any(!valid.responses)) {
        responses[!valid.responses] <- data.frame(status = "error", country = NA, city = NA) |> list()
    }
    results <- do.call(rbind, responses) |> as.data.frame()
    results <- cbind(ips, results)
    names(results) <- c("ips", "status", "country_name", "city")
    results
}
