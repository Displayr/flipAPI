#' Request data from Google Analytics
#'
#' @param dimensions A string (comma separated) or list of strings containing
#'   the dimensions to request.
#' @param metrics Similar to \code{dimensions} but for metrics.
#' @param start.date,end.date The start and end dates for the query. If it is a
#'   string it will be coerced to a date using YMD format.
#' @param secret.text The JSON formatted string containing the secret text that
#'   identifies your account.
#' @param table.num If your account contains multiple tables, use this to select
#'   the table to query. Defaults to the first table.
#'
#' @details See
#'   \url{https://developers.google.com/analytics/devguides/reporting/core/dimsmets}
#'   for details of the available dimensions and metrics.
#'
#'   See
#'   \url{http://wiki.q-researchsoftware.com/wiki/How_to_Import_Data_from_Google_Analytics}
#'   for more information about setting up your Google Analytics account for
#'   use with this function. The parameter \code{secret.text} is the text
#'   contained in the JSON file you get when adding a Service Account.
GoogleAnalytics <- function(dimensions, metrics, start.date, end.date, secret.text, table.num = 1)
{
    .checkGaNames <- function(x, max.num)
    {
        type <- deparse(substitute(x))
        x <- stringr::str_trim(unlist(strsplit(x, ",", fixed = TRUE)))
        have.prefix <- grepl("^ga:", x)
        if (any(!have.prefix))
            x[!have.prefix] <- paste0("ga:", x[!have.prefix])

        if (length(x) > max.num)
            stop("Too many ", type, ". ", length(x), " specified, max ", max.num)

        paste(x, collapse = ",")
    }

    dimensions <- .checkGaNames(dimensions, 7)
    metrics <- .checkGaNames(metrics, 10)

    if (nchar(metrics) == 0)
        stop("You must supply at least 1 metric.")

    if (is.character(start.date))
        start.date <- lubridate::ymd(start.date)
    if (is.character(end.date))
        end.date   <- lubridate::ymd(end.date)

    # We want the dates to go Monday-Sunday, so move start.date to previous Monday
    # and end.date to next Sunday.
    one.day <- lubridate::days(1)
    start.date <- lubridate::floor_date(start.date - one.day, "week") + one.day
    end.date   <- lubridate::ceiling_date(end.date, "week")

    secret.text <- gsub("\n([^[:space:]}])", "\\\\n\\1", secret.text)
    secrets <- jsonlite::fromJSON(secret.text)

    scope <- "https://www.googleapis.com/auth/analytics.readonly"

    token <- httr::oauth_service_token(httr::oauth_endpoints("google"), secrets, scope)

    table.id <- RGoogleAnalytics::GetProfiles(token)$id[table.num]

    query.list <- RGoogleAnalytics::Init(start.date = start.date, end.date = end.date,
        dimensions = dimensions,
        metrics = metrics,
        max.results = 10000,
        table.id = paste0("ga:", table.id))
    ga.query <- RGoogleAnalytics::QueryBuilder(query.list)
    ga.data <- RGoogleAnalytics::GetReportData(ga.query, token,
        split_daywise = (ga.query$start.date() != ga.query$end.date()))

    if ("date" %in% names(ga.data))
    {
        ga.data$date <- lubridate::ymd(ga.data$date)
    }
    if ("pagePathLevel1" %in% names(ga.data))
    {
        ga.data$pagePathNoQuery <- sub("\\?.*", "", ga.data$pagePathLevel1)
        ga.data$pagePathNoQuery <- sub("(.+)/$", "\\1", ga.data$pagePathNoQuery)
    }
    if ("sourceMedium" %in% names(ga.data))
    {
        sourceMedium <- strsplit(ga.data$sourceMedium, " / ", fixed = TRUE)
        sourceMedium <- data.frame(do.call("rbind", sourceMedium), stringsAsFactors = FALSE)
        names(sourceMedium) <- c("source", "medium")
        ga.data <- cbind(ga.data, sourceMedium)
    }

    ga.data
}
