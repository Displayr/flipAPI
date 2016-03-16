#' Get all Hubspot contacts
#'
#' @param hubspot.api.key The Hubspot API key for your profile.
#' @param properties Optional properties to request. If \code{NULL}, the default
#'   properties are returned.
#' @param count The number of items to request at a time. 100 is the maximum.
#' @param verbose Print some diagnostics with each request.
#' @return A \code{data.frame} containing your contacts.
#' @export
GetAllHubspotContacts <- function(hubspot.api.key, properties = NULL, count = 100, verbose = FALSE)
{
    path <- "contacts/v1/lists/all/contacts/all"

    # Count maximum is 100, less than 1 doesn't make sense
    if (count <= 0 || count > 100)
        count <- 100

    if (!is.null(properties))
    {
        properties <- as.list(properties)
        names(properties) <- rep_len("property", length(properties))
    }

    req <- hubspotGet(path = path, query = c(list(hapikey = hubspot.api.key, count = count), properties))
    req.content <- hubspotParse(req)
    contacts <- req.content$contacts

    i <- 0
    while (req.content$`has-more`)
    {
        i <- i + 1
        if (verbose)
            cat(i, "Current offset", req.content$`vid-offset`, "\n")
        req <- hubspotGet(path = path,
            query = c(list(hapikey = hubspot.api.key, count = count, vidOffset = req.content$`vid-offset`),
                properties))
        req.content <- hubspotParse(req)
        contacts <- c(contacts, req.content$contacts)
    }

    .parseContact <- function(contact)
    {
        # If we need these fields later we can add them in
        contact$`form-submissions` <- NULL
        contact$`merge-audits` <- NULL
        contact$`merged-vids` <- NULL

        properties <- sapply(contact$properties, "[[", "value")
        contact$properties <- NULL

        # Only look at the first identity-profile. I don't know how common it is to have multiple
        # identity-profiles.
        for (identity in contact$`identity-profiles`[[1]]$identities)
        {
            properties[[tolower(identity$type)]] <- identity$value
        }
        contact$`identity-profiles` <- NULL

        unlist(c(properties, contact))
    }

    contacts <- lapply(contacts, .parseContact)
    all.names <- unique(unlist(lapply(contacts, names)))

    df <- matrix(nrow = length(contacts), ncol = length(all.names))
    colnames(df) <- all.names

    for (i in seq(along = contacts))
    {
        df[i, names(contacts[[i]])] <- contacts[[i]]
    }

    df <- data.frame(df, stringsAsFactors = FALSE)

    logical.columns <- c("is.contact")
    logical.columns <- logical.columns[logical.columns %in% names(df)]
    for (logical.column in logical.columns)
    {
        df[[logical.column]] <- as.logical(df[[logical.column]])
    }

    date.columns <- c("addedAt", "lastmodifieddate")
    date.columns <- date.columns[date.columns %in% names(df)]
    for (date.column in date.columns)
    {
        df[[date.column]] <- timestampToDate(as.numeric(df[[date.column]]))
    }

    return (df)
}

#' Get all Hubspot companies
#'
#' @inheritParams GetAllHubspotContacts
#' @return A \code{data.frame} containing your companies.
#' @export
GetAllHubspotCompanies <- function(hubspot.api.key, count = 100, verbose = FALSE)
{
    # Use the "recently modified" call because there isn't an "all companies" call
    path <- "companies/v2/companies/recent/modified"

    # Count maximum is 100, less than 1 doesn't make sense
    if (count <= 0 || count > 100)
        count <- 100

    req <- hubspotGet(path = path, query = list(hapikey = hubspot.api.key, count = count))
    req.content <- hubspotParse(req)
    companies <- req.content$results

    i <- 0
    while (req.content$hasMore)
    {
        i <- i + 1
        if (verbose)
            cat(i, "Current offset", req.content$offset, "\n")
        req <- hubspotGet(path = path,
            query = list(hapikey = hubspot.api.key, count = count, offset = req.content$offset))
        req.content <- hubspotParse(req)
        companies <- c(companies, req.content$results)
    }

    .parseCompany <- function(company)
    {
        properties <- sapply(company$properties, "[[", "value")
        c(company_id = company$companyId, properties)
    }

    companies <- lapply(companies, .parseCompany)
    all.names <- unique(unlist(lapply(companies, names)))

    df <- matrix(nrow = length(companies), ncol = length(all.names))
    colnames(df) <- all.names

    for (i in seq(along = companies))
    {
        df[i, names(companies[[i]])] <- companies[[i]]
    }

    df <- data.frame(df, stringsAsFactors = FALSE)

    logical.columns <- c("is_public")
    logical.columns <- logical.columns[logical.columns %in% names(df)]
    for (logical.column in logical.columns)
    {
        df[[logical.column]] <- as.logical(df[[logical.column]])
    }

    numeric.columns <- c("annualrevenue", "hs_analytics_num_page_views", "hs_analytics_num_visits",
        "num_associated_contacts", "num_contacted_notes", "num_notes", "numberofemployees")
    numeric.columns <- numeric.columns[numeric.columns %in% names(df)]
    for (numeric.column in numeric.columns)
    {
        df[[numeric.column]] <- as.numeric(df[[numeric.column]])
    }

    date.columns <- c("closedate", "createdate", "first_contact_createdate",
        "hs_analytics_first_timestamp", "hs_lastmodifieddate", "hubspot_owner_assigneddate",
        "notes_last_contacted", "notes_last_updated", "first_deal_created_date")
    date.columns <- date.columns[date.columns %in% names(df)]
    for (date.column in date.columns)
    {
        df[[date.column]] <- timestampToDate(as.numeric(df[[date.column]]))
    }

    return (df)
}

# From https://cran.r-project.org/web/packages/httr/vignettes/api-packages.html
#' @noRd
hubspotGet <- function(path, ...)
{
    req <- httr::GET("https://api.hubapi.com", path = path, ...)
    hubspotCheck(req)

    req
}

#' @noRd
hubspotCheck <- function(req)
{
    if (httr::status_code(req) == 200)
        return(invisible())

    message <- hubspotParse(req)$message
    stop("HTTP failure: ", req$status_code, "\n", message, call. = FALSE)
}

#' @noRd
hubspotParse <- function(req)
{
    text <- httr::content(req, as = "text")
    if (identical(text, ""))
        stop("No output to parse", call. = FALSE)
    jsonlite::fromJSON(text, simplifyVector = FALSE)
}

#' Convert a Hubspot timestamp to POSIXct
#'
#' Convert a Hubspot timestamp (milliseconds since UNIX epoch) to POSIXct.
#' \code{\link{as.POSIXct}} takes seconds since epoch so we need to divide
#' by 1000.
#'
#' @param timestamp A timestamp in milliseconds.
timestampToDate <- function(timestamp)
{
    as.POSIXct(timestamp / 1000, origin = "1970-01-01")
}

# From http://stackoverflow.com/questions/26561518/built-in-for-getting-list-element-or-default
#' @noRd
`%or%` <- function(a, b)
{
    cmp = function(a, b) if (identical(a, FALSE) ||
                             is.null(a) ||
                             is.na(a) ||
                             is.nan(a) ||
                             length(a) == 0) b else a

    if (length(a) > 1)
        mapply(cmp, a, b)
    else
        cmp(a, b)
}
