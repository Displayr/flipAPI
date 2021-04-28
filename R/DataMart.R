#' Check if a file exists
#'
#' Check whether a file of a given name exists in the Displayr Cloud Drive.
#'
#' @param filename character string. Name of the file to search for.
#'
#' @return TRUE if the file exists, otherwise FALSE.
#'
#' @importFrom httr HEAD add_headers
#' @importFrom utils URLencode
#'
#' @export
QFileExists <- function(filename)
{
    company.secret <- getCompanySecret()
    client.id <- getClientId()
    api.root <- getApiRoot()
    res <- try(HEAD(paste0(api.root, "?filename=", URLencode(filename, TRUE)),
                    config=add_headers("X-Q-Company-Secret" = company.secret,
                                       "X-Q-Project-ID" = client.id)))

    if (is.null(res$status_code) || res$status_code != 200)
    {
        warning("File not found.")
        return (FALSE)
    }
    else
    {
        message("File was found.")
        return (TRUE)
    }
}

#' Opens a Connection
#'
#' Opens a connection for either reading OR writing to a file of a given name.
#' In the reading case, it opens a stream to the file in the Displayr Cloud Drive.
#' In the writing case, it opens a temporary file for writing to and uploads this to the Displayr Cloud Drive on close.
#' Note that writing to a file which already exists will overwrite that file's contents.
#' For more documentation on this function's parameters, see R documentation for opening connections.
#'
#' @param filename character string. Name of file to be opened
#' @param open character string. See documentation for connections.
#' @param blocking logical. See documentation for connections.
#' @param encoding character string. See documentation for connections.
#' @param raw logical. See documentation for connections.
#' @param method character string. See documentation for connections.
#' @param mime.type character string. The mime-type of this file. If not provided, it will be interpreted from the file extension.
#' @param company.token Use this if you need to read from a different company's Displayr Cloud Drive.  You need to contact Support to get this token.
#'
#' @return A curl connection (read) or a file connection (write)
#'
#' @importFrom curl curl new_handle handle_setheaders
#' @importFrom httr POST upload_file add_headers handle
#' @importFrom tools file_ext
#' @importFrom utils URLencode
#'
#' @export
QFileOpen <- function(filename, open = "r", blocking = TRUE,
                      encoding = getOption("encoding"), raw = FALSE,
                      method = getOption("url.method", "default"),
                      mime.type = NA, company.token = NA)
{
    mode <- tolower(open)
    if (mode == "r" || mode == "rb")
    {
        company.secret <- if (missing(company.token)) getCompanySecret() else company.token
        client.id <- getClientId()
        api.root <- getApiRoot()
        h <- new_handle()
        handle_setheaders(h,
            "X-Q-Company-Secret" = company.secret,
            "X-Q-Project-ID" = client.id
        )
        uri <- paste0(api.root, "?filename=", URLencode(filename, TRUE))
        con <- tryCatch(curl(uri, open=mode, handle=h), error=function(c) c)
        if (inherits(con, "condition")) {
            problem <- conditionMessage(con)
            if (problem == "HTTP error 404.")
                problem <- "file not found"
            stopBadRequest(con, paste0("Could not open ", filename, ": ", problem))
        }

        if (!inherits(con, "connection"))

        # to allow functions to parse this 'like' a url connection
        # e.g. so readRDS will wrap this in gzcon when reading
        class(con) <- append(class(con), "url")

        return (con)
    }
    else if (mode == "w" || mode == "wb")
    {
        # We need to make a temporary file because RCurl cannot make a connection for
        # writing, because HTTP needs to know the content length up front.
        if (!missing(company.token))
            stop("'company.token' can only be specified for read operations.\nYou cannot write files to another company's Displayr Cloud Drive.")

        # Check if in valid environment
        getCompanySecret()
        getClientId()

        tmpfile <- paste0(tempfile(), ".", file_ext(filename))
        con <- file(tmpfile, mode, blocking, encoding, raw, method)
        class(con) = append("qpostcon", class(con))

        # store attributes for later access
        attr(con, "tmpfile") <- tmpfile
        attr(con, "filename") <- filename
        if (missing(mime.type)) mime.type <- guess_type(filename)
        attr(con, "mimetype") <- mime.type

        return (con)
    }
    else
    {
        stop("Invalid mode - please use either 'r', 'rb','w' or 'wb'.")
    }
}

#' Closes a QFileOpen connection
#'
#' This is an overload for close.connection which writes the file contents.
#' of a connection opened using QFileOpen to the Displayr Cloud Drive.
#'
#' @param con connection object of class 'qpostcon'. Connection opened with QFileOpen.
#' @param ... arguments passed to or from other methods.
#'
#' @importFrom httr POST add_headers upload_file
#' @importFrom utils URLencode
#'
#' @return NULL invisibly. Called for the purpose of uploading data
#' and assumed to succeed if no errors are thrown.
#'
#' @export
close.qpostcon = function(con, ...)
{
    close.connection(con, ...)
    filename <- attr(con, "filename")
    tmpfile <- attr(con, "tmpfile")
    mimetype <- attr(con, "mimetype")
    on.exit(if(file.exists(tmpfile)) file.remove(tmpfile))

    company.secret <- getCompanySecret()
    client.id <- getClientId()
    api.root <- getApiRoot()
    res <- try(POST(paste0(api.root, "?filename=", URLencode(filename, TRUE)),
                config = add_headers("Content-Type" = mimetype,
                                     "X-Q-Company-Secret" = company.secret,
                                     "X-Q-Project-ID" = client.id),
                encode = "raw",
                body = upload_file(tmpfile)))

    if (!inherits(res, "try-error") && res$status_code == 413)  # 413 comes from IIS when we violate its web.config limits
        stopBadRequest(res, "Could not write to Displayr Cloud Drive. Data to write is too large.")
    else if (inherits(res, "try-error") || res$status_code != 200)
    {
        warning("Closing QFileOpen write connection has encountered an unknown error.")
        stopBadRequest(res, "Could not write to Displayr Cloud Drive. Please try again later.")
    }
    else
        message("File was written successfully.")
    invisible()
}

#' Loads an object
#'
#' Loads an *.rds file from the Displayr Cloud Drive and converts this to an R object.
#'
#' @param filename character string. Name of the file to be opened from the Displayr Cloud Drive.
#' @param company.token Use this if you need to read from a different company's Displayr Cloud Drive.  You need to contact Support to get this token.
#' @param ... Other parameters to pass to read.csv.
#'
#' @return An R object
#'
#' @importFrom haven read_sav
#' @importFrom httr GET add_headers write_disk
#' @importFrom utils URLencode
#'
#' @export
QLoadData <- function(filename, company.token = NA, ...)
{
    tmpfile <- tempfile()
    company.secret <- if (missing(company.token)) getCompanySecret() else company.token
    client.id <- getClientId()
    api.root <- getApiRoot()
    res <- try(GET(paste0(api.root, "?filename=", URLencode(filename, TRUE)),
               config=add_headers("X-Q-Company-Secret" = company.secret,
                                  "X-Q-Project-ID" = client.id),
               write_disk(tmpfile, overwrite = TRUE)))

    on.exit(if(file.exists(tmpfile)) file.remove(tmpfile))

    if (inherits(res, "try-error") || res$status_code != 200)
        stopBadRequest(res, paste0("Sorry, there was an issue connecting to your ",
                                   "Displayr Cloud Drive. Please try again later or contact support."))

    type <- getResponseFileType(res)
    if (is.null(type))
        type <- getFileType(filename)

    if (is.null(type))
        stop("Invalid file type specified. Only 'rds', 'csv' or 'sav' files ",
             "are supported.")


    if (file.exists(tmpfile))
    {
        if (type == "csv")
            obj <- read.csv(tmpfile, ...)
        else if (type == "rds")
            obj <- readRDS(tmpfile, ...)
        else if (type == "sav")
            obj <- read_sav(tmpfile, ...)
        return (obj)
    }
    stop("Could not read from file.")
}

#' Save an object
#'
#' Saves an object to the Displayr Cloud Drive without any transformation.
#' Filename string must have a .csv or .rds extension.
#'
#' @param object object. The object to be uploaded.
#' @param filename character string. Name of the file to be written to.
#' @param ... Other parameters to pass to write.csv or saveRDS.
#'
#' @importFrom haven write_sav
#' @importFrom httr POST add_headers upload_file
#' @importFrom utils URLencode
#'
#' @return NULL invisibly. Called for the purpose of uploading data
#' and assumed to succeed if no errors are thrown.
#'
#' @export
QSaveData <- function(object, filename, ...)
{
    type <- getFileType(filename)
    if (is.null(type) || type == "rda")
        stop("Invalid file type specified. Please name an '.rds' or '.csv' file.")

    tmpfile <- tempfile()
    if (type == "csv")
        write.csv(object, tmpfile, ...)
    else if (type == "rds")
        saveRDS(object, tmpfile, ...)
    else if (type == "sav")
        write_sav(object, tmpfile, ...)

    on.exit(if(file.exists(tmpfile)) file.remove(tmpfile))

    company.secret <- getCompanySecret()
    client.id <- getClientId()
    api.root <- getApiRoot()
    res <- try(POST(paste0(api.root, "?filename=", URLencode(filename, TRUE)),
                config = add_headers("Content-Type" = guess_type(filename),
                                     "X-Q-Company-Secret" = company.secret,
                                     "X-Q-Project-ID" = client.id),
                encode = "raw",
                body = upload_file(tmpfile)))

    if (!inherits(res, "try-error") && res$status_code == 413)  # 413 comes from IIS when we violate its web.config limits
        stopBadRequest(res, "Could not write to Displayr Cloud Drive. Data to write is too large.")
    else if (inherits(res, "try-error") || res$status_code != 200)
    {
        warning("QSaveData has encountered an unknown error.")
        stopBadRequest(res, "Could not save file.")
    }

    msg <- paste("Object uploaded to Displayr Cloud Drive To re-import object use:",
                  "   > library(flipAPI)",
           paste0("   > QLoadData('", filename, "')"),
           sep = "\n")
    message(msg)
    invisible()
}

qSaveImage <- function(filename)
{
    type <- getFileType(filename)
    if (type != "rda")
        stop("File extension needs to be .rda to use QSaveImage")

    tmpfile <- tempfile()
    save.image(tmpfile)
    on.exit(if(file.exists(tmpfile)) file.remove(tmpfile))

    company.secret <- getCompanySecret()
    client.id <- getClientId()
    api.root <- getApiRoot()
    res <- try(POST(paste0(api.root, "?filename=", URLencode(filename, TRUE)),
                    config = add_headers("Content-Type" = guess_type(filename),
                                         "X-Q-Company-Secret" = company.secret,
                                         "X-Q-Project-ID" = client.id),
                    encode = "raw",
                    body = upload_file(tmpfile)))

    if (!inherits(res, "try-error") && res$status_code == 413)  # 413 comes from IIS when we violate its web.config limits
        stopBadRequest(res, "Could not write to Displayr Cloud Drive. Data to write is too large.")
    else if (inherits(res, "try-error") || res$status_code != 200)
    {
        warning("QSaveData has encountered an unknown error.")
        stopBadRequest(res, "Could not save file.")
    }

    msg <- paste("Object uploaded to Displayr Cloud Drive To re-import object use:",
                 "   > library(flipAPI)",
                 paste0("   > QLoadImage('", filename, "')"),
                 sep = "\n")
    message(msg)
    invisible()
}

qLoadImage <- function(filename, company.token = NA)
{
    tmpfile <- tempfile()
    company.secret <- if (missing(company.token)) getCompanySecret() else company.token
    client.id <- getClientId()
    api.root <- getApiRoot()
    res <- try(GET(paste0(api.root, "?filename=", URLencode(filename, TRUE)),
                   config=add_headers("X-Q-Company-Secret" = company.secret,
                                      "X-Q-Project-ID" = client.id),
                   write_disk(tmpfile, overwrite = TRUE)))

    on.exit(if(file.exists(tmpfile)) file.remove(tmpfile))

    if (inherits(res, "try-error") || res$status_code != 200)
        stopBadRequest(res, "Could not load file.")

    type <- getResponseFileType(res)
    if (is.null(type))
        type <- getFileType(filename)

    if (type != "rda")
        stop("Invalid file type specified. Only 'rda' files are supported.")


    if (file.exists(tmpfile))
        load(tmpfile, envir = .GlobalEnv)
    else
        stop("Could not read from file.")
}

########################## HELPER FUNCTIONS AND CONSTANTS ###########################

#' Error when someone tries to use this package outside of Displayr.
#'
#' @return Throws an error.
#'
#' @noRd
stopNotDisplayr <- function(code) {
    stop(paste("This function can only be used from within Displayr. Missing", code))
}

#' Gets company secret from the environment. Throws an error if not found.
#'
#' @return Company secret token as a string.
#'
#' @noRd
getCompanySecret <- function()
{
    secret <- get0("companySecret", ifnotfound = "")
    if (secret == "") stopNotDisplayr("companySecret")
    return (secret)
}

#' Gets region from the environment and builds the api root. Throws an error if not found.
#'
#' @return Region-specific api root as a string.
#'
#' @noRd
getApiRoot <- function()
{
    region <- URLencode(get0("region", ifnotfound = ""), TRUE)
    if (region == "") stopNotDisplayr("region")
    if (region == "app")
        region <- "displayr-app"  # to avoid Dynamic Site Acceleration CDN, which limits uploads to 100MB
    api.root <- paste0("https://", region, ".displayr.com/api/DataMart/")
    return (api.root)
}

#' Gets the client Id (project id) from the environment. Throws an error if not found.
#'
#' @return The client id as a string
#'
#' @noRd
getClientId <- function()
{
    client.id <- gsub("[^0-9]", "", get0("clientId", ifnotfound = ""))
    if (client.id == "") stopNotDisplayr("clientId")
    return (client.id)
}


#' Guesses the type of file from the filename. Used for QSaveData/QLoadData
#'
#' @param filename string. Filename which we are checking
#'
#' @importFrom mime guess_type
#' @importFrom tools file_ext
#'
#' @return string or NULL. The supported file type which we have determined the file to be.
#'
#' @noRd
getFileType <- function(filename)
{
    if (file_ext(filename) == "rds")
        return ("rds")

    if (file_ext(filename) == "sav")
        return ("sav")

    # probably redundant
    if (file_ext(filename) == "csv" || guess_type(filename) == "text/csv")
        return ("csv")

    if (file_ext(filename) == "rda")
        return ("rda")

    return (NULL)
}

#' Gets the file type of a response based on Content-Type. Used for QSaveData/QLoadData
#'
#' @param response Response object.
#'
#' @return string (or NULL). The supported file type which we have determined the file to be else NULL.
#'
#' @noRd
getResponseFileType <- function(response)
{
    content.type <- response$headers$"content-type"
    if (content.type == "text/csv")
        return ("csv")

    return (NULL)
}

#' Throws an error given a response. Appends error received from API if we have one.
#'
#' @param obj object. Either a bad response or a try-error.
#' @param message error string. The error message which will be thrown to the user.
#'
#' @noRd
#' @importFrom httr http_status
stopBadRequest <- function(obj, message = "")
{
    # curl throws a try error and doesn't let us see the error header
    if (inherits(obj, 'try-error'))
        stop(message, call. = FALSE)

    headers <- obj$headers
    if (!is.null(headers) && isTRUE(nzchar(err <- headers$"x-errormessage")))
    {
        # Errors thrown from API when a bad status is received are in this header
        msg <- paste0(message, "\nError: ", err)
        stop(msg, call. = FALSE)
    }
    msg <- paste(message, http_status(obj)$message, sep = "\n")
    stop(msg, call. = FALSE)
}

#' Check that Displayr cloud drive is available by looking for the
#' companySecret variable.
#' @return TRUE if cloud drive is available, otherwise FALSE
#' @export
IsDisplayrCloudDriveAvailable <- function()
{
    company.secret <- get0("companySecret")
    !is.null(company.secret) && company.secret != "UNKNOWN"
}
