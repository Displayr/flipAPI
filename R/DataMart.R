MAX.FILENAME.LENGTH <- 100L

#' Check if a file exists
#'
#' Check whether a file of a given name exists in the Displayr Cloud Drive.
#'
#' @param filename character string. Name of the file to search for.
#'   To reference a file in a subdirectory, use double backslashes after each folder (e.g "subdir\\file.csv").
#' @param show.warning logical scalar. Whether to show a warning when the file
#'   does not exist.
#'
#' @return TRUE if the file exists, otherwise FALSE.
#'
#' @importFrom httr HEAD add_headers
#' @importFrom utils URLencode
#'
#' @export
QFileExists <- function(filename, show.warning = TRUE)
{
    company.secret <- getCompanySecret()
    client.id <- getClientId()
    api.root <- getApiRoot("DataMartFileExists")
    res <- try(GET(paste0(api.root, "?filename=", URLencode(filename, TRUE)),
                    config=add_headers("X-Q-Company-Secret" = company.secret,
                                       "X-Q-Project-ID" = client.id)))

    if (is.null(res$status_code) || res$status_code != 200)
    {
        if (show.warning)
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
#' @param filename character string. Name of file to be opened.
#'   To reference a file in a subdirectory, use double backslashes after each folder (e.g "subdir\\file.csv").
#' @param open character string. See documentation for connections.
#' @param blocking logical. See documentation for connections.
#' @param encoding character string. See documentation for connections.
#' @param raw logical. See documentation for connections.
#' @param method character string. See documentation for connections.
#' @param mime.type character string. The mime-type of this file. If not provided, it will be interpreted from the file extension.
#' @param company.token Use this if you need to read from a different company's Displayr Cloud Drive. You need to contact Support to get this token.
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
#' Loads an *.csv, *.rds, *.xlsx, or *.sav file from the Displayr Cloud Drive and
#' converts this to an R object.
#'
#' @param filename character string. Name of the file to be opened from the Displayr Cloud Drive.
#'   To reference a file in a subdirectory, use double backslashes after each folder (e.g "subdir\\file.csv").
#' @param company.token Use this if you need to read from a different company's Displayr Cloud Drive.  You need to contact Support to get this token.
#' @param ... Other parameters to pass to read.csv.
#'
#' @return An R object
#'
#' @importFrom haven read_sav
#' @importFrom readxl read_excel
#' @importFrom httr GET add_headers write_disk
#' @importFrom utils URLencode
#' @importFrom flipU StopForUserError
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

    msg <- paste0("Sorry, there was an issue connecting to your Displayr Cloud Drive. ",
                  "Please try again later or contact support.")
    if (inherits(res, "try-error"))
        stopBadRequest(res, msg)
    else if (res$status_code != 200)
    {
        if (!QFileExists(filename, show.warning = FALSE))
            StopForUserError(
                "The data file '", filename, "' does not exist in the Displayr cloud drive. ",
                "Ensure that the data file is in the Displayr cloud drive and its name has been correctly specified.",
            )
        else
            stopBadRequest(res, msg)
    }

    type <- getResponseFileType(res)
    if (is.null(type))
        type <- getFileType(filename)

    if (is.null(type) || type == "gif")
        StopForUserError("Invalid file type specified. Only 'rds', 'csv', 'xlsx', or 'sav' files ",
                         "are supported.")


    if (file.exists(tmpfile))
    {
        if (type == "csv")
            obj <- read.csv(tmpfile, ...)
        else if (type == "rds")
            obj <- readRDS(tmpfile, ...)
        else if (type == "sav")
            obj <- read_sav(tmpfile, ...)
        else if (type == "xlsx")
            obj <- readxl::read_excel(tmpfile, ...)
        return (obj)
    }
    stop("Could not read from file.")
}

#' Save an object
#'
#' Saves an object to the Displayr Cloud Drive without any transformation.
#' Filename string must have a .csv, .rds, .xlsx, pptx, gif, or .sav extension.
#'
#' @param object object. The object to be uploaded.
#' @param filename character string. Name of the file to be written to.
#'   To reference a file in a subdirectory, use double backslashes after each folder (e.g "subdir\\file.csv").
#' @param compression.file.size.threshold numeric scalar. Files of size
#' (in bytes) larger than this value will be compressed into a zip file.
#' Defaults to NULL, in which case no compression occurs.
#' @param ... Other parameters to pass to \code{\link{write.csv}}, \code{\link{saveRDS}},
#' \code{\link{write.xlsx}}, or \code{\link{write_sav}}.
#'
#' @importFrom haven write_sav
#' @importFrom httr POST add_headers upload_file
#' @importFrom utils URLencode
#' @importFrom openxlsx write.xlsx
#' @importFrom gganimate anim_save
#' @importFrom tools file_path_sans_ext
#' @importFrom zip zip
#' @return NULL invisibly. Called for the purpose of uploading data
#' and assumed to succeed if no errors are thrown.
#' @note Saving to Powerpoint .pptx files is only possible for rpptx objects created using
#' the \code{officer} package.
#'
#' Saving to .gif file is only supported for \code{"gganim"} objects created using
#' \code{gganimate}.
#'
#' When saving to .xlsx file, \code{object} is first coerced to a data.frame using
#' \code{\link{as.data.frame}}.
#' @importFrom flipU StopForUserError
#' @export
QSaveData <- function(object, filename, compression.file.size.threshold = NULL,
                      ...)
{
    type <- getFileType(filename)
    if (is.null(type) || type == "rda")
        StopForUserError("Invalid file type specified. Please name an '.rds' or '.csv' file.")

    tmpfile <- tempfile(fileext = paste0(".", type))
    if (type == "csv")
        write.csv(object, tmpfile, ...)
    else if (type == "rds")
        saveRDS(object, tmpfile, ...)
    else if (type == "sav")
        write_sav(object, tmpfile, ...)
    else if (type == "pptx" && inherits(object, "rpptx") && requireNamespace("officer"))
        officer:::print.rpptx(object, tmpfile)
    else if (type == "pptx")
        StopForUserError("To save as a Powerpoint pptx file, the input must be created with ",
                         "officer::read_pptx()")
    else if (type == "xlsx")
        openxlsx::write.xlsx(as.data.frame(object, check.names = FALSE), tmpfile, ...)
    else if (type == "gif" && inherits(object, c("gganim", "gif_image")))
        anim_save(tmpfile, object, ...)
    else if (type == "gif")
        StopForUserError("Sorry, current gif files can only be saved to the Cloud Drive for ",
                         "outputs from the gganimate package.")

    is.compressed <- !is.null(compression.file.size.threshold) &&
                     file.size(tmpfile) > compression.file.size.threshold
    if (is.compressed) {
        # Rename the temp file to filename, and zip it
        new.tmpfile <- paste0(dirname(tmpfile), "/", filename)
        file.rename(tmpfile, new.tmpfile)
        zipfile <- tempfile(fileext = ".zip")
        zip(basename(zipfile), basename(new.tmpfile), root = dirname(zipfile))
        file.remove(new.tmpfile)
        filename <- paste0(file_path_sans_ext(filename), ".zip")
        tmpfile <- zipfile
    }

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
    has.errored <- inherits(res, "try-error")

    if (!has.errored && res$status_code == 413) # 413 comes from IIS when we violate its web.config limits
    {
        stopBadRequest(res, "Could not write to Displayr Cloud Drive. Data to write is too large.")
    }
    else if (!has.errored && res$status_code == 422)
    {
        stop("QSaveData has encountered an unknown error. ",
             "422: The file could not properly be saved. ",
             "The likely cause was an incorrect path preceding the filename.")
    }
    else if (has.errored || res$status_code != 200)
    {
        warning("QSaveData has encountered an unknown error.")
        stopBadRequest(res, "Could not save file.")
    }

    if (!is.compressed) {
        if (type != "gif")
            msg <- paste("Object uploaded to Displayr Cloud Drive To re-import object use:",
                         "   > library(flipAPI)",
                         paste0("   > QLoadData('", filename, "')"),
                         sep = "\n")
        else
            msg <- paste0("Object uploaded to Displayr Cloud Drive. To re-import select:\n ",
                          "    Image > Displayr Cloud Drive\n",
                          "then select ", sQuote(filename), " from the dropdown menu.")
        message(msg)
        invisible(msg)
    } else {
        msg <- "Object compressed into a zip file and uploaded to Displayr Cloud Drive."
        warning(msg)
        invisible(msg)
    }
}

#' Deletes a set of objects
#'
#' Deletes a list of objects by filename from the Displayr cloud drive
#'
#' @param filenames collection of character strings. Names of the files to delete.
#'   To reference a file in a subdirectory, use double backslashes after each folder (e.g "subdir\\file.csv").
#' @param company.token Use this if you need to read from a different company's Displayr Cloud Drive.  You need to contact Support to get this token.
#'
#' @importFrom httr DELETE add_headers
#' @importFrom utils URLencode
#'
#' @return NULL invisibly. Called for the purpose of deleting data
#' and assumed to succeed if no errors are thrown.
#'
#' @export
QDeleteFiles <- function(filenames, company.token = getCompanySecret())
{
    company.secret <- company.token
    api.root <- getApiRoot("DataMartBatchDelete")
    url_param_filenames <- sprintf("filenames=%s", filenames)
    filenames.string <- paste(filenames, collapse = ", ")
    res <- try(DELETE(paste0(api.root, "?", URLencode(paste(url_param_filenames, collapse="&"))),
                config=add_headers("X-Q-Company-Secret" = company.secret)))
    if (inherits(res, "try-error") || res$status_code != 200)
    {
        warning("Encountered an error deleting the following files: ", filenames.string)
        stopBadRequest(res, paste0("Could not delete files: ", filenames.string))
    }
    msg <- paste0("Successfully deleted files: ", filenames.string)
    message(msg)
    invisible(msg)
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
getApiRoot <- function(endpoint = "DataMart")
{
    region <- URLencode(get0("region", ifnotfound = ""), TRUE)
    if (region == "") stopNotDisplayr("region")
    if (region == "app")
        region <- "displayr-app"  # to avoid Dynamic Site Acceleration CDN, which limits uploads to 100MB
    api.root <- paste0("https://", region, ".displayr.com/api/", endpoint, "/")
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
#' @param filename string. Filename which we are checking.
#'
#' @importFrom mime guess_type
#' @importFrom tools file_ext
#'
#' @return string or NULL. The supported file type which we have determined the file to be.
#'
#' @noRd
getFileType <- function(filename)
{
    file.ext <- tolower(file_ext(filename))
    if (file.ext %in% c("rds", "sav", "rda", "csv", "pptx", "xlsx", "gif"))
        return(file.ext)
    if (guess_type(filename) == "text/csv")
        return("csv")
    return(NULL)
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

#' @importFrom stats setNames
uploadRScript <- function(r.code,
                          js.code = NULL,
                          filename,
                          upload = TRUE,
                          api.root = Sys.getenv("API_ROOT"),
                          company.secret = Sys.getenv("COMPANY_SECRET"),
                          client.id = Sys.getenv("CLIENT_ID"),
                          send.to = "")
{
    if (nzchar(send.to))
    {
        api.root <- Sys.getenv(paste0("API_ROOT_", send.to))
        company.secret <- Sys.getenv(paste0("COMPANY_SECRET_", send.to))
        client.id <- Sys.getenv(paste0("CLIENT_ID_  ", send.to))
    }
    credentials <- setNames(c(api.root, company.secret, client.id),
                            c("api.root", "company.secret", "client.id"))
    missing.credentials <- which(!nzchar(credentials))
    if (length(missing.credentials) > 0)
        stop("Some of your API credentials are missing: ", paste0(names(credentials[missing.credentials]), collapse = ","))

    if (missing(filename))
        stop(substitute(filename), " argument required as ",
             "filename to write in Displayr Drive is required.")
    type <- file_ext(filename)
    stopifnot("rscript filename extension required" = type == "rscript")
    if (upload)
        checkUploadPossible(api.root = api.root, company.secret = company.secret, client.id = client.id)
    if (upload && nchar(filename) > MAX.FILENAME.LENGTH) {
        filename <- shortenFilename(filename)
    }
    tmpfile <- tempfile()
    file <- file(tmpfile, "wb")
    r.filenames <- if (is.list(r.code)) unlist(r.code) else r.code
    for (current.file in r.filenames)
    {
        cat("# R code below\n", file = tmpfile, append = TRUE)
        file.append(tmpfile, current.file)
    }
    if (!is.null(js.code))
    {
        cat("\n\n// JS Code below\n\n", file = tmpfile, append = TRUE)
        js.filenames <- if (is.list(js.code)) unlist(js.code) else js.code
        for (current.file in js.filenames)
            file.append(tmpfile, current.file)
    }
    close(file)
    if (upload)
    {
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
            warning("uploadRScript has encountered an unknown error.")
            stopBadRequest(res, "Could not save file.")
        }

        msg <- paste("RScript uploaded to Displayr Cloud Drive with name", filename,
                     sep = "\n")
    } else
    {
        file.copy(tmpfile, filename, overwrite = TRUE)
        msg <- paste("RScript created locally with name", filename,
                     "In the directory",
                     getwd(),
                     sep = "\n")
    }
    message(msg)
    invisible()
}

extractRandJSFilesAtDirectory <- function(directory)
{

}

#' @title Upload a Standard R file from the Q-Wiki-Scripts repo
#' @description Take either a directory or Standard R page basename from the
#'   Q-Wiki-Scripts repo and upload it to the users custom rscript via Displayr Drive.
#' @param standard.r.input A string containing the file path to use. This needs to be either
#' \itemize{
#' \item A path to a directory containing two files, a single R and JS source file respectively.
#'   E.g. the Calculation features.
#' \item A path containing the basename of the Standard R page. Suitable for standard R pages
#'   where a directory contains multiple R and JS files.
#' }
#' @param upload A logical whether to upload the script or not (useful to check the script is good before upload).
#' @param filename A string containing the desired name of the output script.
#'   Defaults to \code{NULL} and will deduce the name based off the directory structure.
#' @importFrom tools file_path_as_absolute list_files_with_exts file_path_sans_ext
uploadStandardR <- function(standard.r.input = ".", filename = NULL, upload = TRUE)
{
    stopifnot("Specify charater string pointing to a file or directory path in the 'standard.r.page' argument'" = is.character(standard.r.input),
              "Specify a single string for the 'standard.r.page' argument pointing to a file or directory" = length(standard.r.input) == 1L)

    is.dir <- file.exists(standard.r.input) && file.info(standard.r.input)[["isdir"]]
    files.found <- if (is.dir) list_files_with_exts(standard.r.input, exts = c("js", "R")) else Sys.glob(paste0(standard.r.input, "*"))
    r.and.js.files <- Filter(function(x) file_ext(x) %in% c("R", "js"), files.found)
    standard.r.files.found <- length(r.and.js.files) == 2L && all(file_ext(r.and.js.files) %in% c("js", "R"))
    stopifnot("standard.r.input must point to a directory or the basename of the standard R files" = standard.r.files.found)
    r.and.js.files <- vapply(r.and.js.files, file_path_as_absolute, character(1L))
    directory <- dirname(r.and.js.files[1L])
    od <- setwd(directory)
    on.exit(setwd(od))
    r.file <- r.and.js.files[endsWith(r.and.js.files, ".R")]
    js.file <- r.and.js.files[endsWith(r.and.js.files, ".js")]
    if (is.null(filename))
    {
        git.path <- git2r::discover_repository()
        if (is.null(git.path))
            stop("Current provided path is not part of a git repo")
        git.path <- gsub(".git", "", git.path)
        standard.r.page <- gsub(git.path, "", getwd())
        if (!is.dir)
            standard.r.page <- file.path(standard.r.page, file_path_sans_ext(r.and.js.files[1L]))
        filename <- paste0(paste0(splitPath(standard.r.page), collapse = " - "),
                           ".rscript")
    }
    type <- file_ext(filename)
    stopifnot("rscript filename extension required" = type == "rscript",
              "javascript source file not found at specified directory" = !is.null(js.file),
              "R source file not found at specified directory" = !is.null(r.file))
    if (!upload)
        filename <- file.path(od, filename)
    uploadRScript(r.file, js.file, filename, upload = upload)
}

splitPath <- function(path)
    strsplit(path, "^(?=/)(?!//)|(?<!^)(?<!^/)/", perl = TRUE)[[1L]]

#' @title Upload a QScript file to Displayr drive or collate many JS files.
#' @description Take an arbitrary number of js files and either upload them as
#'   a custom qscript via Displayr Drive or save them locally as a single file.
#' @param ... An arbitrary number of strings that contain the paths to js files
#'   to construct the QScript.
#' @param filename Name of the output file QScript.
#'   If only a single file is provided then it is deduced by the input file name by default.
#'   Otherwise the filename argument must be provided.
#' @param check.include.web A logical to remove any includeWeb calls if the input files
#'   are used in the construction. E.g. if testing a Calculation feature the script might use
#'   a custom QScript Functions for Calculations.js and then call one of those functions with
#'   includeWeb. If any matches are found like this, then the line with the includeWeb is omitted.
#' @param upload A logical whether to upload the script or not (useful to check the script is good before upload).
#' @param api.root API root URL obtained by running \code{flipAPI:::getApiRoot()} in a
#' Calculation in Displayr
#' @param company.secret A string containing the appropriate company secret to access the required Displayr drive
#' @param client.id Client ID obtained by running \code{flipAPI:::getApiRoot()} in a
#' Calculation in Displayr
#' @param send.to Character string the can be used to more conveniently specify
#' an alternative company (i.e. \code{api.root}, \code{company.secret}, and
#' \code{client.id}) to upload the QScript to. See the Details.
#' @details If the \code{send.to} argument is provided, the inputs for \code{api.root},
#' \code{company.secret}, and \code{client.id} are ignored and it is assumed that
#' the user has additional environment variables setup that are used instead. For
#' example, if \code{send.to = "JANE"}, then the function uses
#' \code{Sys.getenv("COMPANY_SECRET_JANE")} for \code{company.secret}
#' (and similarly for \code{client.id} and \code{api.root}).
uploadQScript <- function(..., filename = NULL,
                          check.include.web = TRUE,
                          upload = TRUE,
                          api.root = Sys.getenv("API_ROOT"),
                          company.secret = Sys.getenv("COMPANY_SECRET"),
                          client.id = Sys.getenv("CLIENT_ID"),
                          send.to = "")
{
    if (nzchar(send.to))
    {
        api.root <- Sys.getenv(paste0("API_ROOT_", send.to))
        company.secret <- Sys.getenv(paste0("COMPANY_SECRET_", send.to))
        client.id <- Sys.getenv(paste0("CLIENT_ID_  ", send.to))
    }
    script.files <- pairlist(...)
    stopifnot("One or more paths to files are required to create a .qscript output file" = !is.null(script.files))
    all.character <- all(vapply(script.files, is.character, logical(1L)))
    stopifnot("All provided file paths should be character strings" = all.character)
    files.exist <- vapply(script.files, file.exists, logical(1L))
    if (any(!files.exist))
        stop("Cannot find the input file(s): ", unlist(script.files)[!files.exist])
    if (is.null(filename))
    {
        git.path <- lapply(script.files, git2r::discover_repository)
        if (is.null(unlist(git.path)))
            stop("Current provided path is not part of a git repo")
        git.path <- unique(unlist(git.path))
        git.path <- gsub(".git", "", git.path)
        script.files <- lapply(script.files, normalizePath)
        qscript.page.candidates <- gsub(git.path, "", script.files)
        correct.slash <- .Platform$file.sep
        qscript.page.candidates <- gsub(paste0("src", correct.slash), "", qscript.page.candidates)
        leading.qscript <- paste0("^QScript", correct.slash)
        qscript.page.candidates <- Filter(function(x) grepl(leading.qscript, x),
                                          qscript.page.candidates)
        if (length(qscript.page.candidates) != 1L)
            stop("Cannot deduce correct qscript output name from available input files. ",
                 "Please provide a filename before re-running")
        qscript.page <- sub(leading.qscript, "", qscript.page.candidates)
        feature <- gsub(".js$", "", qscript.page)
        filename <- paste0(paste0(splitPath(feature), collapse = " - "), ".qscript")
    }
    if (upload && nchar(filename) > MAX.FILENAME.LENGTH) {
        filename <- shortenFilename(filename, MAX.FILENAME.LENGTH)
    }
    type <- file_ext(filename)
    stopifnot("qscript filename extension required" = type == "qscript")
    if (upload)
        checkUploadPossible(api.root = api.root, company.secret = company.secret, client.id = client.id)
    tmpfile <- tempfile()
    file <- file(tmpfile, "wb")
    if (!check.include.web)
    {
        for (current.file in unlist(script.files))
            file.append(tmpfile, current.file)
    }
    else
    {
        files.used <- vapply(tools::file_path_sans_ext(script.files),
                             function(x) rev(splitPath(x))[1],
                             character(1L),
                             USE.NAMES = FALSE)
        code.lines <- lapply(script.files, readLines)
        includeWeb.called <- lapply(code.lines, function(x) grepl("includeWeb\\(", x, perl = TRUE))
        if (any(unlist(includeWeb.called)))
        {
            include.web.lines <- mapply(function(lines, include.lines) lines[include.lines],
                                        code.lines, includeWeb.called,
                                        SIMPLIFY = FALSE)
            files.used.patt <- paste0(files.used, collapse = "|")
            dev.includeweb <- lapply(include.web.lines,
                                     function(x) grepl(files.used.patt, x))
            if (any(unlist(dev.includeweb)))
            {
                code.lines <- mapply(function(code.lines, include.web.lines, dev.includeweb, ind) {
                    if (any(dev.includeweb))
                    {
                        code.call <- paste0(code.lines[which(include.web.lines)[dev.includeweb]], sep = '\n')
                        warning("Line of code calling \n\n", code.call,
                                "\nhas been removed since this file is used as an input file in the QScript.",
                                call. = FALSE)
                        code.lines <- code.lines[-which(include.web.lines)[dev.includeweb]]
                    }
                    code.lines
                },
                code.lines, includeWeb.called, dev.includeweb, seq_along(code.lines),
                SIMPLIFY = FALSE)
            }
        }
        lapply(code.lines, cat, sep = "\n", file = file)
    }
    close(file)
    if (upload)
    {
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

        msg <- paste("QScript uploaded to Displayr Cloud Drive with name", filename,
                     sep = "\n")
    }
    else
    {
        file.copy(tmpfile, filename, overwrite = TRUE)
        msg <- paste("QScript created locally with name", filename,
                     "In the directory",
                     getwd(),
                     sep = "\n")
    }
    message(msg)
    invisible()
}

checkUploadPossible <- function(api.root, company.secret, client.id)
{
    stopifnot("api.root argument required to upload" = !is.null(api.root),
              "company.secret argument required to upload" = !is.null(company.secret),
              "client.id argument required to upload" = !is.null(client.id))
}


shortenFilename <- function(filename, max.filename.length) {
    if (!any(grepl(" - ", filename)))
        stop("Cannot reduce filename length automatically since no ' - '",
             "characters appear in the filename. Please manually specify a shorter filename.")
    split.name <- strsplit(filename, " - ")[[1L]]
    while (nchar(filename) > max.filename.length) {
        split.name <- split.name[-1L]
        filename <- paste0(split.name, collapse = " - ")
    }
    filename
}
