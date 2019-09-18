#' NOTE: These functions are currently only supported for use through DisplayR. The API cannot be accessed externally. 

url <- "http://localhost/API/DataMart" # JUST USE APP.DISPLAYR.COM? 
companySecret <- "141e27d4-19e1-47cd-86fc-9604a29141cf"

companySecret <- ifelse(exists("companySecret"), companySecret, "")
if (companySecret == "")
    message("Warning: This package is currently only supported for use through DisplayR. You will not be able to run any of these functions.")

#' Check if a file exists in the Data Mart
#' @param filename character string. Name of the file to search for
#' @return Returns true if the file exists.
#' @importFrom httr HEAD add_headers
#' @export
QFileExists <- function(filename) 
{
    res <- try(HEAD(paste0(url, "?filename=", filename), 
                    config=add_headers("x-q-company-secret" = companySecret)))
    if (is.null(res$status_code) || res$status_code != 200)
    {
        msg <- ifelse(res$status_code == 404 , "File not found.", "Could not connect to Data Mart.")
        warning(msg)
        return (FALSE)
    } else 
    {
        message("File was found.")
        return (TRUE)
    }
}

#' Opens a file in the data mart for reading or writing
#' @param filename character string. Name of file to be opened
#' @param open character string. A description of how to open the connection
#' @param blocking logical. Whether or not the connection blocks can be specified
#' @param encoding The name of the encoding to be assumed.
#' @param raw logical. Whether this connection should be treated as a byte stream
#' @param method character string. See documentation for connections
#' @return Returns a connection to a data blob
#' @importFrom curl curl handle
#' @importFrom httr POST upload_file add_headers
#' @inportFrom mime file_type
#' @export
QFileOpen <- function(filename, open = "r", blocking = TRUE, 
                      encoding = getOption("encoding"), raw = FALSE, 
                      method = getOption("url.method", "default"))
{
    mode <- tolower(open)
    if (mode == "r" || mode == "rb") 
    {
        h <- new_handle()
        handle_setheaders(h,
            "X-Q-Company-Secret" = companySecret
        )
        conn <- try(curl(paste0(url,"/DataMart?filename=", filename),
                        open = mode,
                        handle = h), 
                    silent = TRUE)
        
        if (!inherits(conn,"connection"))
        {
            msg <- ifelse(grepl("404", conn), "File not found.", "Could not connect to Data Mart.")
            stop(msg)
        }
        
        # to allow functions to parse this 'like' a url connection
        # e.g. so readRDS will wrap this in gzcon when reading
        class(conn) <- append(class(conn), "url")
        return (conn)
    } else if (mode == "w" || mode == "wb") 
    {
        tmpfile <- paste0(tempfile(), ".", file_ext(filename))
        conn <- file(tmpfile, mode, blocking, encoding, raw, method)
        class(conn) = append("qpostconn", class(conn))
        
        #do error checking here? e.g. HTTP 400 
        
        # store attributes for later access
        attr(conn, "tmpfile") <- tmpfile
        attr(conn, "filename") <- filename

        return (conn)
    }
    warning("Invalid mode - please use either 'r', 'rb','w' or 'wb'.")
}

# Overload so that we post to the Data Mart on close
close.qpostconn = function(conn) 
{
    close.connection(conn)
    filename <- attr(conn, "filename")
    tmpfile <- attr(conn, "tmpfile")
    
    res <- try(POST(paste0(url, "?filename=", filename),
                config = add_headers("Content-Type" = mime::guess_type(filename),
                                     "X-Q-Company-Secret" = companySecret),
                encode = "raw",
                body = upload_file(tmpfile)))
    
    
    
    if (inherits(res, "try-error") || res$status_code != 200)
        warning("Could not write to data mart.")
    else 
        message("File was written successfully.")
    
    invisible(file.remove(tmpfile))
}

#' Loads rds data from the data mart into an object
#' @param filename character string. Name of the file to be opened from the Data Mart
#' @return Returns an R object
#' @importFrom httr GET add_headers write_disk
#' @export
QLoadData <- function(filename) 
{
    if (file_ext(filename) != "rds") 
        stop("Can only load data from rds objects.")
    
    tmpfile <- tempfile()

    req <- try(GET(paste0(url, "?filename=", filename),
               config=add_headers("X-Q-Company-Secret" = companySecret),
               write_disk(tmpfile, overwrite = TRUE)))
    
    if (inherits(req, "try-error") || req$status_code != 200)
    {
        msg <- ifelse(req$status_code == 404, "File not found.", "Could not connect to Data Mart.")
        stop(msg)
    }
        
    obj <- readRDS(tmpfile)
    invisible(file.remove(tmpfile))
    return (obj)
    
}

#' Saves an object into an rds file blob
#' @param object object. The object to be uploaded
#' @param filename character string. Name of the file to be written to
#' @importFrom httr POST add_headers upload_file
#' @export 
QSaveData <- function(object, filename)
{
    if (file_ext(filename) == "") 
        filename <- append(filename, ".rds")
    
    if (file_ext(filename) != "rds")
        stop("File-type must be .rds")
    
    tmpfile <- tempfile()
    saveRDS(object, tmpfile)
    
    res <- try(POST(paste0(url, "?filename=", filename), 
                config = add_headers("Content-Type" = "application/x-gzip", # default is gzip for saveRDS
                                     "X-Q-Company-Secret" = companySecret),
                encode = "raw",
                body = upload_file(tmpfile)))
    
    invisible(file.remove(tmpfile))
    
    if (inherits(res, "try-error") || res$status_code != 200)
        stop("Could not save file")
    
    message(paste("Object uploaded to Data Mart To re-import object use:",
                  "   > library(flipAPI)",
           paste0("   > QLoadData('", filename, "')"),
           sep = "\n"))
}


