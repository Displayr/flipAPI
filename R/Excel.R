#' Converts URL from cloud storage to direct link
#' 
#' @description Currently only supports links from Dropbox, OneDrive and Google Drive
#' @param url String; url to convert
#' @export
GetDirectLink <- function (url)
{
    # Convert link from a re-direct to direct URL
    if (grepl("dropbox", url))
        return(gsub("dl=0$", "dl=1", url))
    if (grepl("doc.google.com", url))
        return(gsub("edit[#?].*", "export?format=xlsx", url))
    if (grepl("drive.google.com", url))
        return(gsub("/open?", "/uc?export=download&", url, fixed = TRUE))
    if (grepl("sharepoint.com", url) && substr(url, nchar(url), nchar(url)) != "1")
        return(paste0(url, "?download=1"))
    return(url)
}

#' Download and read Excel (.xlsx) file
#'
#' @param url Link to XLSX file. Can also be the path to a file on your local machine.
#' @param sheet Sheet of excel workbook to read from. By default only the first worksheet is read
#' @importFrom utils download.file
#' @importFrom readxl read_excel
#' @importFrom httr GET
#' @importFrom flipTransformations ParseUserEnteredTable ParseAsDataFrame
#' @param want.data.frame Whether to return a data frame instead of a matrix or vector. If this is set to \code{FALSE} (default), then the function will return the simplest data structure it can.
#' @param want.factors Whether a text variable should be converted to a factor in a data frame.
#' @param want.col.names Whether to interpret the first row as column names in a data frame. This is ignored if \code{want.data.frame} is \code{FALSE}.
#' @param want.row.names Whether to interpret the first col as row names in a data frame. This is ignored if \code{want.data.frame} is \code{FALSE}.
#' @param us.format Whether to use the US convention when parsing dates in a data frame.
#' @param ... Other parameters to pass to readxl::read_excel
#' @export
DownloadXLSX <- function(url, sheet = 1, want.data.frame = FALSE, want.factors = TRUE,
                         want.col.names = NULL, want.row.names = NULL, us.format = TRUE, ...)
{
    use.local <- file.exists(url)
    tmp.name <- url
    if (want.data.frame)
    {
        if (is.null(want.col.names))
            want.col.names <- TRUE
        if (is.null(want.row.names))
            want.row.names <- FALSE
    }
    if (!use.local)
    {
        if (!(grepl("http|^ftp", url)))
            stop("URL should begin with 'http', 'https' or 'ftp'")
    
        url <- GetDirectLink(url) 
        tmp.name <- tempfile(tmpdir = ".")
        tmp.file <- try(download.file(url, destfile = tmp.name, mode = "wb"))
        if (inherits(tmp.file, "try-error"))
            stop("Could not download file from ", url, "\n")
    }
    res <- suppressMessages(try(read_excel(tmp.name, sheet = sheet, col_names = (want.data.frame && want.col.names), ...)))
    if (!use.local && inherits(res, "try-error"))
    {
        # Try to use re-direct url and try again
        retry <- GET(url)
        url <- gsub("redir?", "download", retry$url) # one-drive
        unlink(tmp.name)
        tmp.name <- tempfile(tmpdir = ".")

        tmp.file <- try(download.file(url, destfile = tmp.name, mode="wb"))
        if (inherits(tmp.file, "try-error"))
        {
            unlink(tmp.name)
            stop("Could not download file from ", url, "\n")
        }
        
        res <- try(read_excel(tmp.name, sheet = sheet, col_names = (want.data.frame && want.col.names), ...))
        if (inherits(res, "try-error"))
        {
            unlink(tmp.name)
            stop("File is not a valid XLSX or XLS file\n")
        }
    }
    if (!use.local)
        unlink(tmp.name)

    if (!want.data.frame)
    {
        if (!is.null(want.row.names))
            warning("Parameter 'want.row.names' is ignored want 'want.data.frame' is false.")
        if (!is.null(want.col.names))
            warning("Parameter 'want.col.names' is ignored want 'want.data.frame' is false.")
        res <- as.matrix(res)
        res[which(is.na(res))] <- ""
        dimnames(res) <- NULL
        return(ParseUserEnteredTable(res, want.data.frame = FALSE))
    }

    res.formatted <- ParseAsDataFrame(res, warn = TRUE, want.factors = want.factors, 
                                      want.col.names = FALSE, want.row.names = want.row.names,
                                      us.format=us.format)
    return(res.formatted)
}
