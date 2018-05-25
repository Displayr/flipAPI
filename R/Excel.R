#' Download and read Excel (.xlsx) file
#'
#' @param url Link to XLSX file. Can also be the path to a file on your local machine.
#' @param sheet Sheet of excel workbook to read from. By default only the first worksheet is read
#' @importFrom utils download.file
#' @importFrom readxl read_xlsx
#' @importFrom httr GET
#' @importFrom flipTransformations ParseUserEnteredTable ParseAsDataFrame
#' @param want.data.frame Whether to return a data frame instead of a matrix or vector.
#' @param want.factors Whether a text variable should be converted to a factor in a data frame.
#' @param want.col.names Whether to interpret the first row as column names in a data frame.
#' @param want.row.names Whether to interpret the first col as row names in a data frame.
#' @param us.format Whether to use the US convention when parsing dates in a data frame.
#' @param ... Other parameters tot pass to readxl::read_xlsx
#' @export
DownloadXLSX <- function(url, sheet = 1, want.data.frame = FALSE, want.factors = TRUE,
                         want.col.names = TRUE, want.row.names = FALSE, us.format = TRUE, ...)
{
    use.local <- file.exists(url)
    tmp.name <- url
    if (!use.local)
    {
        if (!(grepl("http|^ftp", url)))
            stop("URL should begin with 'http', 'https' or 'ftp'")
    
        # Convert link from a re-direct to direct URL
        if (grepl("dropbox", url))
            url <- gsub("dl=0$", "dl=1", url)
        if (grepl("google", url))
            url <- gsub("edit[#?].*", "export?format=xlsx", url)
    
        tmp.name <- tempfile(tmpdir = ".")
        tmp.file <- try(download.file(url, destfile=tmp.name, mode="wb"))
        if (inherits(tmp.file, "try-error"))
            stop("Could not download file from ", url, "\n")
    }
    res <- try(read_xlsx(tmp.name, sheet=sheet, col_names=(want.data.frame && want.col.names), ...))
    if (!use.local && inherits(res, "try-error"))
    {
        # Try to use re-direct url and try again
        retry <- GET(url)
        url <- gsub("redir?", "download", retry$url) # one-drive
        unlink(tmp.name)
        tmp.name <- tempfile(tmpdir = ".")

        tmp.file <- try(download.file(url, destfile = tmp.name, mode="wb"))
        if (inherits(tmp.file, "try-error"))
            stop("Could not download file from ", url, "\n")

        res <- try(read_xlsx(tmp.name, sheet = sheet, col_names = (want.data.frame && want.col.names), ...))
        if (inherits(res, "try-error"))
            stop("File is not a valid XLSX file\n")
    }
    if (!use.local)
        unlink(tmp.name)

    if (!want.data.frame)
    {
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
