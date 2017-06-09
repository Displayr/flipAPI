#' Download and read Excel (.xlsx) file
#'
#' @param url Link to XLSX file.
#' @param sheet Sheet of excel workbook to read from. By default only the first worksheet is read
#' @importFrom utils download.file
#' @importFrom readxl read_xlsx
#' @importFrom httr GET
#' @export
DownloadXLSX <- function(url, sheet = 1)
{
    # Convert link from a re-direct to direct URL
    if (grepl("dropbox", url))
        url <- gsub("dl=0$", "dl=1", url)
    if (grepl("google", url))
        url <- gsub("edit[#?].*", "export?format=xlsx", url)
    
    tmp.name <- tempfile(tmpdir=".")
    tmp.file <- try(download.file(url, destfile=tmp.name, mode="wb"))
    if (inherits(tmp.file, "try-error"))
        stop("Could not download file from ", url, "\n")
    res <- try(read_xlsx(tmp.name, sheet=sheet))
    if (inherits(res, "try-error"))
    {
        # Try to use re-direct url and try again
        retry <- GET(url)
        url <- gsub("redir?", "download", retry$url) # one-drive
        unlink(tmp.name)
        tmp.name <- tempfile(tmpdir=".")
        
        tmp.file <- try(download.file(url, destfile=tmp.name, mode="wb"))
        if (inherits(tmp.file, "try-error"))
            stop("Could not download file from ", url, "\n")
        res <- try(read_xlsx(tmp.name, sheet=sheet))
        if (inherits(res, "try-error"))
            stop("File is not a valid XLSX file\n")
    }
    unlink(tmp.name)
    return(res)
}
    
    