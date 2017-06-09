#' Download and read Excel (.xlsx) file
#'
#' @param url Link to XLSX file. Note that this should be a direct link (See \url{https://milanaryal.com/direct-linking-to-your-files-on-dropbox-google-drive-and-onedrive/}.
#' @param sheet Sheet of excel workbook to read from. By default only the first worksheet is read
#' @importFrom utils download.file
#' @importFrom readxl read_xlsx
#' @export
DownloadXLSX <- function(url, sheet = 1)
{
    # Try to change link so that it directly download NOT preview
    # For Dropbox
    url <- gsub("dl=0$", "dl=1", url)
    
    tmp.name <- tempfile(tmpdir=".")
    tmp.file <- try(download.file(url, destfile=tmp.name, mode="wb"))
    if (inherits(tmp.file, "try-error"))
        stop("Could not download file:", url, "\n")
    res <- try(read_xlsx(tmp.name, sheet=sheet))
    if (inherits(res, "try-error"))
        stop("Could not read file. Check that file is an a valid XLSX file and that url is a direct link.")
    else
        unlink(tmp.name)
    return(res)
}
    
    