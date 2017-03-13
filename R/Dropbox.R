#' Export R object to Dropbox
#' @param object R object to export
#' @param token Dropbox access token with read and write access.
#' @param file Name of output file. If no filename is supplied, by default the object will be saved to a file
#'    named \code{<object>.rds}. This will overwrite existing files in your dropbox with the same name.
#'    When multiple copies of the same object under different conditions are saved, it may be preferrable to
#'    manually specify the filename.
#' @param verbose Show status of export process.
#' @importFrom httr PUT add_headers upload_file
#' @export

ExportToDropbox <- function(object, token, file=NA, verbose=TRUE)
{
    if (is.na(file))
        file <- paste(as.character(substitute(object)), ".rds", sep="")
    saveRDS(object, file=file)
    if (verbose)
         cat("Writing object to", file, "\n")
    
    put_url <- sprintf("https://content.dropboxapi.com/1/files_put/auto/%s?param=overwrite=true", file)
    pp <- PUT(put_url, 
    config=add_headers("Authorization" = sprintf("Bearer %s", token)), 
    body=upload_file(file))
    if (verbose)
        print(pp)
}

#' Import R object from file in Dropbox
#' @param importfile Name of file that the object was saved to. This is generally of the form \code{<object>.rds}.
#' @param token Dropbox access token with read and write access.
#' @importFrom httr GET add_headers write_disk
#' @export

ImportFromDropbox <- function(importfile, token)
{
    localfile = "tmp.rds"
    gurl <- sprintf("https://content.dropboxapi.com/1/files/auto/%s", importfile)
    gg <- GET(gurl, 
               config=add_headers("Authorization" = sprintf("Bearer %s", token)),
               write_disk(localfile))
    obj <- readRDS(localfile)
    return(obj)
}