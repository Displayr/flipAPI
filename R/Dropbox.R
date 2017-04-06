#' Export R object to Dropbox
#' @param object R object to export
#' @param token Dropbox access token with read and write access.
#' @param file Name of output file. If no filename is supplied, by default the object will be saved to a file
#'    named \code{<object>.rds}. This will overwrite existing files in your dropbox with the same name.
#'    When multiple copies of the same object under different conditions are saved, it may be preferrable to
#'    manually specify the filename.
#' @param reexport.seconds Time in seconds after which object will be re-exported. This option works only in Displayr. See \link{UpdateObject} for more details.
#' @importFrom httr PUT add_headers upload_file
#' @export

ExportToDropbox <- function(object, token, file=NA, reexport.seconds = -1)
{
    if (reexport.seconds > 0)
        message(sprintf("R output expires in %d", round(reexport.seconds)))
    if (is.na(file))
        file <- paste(as.character(substitute(object)), ".rds", sep="")
    saveRDS(object, file=file)
    
    put_url <- sprintf("https://content.dropboxapi.com/1/files_put/auto/%s?param=overwrite=true", file)
    pp <- try(PUT(put_url, 
    config=add_headers("Authorization" = sprintf("Bearer %s", token)), 
    body=upload_file(file)))
    
    returnMsg <- "Could not upload object. Check that dropbox token is correct."
    if (!inherits(pp, "try-error") && pp$status_code == 200)
    {
        cmd1 <- sprintf("TriggerObjectUpdate('%s', <document api key>)", 
                        as.character(substitute(object)))
        cmd2 <- sprintf("ImportFromDropbox('%s', <dropbox token>)", file)
        returnMsg <- paste("Object uploaded to dropbox. To re-import object use:\n   > library(flipAPI)\n   >", cmd1, "\n   >", cmd2, "\n")
    }
    invisible(file.remove(file))
    returnMsg
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
    gg <- try(GET(gurl, 
               config=add_headers("Authorization" = sprintf("Bearer %s", token)),
               write_disk(localfile, overwrite=TRUE)))
    
    if (inherits(gg, "try-error") || gg$status_code == 500)
        stop("Could not import from Dropbox. Check the dropbox token")
    
    if (gg$status_code != 200)
        stop("Could not import from Dropbox. Check the name of the import file")
    
    obj <- readRDS(localfile)
    invisible(file.remove(localfile))
    return(obj)
}