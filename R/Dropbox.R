#' Check that token is valid
#' @return Returns a true if token is valid.
#' @param token Dropbox API token to check.
#' @export
CheckDropboxToken <- function(token)
{
    url <- "https://api.dropboxapi.com/2/users/get_current_account"
    pp <- try(POST(url, config=add_headers('Authorization' = sprintf("Bearer %s", token))))
    if (is.null(pp$status_code) || pp$status_code != 200)
    {
        warning("Could not retrieve account information. Token may be invalid or not valid for the Dropbox API v2.")
        return(FALSE)
    }
    else
    {
        message("Token is valid.")
        return(TRUE)
    }
}



#' Export R object to Dropbox
#' @param object R object to export
#' @param token Dropbox access token with read and write access.
#' @param file Name of output file. If no filename is supplied, by default the object will be saved to a file
#'    named \code{<object>.rds}. This will overwrite existing files in your dropbox with the same name.
#'    When multiple copies of the same object under different conditions are saved, it may be preferrable to
#'    manually specify the filename.
#' @param reexport.seconds Time in seconds (must be greater than 600) after which object will be re-exported. This option works only in Displayr. By default this option is disabled.
#' @importFrom httr POST add_headers upload_file
#' @export

ExportToDropbox <- function(object, token, file=NA, reexport.seconds = -1)
{
    # Displayr server ignores expiry if less than 600
    if (reexport.seconds > 600)
        message(sprintf("R output expires in %d seconds with wakeup", round(reexport.seconds)))
    if (is.na(file))
        file <- paste(as.character(substitute(object)), ".rds", sep="")
    saveRDS(object, file=file)
    
    #put_url <- sprintf("https://content.dropboxapi.com/1/files_put/auto/%s?param=overwrite=true", file)
    put_url <- "https://content.dropboxapi.com/2/files/upload"
    pp <- try(POST(put_url, 
        config=add_headers("Authorization" = sprintf("Bearer %s", token),
                           "Dropbox-API-Arg" = paste0("{\"path\": \"/", file,
                                    "\",\"mode\": \"add\",\"autorename\": true,\"mute\": false}"),
                           "Content-Type" = "application/octet-stream"), 
        body=upload_file(file)))
    
    returnMsg <- "Could not upload object. Check that dropbox token is correct."
    if (!inherits(pp, "try-error") && pp$status_code == 200)
    {
        cmd <- sprintf("ImportFromDropbox('%s', <dropbox token>)", file)
        returnMsg <- paste("Object uploaded to dropbox. To re-import object use:\n   > library(flipAPI)\n   >", cmd, "\n")
    } else if (!inherits(pp, "try-error"))
    {
        res <- content(pp)
        returnMsg <- res[[1]]
    }
    invisible(file.remove(file))
    returnMsg
}

#' Import R object from file in Dropbox
#' @param importfile Name of file that the object was saved to. This is generally of the form \code{<object>.rds}.
#' @param token Dropbox access token with read and write access.
#' @importFrom httr POST add_headers write_disk content
#' @export

ImportFromDropbox <- function(importfile, token)
{
    localfile = "tmp.rds"
    #url <- sprintf("https://content.dropboxapi.com/1/files/auto/%s", importfile)
    url <- "https://content.dropboxapi.com/2/files/download"
    pp <- try(POST(url, 
               config=add_headers("Authorization" = sprintf("Bearer %s", token),
                                  "Dropbox-API-Arg" = paste0("{\"path\": \"/", importfile, "\"}")),
               write_disk(localfile, overwrite=TRUE)))
    
    if (inherits(pp, "try-error") || pp$status_code == 500)
        stop("Could not import from Dropbox. Check the dropbox token")
    
    if (pp$status_code != 200)
        stop("Could not import from Dropbox. Check the name of the import file")
    
    obj <- readRDS(localfile)
    invisible(file.remove(localfile))
    return(obj)
}