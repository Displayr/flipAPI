#' Export R object to Dropbox
#' @param object R object to export
#' @param token Dropbox access token with read and write access.
#' @param file Name of output file. If no filename is supplied, by default the object will be saved to a file
#'    named \code{<object>.rds}. This will overwrite existing files in your dropbox with the same name.
#'    When multiple copies of the same object under different conditions are saved, it may be preferrable to
#'    manually specify the filename.
#' @param verbose Show status of export process.
#' @param update Set to \code{TRUE} if object should be automatically re-exported from Displayr when object is updated.
#' @importFrom httr PUT add_headers upload_file
#' @export

ExportToDropbox <- function(object, token, file=NA, verbose=TRUE, update=TRUE)
{
    if (update)
        message("R output expires in 600")
    if (is.na(file))
        file <- paste(as.character(substitute(object)), ".rds", sep="")
    saveRDS(object, file=file)
    if (verbose)
         cat("Writing object to", file, "\n")
    
    put_url <- sprintf("https://content.dropboxapi.com/1/files_put/auto/%s?param=overwrite=true", file)
    pp <- try(PUT(put_url, 
    config=add_headers("Authorization" = sprintf("Bearer %s", token)), 
    body=upload_file(file)))
    if (verbose)
        print(pp)
    if (verbose && !inherits(pp, "try-error"))
    {
        cmd1 <- sprintf("UpdateObject(%s, <project api key>)", 
                        as.character(substitute(object)))
        cmd2 <- sprintf("ImportFromDropbox('%s', <dropbox token>)", file)
        cat("To re-import object use:\n   >", cmd1, "\n   >", cmd2, "\n")
    }
    invisible(file.remove(file))
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
    invisible(file.remove(localfile))
    return(obj)
}