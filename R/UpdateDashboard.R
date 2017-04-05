#' Update R object in a dashboard using an API post request
#' @param object String containing name of R object to export
#' @param document.key API key of the document in Displayr. This can be obtained by navigating to your documents in \code{https://app.displayr.com}, and clicking on the API button next to the specified dashboard.
#' @importFrom httr POST
#' @export

UpdateObject <- function(object, document.key)
{
    message("R output expires in 600")
    url <- sprintf("https://app.displayr.com/API/RunScript?project=%s",
                   document.key)
    script <- sprintf("
    var find_ritem = function (reference_name) {
       var find_ritem_recurse = function (reference_name, sub_items) {
           for (var i = 0; i < sub_items.length; i++) {
               var item = sub_items[i];
               if (item.type === 'R Output') {
                   if (item.referenceName === reference_name) {
                       return item;
                   }
               }
               if (item.type === 'ReportGroup') {
                   var found_item = find_ritem_recurse(reference_name, item.subItems);
                   if (found_item)
                       return found_item;
               }
           }
           return null;
       }
       return find_ritem_recurse(reference_name, project.report.subItems);
    };
    
    var item_user_api_post = find_ritem('%s');
    item_user_api_post.update();", object)
    req <- try(POST(url, body=script, encode="raw"))
    returnMsg <- "Object could not be updated"
    
    if (!inherits(req, "try-error"))
    {
        if(req$status_code == 200)
            returnMsg <- "Object updated"
        if (any(grepl("Cannot read property 'update'", req$headers$'x-errormessage')))
            returnMsg <- "Object does not exist in project. Check that you are using the object name (and not the label)."
        if (any(grepl("it is not a GUID", req$headers$'x-errormessage')))
            returnMsg <- "Invalid document API key"
    }
    returnMsg
}