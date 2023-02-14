#' Upload a metric to Factbase.
#'
#' @param data A data.frame with at least three columns, being (in order) for
#'   * measurements (must be numeric).  The column name will name the metric.
#'   * date/time (Data Science to specify the formats that normal users would expect to be supported
#'     in where there will be a lot of data; supply automatic conversion if you think that is
#'     reasonable).  The column name is unimportant.
#'   * dimension 1 (coerced to character).  The column name will be used for the dimension name.
#'   * …
#'   * dimension n
#' @param token A guid that identifies and authenticates the request.  Talk to Oliver if you need
#'   one of these.
#' @param mode One of "replace_all", "append" or "append_or_update" See comments for
#'   FactPostUpdateType.
#' @param aggregation One of "none", "minimum", "maximum", "sum", "average", "first", "last".
#' @param definition A detailed explanation of the meaning and derivation of the metric.
#' @param hyperlink A link to a web page where more can be read about the metric.
#' @param save_failed_json_to If set then the JSON for this request will be saved to the named file
#'   in your Displayr Drive.  This is helpful when trying to reproduce a problem for debugging.
#' 
#' @return The value of `data` that was passed in, so caller can see data uploaded if this is the
#'   last call in R code.
#'
#' @importFrom RJSONIO toJSON
#' @export
UploadMetricToFactbase <- function(data, token, mode="replace_all", aggregation="sum",
        definition=NULL, hyperlink=NULL, save_failed_json_to=NULL) {
    if (!is.data.frame(data))
        # Include the data in the error message because often this will be an SQL error,
        # returned instead of a data.frame.  This makes it easier for users to spot the problem.
        stop(paste("'data' must be a data.frame, but got", format(data)))
    if (length(is.data.frame) == 0)
        stop("There must be at least one column in 'data'")
    if (!is.numeric(data[[1]]))
        stop("The first column in 'data' must contain the metric, and be numeric")
    if (!(aggregation %in% c("none", "minimum", "maximum", "sum", "average", "first", "last")))
        stop(paste("Unknown 'aggregation':", aggregation))

    # Build dimensions.
    original_data <- data
    data <- c(data)  # avoid modifying caller's data.frame
    n <- names(data)
    metric_name <- n[1]
    if (n[2] == "_When") {
        dimension_columns <- 3:length(n)
        time_dimension <- list(
            list(
                name="_When",
                dimensionType="in_data",
                valueType="datetime"
            )
        )
        if (inherits(data[[2]], "Date"))
            data[[2]] <- as.POSIXct(data[[2]])
        if (!inherits(data[[2]], "POSIXct"))
            stop("The _When column must be of class POSIXct")
        data[[2]] <- as.numeric(data[[2]]) * 1000  # convert from POSIXct (seconds since 1970)
                                                  # to JavaScript (ms since 1970)
    } else {
        dimension_columns <- 2:length(n)
        time_dimension <- list()
    }
    dimension_data <- lapply(data[dimension_columns], function(column) { as.character(column)})
    dimension_names <- n[dimension_columns]
    text_dimensions <- mapply(function(v, name) {
        list(
            name=name,
            dimensionType="in_data",
            valueType="text")
    }, dimension_data, dimension_names, SIMPLIFY=FALSE, USE.NAMES=FALSE)
    dimensions <- c(time_dimension, text_dimensions)

    # Structure observations as a list of lists for toJSON.
    list_for_observation <- function(...) {
        list(...)
    }
    mapply_args <- c(list_for_observation, unname(data), list(SIMPLIFY=FALSE, USE.NAMES=FALSE))
    observations <- do.call("mapply", mapply_args)

    # Make HTTP request
    metric <- list(
        name=metric_name,
        valueType="real",
        aggregation=aggregation
    )
    if (!is.null(definition))
        metric$definition <- definition
    if (!is.null(hyperlink))
        metric$hyperlink <- hyperlink
    body <- toJSON(list(
        metric=metric,
        update=mode,
        dimensions=dimensions,
        data=observations
    ), digits=15, .na="null")  # May need in future: .inf="null"
    post_to_factbase(body, token, save_failed_json_to)

    original_data
}

#' @importFrom httr POST timeout add_headers content
post_to_factbase <- function(body, token, save_failed_json_to) {
    message(paste0("POSTing ", nchar(body), " characters from ", Sys.info()["nodename"]))
    url <- "https://factbase.azurewebsites.net/fact"
    r <- POST(url, body = body, encode = "json", add_headers(`x-facttoken` = token), timeout(3600))
    if (r$status_code != 200) {
        if (!is.null(save_failed_json_to)) {
            connection <- QFileOpen(save_failed_json_to, "w",
                mime.type="application/json")
            writeLines(body, connection)
            close(connection)
        }
        stop(paste0(r$status_code, ": ", content(r, "text")))
    }
}

#' Upload a relationship to Factbase.
#'
#' @param data A data.frame with at least two columns, each of which should be coerced to character
#'   vectors.  The first column is the dimension we are mapping from.  Subsequent columns contain
#'   labels in dimensions that we are mapping to.  The names of these columns to be used as the
#'   names of these dimensions.
#' @param token A guid that identifies and authenticates the request.  Talk to Oliver if you need
#'   one of these.
#' @param mode One of "replace_all", "append" or "append_or_update" See comments for
#'   FactPostUpdateType.
#' @param save_failed_json_to If set then the JSON for this request will be saved to the named file
#'   in your Displayr Drive.  This is helpful when trying to reproduce a problem for debugging.
#'
#' @return The value of `data` that was passed in, so caller can see data uploaded if this is the
#'   last call in R code.
#'
#' @importFrom RJSONIO toJSON
#' @export
UploadRelationshipToFactbase <- function(data, token, mode="replace_all", save_failed_json_to=NULL) {
    if (!is.data.frame(data))
        # Include the data in the error message because often this will be an SQL error,
        # returned instead of a data.frame.  This makes it easier for users to spot the problem.
        stop(paste("'data' must be a data.frame, but got", format(data)))
    if (length(data) < 2)
        stop("There must be at least two columns in 'data'")
    original_data <- data

    # Build dimensions.
    dimensions <- mapply(function(v, name, i) {
        list(
            name=name,
            dimensionType="in_data",
            valueType="text")
    }, data, names(data), SIMPLIFY=FALSE, USE.NAMES=FALSE)
    dimensions[[1]]$unique <- TRUE

    # Coerce all to character vectors.
    data <- lapply(data, function(column) { as.character(column) })

    # Structure observations as a list of lists for toJSON.
    list_for_observation <- function(...) {
        list(...)
    }
    mapply_args <- c(list_for_observation, unname(data), list(SIMPLIFY=FALSE, USE.NAMES=FALSE))
    observations <- do.call("mapply", mapply_args)

    # Make HTTP request
    body <- toJSON(list(
        relationship=list(
            type="many_to_one"
        ),
        update=mode,
        dimensions=dimensions,
        data=observations
    ), digits=15, .na="null")
    message(paste("Dimensions:", paste(vapply(dimensions, function(d) {d$name}, ""),
        collapse=", ")))
    post_to_factbase(body, token, save_failed_json_to)

    original_data
}
