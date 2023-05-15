#' Upload a metric to Factbase.
#'
#' @param data A data.frame with at least two columns, being (in order) for
#'   * measurements (must be numeric).  Optional.  The column name will name the metric.
#'   * date/time (Data Science to specify the formats that normal users would expect to be supported
#'     in where there will be a lot of data; supply automatic conversion if you think that is
#'     reasonable).  This column will be identified using its name, which must be `When`.
#'   * dimension 1 (coerced to character).  The column name will be used for the dimension name.
#'   * …
#'   * dimension n
#' @param token A guid that identifies and authenticates the request.  Talk to Oliver if you need
#'   one of these.
#' @param name A name for the metric.  If NULL then the name of the first column in `data` will
#'   be used.
#' @param mode One of "replace_all", "append" or "append_or_update" See comments for
#'   FactPostUpdateType.
#' @param aggregation One of "none", "minimum", "maximum", "sum", "average", "first", "last".
#' @param definition A detailed explanation of the meaning and derivation of the metric.
#' @param hyperlink A link to a web page where more can be read about the metric.
#' @param period_type (Optional) One of "day", "week", "month", "quarter" or "year".
#'   This indicates that the data has been pre-aggregated into periods of that duration.
#'   Then When column should contain date/times for the _start_ of each period.
#'   There may be no duplicate values, and the When column will be used to match data
#'   (see update_key).
#' @param update_key The name of a column that can be used to update the data, when `mode` is
#'   "append_or_update".  Data in this column must be unique, which implies some sort of aggregation
#'   for date/time data.
#' @param save_failed_json_to If set then the JSON for this request will be saved to the named file
#'   in your Displayr Drive.  This is helpful when trying to reproduce a problem for debugging.
#' @param test_return_json For testing only.  Ignore.
#' 
#' @return The value of `data` that was passed in, so caller can see data uploaded if this is the
#'   last call in R code.
#'
#' @importFrom RJSONIO toJSON
#' @importFrom flipTime AsDateTime
#' @export
UploadMetricToFactbase <- function(data, token, name=NULL, mode="replace_all", aggregation="sum",
        definition=NULL, hyperlink=NULL, period_type = NULL, update_key=NULL,
        save_failed_json_to=NULL, test_return_json=FALSE) {
    if (!is.data.frame(data))
        # Include the data in the error message because often this will be an SQL error,
        # returned instead of a data.frame.  This makes it easier for users to spot the problem.
        stop(paste("'data' must be a data.frame, but got", format(data)))
    if (length(is.data.frame) == 0)
        stop("There must be at least one column in 'data'")
    if (!(aggregation %in% c("none", "minimum", "maximum", "sum", "average", "first", "last")))
        stop(paste("Unknown 'aggregation':", aggregation))
    if (!is.null(period_type) && !(period_type %in% c("day", "week", "month", "quarter", "year")))
        stop(paste("Unknown 'period_type:'", period_type))

    # Build dimensions.
    original_data <- data
    data <- c(data)  # avoid modifying caller's data.frame
    column_names <- names(data)
    when_column <- find_when_column(column_names)
    if (when_column == 1) {
        if (is.null(name))
           stop("You have not included a column for the metric so you must supply the metric name in the `name` argument")  #nolint
    } else {
        if (!is.numeric(data[[1]]))
            stop("The first column in 'data' must contain the metric and be numeric, or if there is no metric then the first column must be called `When` and contain date/time data")  #nolint
    }
    metric_name <- if (is.null(name)) column_names[1] else name
    dimension_columns <- (when_column+1):length(column_names)
    time_dimension <- list(
        list(
            name="_When",
            dimensionType=if (is.null(period_type)) "in_data" else "period_type_in_table_name",
            valueType="datetime",
            unique=!is.null(update_key) && update_key == column_names[when_column] || !is.null(period_type)
        )
    )
    if (!is.null(period_type))
        time_dimension[[1]]$valueForTheseObservations <- period_type
    data[[when_column]] <- AsDateTime(data[[when_column]])
    data[[when_column]] <- as.numeric(data[[when_column]]) * 1000  # convert from POSIXct (seconds since 1970)
                                                                   # to JavaScript (ms since 1970)
    dimension_data <- lapply(data[dimension_columns], function(column) { as.character(column)})
    dimension_names <- column_names[dimension_columns]
    text_dimensions <- mapply(function(v, name) {
        list(
            name=name,
            dimensionType="in_data",
            valueType="text",
            unique=!is.null(update_key) && update_key == name)
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
    if (test_return_json) {
        return(body)
    }
    post_to_factbase(body, token, save_failed_json_to)

    original_data
}

is_when_column <- function(column_name) {
    tolower(column_name) %in% c("_when", "when")  # `_when` retained for compatiblity with old callers
}

find_when_column <- function(column_names) {
    if (is_when_column(column_names[1]))
        1
    else
        if (is_when_column(column_names[2]))
            2
        else
            # The HTTP API was designed to handle datetime-less data, but this hasn't 
            # been tested and probably does not work.  This error has the benefit that
            # it helps people get their input right, when they miss the need for the
            # `When`` column.
            stop("You must include date/time data in a column called 'When'.  Talk to support if you want this constraint relaxed.")  # nolint
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
#' @param test_return_json For testing only.  Ignore.
#'
#' @return The value of `data` that was passed in, so caller can see data uploaded if this is the
#'   last call in R code.
#'
#' @importFrom RJSONIO toJSON
#' @export
UploadRelationshipToFactbase <- function(data, token, mode="replace_all",
        save_failed_json_to=NULL, test_return_json=FALSE) {
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
    if (test_return_json) {
        return(body)
    }
    post_to_factbase(body, token, save_failed_json_to)

    original_data
}
