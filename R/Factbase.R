# Many of these routines have a `test` parameter, which is only used during testing or debugging.
#
# List entries:
# $save_failed_json_to  If set then the JSON for this request will be saved to the named file
#   in your Displayr Drive.  This is helpful when trying to reproduce a problem for debugging.
# $force_parquet  Pushes uploads to use parquet even when they would prefer JSON.
# $factbase_host  Override the hostname for Factbase.  e.g. factbase-master.azurewebsites.net


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
#' @param name (optional) A name for the metric.  If NULL then the name of the first column in `data` will
#'   be used.
#' @param mode (optional) One of "replace_all", "append" or "append_or_update" See comments for
#'   FactPostUpdateType.
#' @param aggregation (optional) One of "none", "minimum", "maximum", "sum", "average", "first", "last".
#' @param time_aggregation (optional) One of "minimum", "maximum", "sum", "average", "first", "last".
#'   If supplied then this operation will be used when aggregating data in different periods,
#'   and `aggregation` will only be used to aggregate data in different label dimensions.
#' @param definition A detailed explanation of the meaning and derivation of the data.
#' @param hyperlink A link to a web page where more can be read about the data.
#'   Preferably this is a link into the system that calls this function.
#' @param owner The name (usually an email address) of whoever should be contacted to deal with problems
#'   or questions about this data.
#' @param period_type (optional) One of "day", "week", "month", "quarter" or "year".
#'   This indicates that the data has been pre-aggregated into periods of that duration.
#'   The When column should contain date/times for the _start_ of each period.
#'   There may be no duplicate values, and the When column will be used to match data
#'   (see update_key).  In this situation you will typically have multiple calls to this function:
#'   one for each period type that your data is aggregated to.  To the user they will appear as a single
#'   metric, but Factbase will automatically choose the correct data according to the query.
#' @param update_key (optional) The name of a column that can be used to update the data, when `mode` is
#'   "append_or_update".  Data in this column must be unique, which implies some sort of aggregation
#'   for date/time data.
#' @param test (optional) For testing only.  Ignore.
#' 
#' @return The value of `data` that was passed in, so caller can see data uploaded if this is the
#'   last call in R code.
#'
#' @importFrom RJSONIO toJSON
#' @importFrom flipTime AsDateTime
#' @export
UploadMetricToFactbase <- function(data, token, name=NULL, mode="replace_all", aggregation="sum",
        time_aggregation=NULL, definition=NULL, hyperlink=NULL, owner=NULL,
        period_type=NULL, update_key=NULL,
        test=list()) {
    if (!is.data.frame(data))
        # Include the data in the error message because often this will be an SQL error,
        # returned instead of a data.frame.  This makes it easier for users to spot the problem.
        stop(paste("'data' must be a data.frame, but got", format(data)))
    if (length(is.data.frame) == 0)
        stop("There must be at least one column in 'data'")
    if (!(aggregation %in% c("none", "minimum", "maximum", "sum", "average", "first", "last")))
        stop(paste("Unknown 'aggregation':", aggregation))
    if (!is.null(time_aggregation) && !(time_aggregation %in% c("none", "minimum", "maximum", "sum", "average", "first", "last")))
        stop(paste("Unknown 'time_aggregation':", time_aggregation))
    if (!is.null(period_type) && !(period_type %in% c("day", "week", "month", "quarter", "year")))
        stop(paste("Unknown 'period_type':", period_type))
    if (!is.null(update_key)) {
        if (!is.character(update_key))
            stop("'update_key' must be character data")
        if (length(update_key) != 1)
            stop("'update_key' currently only supports a single column name.  Complain to us if this is causing you trouble")
    }
    ensureDefinitionHyperlinkOwnerSupplied(definition, hyperlink, owner)
    
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
            unique=!is.null(update_key) && update_key == column_names[when_column]
        )
    )
    if (!is.null(period_type))
        time_dimension[[1]]$valueForTheseObservations <- list(day='Day',week='Week',month='Month',quarter='Quarter',year='Year')[[period_type]]
    data[[when_column]] <- datetimes_for_factbase(data[[when_column]])
    
    for (dimension_i in dimension_columns) { 
        data[[dimension_i]] <- as.character(data[[dimension_i]])
    }
    dimension_names <- column_names[dimension_columns]
    text_dimensions <- mapply(function(v, name) {
        list(
            name=name,
            dimensionType="in_data",
            valueType="text",
            unique=!is.null(update_key) && update_key == name)
    }, dimension_names, dimension_names, SIMPLIFY=FALSE, USE.NAMES=FALSE)
    dimensions <- c(time_dimension, text_dimensions)

    observations <- dataframe_to_json_ready_observations(data)
    
    # Make HTTP request
    metric <- list(
        name=metric_name,
        valueType="real",
        aggregation=aggregation,
        timeAggregation=time_aggregation
    )
    metric <- add_definition_etc(metric, definition, hyperlink, owner)
    body <- toJSON(list(
        metric=metric,
        update=mode,
        dimensions=dimensions,
        data=observations
    ), digits=15, .na="null")  # May need in future: .inf="null"
    post_json_to_factbase(to_url("fact", test=test), body, token, test)

    original_data
}

is_when_column <- function(column_name) {
    tolower(column_name) %in% c("_when", "when")  # `_when` retained for compatibility with old callers
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

dataframe_to_json_ready_observations <- function(data) {
    # Structure observations as a list of lists for toJSON.
    list_for_observation <- function(...) {
        list(...)
    }
    mapply_args <- c(list_for_observation, unname(data), list(SIMPLIFY=FALSE, USE.NAMES=FALSE))
    do.call("mapply", mapply_args)
}

datetimes_for_factbase <- function(v) {
    posixct <- AsDateTime(v)
    as.numeric(posixct) * 1000  # from POSIXct (seconds since 1970) to JavaScript (ms since 1970)
}

add_definition_etc <- function(l, definition, hyperlink, owner) {
    if (!is.null(definition))
        l$definition <- definition
    if (!is.null(hyperlink))
        l$hyperlink <- hyperlink
    if (!is.null(owner))
        l$owner <- owner
    l
}

to_url <- function(..., test=list()) {
    hostname <- if (is.character(test$factbase_host)) test$factbase_host[1] else "factbase.azurewebsites.net"
    base_url <- paste0('https://', hostname, '/')
    do.call(paste0, c(base_url, list(...)))
}

post_json_to_factbase <- function(url, body, token, test) {
    if (Encoding(body) == "latin1")
        stop("'body' must be supplied encoded as 'UTF-8' or 'unknown', but we got 'latin1'")
    body_size <- nchar(body, type="bytes")
    post_to_factbase(url, "application/json", body, body_size, token, test)
}

#' @importFrom httr timeout add_headers content
post_to_factbase <- function(url, mime_type, body, body_size, token, test) {
    stop_if_request_too_large(body_size)
    message(paste0("POSTing ", body_size, " bytes to ", Sys.info()["nodename"], " at ", Sys.time()))
    headers <- add_headers(
        `x-facttoken` = token,
        `content-type` = mime_type)
    r <- httrPOST(url, body=body, headers, timeout(3600))
    if (r$status_code != 200) {
        if (!is.null(test$save_failed_json_to)) {
            connection <- QFileOpen(test$save_failed_json_to, "w",
                                    mime.type=mime_type)
            close(connection)
        }
        stop(paste0(r$status_code, ": ", content(r, "text")))
    }
}

# Used instead of POST so that we can mock it.
#' @importFrom httr POST
httrPOST <- function(url=NULL, config=list(), ..., body=NULL, encode=c("multipart", "form", "json", "raw"), handle=NULL) {
    arg_list <- c(list(url=url, config=config), list(...), list(body=body, encode=encode, handle=handle))
    do.call(POST, arg_list)
}

stop_if_request_too_large <- function (request_body_size) {
    MAX_BODY_SIZE <- 500000000  # matches FUNCTIONS_REQUEST_BODY_SIZE_LIMIT in portal > Factbase > Configuration
    if (request_body_size > MAX_BODY_SIZE)
        stop(paste0("Your data uses ", request_body_size, "bytes, but the limit is ", MAX_BODY_SIZE), ".  Reduce the quantity of data you are sending.")
}

#' Upload a relationship to Factbase.
#'
#' @inheritParams UploadMetricToFactbase
#' @param data A data.frame with at least two columns, each of which should be coerced to character
#'   vectors.  The first column is the dimension we are mapping from.  Subsequent columns contain
#'   labels in dimensions that we are mapping to.  The names of these columns to be used as the
#'   names of these dimensions.
#' @param token A guid that identifies and authenticates the request.  Talk to Oliver if you need
#'   one of these.
#'
#' @return The value of `data` that was passed in, so caller can see data uploaded if this is the
#'   last call in R code.
#'
#' @importFrom RJSONIO toJSON
#' @export
UploadRelationshipToFactbase <- function(data, token, mode="replace_all",
        definition=NULL, hyperlink=NULL, owner=NULL, test=list()) {
    if (!is.data.frame(data))
        # Include the data in the error message because often this will be an SQL error,
        # returned instead of a data.frame.  This makes it easier for users to spot the problem.
        stop(paste("'data' must be a data.frame, but got", format(data)))
    if (length(data) < 2)
        stop("There must be at least two columns in 'data'")
    ensureDefinitionHyperlinkOwnerSupplied(definition, hyperlink, owner)
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
    relationship <- list(
        type="many_to_one"
    )
    relationship <- add_definition_etc(relationship, definition, hyperlink, owner)
    body <- toJSON(list(
        relationship=relationship,
        update=mode,
        dimensions=dimensions,
        data=observations
    ), digits=15, .na="null")
    post_json_to_factbase(to_url("fact", test=test), body, token, test)

    original_data
}


#' Upload a table of raw data to Factbase.
#'
#' @inheritParams UploadMetricToFactbase
#' @param table_name The name to use to refer to this data in Factbase.
#' @param data A data.frame containing columns of data.  Character, factor (converted to character),
#'   numeric, boolean (converted to character) and date/time (`Date` or `POSIXt`) columns are acceptable.
#' @param na_columns (optional) If set then this should be a character vector naming the
#'   columns that may contain NAs, which will be converted into nulls int the resultant table.
#'
#' @return The value of `data` that was passed in, so caller can see data uploaded if this is the
#'   last call in R code.
#'
#' @importFrom flipTime AsDateTime
#' @importFrom arrow write_parquet BufferOutputStream
#' @importFrom RJSONIO toJSON
#' @export
UploadTableToFactbase <- function(table_name, data, token, mode="replace_all", definition=NULL, hyperlink=NULL, owner=NULL, na_columns=NULL, test=list()) {
    if (!is.character(table_name))
        stop('table_name must be a unitary character vector')
    if (!is.data.frame(data))
        # Include the data in the error message because often this will be an SQL error,
        # returned instead of a data.frame.  This makes it easier for users to spot the problem.
        stop(paste("'data' must be a data.frame, but got", format(data)))
    if (length(data) < 1)
        stop("There must be at least one column in 'data'")
    ensureDefinitionHyperlinkOwnerSupplied(definition, hyperlink, owner)
    if (!is.null(na_columns)) {
        if (!is.character(na_columns))
            stop("'na_columns' must be character data")
    }
    original_data <- data
    
    ndata_points <- nrow(data) * length(data)
    use_parquet <- ndata_points > 1000000 || isTRUE(test$force_parquet)
    endpoint <- "table"
    if (use_parquet) {
        # Row-oriented Apache parquet format
        stream <- BufferOutputStream$create()
        write_parquet(data, sink=stream)
        buffer <- stream$finish()
        body <- buffer$data()
        body_size <- length(body)
        url <- to_url(
            endpoint,
            "?table=", URLencode(table_name, reserved=T),
            "&update=", URLencode(mode, reserved=T),
            "&definition=", URLencode(definition, reserved=T),
            "&hyperlink=", URLencode(hyperlink, reserved=T),
            "&owner=", URLencode(owner, reserved=T),
            test=test)
        post_to_factbase(url, 'application/vnd.apache.parquet', body, body_size, token, test)
    } else {
        # Ye olde JSON format.  Simple to understand, but slow.  Large quantities of row-oriented
        # JSON is very slow to produce (30 mins for 400MB).
        columns <- mapply(function(v, name, i) {
            nullable <- if(is.null(na_columns)){F}else{name %in% na_columns}
            if (!nullable && any(is.na(v)))
                stop(paste0('data[["', name, '"]] contains NAs.  Factbase will accept these and convert them into nulls if you supply this column name in the na_columns parameter'))
            list(
                name=name,
                valueType=value_type_for_vector(v, name),
                mayContainNulls=nullable)
        }, data, names(data), SIMPLIFY=FALSE, USE.NAMES=FALSE)
        
        data <- data.frame(mapply(function(v, name) {
            if (value_type_for_vector(v, name) == "datetime")
                datetimes_for_factbase(v)
            else
                v
        }, data, names(data), SIMPLIFY=FALSE));
        
        observations <- dataframe_to_json_ready_observations(data)
        
        body <- list(
            tableName=table_name,
            update=mode,
            columnDefinitions=columns,
            rows=observations
        )
        body <- add_definition_etc(body, definition, hyperlink, owner);
        
        request_body <- toJSON(body, digits=15, .na="null")
        post_json_to_factbase(to_url(endpoint, test=test), request_body, token, test)
    }
    original_data
}

value_type_for_vector <- function(v, column_name) {
    if (is.null(v))
        stop('Columns in `data` may not by NULL')
    else if (inherits(v, c("Date", "POSIXt")))
        "datetime"
    else if (is.character(v) || is.factor(v))
        "text"
    else if (is.numeric(v))
        "real"
    else
        stop(paste('Cannot work out which data type to use for column', column_name, 'containing a', typeof(v), 'vector.  Only Date, POSIXt, text or real are accepted'))
}

ensureDefinitionHyperlinkOwnerSupplied <- function(definition, hyperlink, owner) {
    if (is.null(definition))
        stop("argument \"definition\" is missing")
    if (!is.character(definition) || length(definition) != 1)
        stop("argument \"definition\" must be a character vector of length 1")
    if (is.null(hyperlink))
        stop("argument \"hyperlink\" is missing")
    if (!is.character(hyperlink) || length(hyperlink) != 1)
        stop("argument \"hyperlink\" must be a character vector of length 1")
    if (is.null(owner))
        stop("argument \"owner\" is missing")
    if (!is.character(owner) || length(owner) != 1)
        stop("argument \"owner\" must be a character vector of length 1")
}

#' Creates or updates a metric described by a formula over other metrics.
#' See https://factbase.azurewebsites.net/static/pages/help.html#penetration
#'
#' @inheritParams UploadMetricToFactbase
#' @param metric_name The name that will appear for selection by Factbase users.
#' @param numerator The name of an existing metric.  See the documentation reference above.
#' @param denominator The name of an existing metric.  See the documentation reference above.
#' @param dimensions_to_count A character vector of label dimension names.  See the documentation
#'  reference above.
#'
#' @return The value of `data` that was passed in, so caller can see data uploaded if this is the
#'   last call in R code.
#'
#' @importFrom RJSONIO toJSON
#' @export
UpdateFactbasePenetrationFormula <- function(metric_name, token, numerator, denominator, dimensions_to_count, definition, hyperlink, owner, test=list()) {
    if (!is.character(metric_name) || length(metric_name) != 1)
        stop("metric_name must be a character vector of length 1")
    if (!is.character(token) || length(token) != 1)
        stop("token must be a character vector of length 1")
    if (!is.character(numerator) || length(numerator) != 1)
        stop("numerator must be a character vector of length 1")
    if (!is.character(denominator) || length(denominator) != 1)
        stop("denominator must be a character vector of length 1")
    if (!is.character(dimensions_to_count) || length(metric_name) < 1)
        stop("dimensions_to_count must contain a character vector with a length of at least 1")
    ensureDefinitionHyperlinkOwnerSupplied(definition, hyperlink, owner)

    body <- toJSON(list(
        type="penetration",
        numeratorMetricName=numerator,
        denominatorMetricName=denominator,
        dimensionsToCount=list(dimensions_to_count)
    ))

    url <- to_url(
        "formula?metric=", URLencode(metric_name, reserved=T),
        "&definition=", URLencode(definition, reserved=T),
        "&hyperlink=", URLencode(hyperlink, reserved=T),
        "&owner=", URLencode(owner, reserved=T),
        test=test)
    post_json_to_factbase(url, body, token, test)
}

#' Creates or updates a metric described by a formula over two existing metrics.
#' See https://factbase.azurewebsites.net/static/pages/help.html#ratio
#'
#' @inheritParams UpdateFactbasePenetrationFormula
#' @param smoothing.window (optional) The period over which to smooth the data.  One of "day", "week",
#'   "month", "quarter" or "year".  See the documentation reference above.
#' @param smoothing.sum (optional) TRUE to smooth using a rolling sum.  If not specified then a rolling
#'   average will be used.  See the documentation reference above.
#'
#' @return The value of `data` that was passed in, so caller can see data uploaded if this is the
#'   last call in R code.
#'
#' @importFrom RJSONIO toJSON
#' @export
UpdateFactbaseRatioFormula <- function(metric_name, token, numerator, denominator, definition, hyperlink, owner, smoothing.window=NULL, smoothing.sum=F, test=list()) {
    if (!is.character(metric_name) || length(metric_name) != 1)
        stop("metric_name must be a character vector of length 1")
    if (!is.character(token) || length(token) != 1)
        stop("token must be a character vector of length 1")
    if (!is.character(numerator) || length(numerator) != 1)
        stop("numerator must be a character vector of length 1")
    if (!is.character(denominator) || length(denominator) != 1)
        stop("denominator must be a character vector of length 1")
    if (!is.null(smoothing.window) && !(smoothing.window %in% c("day", "week", "month", "quarter", "year")))
        stop(paste("Unknown 'smoothing.window':", smoothing.window))
    if (!is.logical(smoothing.sum) || length(smoothing.sum) != 1)
        stop("smoothing.sum must be a logical vector of length 1")
    ensureDefinitionHyperlinkOwnerSupplied(definition, hyperlink, owner)
    
    body <- list(
        type="ratio",
        numeratorMetricName=numerator,
        denominatorMetricName=denominator
    )
    if (!is.null(smoothing.window))
        body$smoothing <- list(
            window=smoothing.window,
            sum=smoothing.sum
        )
    
    json <- toJSON(body)

    url <- to_url(
        "formula?metric=", URLencode(metric_name, reserved=T),
        "&definition=", URLencode(definition, reserved=T),
        "&hyperlink=", URLencode(hyperlink, reserved=T),
        "&owner=", URLencode(owner, reserved=T),
        test=test)
    message(json)
    post_json_to_factbase(url, json, token, test)
}

#' WARNING: THIS FEATURE IS INCOMPLETE.  DO NOT USE THIS FUNCTION.
#' 
#' Adds provenance information to data that will be uploaded to Factbase.  This should be called
#' in steps prior to the operation that sends the data to Factbase so that chains of calculations
#' can be shown to the user, and so that Factbase can determine the step at which a chain of
#' calculations has broken.
#'
#' @inheritParams UploadMetricToFactbase
#' @param x An object containing data that will probably eventually find its way into Factbase.
#' @param description A description of where `x` came from.
#'
#' @return `x` with provenance information added as an attribute.  The new information will be appended
#'   to any existing provenance.
#'
#' @export
AddFactbaseProvenance <- function (x, description, hyperlink=NA_character_, owner=NA_character_) {
    if (is.null(x))
        stop("x may not be NULL")
    if (!is.character(description) || length(description) != 1)
        stop("description must be a character vector of length 1")
    if (!is.character(hyperlink) || length(hyperlink) != 1)
        stop("hyperlink must be a character vector of length 1")
    if (!is.character(owner) || length(owner) != 1)
        stop("owner must be a character vector of length 1")
    
    provenance <- data.frame(description=description, hyperlink=hyperlink, owner=owner, when=Sys.time())
    existing_provenance <- attr(x, "factbase.provenance")
    if (!is.null(existing_provenance)) {
        if (!is.data.frame(existing_provenance))
            stop("factbase.provenance attribute is not a data.frame!")
        provenance <- rbind(existing_provenance, provenance)
    }
    attr(x, "factbase.provenance") <- provenance
    x
}
