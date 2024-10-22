#' This function is for Displayr internal use only.
#'
#' The R code for the standard R item *Upload to Factbase*, which provides a
#' zero code way to upload a metric using the Factbase R API.
#'
#' Parameters come out of the R UI JavaScript code in
#' https://github.com/Displayr/factbase/blob/master/displayr/Upload%20to%20Factbase.RScript
#' See that code to understand the meaning of those controls.
#'
#' We would prefer not to export this function, but not doing so causes build
#' failures.  We cannot move the function into flipFormat because it calls
#' functions in flipAPI that would trigger circular import dependencies because
#' flipAPI depends on flipFormat.
#'
#' @return An HTMLwidget that shows summary details about the upload.
#'
#' @param factbase.token Undocumented
#' @param mode Undocumented
#' @param aggregation Undocumented
#' @param time_aggregation Undocumented
#' @param period_type Undocumented
#' @param definition Undocumented
#' @param hyperlink Undocumented
#' @param owner Undocumented
#' @param do.upload Undocumented
#' @param selection.type Undocumented
#' @param input.table Undocumented
#' @param make.dummy.metric Undocumented
#' @param metric.variables Undocumented
#' @param metric.column.names Undocumented
#' @param date.column.name Undocumented
#' @param date.variable Undocumented
#' @param dimension.column.names Undocumented
#' @param dimension.variables Undocumented
#' @param output.type Undocumented
#' @param start.date Undocumented
#' @param time.zone Undocumented
#' @param update.period Undocumented
#' @param update.frequency Undocumented
#' @param us.format Undocumented
#'
#' @importFrom flipU ConvertCommaSeparatedStringToVector
#' @export
FactbaseUploadWidget <- function(factbase.token = "",
                                 mode = "Replace all",
                                 aggregation = "None",
                                 time_aggregation = NULL,
                                 period_type = NULL,
                                 definition = NULL,
                                 hyperlink = NULL,
                                 owner = NULL,
                                 do.upload = TRUE,
                                 selection.type = "Table",
                                 input.table = NULL,
                                 make.dummy.metric = NULL,
                                 metric.variables = NULL,
                                 metric.column.names = "",
                                 date.column.name = "",
                                 date.variable = NULL,
                                 dimension.column.names = "",
                                 dimension.variables = NULL,
                                 output.type = "Data Summaries",
                                 start.date = "",
                                 time.zone = "Australia/Sydney",
                                 update.period = "days",
                                 update.frequency = 1,
                                 us.format = FALSE) {
    options(digits.secs = 6)
    automatic.updating <- paste0("Automatic Updating\nLast updated on Displayr's East Coast US Server Time Zone:\n", format(Sys.time(), "%a %b %d  %X %Y"))
    
    
    .nInputs <- function(control.name.prefix = "formTable")
        length(ls(pattern = paste0("^", control.name.prefix, "[0-9]+$"),
                  envir = .GlobalEnv))
    
    getNewVariableNamesFromGui <- function(base.name) {
        n.names <- .nInputs(base.name)
        names.from.gui <- vapply(seq_len(n.names),
                                 FUN = function(x, base.name) get0(paste0(base.name, x)),
                                 FUN.VALUE = character(1),
                                 base.name = base.name)
    }
    
    # Organize Data
    if (selection.type == "Table") {
        input.data <- input.table
        
        if (make.dummy.metric) {
            input.data <- cbind("_Dummy" = rep(1, nrow(input.data)), input.data)
            selected.metric.columns <- "_Dummy"
        } else {
            selected.metric.columns <- ConvertCommaSeparatedStringToVector(metric.column.names)
        }
        
        n.metrics <- length(selected.metric.columns)
        if (n.metrics == 0)
            stop("Select at least one metric column.")
        
        
        selected.dimension.columns <- ConvertCommaSeparatedStringToVector(dimension.column.names)
        n.dimensions <- length(selected.dimension.columns)
        if (n.dimensions == 0)
            stop("Select at least one dimension column.")
        
        selected.date.column <- date.column.name
        
        selected.columns <- c(selected.metric.columns, selected.date.column, selected.dimension.columns)
        missing.columns <- ! selected.columns %in% colnames(input.data)
        if (any(missing.columns)) {
            stop("Some of the selected columns were not found in the input table: ", paste0(selected.columns[missing.columns], collapse = ", "))
        }
        
        metric.data <- input.data[, selected.metric.columns, drop = FALSE]
        date.var <- input.data[, selected.date.column, drop = FALSE]
        colnames(date.var) <- "_When"
        dimension.data <- input.data[, selected.dimension.columns, drop = FALSE]
        
    } else {
        date.var <- date.variable
        if (make.dummy.metric) {
            metric.data <- matrix(rep(1, NROW(date.var)), ncol = 1)
        } else {
            metric.data <- data.frame(metric.variables)
        }
        dimension.data <- data.frame(dimension.variables)
    }
    
    metric.names <- getNewVariableNamesFromGui("formMetricName")
    dimension.names <- getNewVariableNamesFromGui("formDimensionName")
    
    metric.data <- ConditioanllyRenameVariables(metric.data, metric.names)
    dimension.data <- ConditioanllyRenameVariables(dimension.data, dimension.names)
    
    data.list <- list(metric.data, "_When" = date.var, dimension.data)
    
    # Check lengths
    lengths <- vapply(data.list, FUN = NROW, FUN.VALUE = numeric(1))
    if (any(lengths != lengths[1]))
        stop("The metrics, date, and dimensions have different numbers of rows: ", paste0(lengths, collapse = ", "))
    
    input.data <- do.call(cbind, data.list)
    
    data.check <- CheckDataForFactbaseMetricUpload(input.data)
    data.errors <- attr(data.check, "data.errors")
    
    # Check period_type argument
    period_type = ParseFactbaseOption(period_type)
    if (!is.null(period_type) && period_type == "none")
        period_type <- NULL
    
    # Check period_type argument
    time_aggregation = ParseFactbaseOption(time_aggregation)
    if (!is.null(time_aggregation) && time_aggregation == "none")
        time_aggregation <- NULL
    
    # send to factbase
    # have to send 1 metric at a time
    # but all dimensions included each time
    if (do.upload) {
        if (data.errors)
            stop("Errors were detected in the data and it has not been uploaded to Factbase. Uncheck FACTBASE > Upload to Factbase and select OUTPUT > Display > Data Summaries for more details.")
        n.metrics <- NCOL(metric.data)
        non.metric.ind <- (n.metrics + 1):NCOL(input.data)
        for (j in seq_len(n.metrics)) {
            this.upload.data <- input.data[, c(j, non.metric.ind)]
            UploadMetricToFactbase(
                this.upload.data,
                token = factbase.token,
                mode = ParseFactbaseOption(mode),
                aggregation = ParseFactbaseOption(aggregation),
                time_aggregation = time_aggregation,
                period_type = period_type,
                definition = definition,
                hyperlink = hyperlink,
                owner = owner
            )
        }
    } else {
        warning("Not uploading to Factbase. Tick FACTBASE > Upload to Factbase when you are ready to upload.")
    }
    
    
    # Output
    FactbaseOutput(
        output.type = output.type,
        input.data = input.data,
        data.summaries = data.check
    )
}

# Given a data frame intended for factbase,
# comute summary information for each column
#' @importFrom stats complete.cases
DataSummaryForFactbase <- function(df) {
    summaries <- lapply(df, SummarizeFactbaseVariable)
    attr(summaries, "Sample Size") <- nrow(df)
    attr(summaries, "Complete Cases") <- sum(complete.cases(df))
    summaries
}

# Compute summary statistics for a variable based on
# it's type
#' @importFrom flipTime IsDateTime
#' @importFrom stats quantile median
SummarizeFactbaseVariable <- function(x) {
    n.missing = 0
    summary.statistics = ""
    type = "unknown"
    if (is.numeric(x)) {
        n.missing <- sum(is.na(x))
        quant <- quantile(x,
                          probs = c(0.05, 0.25, 0.75, 0.95),
                          na.rm = TRUE)
        s <- c("Minium" = min(x),
               "5th Percentile" = unname(quant["5%"]),
               "25th Percentile" = unname(quant["25%"]),
               "Median" = median(x, na.rm = TRUE),
               "75th Percentile" = unname(quant["75%"]),
               "95th Percentile" = unname(quant["95%"]),
               "Maximum" = max(x))
        
        s <- round(s, 1)
        summary.statistics <- matrix(s, ncol = 1)
        rownames(summary.statistics) = names(s)
        type <- "Numeric"
    }
    
    if (is.factor(x)) {
        n.missing <- sum(is.na(x))
        s <- round(prop.table(table(x))*100, 1)
        s.names <- names(s)
        s <- paste0(s, "%")
        summary.statistics <- matrix(s, ncol = 1)
        rownames(summary.statistics) <- s.names
        type <- "Categorical"
    }
    
    if (IsDateTime(x) || inherits(x, what = c("POSIXct", "POSIXt"))) {
        n.missing <- sum(is.na(x))
        summary.statistics <- c("Start" = min(x, na.rm = TRUE),
                                "End" = max(x, na.rm = TRUE))
        type <- "Date"
    }
    
    if (is.character(x)) {
        n.missing <- sum(is.na(x) | !nzchar(trimws(x)))
        summary.statistics <- c("Unique Entries" = length(unique(x)),
                                "Numeric Entries" = sum(!is.na(suppressWarnings(as.numeric(x)))))
        type <- "Text"
    }
    
    list("Missing" = n.missing,
         "Summary" = summary.statistics,
         "Type" = type)
}

#' @importFrom flipTime IsDateTime
#' @importFrom flipTime AsDate
CheckFactbaseDateVariable <- function(date.var) {
    # Check Date ("_When") column
    date.issues <- character(0)
    if (!IsDateTime(date.var)) {
        try.date.var <- AsDate(date.var, on.parse.failure = "silent")
        date.missing <- is.na(try.date.var)
        if (all(is.na(try.date.var))) {
            date.issues <- "The format of the Date variable could not be detected. This is likely because there it contains a mixture of US and non-US dates, or there are entries which do not resemble dates of supported formats."
        } else {
            date.issues <- paste0("The Date contains ", sum(date.missing), " cases with missing values.")
        }
        
    }
    date.issues
}

# x is a list of summaries of variables generated by
# DataSummaryForFactbase
IdentifyFactbaseDataIssues <- function(x) {
    n.cases <- attr(x, "Sample Size")
    x <- x[names(x) != "_When"]
    v.names <- names(x)
    data.integrity.issues <- character(0)
    possible.data.mess <- character(0)
    for (j in seq_along(x)) {
        variable <- x[[j]]
        type <- variable[["Type"]]
        n.missing <- variable[["Missing"]]
        if (n.missing > 0) {
            if (type == "Categorical") {
                if (n.missing < 10) {
                    data.integrity.issues <- c(data.integrity.issues,
                                               paste0(v.names[j], " has ", n.missing, " missing cases. There may be a data integrity problem."))
                } else {
                    data.integrity.issues <- c(data.integrity.issues,
                                               paste0(v.names[j], " has ", n.missing, " missing cases. Perhaps a new category should be added?"))
                }
            } else if (type == "Date") {
                data.integrity.issues <- c(data.integrity.issues,
                                           paste0(v.names[j], " has ", n.missing, " missing cases. These cases cannot be used in Factbase"))
            } else {
                data.integrity.issues <- c(data.integrity.issues,
                                           paste0(v.names[j], " has ", n.missing, " missing cases."))
            }
            if (type == "Text") {
                if (variable[["Summary"]]["Numeric Entries"] > 0)
                    possible.data.mess <- c(possible.data.mess,
                                            paste0(v.names[j], " looks to contain a mix of numeric and text entries."))
                prop.unique <- variable[["Summary"]]["Unique Entries"] / n.cases
                if (prop.unique > 0.95 && prop.unique < 1 ) {
                    possible.data.mess <- c(possible.data.mess,
                                            paste0(v.names[j], " looks like an ID variable but its entries are not unique."))
                }
            }
        }
    }
    list(data.integrity.issues = data.integrity.issues,
         possible.data.mess = possible.data.mess)
}


FactbaseOutput <- function(input.data, data.summaries, output.type = "Data Summaries") {
    if (output.type == "Data")
        return (input.data)
    
    CreateFactbaseMetricSummary(data.summaries)
}

CheckDataForFactbaseMetricUpload <- function(input.data, warn = TRUE) {
    data.summaries <- DataSummaryForFactbase(input.data)
    date.issues <- CheckFactbaseDateVariable(input.data[, "_When"])
    data.issues <- IdentifyFactbaseDataIssues(data.summaries)
    if (warn) {
        all.issues <- c(date.issues, unlist(data.issues))
        lapply(all.issues, FUN = function (x) warning(x))
    }
    attr(data.summaries, "date.issues") <- date.issues
    attr(data.summaries, "data.issues") <- data.issues
    attr(data.summaries, "data.errors") <- length(date.issues) > 0 || length(data.issues[["data.integrity.issues"]]) > 0
    data.summaries
}

ParseFactbaseOption <- function(selection) {
    if (is.null(selection))
        return(NULL)
    selection <- gsub(" ", "_", selection)
    selection <- tolower(selection)
    selection
}

ConditioanllyRenameVariables <- function(data, names) {
    current.names <- colnames(data)
    override.inds <- which(nzchar(names))
    if (length(override.inds) == 0)
        return(data)
    
    current.names[override.inds] <- names[override.inds]
    colnames(data) <- current.names
    data
}

#' @importFrom knitr kable
CreateFactbaseMetricSummary <- function (x) {
    addTable <-function(data) {
        out <- kable(data, format = "html", col.names = "",
                     table.attr = "class=\"cmd-table-one-stat\"")
        ## change table headers to span multiple columns
        out <- gsub("<th style=\"text-align:right;\">  </th>", "", out, fixed = TRUE)
        out <- gsub("<th style=\"text-align:left;\">",
                    "<th colspan = \"2\">", out, fixed = TRUE)
        return(out)
    }
    
    missing.by.variable <- vapply(x, FUN = function (x) x[["Missing"]], FUN.VALUE = numeric(1))
    variable.summaries <- lapply(x, FUN = function (x) x[["Summary"]])
    variable.types <- vapply(x, FUN = function(x) x[["Type"]], FUN.VALUE = character(1))
    
    
    tfile <- createTempFile()
    cata <- createCata(tfile)
    
    
    
    ## Use same styling as our Choice Modeling - Experimental Design widghet
    addCss("analysisreport.css", cata)
    
    ## Needed so that Box has scollbar
    cata("<div class=\"analysis-report-main-container\">")
    
    ## Title
    cata("<h1>Upload to Factbase</h1>")
    
    ##
    cata("<h2>Sample</h2>")
    cata(paste0("Sample Size: ", attr(x, "Sample Size")))
    cata("<br>")
    cata(paste0("Complete Cases: ", attr(x, "Complete Cases")))
    
    cata("<h3>Missing Data by Variable</h3>")
    
    cata(addTable(missing.by.variable))
    
    cata("<h2>Variable Summaries</h2>")
    for (j in seq_along(variable.summaries)) {
        cata(paste0("<h3>", names(variable.summaries)[j], "</h3>"))
        cata(variable.types[j])
        cata(addTable(variable.summaries[j]))
    }
    
    cata("</div>\n")
    
    html <- paste(readLines(tfile), collapse = "\n")
    ## browseURL(tfile)
    out <- boxIframeless(html, text.as.html = TRUE,
                         font.family = "Circular, Arial, sans-serif",
                         background.color = "White",
                         font.size = 8)
    out
}

# Copied from flipFormat because they are internal and I couldn't figure out
# how to export it without including it in the generated documentation.
# refactor code in CreateChoiceModelDesignWidget
createTempFile <- function()
{
    tfile <- tempfile(fileext = ".html")
    on.exit(if (file.exists(tfile)) file.remove(tfile))
    tfile
}

# Copied from flipFormat because they are internal and I couldn't figure out
# how to export it without including it in the generated documentation.
createCata <- function(tfile)
{
    cata <- function(...)
        cat(..., file = tfile, append = TRUE)
}

# Copied from flipFormat because they are internal and I couldn't figure out
# how to export it without including it in the generated documentation.
addCss <- function(file.name, cata, in.css.folder = TRUE)
{
    file.path <- if (in.css.folder)
        system.file("css", file.name, package = "flipFormat")
    else
        file.name
    if (file.exists(file.path))
    {
        cata("<style>\n")
        cata(readLines(file.path))
        cata("</style>\n\n")
    }
    else
        stop("CSS file ", file.path, " not found.")
}

# Copied from flipFormat because they are internal and I couldn't figure out
# how to export it without including it in the generated documentation.
#' @importFrom rhtmlMetro Box
boxIframeless <- function(...)
{
    w <- Box(...)
    attr(w, "can-run-in-root-dom") <- TRUE
    w
}
