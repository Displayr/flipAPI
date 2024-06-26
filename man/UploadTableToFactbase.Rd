% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Factbase.R
\name{UploadTableToFactbase}
\alias{UploadTableToFactbase}
\title{Upload a table of raw data to Factbase.}
\usage{
UploadTableToFactbase(
  table_name,
  data,
  token,
  mode = "replace_all",
  definition = NULL,
  hyperlink = NULL,
  owner = NULL,
  na_columns = NULL,
  unique_columns = NULL,
  test = list()
)
}
\arguments{
\item{table_name}{The name to use to refer to this data in Factbase.}

\item{data}{A data.frame containing columns of data.  Character, factor (converted to character),
numeric, boolean (converted to character) and date/time (`Date` or `POSIXt`) columns are acceptable.}

\item{token}{A Personal Access Token generated in Factbase at Add data > personal access tokens.}

\item{mode}{(optional) One of "replace_all", "append" or "append_or_update" See comments for
FactPostUpdateType.}

\item{definition}{A detailed explanation of the meaning and derivation of the data.}

\item{hyperlink}{A link to a web page where more can be read about the data.
Preferably this is a link into the system that calls this function.}

\item{owner}{The name (usually an email address) of whoever should be contacted to deal with problems
or questions about this data.}

\item{na_columns}{(optional) If set then this should be a character vector naming the
columns that may contain NAs, which will be converted into nulls int the resultant table.}

\item{unique_columns}{(optional) If set then this should be a character vector naming the
columns that will contain values that are all unique.  This allows you to use other values than
'update_all' for 'mode'.}

\item{test}{(optional) For testing only.  Ignore.}
}
\value{
The value of `data` that was passed in, so caller can see data uploaded if this is the
  last call in R code.
}
\description{
Upload a table of raw data to Factbase.
}
