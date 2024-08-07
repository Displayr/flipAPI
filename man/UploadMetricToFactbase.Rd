% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Factbase.R
\name{UploadMetricToFactbase}
\alias{UploadMetricToFactbase}
\title{Upload a metric to Factbase.}
\usage{
UploadMetricToFactbase(
  data,
  token,
  name = NULL,
  mode = "replace_all",
  aggregation = "sum",
  time_aggregation = NULL,
  definition = NULL,
  hyperlink = NULL,
  owner = NULL,
  period_type = NULL,
  update_key = NULL,
  as_percentage = F,
  test = list()
)
}
\arguments{
\item{data}{A data.frame with at least two columns, being (in order) for
* measurements (must be numeric).  Optional.  The column name will name the metric.
* date/time (Data Science to specify the formats that normal users would expect to be supported
  in where there will be a lot of data; supply automatic conversion if you think that is
  reasonable).  This column will be identified using its name, which must be `When`.
* dimension 1 (coerced to character).  The column name will be used for the dimension name.
* …
* dimension n}

\item{token}{A Personal Access Token generated in Factbase at Add data > personal access tokens.}

\item{name}{(optional) A name for the metric.  If NULL then the name of the first column in `data` will
be used.}

\item{mode}{(optional) One of "replace_all", "append" or "append_or_update" See comments for
FactPostUpdateType.}

\item{aggregation}{(optional) One of "none", "minimum", "maximum", "sum", "average", "first", "last",
"median", "percentile90", "percentile95", "countdistinct(<dimensionname>)" (where <dimensionname>
is one of the label dimensions in data).}

\item{time_aggregation}{(optional) One of "minimum", "maximum", "sum", "average", "first", "last".
If supplied then this operation will be used when aggregating data in different periods,
and `aggregation` will only be used to aggregate data in different label dimensions.}

\item{definition}{A detailed explanation of the meaning and derivation of the data.}

\item{hyperlink}{A link to a web page where more can be read about the data.
Preferably this is a link into the system that calls this function.}

\item{owner}{The name (usually an email address) of whoever should be contacted to deal with problems
or questions about this data.}

\item{period_type}{(optional) One of "day", "week", "month", "quarter" or "year".
This indicates that the data has been pre-aggregated into periods of that duration.
The When column should contain date/times for the _start_ of each period.
There may be no duplicate values, and the When column will be used to match data
(see update_key).  In this situation you will typically have multiple calls to this function:
one for each period type that your data is aggregated to.  To the user they will appear as a single
metric, but Factbase will automatically choose the correct data according to the query.}

\item{update_key}{(optional) The name of a column that can be used to update the data, when `mode` is
"append_or_update".  Data in this column must be unique, which implies some sort of aggregation
for date/time data.}

\item{as_percentage}{If set to TRUE then when shown this formula's values will be multiplied
by 100 and given a percent sign.}

\item{test}{(optional) For testing only.  Ignore.}
}
\value{
The value of `data` that was passed in, so caller can see data uploaded if this is the
  last call in R code.
}
\description{
Upload a metric to Factbase.
}
