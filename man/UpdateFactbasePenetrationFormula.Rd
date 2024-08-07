% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Factbase.R
\name{UpdateFactbasePenetrationFormula}
\alias{UpdateFactbasePenetrationFormula}
\title{Creates or updates a metric described by a formula over other metrics.
See https://factbase.azurewebsites.net/static/pages/help.html#penetration}
\usage{
UpdateFactbasePenetrationFormula(
  metric_name,
  token,
  numerator,
  denominator,
  dimensions_to_count,
  definition,
  hyperlink,
  owner,
  as_percentage = F,
  test = list()
)
}
\arguments{
\item{metric_name}{The name that will appear for selection by Factbase users.}

\item{token}{A Personal Access Token generated in Factbase at Add data > personal access tokens.}

\item{numerator}{The name of an existing metric.  See the documentation reference above.}

\item{denominator}{The name of an existing metric.  See the documentation reference above.}

\item{dimensions_to_count}{A character vector of label dimension names.  See the documentation
reference above.}

\item{definition}{A detailed explanation of the meaning and derivation of the data.}

\item{hyperlink}{A link to a web page where more can be read about the data.
Preferably this is a link into the system that calls this function.}

\item{owner}{The name (usually an email address) of whoever should be contacted to deal with problems
or questions about this data.}

\item{as_percentage}{If set to TRUE then when shown this formula's values will be multiplied
by 100 and given a percent sign.}

\item{test}{(optional) For testing only.  Ignore.}
}
\value{
The value of `data` that was passed in, so caller can see data uploaded if this is the
  last call in R code.
}
\description{
Creates or updates a metric described by a formula over other metrics.
See https://factbase.azurewebsites.net/static/pages/help.html#penetration
}
