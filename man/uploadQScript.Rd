% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DataMart.R
\name{uploadQScript}
\alias{uploadQScript}
\title{Upload a QScript file to Displayr drive or collate many JS files.}
\usage{
uploadQScript(
  ...,
  filename = NULL,
  check.include.web = TRUE,
  upload = TRUE,
  api.root = Sys.getenv("API_ROOT"),
  company.secret = Sys.getenv("COMPANY_SECRET"),
  client.id = Sys.getenv("CLIENT_ID"),
  send.to = ""
)
}
\arguments{
\item{...}{An arbitrary number of strings that contain the paths to js files
to construct the QScript.}

\item{filename}{Name of the output file QScript.
If only a single file is provided then it is deduced by the input file name by default.
Otherwise the filename argument must be provided.}

\item{check.include.web}{A logical to remove any includeWeb calls if the input files
are used in the construction. E.g. if testing a Calculation feature the script might use
a custom QScript Functions for Calculations.js and then call one of those functions with
includeWeb. If any matches are found like this, then the line with the includeWeb is omitted.}

\item{upload}{A logical whether to upload the script or not (useful to check the script is good before upload).}

\item{api.root}{API root URL obtained by running \code{flipAPI:::getApiRoot()} in a
Calculation in Displayr}

\item{company.secret}{A string containing the appropriate company secret to access the required Displayr drive}

\item{client.id}{Client ID obtained by running \code{flipAPI:::getApiRoot()} in a
Calculation in Displayr}

\item{send.to}{Character string the can be used to more conveniently specify
an alternative company (i.e. \code{api.root}, \code{company.secret}, and
\code{client.id}) to upload the QScript to. See the Details.}
}
\description{
Take an arbitrary number of js files and either upload them as
  a custom qscript via Displayr Drive or save them locally as a single file.
}
\details{
If the \code{send.to} argument is provided, the inputs for \code{api.root},
\code{company.secret}, and \code{client.id} are ignored and it is assumed that
the user has additional environment variables setup that are used instead. For
example, if \code{send.to = "JANE"}, then the function uses
\code{Sys.getenv("COMPANY_SECRET_JANE")} for \code{company.secret}
(and similarly for \code{client.id} and \code{api.root}).
}
