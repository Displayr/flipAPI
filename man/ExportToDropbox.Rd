% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Dropbox.R
\name{ExportToDropbox}
\alias{ExportToDropbox}
\title{Export R object to Dropbox}
\usage{
ExportToDropbox(object, token, file = NA, reexport.seconds = 600)
}
\arguments{
\item{object}{R object to export}

\item{token}{Dropbox access token with read and write access.}

\item{file}{Name of output file. If no filename is supplied, by default the object will be saved to a file
named \code{<object>.rds}. This will overwrite existing files in your dropbox with the same name.
When multiple copies of the same object under different conditions are saved, it may be preferrable to
manually specify the filename.}

\item{reexport.seconds}{Time in seconds after which object will be re-exported. This option works only in Displayr. See \link{TriggerObjectUpdate} for more details.}
}
\description{
Export R object to Dropbox
}