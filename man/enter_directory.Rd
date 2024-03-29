% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dir_functions.R
\name{enter_directory}
\alias{enter_directory}
\alias{go_proj_directory}
\alias{go_up_directory}
\alias{list_file_type}
\title{Navigate between directories and list directory content}
\usage{
enter_directory(path, create = FALSE, showWarnings = TRUE, ...)

go_proj_directory()

go_up_directory()

list_file_type(path = ".", extension = ".txt")
}
\arguments{
\item{path}{a character vector containing a single path name.}

\item{create}{logical; should the directory be created if it does
not exist yet?}

\item{showWarnings}{logical; should the warnings on failure be shown?}

\item{...}{other arguments passed to dir.create().}

\item{extension}{file extension.}
}
\value{
Nothing, called for its side effects.
}
\description{
\code{enter_directory()} sets working directory.
\code{go_proj_directory()} sets the working directory back to the RStudio
project directory.
\code{go_up_directory()} moves up in the directory tree relative to the current
working directory.
\code{list_file_type()} lists all files in the given directory with the
given extension.
}
\details{
Wrappers around the base functions \code{dir.exists()} and \code{dir.create()}.
If \code{create = FALSE} in \code{enter_directory()}, the function raises an error,
if the path does not exist.
}
