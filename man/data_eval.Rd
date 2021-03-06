% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_eval.R
\name{data_eval}
\alias{data_eval}
\title{Create tibbles from user objects and/or user data}
\usage{
data_eval(.data = NULL, ..., .allow_null = FALSE)
}
\arguments{
\item{.data}{A data.frame/tibble, or NULL}

\item{...}{Arguments are passed to \link[dplyr]{transmute} if \code{data} is
present, and \link[tibble]{tibble} if it is not.}

\item{.allow_null}{Flag to allow NULL values as arguments
(these will not appear as columns in the output)}
}
\value{
A \link[tibble]{tibble} with the results of ...
}
\description{
Create tibbles from user objects and/or user data
}
