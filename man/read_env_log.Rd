% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/envlogger.R
\name{read_env_log}
\alias{read_env_log}
\title{Read an EnvLogger logfile}
\usage{
read_env_log(path, check = TRUE)
}
\arguments{
\item{path}{Path to an EnvLogger logfile}

\item{check}{Logical, whether or not to check if path points to an EnvLogger file. Defaults to \code{TRUE}. Only meant to be \code{FALSE} when called from within \code{read_env()}, so as not to repeat the same check multiple times. \strong{Do not set it to \code{FALSE} when running this function directly.}
#'}
}
\value{
A tibble with one row for each interaction with an EnvLogger and 15 columns with relevant metadata.
}
\description{
Given a file path, check if it points to an EnvLogger logfile and, if \code{TRUE}, read it.
}
\examples{
path <- rtools_example("log_")[1]
read_env_log(path)
}
\seealso{
\code{\link[=read_env_header]{read_env_header()}}, \code{\link[=read_env_data]{read_env_data()}}, \code{\link[=read_env]{read_env()}}, \code{\link[=plot_env]{plot_env()}}
}
