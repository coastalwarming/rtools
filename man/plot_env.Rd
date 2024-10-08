% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/envlogger.R
\name{plot_env}
\alias{plot_env}
\title{Plot data from one EnvLogger}
\usage{
plot_env(env_data)
}
\arguments{
\item{env_data}{A tibble with EnvLogger data (columns \code{t} and \code{temp})}
}
\value{
A ggplot
}
\description{
Plot data from one EnvLogger
}
\examples{
path <- rtools_example()[3]
env_data <- read_env(path, zero_secs = TRUE)
plot_env(env_data$data[[1]])
}
\seealso{
\code{\link[=read_env]{read_env()}}
}
