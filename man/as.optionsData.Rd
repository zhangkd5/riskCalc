% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/options.R
\name{as.optionsData}
\alias{as.optionsData}
\alias{as.optionsData.default}
\alias{as.optionsData.stocksData}
\title{Coercion stocksData to optionsData}
\usage{
as.optionsData(x, ...)

\method{as.optionsData}{default}(x, ...)

\method{as.optionsData}{stocksData}(x, ...)
}
\arguments{
\item{x}{an object.}

\item{...}{further arguments to be passed to or from methods.}
}
\value{
An object of class \code{optionsData}
}
\description{
Methods for coercing \code{stocksData} objects to class \code{optionsData}.
}
\details{
If \code{x} is \code{NULL}, the method will return an empty \code{optionsData} object.
}
\seealso{
\code{\link{stocks}}, \code{\link{addOptions}}
}

